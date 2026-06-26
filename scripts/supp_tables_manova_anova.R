# Reproduce MANOVA (Pillai) and follow-up ANOVA (eta^2) results from the
# paleoclimate analysis, for the supplementary tables.
# Replicates the model structure in paleoclimate_analysis.R.
# Run from project root (residual_pd_analyses/).

suppressMessages({library(dplyr); library(data.table); library(sf)})

taxa <- c('amphibians', 'birds', 'mammals', 'squamates')
vars.current <- c('temp', 'prec', 'tempseas', 'precseas', 'npp', 'tri_current')
vars.paleo   <- c('paleotemp_cumchange', 'paleoprec_cumchange',
                  'paleotemp_slope', 'paleoprec_slope')

hexgrid_list <- readRDS('../objects/hexgrid_list_paleoclimate.rds')
hexgrid_list <- lapply(hexgrid_list, function(x) as.data.frame(st_drop_geometry(x)))
sp_cramus    <- readRDS('../objects/sp_cramus.rds')

# rebuild per-region combined data (only cradle/museum cells in designated regions)
hpr <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  parts <- list()
  for (typ in c('cradle', 'museum')){
    for (r in names(sp_cramus[[t]][[typ]])){
      parts[[paste(typ, r)]] <- hexgrid_list[[t]] %>% filter(type == typ & geo %in% r)
    }
  }
  hpr[[t]] <- rbindlist(parts)
}

results_manova <- data.frame()
results_anova  <- data.frame()

for (t in taxa){
  dat <- as.data.frame(hpr[[t]] %>% select(all_of(c(vars.current, vars.paleo, 'type', 'geo'))))
  dat$type <- factor(dat$type); dat$geo <- factor(dat$geo)

  Y_current <- scale(log(dat[, vars.current]))
  Y_paleo   <- cbind(scale(log(dat[, vars.paleo[1:2]])), scale(dat[, vars.paleo[3:4]]))

  for (blk in c('current', 'paleo')){
    Y <- if (blk == 'current') Y_current else Y_paleo
    vnames <- if (blk == 'current') vars.current else vars.paleo
    for (eff in c('type', 'geo')){
      m  <- manova(Y ~ dat[[eff]])
      st <- summary(m, test = 'Pillai')$stats
      results_manova <- rbind(results_manova, data.frame(
        taxon = t, block = blk, effect = eff,
        pillai = st[1, 'Pillai'], approx_F = st[1, 'approx F'],
        num_df = st[1, 'num Df'], den_df = st[1, 'den Df'], p = st[1, 'Pr(>F)']))
      av <- summary.aov(m); names(av) <- vnames
      for (v in vnames){
        tab <- av[[v]]
        sse <- tab$`Sum Sq`[1]; ssr <- tab$`Sum Sq`[2]
        results_anova <- rbind(results_anova, data.frame(
          taxon = t, variable = v, block = blk, effect = eff,
          F.stat = tab$`F value`[1], p = tab$`Pr(>F)`[1],
          eta2 = sse / (sse + ssr)))
      }
    }
  }
}

outdir <- '../manuscript/2025_09_17_CommsBio/20260130_revision_1/supp_tables/'
write.csv(results_manova, paste0(outdir, 'manova_results.csv'), row.names = FALSE)
write.csv(results_anova,  paste0(outdir, 'anova_results.csv'),  row.names = FALSE)

cat('=== MANOVA (Pillai) ===\n')
print(results_manova, digits = 3)
cat('\n=== mean eta^2 by taxon x block x effect ===\n')
print(aggregate(eta2 ~ taxon + block + effect, results_anova, mean), digits = 3)
cat('\nSaved manova_results.csv and anova_results.csv\n')
