# Supplementary table: RRPP ANOVA of DR ~ residual PD category (low vs high).
# Rebuilds dr_cramus_all from saved objects (no EPM recomputation) and extracts
# the full ANOVA table per clade. Run from project root (residual_pd_analyses/).

library(data.table)
library(RRPP)

taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

sp_cramus <- readRDS('../objects/sp_cramus_alltogether.rds')
dr_files  <- list.files('../objects', pattern = 'dr_mean_', full.names = TRUE)
dr_list <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  dr_df <- readRDS(dr_files[grep(t, dr_files, ignore.case = TRUE)])
  v <- dr_df$dr; names(v) <- rownames(dr_df)
  dr_list[[t]] <- v
}

res <- data.frame()
for (t in taxa){
  tmp <- list()
  for (xx in c('cradle', 'museum')){
    drv <- dr_list[[t]][sp_cramus[[t]][[xx]]]; drv <- drv[!is.na(drv)]
    tmp[[xx]] <- data.frame(dr = drv, type = xx)
  }
  d <- rbindlist(tmp); d$type <- factor(d$type, levels = c('cradle', 'museum'))
  n_low  <- sum(d$type == 'cradle')
  n_high <- sum(d$type == 'museum')
  fit <- lm.rrpp(dr ~ type, data = d, iter = 999, RRPP = TRUE, print.progress = FALSE)
  tab <- summary(fit)$table
  res <- rbind(res, data.frame(
    Clade   = t,
    n_low   = n_low,
    n_high  = n_high,
    Df      = tab$Df[1],
    F       = round(tab$F[1], 3),
    Rsq     = round(tab$Rsq[1], 4),
    Z       = round(tab$`Z (from F)`[1], 3),
    p       = signif(tab$`Pr(>F)`[1], 3)
  ))
}
print(res)
write.csv(res, '../manuscript/2025_09_17_CommsBio/20260130_revision_1/supp_tables/dr_anova_low_high.csv',
          row.names = FALSE)
cat('\nSaved dr_anova_low_high.csv\n')
