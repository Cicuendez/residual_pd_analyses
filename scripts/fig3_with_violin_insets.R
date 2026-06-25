# =============================================================================
# Figure 3: regression DR ~ residual PD (continuous) with violin insets
# (discrete: low vs high residual PD), top-right of each panel.
# Violin style matched to the paleoclimate analyses (semi-transparent violin +
# opaque boxplot inside, same colour). No taxon image inside the violin (the
# panel already carries the taxon image, bottom-left).
# =============================================================================

# packages ----
library(tidyverse)
library(patchwork)
library(png)
library(grid)
library(data.table)
library(RRPP)

# taxa & aesthetics ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')
group_colors <- c(mammals = '#003f5c', squamates = '#7a5195',
                  amphibians = '#ef5675', birds = '#ffa600')
col_crad <- '#2F6BA8'  # blue  -> low residual PD (cradle)
col_mus  <- '#A72E37'  # red   -> high residual PD (museum)

alpha_value <- 0.05
pointsize   <- 0.1
lwidth      <- 0.8
linetype    <- 'solid'
grcol       <- 'gray80'

# =============================================================================
# DATA
# =============================================================================
hexgrid_list <- readRDS('../objects/hexgrid_list_v2.rds')
hexgrid_all  <- rlist::list.rbind(hexgrid_list)

# discrete classification: cradle (low resPD) / museum (high resPD)
for (t in taxa){
  quants <- quantile(hexgrid_list[[t]]$resloess_pd_rich, c(0.1, 0.9))
  hexgrid_list[[t]] <- hexgrid_list[[t]] %>%
    mutate(type = case_when(resloess_pd_rich <= quants[1] ~ 'cradle',
                            resloess_pd_rich >= quants[2] ~ 'museum',
                            TRUE ~ 'none'))
}

# linear models (continuous) ----
rsq <- setNames(vector('list', length(taxa)), taxa)
est <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  fit <- lm(scale(resloess_pd_rich) ~ scale(logdr), data = hexgrid_list[[t]])
  s   <- summary(fit)
  rsq[[t]] <- s$adj.r.squared
  est[[t]] <- s$coefficients[2, 'Estimate']
}

# rebuild DR-in-cradles/museums from saved objects (no EPM needed) ----
sp_cramus <- readRDS('../objects/sp_cramus_alltogether.rds')
dr_files  <- list.files('../objects', pattern = 'dr_mean_', full.names = TRUE)
dr_list <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  fn    <- dr_files[grep(t, dr_files, ignore.case = TRUE)]
  dr_df <- readRDS(fn)
  v <- dr_df$dr; names(v) <- rownames(dr_df)
  dr_list[[t]] <- v
}

dr_cramus_all <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  tmp <- list()
  for (xx in c('cradle', 'museum')){
    species  <- sp_cramus[[t]][[xx]]
    drv      <- dr_list[[t]][species]; drv <- drv[!is.na(drv)]
    tmp[[xx]] <- data.frame(sp = names(drv), dr = drv, type = xx, taxa = t)
  }
  d <- data.table::rbindlist(tmp)
  d$type <- factor(d$type, levels = c('cradle', 'museum'))
  dr_cramus_all[[t]] <- d
}

# RRPP ANOVA (p and Z) ----
pval <- setNames(numeric(length(taxa)), taxa)
zval <- setNames(numeric(length(taxa)), taxa)
for (t in taxa){
  m <- RRPP::lm.rrpp(dr ~ type, data = dr_cramus_all[[t]], iter = 999,
                     RRPP = TRUE, print.progress = FALSE)
  st <- summary(m)$table
  pval[t] <- st$`Pr(>F)`[1]
  zval[t] <- st$`Z (from F)`[1]
}

# =============================================================================
# VIOLIN INSETS (climate style)
# =============================================================================
make_violin <- function(t){
  ggplot(dr_cramus_all[[t]], aes(x = type, y = log(dr), fill = type)) +
    geom_violin(trim = TRUE, scale = 'width', color = NA, alpha = 0.65) +
    geom_boxplot(width = 0.16, alpha = 1, outlier.shape = NA,
                 linewidth = 0.2, color = 'grey20') +
    scale_fill_manual(values = c(cradle = col_crad, museum = col_mus)) +
    scale_x_discrete(labels = c(cradle = 'low', museum = 'high')) +
    labs(x = NULL, y = 'log(DR)') +
    theme_classic(base_size = 6) +
    theme(legend.position = 'none',
          axis.title.y = element_text(size = 5),
          axis.text  = element_text(size = 5),
          axis.ticks = element_line(linewidth = 0.2),
          axis.line  = element_line(linewidth = 0.2),
          plot.background = element_rect(fill = 'transparent', color = NA),
          panel.background = element_rect(fill = 'transparent', color = NA))
}
violin_list <- setNames(lapply(taxa, make_violin), taxa)

# =============================================================================
# REGRESSION PANELS (taxon image bottom-left) + inset top-right
# =============================================================================
ymin_pic <- setNames(rep(-1500, 4), taxa); ymax_pic <- setNames(rep(-800, 4), taxa)
xmin_pic <- setNames(rep(-4.5, 4), taxa);  xmax_pic <- setNames(rep(-3, 4), taxa)

panels <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  img <- readPNG(paste0('../taxa_images/', t, '.png'))
  g   <- rasterGrob(img, interpolate = TRUE)

  base <- ggplot(data = hexgrid_all[hexgrid_all$group != t, ],
                 aes(y = resloess_pd_rich, x = logdr)) +
    geom_point(color = grcol, size = pointsize, alpha = alpha_value) +
    geom_point(data = hexgrid_list[[t]], color = group_colors[t],
               size = pointsize, alpha = alpha_value) +
    stat_smooth(data = hexgrid_list[[t]], method = 'lm', geom = 'line',
                alpha = 0.5, se = FALSE, color = 'white',
                linetype = linetype, linewidth = lwidth + 2) +
    geom_smooth(data = hexgrid_list[[t]], method = 'lm', se = FALSE,
                color = group_colors[t], linetype = linetype, linewidth = lwidth) +
    ylim(-1500, 1000) +
    annotate('text', x = -0.5, y = -950,  size = 2.5, parse = TRUE,
             label = paste0('R^2 == ', round(rsq[[t]], 3))) +
    annotate('text', x = -0.5, y = -1100, size = 2.5,
             label = paste0('Estimate = ', round(est[[t]], 3))) +
    annotate('text', x = -0.5, y = -1250, size = 2.5, label = 'p <<< 0.001') +
    annotation_custom(grob = g, xmin = xmin_pic[t], xmax = xmax_pic[t],
                      ymin = ymin_pic[t], ymax = ymax_pic[t]) +
    theme_bw() +
    labs(x = 'log(DR rate)', y = 'residual PD') +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(fill = NA, colour = 'grey30'),
          plot.title = element_text(hjust = 0.5, face = 'bold'))

  panels[[t]] <- base +
    inset_element(violin_list[[t]], left = 0.60, bottom = 0.58,
                  right = 1.0, top = 1.0, align_to = 'panel')
}

fig3 <- wrap_plots(panels, ncol = 2)

ggsave('plots/all_reg_v8.png', plot = fig3, width = 7, height = 6, dpi = 350)
ggsave('plots/all_reg_v8.pdf', plot = fig3, width = 7, height = 6)

cat('\n--- p / Z values (low vs high residual PD) ---\n')
for (t in taxa) cat(sprintf('%-10s p = %.4f | Z = %.3f\n', t, pval[t], zval[t]))
cat('\nSaved plots/all_reg_v8.png and .pdf\n')
