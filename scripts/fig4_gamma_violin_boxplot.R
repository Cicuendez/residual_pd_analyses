# =============================================================================
# Figure 4: gamma statistic in low vs high residual PD regions.
# Restyled to match the climate figure (semi-transparent violin + opaque
# boxplot inside, same colour). Arrows explaining the sign of gamma are added
# directly in R, only in the amphibians panel.
# Reads the saved gamma values (output/gamma_stat_values.rds); no recomputation.
# Run from the project root (residual_pd_analyses/).
# =============================================================================

library(tidyverse)
library(patchwork)
library(data.table)
library(png)
library(grid)

taxa <- c('amphibians', 'birds', 'mammals', 'squamates')
col_low  <- '#2F6BA8'  # blue -> low residual PD (cradle)
col_high <- '#A72E37'  # red  -> high residual PD (museum)

gv <- readRDS('output/gamma_stat_values.rds')

# rebuild combined data frame per taxon from saved gamma values ----
build <- function(t){
  rows <- list()
  for (xx in c('cradle', 'museum')){
    for (r in names(gv[[t]][[xx]])){
      g <- gv[[t]][[xx]][[r]]
      rows[[paste(xx, r)]] <- data.frame(taxa = t, region_name = r,
                                         resPD = xx, gamma = g)
    }
  }
  d <- rbindlist(rows)
  d$resPD <- factor(d$resPD, levels = c('cradle', 'museum'))
  d
}
gdf <- setNames(lapply(taxa, build), taxa)

# taxon-image coordinates (from original script) ----
xmin_pic <- setNames(c(1.7, 1.7, 0.7, 1.6), taxa)
xmax_pic <- setNames(c(2.3, 2.3, 1.4, 2.4), taxa)
ymin_pic <- setNames(c(-2.5, 5.5, 0.5, 4), taxa)
ymax_pic <- setNames(c(-5.0, 10.0, 4, 11), taxa)

plots <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  img <- readPNG(paste0('../taxa_images/', t, '.png'))
  g   <- rasterGrob(img, interpolate = TRUE)

  p <- ggplot(gdf[[t]], aes(x = resPD, y = gamma, fill = resPD)) +
    geom_hline(yintercept = 0, linewidth = 0.2, color = 'gray40', linetype = 'dashed') +
    geom_violin(trim = TRUE, width = 0.75, color = NA, alpha = 0.55) +
    geom_boxplot(width = 0.07, color = 'gray20', alpha = 0.85,
                 outlier.shape = NA, linewidth = 0.2) +
    scale_fill_manual(values = c(cradle = col_low, museum = col_high)) +
    scale_x_discrete(labels = c('low resPD', 'high resPD')) +
    labs(x = '', y = 'gamma value', title = toupper(t)) +
    annotation_custom(grob = g, xmin = xmin_pic[t], xmax = xmax_pic[t],
                      ymin = ymin_pic[t], ymax = ymax_pic[t]) +
    theme_classic() +
    theme(legend.position = 'none',
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 10),
          axis.text.x = element_text(size = 7))

  # triangles explaining the meaning of gamma sign (amphibians panel only) ----
  if (t == 'amphibians'){
    tri_col <- 'gray40'
    xc <- 0.5      # left side of the panel
    tw <- 0.045    # half-width of the triangle base (x units)
    p <- p +
      # triangle pointing up (positive gamma) + short label above it
      annotate('polygon', x = c(xc - tw, xc + tw, xc), y = c(0.45, 0.45, 2.0),
               fill = tri_col, color = NA) +
      annotate('text', x = xc + 0.09, y = 1.2, hjust = 0, vjust = 0.5, size = 2.0,
               color = tri_col, fontface = 'italic', label = 'present') +
      # triangle pointing down (negative gamma) + short label to its right
      annotate('polygon', x = c(xc - tw, xc + tw, xc), y = c(-0.45, -0.45, -2.0),
               fill = tri_col, color = NA) +
      annotate('text', x = xc + 0.09, y = -1.2, hjust = 0, vjust = 0.5, size = 2.0,
               color = tri_col, fontface = 'italic', label = 'past') +
      coord_cartesian(clip = 'off')
  }
  plots[[t]] <- p
}

fig4 <- wrap_plots(plots)
ggsave('plots/gamma_v2.png', fig4, width = 7, height = 6, dpi = 350)
ggsave('plots/gamma_v2.pdf', fig4, width = 7, height = 6)
cat('Saved plots/gamma_v2.png and .pdf\n')
