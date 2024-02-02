





# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'caret', 'png', 'grid')
easypackages::libraries(packages)

# taxa ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# Import vertebrate data ----
hexgrid_list <- readRDS('../objects/hexgrid_list_v2.rds')

hexgrid_all <- rlist::list.rbind(hexgrid_list)


# Regression DR ~ resPD ----
#plot(logdr ~ res_pd_rich, data = hex_grid, pch = 16, col = 'gray80')
fit.lm <- vector('list', length(taxa))
summary.lm <- vector('list', length(taxa))
rsq <- vector('list', length(taxa))
names(fit.lm) <- names(summary.lm) <- names(rsq) <- taxa

colnames(hexgrid_list[[t]])
for (t in taxa){
  print(paste0('LM for ', t))
  fit.lm[[t]] <- lm(dr ~ resloess_pd_rich, data = hexgrid_list[[t]])
  summary.lm[[t]] <- summary(fit.lm[[t]])
  rsq[[t]] <- summary.lm[[t]]$adj.r.squared
}

summary.lm

for (t in taxa){
  print(round(summary.lm[[t]]$coefficients[2, 'Estimate'], 2))
}

table_regression_dr_resPD <- data.frame(Clade = NULL, 
                                        Estimate = NULL, 
                                        'R squared' = NULL, 
                                        'p-value' = NULL)
for (t in taxa){
  results <- data.frame(Clade = t, 
                        Estimate = summary.lm[[t]]$coefficients[2, 'Estimate'], 
                        'R squared' = round(summary.lm[[t]]$adj.r.squared, 3), 
                        'p-value' = summary.lm[[t]]$coefficients[2, 4])
  
  table_regression_dr_resPD <- rbind(table_regression_dr_resPD, results)
}

table_regression_dr_resPD

write.table(table_regression_dr_resPD, 'Tables/Supp_Table_1_lm_dr_resPD.csv', 
            quote = FALSE, 
            sep = ';', dec = '.', row.names = FALSE)



# PLOTS ----
# REGRESSION DR ~ RESIDUALS ----
# All points gray and highlight each group separately
grcol <- 'gray80'
#alpha_value <- 0.1
#pointsize <- 0.5
alpha_value <- 0.05
pointsize <- 0.1
lwidth <- 0.8
linetype <- 'solid'

group_colors <- c(mammals = '#003f5c', squamates = '#7a5195', 
                  amphibians = '#ef5675', birds = '#ffa600')

#hexgrid_all <- rlist::list.rbind(hexgrid_list)

reg.plot.list <- vector('list', length = length(taxa))
names(reg.plot.list) <- taxa

xmin_pic <- setNames(c(-1800, -1800, -1800, -1800), taxa)
xmax_pic <- setNames(c(-1200, -1200, -1200, -1000), taxa)
ymin_pic <- setNames(c(0.75, 0.75, 0.75, 0.75), taxa)
ymax_pic <- setNames(c(1, 1, 1, 1), taxa)

for (t in taxa){
  taxapic_fn <- paste0('../taxa_images/', t, '.png')
  img <- readPNG(taxapic_fn)
  g <- rasterGrob(img, interpolate = TRUE)
  
  reg.plot.list[[t]] <- ggplot(data = hexgrid_all[hexgrid_all$group != t,], 
                               aes(x = resloess_pd_rich, y = dr)) +
    geom_point(color = grcol, 
               size = pointsize, alpha = alpha_value) +
    geom_point(data = hexgrid_list[[t]], 
               color = group_colors[t], size = pointsize, 
               alpha = alpha_value) +
    stat_smooth(data = hexgrid_list[[t]], method = 'lm',
                geom = 'line', alpha = 0.5, se = FALSE,
                color = 'white', 
                linetype = linetype, linewidth = lwidth + 2) +
    geom_smooth(data = hexgrid_list[[t]],
                method = "lm", se = FALSE, color = group_colors[t], 
                linetype = linetype, linewidth = lwidth) +
#    xlim(-1000, 1000) +
    ylim(0, 1) +
    annotate(geom = 'text', x = -1500, y = 0.41, 
             label = paste0('R^2 == ', round(rsq[[t]], 3)), parse = TRUE, 
             size = 2.5) +
    annotate(geom = 'text', x = -1500, y = 0.36, 
             label = paste0('p <<< 0.001'), size = 2.5) +
    annotation_custom(grob = g, 
                      xmin = xmin_pic[t], xmax = xmax_pic[t], 
                      ymin = ymin_pic[t], ymax = ymax_pic[t]) +
    theme_bw() +
    labs(x = 'residual PD', y = 'DR rate') +
    theme(element_blank(), 
          legend.position = 'bottom', 
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'))
}

#wrap_plots(reg.plot.list)
ggsave('plots/all_reg_v6.png', plot = wrap_plots(reg.plot.list), width = 7, height = 6)
ggsave('plots/all_reg_v6.pdf', plot = wrap_plots(reg.plot.list), width = 7, height = 6)

