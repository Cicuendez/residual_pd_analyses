

packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')

easypackages::libraries(packages)

# taxa ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# epm objects ----
epm_list <- vector('list', length(taxa))
names(epm_list) <- taxa

for (t in taxa){
  epm_list[[t]] <- readRDS(paste0('../objects/EPM_', t, '.rds'))
  print(paste0(t, ' EPM imported'))
}

# dr and pd filenames ----
dr_objects <- list.files('../objects', full.names = TRUE)[grep('dr_mean_', list.files('../objects'))]
pd_objects <- list.files('../objects', full.names = TRUE)[grep('pd_mean_', list.files('../objects'))]

# set the hexgrid_list 
hexgrid_list <- vector('list', length(taxa))
names(hexgrid_list) <- taxa

for (t in taxa){
  print(toupper(t))
  
  # Get richness grid ----
  sprichness_grid <- epm_list[[t]]$grid['spRichness']
  
  # Get DR grid ----
  dr_fn <- dr_objects[grep(t, dr_objects, ignore.case = TRUE)]
  dr_mean <- readRDS(dr_fn)
  dr_vector <- dr_mean$dr
  names(dr_vector) <- rownames(dr_mean)
  epm_list[[t]] <- addTraits(epm_list[[t]], dr_vector, replace = TRUE)
  
  print(paste0('calculating mean DR grid for ', t))
  epm_dr <- gridMetrics(epm_list[[t]], metric = 'mean')
  print(paste0(t, ' mean DR grid calculated'))
  
  colnames(epm_dr$grid)[colnames(epm_dr$grid) == 'mean'] <- 'dr'
  dr_grid <- epm_dr$grid['dr']
  
  # Get PD grid ----
  pd_fn <- pd_objects[grep(t, pd_objects, ignore.case = TRUE)]
  pd_mean <- readRDS(pd_fn)
  
  # Make hexgrid ----
  hexgrid_list[[t]] <- cbind(sprichness_grid, 
                             dr = dr_grid$dr, 
                             logdr = log(dr_grid$dr), 
                             pd = pd_mean)
  
  # Set cell ID ----
  hexgrid_list[[t]]$id <- rownames(hexgrid_list[[t]])
  
  # Set taxa group ----
  hexgrid_list[[t]]$group <- t
  
  print(paste0('HEXGRID FOR ', toupper(t), ' DONE!'))
  print('####################')
}

# save hexgrid list ----
saveRDS(hexgrid_list, '../objects/hexgrid_list.rds', version = 2)

# get land birds ----
sf_use_s2(FALSE)

continents <- st_read('../../SPATIAL_DATA/Continents/All_continents.shp')
world_dissolved <- st_union(continents)
plot(world_dissolved, col = 'gray80', border = 'transparent')

st_touches(hexgrid_list[['birds']], world_dissolved)

# Import vertebrate data ----
hexgrid_list <- readRDS('../objects/hexgrid_list_v2.rds')

hexgrid_list[['birds']] <- readRDS('../objects/hex_grid_ter_BIRDS.rds')
colnames(hexgrid_list[['mammals']])
colnames(hexgrid_list[['birds']])
hexgrid_list[['birds']] <- hexgrid_list[['birds']][, -which(colnames(hexgrid_list[['birds']]) == 'dr_sd')]
hexgrid_list[['birds']]$id <- rownames(hexgrid_list[['birds']])
hexgrid_list[['birds']]$group <- 'birds'



behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
for (t in taxa){
  st_crs(hexgrid_list[[t]]) <- behrmann
}

hexgrid_all <- rlist::list.rbind(hexgrid_list)
proj_hexgrid <- crs(hexgrid_all)


# Get world data for plotting ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_projected <- st_transform(world, crs = proj_hexgrid)

# plot richness ----
richness_plots <- vector('list', length(taxa))
names(richness_plots) <- taxa
birds <- 'birds'

for (t in taxa){
  richness_plots[[t]] <- ggplot() +
    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
    geom_sf(data = hexgrid_list[[t]], aes(fill = spRichness), col = 'transparent') +
    scale_fill_viridis_c(option = 'viridis', name = 'richness') +
    labs(title = paste0(t, ' species richness')) +
    theme_minimal() +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'))
}

wrap_plots(richness_plots)
ggsave('plots/all_richness.png', plot = last_plot())


# plot PD ----
pd_plots <- vector('list', length(taxa))
names(pd_plots) <- taxa
for (t in taxa){
  pd_plots[[t]] <- ggplot() +
    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
    geom_sf(data = hexgrid_list[[t]], aes(fill = pd/1000), col = 'transparent') +
    scale_fill_viridis_c(option = 'viridis', name = 'PD x 1000') +
    theme_minimal() +
    labs(title = paste0(t, ' PD')) +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'))
  
}

wrap_plots(pd_plots)
ggsave('plots/all_pd.png', plot = last_plot())


# plot DR ----
dr_plots <- vector('list', length(taxa))
names(dr_plots) <- taxa

for (t in taxa){
  dr_plots[[t]] <- ggplot() +
    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
    geom_sf(data = hexgrid_list[[t]], aes(fill = logdr), col = 'transparent') +
    scale_fill_viridis_c(option = 'viridis', name = 'logDR') +
    theme_minimal() +
    labs(title = paste0(t, ' log DR')) +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'))
}

wrap_plots(dr_plots)
ggsave('plots/all_dr.png', plot = last_plot())

# LM DR ~ Richness ----
drrichness_plots <- vector('list', length(taxa))
names(drrichness_plots) <- taxa

for(t in taxa){
  #dr_richness_lm <- lm(dr ~ spRichness, data = hexgrid_list[[t]])
  drrichness_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                  aes(x = spRichness, y = dr)) +
    geom_point(color = 'gray80') +
    geom_smooth(method = "lm", se = FALSE, color = 'gray50', 
                linetype = "dashed", size = 0.5) +
    geom_smooth(method = 'loess', se = FALSE, color = 'gray50', 
                linetype = "solid", size = 0.8) +
    labs(title = paste0(t, ' DR~richness')) +
    theme_minimal() +
    theme(legend.position = 'right', 
          legend.key.width = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold')
          )
  
}

wrap_plots(drrichness_plots)
ggsave('plots/all_dr-richness.png', plot = last_plot())

# LM DR ~ PD ----
pddr_plots <- vector('list', length(taxa))
names(pddr_plots) <- taxa

for(t in taxa){
  #dr_richness_lm <- lm(dr ~ spRichness, data = hexgrid_list[[t]])
  pddr_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                  aes(x = pd, y = dr)) +
    geom_point(color = 'gray80') +
    geom_smooth(method = "lm", se = FALSE, color = 'gray50', 
                linetype = "dashed", size = 0.5) +
    geom_smooth(method = 'loess', se = FALSE, color = 'gray50', 
                linetype = "solid", size = 0.8) +
    labs(title = paste0(t, ' PD~DR')) +
    theme_minimal() +
    theme(legend.position = 'right', 
          legend.key.width = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold')
    )
  
}

wrap_plots(pddr_plots)
ggsave('plots/all_pd-dr.png', plot = last_plot())



# LM PD ~ Richness ----
pdrichness_plots <- vector('list', length(taxa))
names(pdrichness_plots) <- taxa

for(t in taxa){
  #dr_richness_lm <- lm(dr ~ spRichness, data = hexgrid_list[[t]])
  pdrichness_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                  aes(x = spRichness, y = pd)) +
    geom_point(color = 'gray80') +
    geom_smooth(method = "lm", se = FALSE, color = 'gray50', 
                linetype = "dashed", size = 0.5) +
    geom_smooth(method = 'loess', se = FALSE, color = 'gray50', 
                linetype = "solid", size = 0.8) +
    labs(title = paste0(t, ' PD~richness')) +
    theme_minimal() +
    theme(legend.position = 'right', 
          legend.key.width = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold')
    )
  
}

wrap_plots(pdrichness_plots)
ggsave('plots/all_pd-richness.png', plot = last_plot())

# LOESS PD ~ Richness: get residuals ----
pd_richness_loess_plots <- vector('list', length(taxa))
names(pd_richness_loess_plots) <- taxa

for (t in taxa){
  pd_richness_loess <- loess(pd ~ spRichness, data = hexgrid_list[[t]], 
                             span=0.75, deg=1)
  hexgrid_list[[t]]$resloess_pd_rich <- residuals(pd_richness_loess)
  
  pd_richness_loess_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                         aes(x = spRichness, y = pd)) +
    geom_point(aes(color = resloess_pd_rich)) +
    scale_color_gradient2(low = '#2F6BA8',
                          mid = 'gray95',
                          high = '#A72E37',
                          midpoint = 0, 
                          name = 'loess resid') +
    geom_smooth(method = "lm", se = FALSE, color = 'gray50', 
                linetype = "dashed", size = 0.5) +
    geom_smooth(method = 'loess', se = FALSE, method.args = list(deg=1, alpha=0.75), 
                color = 'gray50', 
                linetype = "solid", size = 0.8) +
    labs(title = paste0(t, ' PD~Richness')) +
    theme_minimal() +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, face = 'bold'))
  
}

wrap_plots(pd_richness_loess_plots)
ggsave('plots/all_pd-richness_loess.pdf', plot = last_plot())

# MAP RESIDUALS (LOESS) ----
resmaps <- vector('list', length(taxa))
names(resmaps) <- taxa

for (t in taxa){
  resmaps[[t]] <- ggplot() +
    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
    geom_sf(data = hexgrid_list[[t]], aes(fill = resloess_pd_rich), 
            col = 'transparent') +
    scale_fill_gradient2(low = '#2F6BA8', # blue
                         mid = "white",
                         high = '#A72E37', # red
                         midpoint = 0, 
                         name = 'res') +
    labs(title = paste0(t, ' res PD~richness')) +
    theme_minimal() +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 45, hjust = 1))
  
  
}

wrap_plots(resmaps)
ggsave('plots/all_resmaps.png', plot = last_plot())

# MAP RESIDUALS QUANTILES ----
resmaps_quant <- vector('list', length = length(taxa))
names(resmaps_quant) <- taxa

#for (t in taxa){
#  quants <- quantile(hexgrid_list[[t]]$resloess_pd_rich, c(0.1, 0.9))
#  resmaps_quant[[t]] <- hexgrid_list[[t]] %>% 
#    mutate(type = case_when(resloess_pd_rich <= quants[1] ~ 'cradle', 
#                            resloess_pd_rich >= quants[2] ~ 'museum', 
#                            (quants[1] < resloess_pd_rich & 
#                               resloess_pd_rich < quants[2]) ~ 'none')) %>% 
#    ggplot(data = .) +
#    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
#    geom_sf(aes(fill = type), color = 'transparent') +
#    scale_fill_manual(name = 'residuals', 
#                      values = c(cradle = '#2F6BA8', 
#                                 none = 'gray90', 
#                                 museum = '#A72E37')) +
#    labs(title = paste0(t, ' res 10%')) +
#    theme_minimal() +
#    theme(legend.position = 'bottom', 
#          plot.title = element_text(hjust = 0.5, 
#                                    face = 'bold'),
#          legend.text = element_text(angle = 45, hjust = 1), 
#          legend.key.height = unit(0.2,"cm"))
#  
#  
#    }

resmaps_quant <- vector('list', length = length(taxa))
names(resmaps_quant) <- taxa

for (t in taxa){
  quants <- quantile(hexgrid_list[[t]]$resloess_pd_rich, c(0.1, 0.9))
  resmaps_quant[[t]] <- hexgrid_list[[t]] %>% 
    mutate(type = case_when(resloess_pd_rich <= quants[1] ~ 'cradle', 
                            resloess_pd_rich >= quants[2] ~ 'museum', 
                            (quants[1] < resloess_pd_rich & 
                               resloess_pd_rich < quants[2]) ~ 'none')) %>% 
    ggplot(data = .) +
    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
    geom_sf(aes(fill = type), color = 'transparent') +
    scale_fill_manual(name = 'residuals', 
                      values = c(cradle = '#2F6BA8', 
                                 none = 'gray90', 
                                 museum = '#A72E37')) +
    labs(title = paste0(t, ' res 10%')) +
    theme_void() +
    theme(legend.position = 'none', 
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 45, hjust = 1), 
          legend.key.height = unit(0.2,"cm"))
  
  
}


wrap_plots(resmaps_quant)
ggsave('plots/all_resmaps_quant.pdf', plot = last_plot())


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

hexgrid_all <- rlist::list.rbind(hexgrid_list)

reg.plot.list <- vector('list', length = length(taxa))
names(reg.plot.list) <- taxa

for (t in taxa){
  reg.plot.list[[t]] <- ggplot(data = hexgrid_all[hexgrid_all$group != t,], 
                               aes(x = resloess_pd_rich, y = logdr)) +
    geom_point(color = grcol, 
               size = pointsize, alpha = alpha_value) +
    geom_point(data = hexgrid_list[[t]], 
               color = group_colors[t], size = pointsize, 
               alpha = alpha_value) +
    geom_smooth(data = hexgrid_list[[t]],
                method = "lm", se = FALSE, color = group_colors[t], 
                linetype = linetype, linewidth = lwidth) +
    xlim(-1000, 1000) +
    ylim(-3.5, 0.5) +
    theme_minimal() +
    labs(title = t, x = 'residuals PD~richness', y = 'log DR rate') +
    theme(legend.position = 'bottom', 
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'))
}

wrap_plots(reg.plot.list)
ggsave('plots/all_reg_v3.png', plot = wrap_plots(reg.plot.list))
ggsave('plots/all_reg_v3.pdf', plot = wrap_plots(reg.plot.list))

# save hexgrid_list ----
saveRDS(hexgrid_list, '../objects/hexgrid_list_v2.rds')


# _######### ----
# THE END ----
# _######### ----





alpha_value <- 0.05
pointsize <- 0.1
linesize_value <- 0.8
linecolor <- 'red'
linetype <- 'solid'
group_colors_0 <- c(mammals = 'red', squamates = 'green', 
                  amphibians = 'gold', birds = 'purple')

group_colors_1 <- c(mammals = '#003f5c', squamates = '#7a5195', 
                    amphibians = '#ef5675', birds = '#ffa600')
# https://www.learnui.design/tools/data-color-picker.html#palette

group_colors_2 <- c()
# Benítez-López A., et al. 2021. The island rule explains consistent 
# patterns of body size evolution in terrestrial vertebrates. 
# Nat. Ecol. Evol. 5:768–786.

group_colors <- group_colors_1

plot(1:4, 1:4, col = group_colors, cex = 5, pch = 16)


# All regression lines together ----
reg_plot <- ggplot(data = hex_grid_all, aes(x = resloess_pd_rich, y = dr)) +
  geom_point(data = hex_grid_all, aes(color = group), 
             size = pointsize,
             alpha = alpha_value) +
  scale_color_manual(values = group_colors) +
  geom_smooth(data = hex_grid_all[hex_grid_all$group == 'squamates',], 
              method = "lm", color = group_colors['squamates'], 
              linetype = linetype, size = linesize_value) +
  geom_smooth(data = hex_grid_all[hex_grid_all$group == 'mammals',], 
              method = "lm", color = group_colors['mammals'], 
              linetype = linetype, size = linesize_value) +
  geom_smooth(data = hex_grid_all[hex_grid_all$group == 'amphibians',], 
              method = "lm", color = group_colors['amphibians'], 
              linetype = linetype, size = linesize_value) +
  geom_smooth(data = hex_grid_all[hex_grid_all$group == 'birds',], 
              method = "lm", color = group_colors['birds'], 
              linetype = linetype, size = linesize_value) +
  ylim(0, 0.75) +
  #annotate(geom = 'text', x = 500, y = -0.9, 
  #         label = paste0('R^2 = ', round(rsq_loess, 2))) +
  #annotate(geom = 'text', x = 500, y = -1.1, 
  #         label = paste0('p <<< 0.001')) +
  theme_minimal() +
  labs(title = '') +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'))

# Separate regression plots ----
# All points gray and highlight each group separately
grcol <- 'gray80'
alpha_value <- 0.1
pointsize <- 0.5

taxa <- c('amphibians', 'birds', 'mammals', 'squamates')
reg.plot.list <- vector('list', length = length(taxa))
names(reg.plot.list) <- taxa

for (t in taxa){
  reg.plot.list[[t]] <- ggplot(data = hex_grid_all[hex_grid_all$group != t,], 
                          aes(x = resloess_pd_rich, y = dr)) +
    geom_point(color = grcol, 
               size = pointsize, alpha = alpha_value) +
    geom_point(data = hex_grid_all[hex_grid_all$group == t,], 
               color = group_colors[t], size = pointsize, 
               alpha = alpha_value) +
    geom_smooth(data = hex_grid_all[hex_grid_all$group == t,],
                method = "lm", color = group_colors[t], 
                linetype = linetype, size = linesize_value) +
    ylim(0, 0.75) +
    theme_minimal() +
    labs(title = t, x = 'residuals PD~richness', y = 'DR rate') +
    theme(legend.position = 'bottom', 
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'))
}

wrap_plots(reg.plot.list)
ggsave('plots/all_reg.png', plot = last_plot())

# Maps residuals ----
# maps of the residuals PD~richness: 4-panel figure 

# Common scale color
map.plot <- ggplot() +
  facet_wrap(facets = 'group', nrow = 2) +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = hex_grid_all, 
          aes(fill = resloess_pd_rich), col = 'transparent') +
  scale_fill_gradient2(low = '#2F6BA8', # blue
                       mid = "white",
                       high = '#A72E37', # red
                       midpoint = 0, 
                       name = 'residuals') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, 
                                     face = 'bold'),
        legend.text = element_text(angle = 45, hjust = 1))

# Independent scale color
map.list <- vector('list', length = length(taxa))
names(map.list) <- taxa

for (t in taxa){
  print(paste0('plotting map of ', t))
  map.list[[t]] <- ggplot() +
    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
    geom_sf(data = hex_grid_all[hex_grid_all$group == t,], 
            aes(fill = resloess_pd_rich), color = 'transparent') +
    scale_fill_gradient2(low = '#2F6BA8', # blue
                         mid = "white",
                         high = '#A72E37', # red
                         midpoint = 0, 
                         name = 'residuals') +
    theme_minimal() +
    labs(title = t) +
    theme(legend.position = 'bottom', 
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5, 
                                       face = 'bold'),
          legend.text = element_text(angle = 45, hjust = 1), 
          legend.key.height = unit(0.2,"cm"))
  
}

mapquant.list <- vector('list', length = length(taxa))
names(mapquant.list) <- taxa

# map residuals quantiles ----
for (t in taxa){
  print(paste0('plotting map of ', t))
  quants <- quantile(hex_grid_all[hex_grid_all$group == t,]$resloess_pd_rich, c(0.1, 0.9))
  mapquant.list[[t]] <- ggplot() +
    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
    geom_sf(data = hex_grid_all[hex_grid_all$group == t,], 
            aes(fill = resloess_pd_rich), color = 'transparent') +
    scale_fill_steps2(name = 'residuals', 
                      low = '#2F6BA8', 
                      mid = 'gray90', 
                      high = '#A72E37', 
                      breaks = quants) +
    theme_minimal() +
    labs(title = t) +
    theme(legend.position = 'bottom', 
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5, 
                                       face = 'bold'),
          legend.text = element_text(angle = 45, hjust = 1), 
          legend.key.height = unit(0.2,"cm"))
  
}

rownames(hex_grid_all)


wrap_plots(map.list)
ggsave('plots/all_map.png', plot = last_plot())

# Maps highDR richness ----
highdr_richness_grid_list <- readRDS('../objects/highdr_richness_grid_list.rds')

highdr_richness_plot_list <- vector('list', length(taxa))
names(highdr_richness_plot_list) <- taxa

for (t in taxa){
  richness_grid <- highdr_richness_grid_list[[t]]
  st_crs(richness_grid) <- behrmann
  
  highdr_richness_plot_list[[t]] <- ggplot() +
    geom_sf(data = world_behrmann, col = 'transparent', fill = 'gray80') +
    geom_sf(data = richness_grid, aes(fill = spRichness), 
            col = 'transparent') +
    scale_fill_viridis_c(option = 'viridis', name = 'richness') +
    theme_minimal() +
    labs(title = t) +
    theme(legend.position = 'bottom',
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, face = 'bold'))
  
  
}

wrap_plots(highdr_richness_plot_list)
ggsave('plots/all_highdr_richness.png', plot = last_plot())

# Maps lowDR richness ----
lowdr_richness_grid_list <- readRDS('../objects/lowdr_richness_grid_list.rds')

lowdr_richness_plot_list <- vector('list', length(taxa))
names(lowdr_richness_plot_list) <- taxa

for (t in taxa){
  richness_grid <- lowdr_richness_grid_list[[t]]
  st_crs(richness_grid) <- behrmann
  
  lowdr_richness_plot_list[[t]] <- ggplot() +
    geom_sf(data = world_behrmann, col = 'transparent', fill = 'gray80') +
    geom_sf(data = richness_grid, aes(fill = spRichness), 
            col = 'transparent') +
    scale_fill_viridis_c(option = 'viridis', name = 'richness') +
    theme_minimal() +
    labs(title = t) +
    theme(legend.position = 'bottom',
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, face = 'bold'))
  
  
}

wrap_plots(lowdr_richness_plot_list)
ggsave('plots/all_lowdr_richness.png', plot = last_plot())



# Regression residuals ~ highDR richness ----
hexgrid_list <- list(hex_grid_amph, hex_grid_birds, 
                     hex_grid_mam, hex_grid_squam)
names(hexgrid_list) <- taxa

names(highdr_richness_grid_list)

for (t in taxa){
  hexgrid_list[[t]]$resloess_pd_rich
}


nrow(hexgrid_list[['birds']])
nrow(highdr_richness_grid_list[['birds']])

highdr_richness_grid_list[['birds']]

?tableFromEpmGrid

epm_birds <- readRDS('../objects/EPM_birds.rds')

st_crs(highdr_richness_grid_list[['birds']])
behrmann

st_crs(highdr_richness_grid_list[['birds']]) <- st_crs(epm_birds$grid)
st_crs(hexgrid_list[['birds']]) <- st_crs(epm_birds$grid)

highdr_richness_grid_list[['birds']] <- st_transform(highdr_richness_grid_list[['birds']], st_crs(epm_birds$grid))



st_crs(epm_birds)
st_crs(epm_birds$grid)



x <- tableFromEpmGrid(epm_birds, hexgrid_list[['birds']]['resloess_pd_rich'], 
                      highdr_richness_grid_list[['birds']], 
                      n = 1000, dropSingleSpCells = TRUE)



