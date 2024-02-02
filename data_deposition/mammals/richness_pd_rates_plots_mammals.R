# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'caret')
easypackages::libraries(packages)

# Import data ----
# Import hexagonal grid with richness, DR, PD, and residuals PD~DR 
hex_grid <- readRDS('../objects/hex_grid_MAMMALS.rds')
hex_grid0 <- hex_grid
hex_grid <- hex_grid %>%
  filter(spRichness > 1)

# PROJECTION ----
proj_hexgrid <- crs(hex_grid) # behrmann projection
#behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Get world data for plotting ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_projected <- st_transform(world, crs = proj_hexgrid)


################# 
##### PLOTS #####
################# 

# plot epm richness ----
epm_richness_plot <- ggplot() +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = hex_grid, aes(fill = spRichness), col = 'transparent') +
  scale_fill_viridis_c(option = 'viridis', name = 'richness') +
  theme_minimal() +
  labs(title = 'MAMMAL species richness') +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, face = 'bold'))

# Plot richness with base R 
vn <- length(unique(sprichness_grid$spRichness))
vir <- setNames(object = viridis(vn), nm = c(1:vn))
plot(world_projected$geometry, col = 'gray90', border = 'gray90')
plot(hex_grid, col = vir[hex_grid$spRichness], 
     border = NA, add = TRUE)
plot(hex_grid0[hex_grid0$spRichness == 1,], col = 'red', 
     border = NA, add = TRUE)


# plot PD ----
epm_pd_plot <- ggplot() +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = hex_grid, aes(fill = pd/1000), col = 'transparent') +
  scale_fill_viridis_c(option = 'viridis', name = 'PD x 1000') +
  theme_minimal() +
  labs(title = 'MAMMAL PD') +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, face = 'bold'))

# plot log DR ----
epm_logdr_plot <- ggplot() +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = hex_grid, aes(fill = logdr), col = 'transparent') +
  scale_fill_viridis_c(option = 'viridis', name = 'logDR') +
  theme_minimal() +
  labs(title = 'MAMMAL log DR rate') +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'))

# plot DR ----
epm_dr_plot <- ggplot() +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = hex_grid, aes(fill = dr), col = 'transparent') +
  scale_fill_viridis_c(option = 'viridis', name = 'DR') +
  theme_minimal() +
  labs(title = 'MAMMAL DR rate') +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'))

# LM DR ~ Richness ----
dr_richness_lm <- lm(dr ~ spRichness, data = hex_grid)
summary(dr_richness_lm)

ggplot(data = hex_grid, 
       aes(x = spRichness, y = logdr)) +
  geom_point(color = 'gray80') +
  geom_smooth(method = "lm", color = 'gray50', 
              linetype = "dashed", size = 0.5) +
  geom_smooth(method = 'loess', color = 'gray50', 
              linetype = "solid", size = 0.8) +
  theme_minimal() +
  theme(legend.position = 'right')

# LM sd(DR) ~ Richness ----
sddr_richness_lm <- lm(dr_sd ~ spRichness, data = hex_grid)
summary(sddr_richness_lm)

ggplot(data = hex_grid, 
       aes(x = spRichness, y = dr_sd)) +
  geom_point(color = 'gray80') +
  geom_smooth(method = "lm", color = 'gray50', 
              linetype = "dashed", size = 0.5) +
  geom_smooth(method = 'loess', color = 'gray50', 
              linetype = "solid", size = 0.8) +
  theme_minimal() +
  theme(legend.position = 'right')



# LM PD ~ Richness regression: get residuals----
pd_richness_lm <- lm(pd ~ spRichness, data = hex_grid)
summary(pd_richness_lm)
hex_grid$reslm_pd_rich <- residuals(pd_richness_lm)


# LOESS PD ~ Richness: get residuals ----
pd_richness_loess <- loess(pd ~ spRichness, data = hex_grid, 
                     span=0.75, deg=1)
# the span and deg are because below in the ggplot I use this argument 
# "method.args = list(deg=1, alpha=0.75)", 
# span in the loess function is the same as alpha in the geom_smooth function
summary(pd_richness_loess)
hex_grid$resloess_pd_rich <- residuals(pd_richness_loess)


# Plot regression PD ~ Richness ----
# colored by residual (LOESS)
scatterplot_residuals <- ggplot(data = hex_grid, 
                                aes(x = spRichness, y = pd)) +
  geom_point(aes(color = resloess_pd_rich)) +
  scale_color_gradient2(low = '#2F6BA8',
                        mid = 'gray95',
                        high = '#A72E37',
                        midpoint = 0, 
                        name = 'loess resid') +
  geom_smooth(method = "lm", color = 'gray50', 
              linetype = "dashed", size = 0.5) +
  geom_smooth(method = 'loess', method.args = list(deg=1, alpha=0.75), 
              color = 'gray50', 
              linetype = "solid", size = 0.8) +
  labs(title = 'Mammals') +
  theme_minimal() +
  theme(legend.position = 'right', 
        plot.title = element_text(hjust = 0.5, face = 'bold'))

# MAP RESIDUALS (LOESS) ----
# plot map of residuals richness ~ PD
res_pd_richness_mammals_plot <- ggplot() +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = hex_grid, aes(fill = resloess_pd_rich), col = 'transparent') +
  scale_fill_gradient2(low = '#2F6BA8', # blue
                       mid = "white",
                       high = '#A72E37', # red
                       midpoint = 0, 
                       name = 'residuals') +
  theme_minimal() +
  labs(title = 'Mammals', 
       subtitle = 'residuals LOESS PD~richness') +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, 
                                  face = 'bold'),
        legend.text = element_text(angle = 45, hjust = 1))


# Regression DR ~ (residuals PD~richness) ----
#plot(logdr ~ res_pd_rich, data = hex_grid, pch = 16, col = 'gray80')
dr_LMpdrichness_lm <- lm(dr ~ reslm_pd_rich, data = hex_grid)
s_lm <- summary(dr_LMpdrichness_lm)
rsq_lm <- s_lm$adj.r.squared

dr_LOESSpdrichness_lm <- lm(dr ~ resloess_pd_rich, data = hex_grid)
s_loess <- summary(dr_LOESSpdrichness_lm)
rsq_loess <- s_loess$r.squared

dr_respdrich_plot_mammals <- ggplot(data = hex_grid, 
                            aes(x = resloess_pd_rich, y = logdr)) +
  geom_point(aes(color = resloess_pd_rich)) +
  scale_color_gradient2(low = '#2F6BA8',
#                        mid = "white",
                        mid = 'gray95',
                        high = '#A72E37',
                        midpoint = 0) +
  geom_smooth(method = 'loess', color = 'gray50', 
              linetype = "dashed", size = 0.5) +
  geom_smooth(method = "lm", color = 'black', 
              linetype = "solid", size = 0.5) +
  #annotate(geom = 'text', x = 500, y = -0.9, 
  #         label = paste0('R^2 = ', round(rsq_loess, 2))) +
  #annotate(geom = 'text', x = 500, y = -1.1, 
  #         label = paste0('p <<< 0.001')) +
  theme_minimal() +
  labs(title = 'Mammals') +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'))

# Save grid ----
saveRDS(hex_grid, '../objects/hex_grid_MAMMALS.rds')

# Save plots ----
ggsave(filename = 'plots/richness_mammals.pdf', 
       plot = epm_richness_plot)
ggsave(filename = 'plots/pd_mammals.pdf', 
       plot = epm_pd_plot)
ggsave(filename = 'plots/dr_mammals.pdf', 
       plot = epm_logdr_plot)
ggsave(filename = 'plots/pd_richness_mammals.pdf', 
       plot = scatterplot_residuals)
ggsave(filename = 'plots/res_mammals.pdf', 
       plot = res_pd_richness_mammals_plot)
ggsave(filename = 'plots/regression_dr_mammals.pdf', 
       plot = dr_respdrich_plot_mammals)








