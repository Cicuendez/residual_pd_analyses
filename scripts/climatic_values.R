


# Objective ----
# Plot cradles and museums on a climatic space defined by 
# temperature and precipitation. It will be 4 panels, one per 
# vertebrate group.

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'png', 'grid', 'RRPP')
easypackages::libraries(packages)

# hexagonal grid vertebrates ----
hexgrid_list <- readRDS('../objects/hexgrid_list_geo_v3.rds')
taxa <- names(hexgrid_list)
head(hexgrid_list[[1]])
hexgrid_list[[1]]$temp

# Environmental rasters -----
#?getData
#wc_temp <- raster::getData("worldclim",var="tmean",res=10)
#wc_prec <- raster::getData("worldclim",var="prec",res=10)
env_folder <- '/Volumes/Macintosh HD/Users/cicu/OneDrive - Universidad Complutense de Madrid (UCM)/CIENCIA/DATA_GENERAL/WORLDCLIM/wc2.1_10m_bio'
env_files <- list.files(env_folder, full.names = TRUE)
worldclim <- lapply(env_files, FUN = raster)
worldclim <- stack(worldclim)
res(worldclim)

# net primary productivity npp 
npp_raster <- raster('/Users/cicu/OneDrive - Universidad Complutense de Madrid (UCM)/CIENCIA/DATA_GENERAL/Net_primary_productivity/npp-geotiff/npp_geotiff.tif')
res(npp_raster)

# topographic complexity (terrain roughness index tri)
# http://envirem.github.io/#downloads
# tri current
tri_current_raster <- raster('/Users/cicu/OneDrive - Universidad Complutense de Madrid (UCM)/CIENCIA/DATA_GENERAL/envirem/elevation_variables/current/elev_global_current_10arcmin_geotiff/current_10arcmin_tri.tif')
plot(tri_current_raster)
res(tri_current_raster)

# tri holocene
tri_holo_raster <- raster('/Users/cicu/OneDrive - Universidad Complutense de Madrid (UCM)/CIENCIA/DATA_GENERAL/envirem/elevation_variables/holocene/elev_global_holo_10arcmin_geotiff/holo_10arcmin_tri.tif')
plot(tri_holo_raster)

# tri lgm
tri_lgm_raster <- raster('/Users/cicu/OneDrive - Universidad Complutense de Madrid (UCM)/CIENCIA/DATA_GENERAL/envirem/elevation_variables/lgm/elev_global_lgm_10arcmin_geotiff/lgm_10arcmin_tri.tif')
plot(tri_lgm_raster)

# 1. TEMPERATURE VS PRECIPITATION ----

# Get temperature and precipitation rasters
temp_raster <- worldclim$wc2.1_10m_bio_01
prec_raster <- worldclim$wc2.1_10m_bio_12

# Reproject rasters ----
temp_ea <- projectRaster(temp_raster, crs = crs(hexgrid_list[[1]]))
prec_ea <- projectRaster(prec_raster, crs = crs(hexgrid_list[[1]]))
npp_ea <- projectRaster(npp_raster, crs = crs(hexgrid_list[[1]]))
tri_current_raster <- projectRaster(tri_current_raster, crs = crs(hexgrid_list[[1]]))
tri_holo_raster <- projectRaster(tri_holo_raster, crs = crs(hexgrid_list[[1]]))
tri_lgm_raster <- projectRaster(tri_lgm_raster, crs = crs(hexgrid_list[[1]]))


# Plot raster ----
temp_ea_df <- as.data.frame(temp_ea, xy = TRUE)
head(temp_ea_df)

temp_ea_df %>% 
  filter(!is.na(wc2.1_10m_bio_01)) %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = wc2.1_10m_bio_01)) +
  #geom_sf(data = squam_sf, fill = 'red', alpha = 0.2, color = 'transparent') +
  coord_sf()


# Extract climatic values ----
for (t in taxa){
  print(toupper(t))
  
  # get temperature values per grid cell
  print(paste0('extracting temperature values for ', t))
  temp_vals <- raster::extract(x = temp_ea, y = hexgrid_list[[t]], 
                               fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$temp <- as.vector(temp_vals)
  
  # get precipitation values per grid cell 
  print(paste0('extracting precipitation values for ', t))
  prec_vals <- raster::extract(x = prec_ea, y = hexgrid_list[[t]], 
                               fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$prec <- as.vector(prec_vals)
  
  print('##############################')
}

head(hexgrid_list[[t]])

# Colors ----

crad_col <- '#2F6BA8' # blue
mus_col <- '#A72E37' # red

crad_col <- '#355070'
mus_col <- '#B56576'
mid_col <- 'gray80'
mid_col <- 'gold'

crad_col <- '#5B8E7D'
crad_col <- '#548687'
crad_col <- '#56A3A6'
crad_col <- '#2A9D8F'
crad_col <- '#20A39E'
mus_col <- '#BC4B51'
mid_col <- 'gray80'


crad_col <- '#16697a'
mus_col <- '#ffa62b'
mid_col <- 'gray80'


plot(1:3, 1:3, col = c(crad_col, mid_col, mus_col), pch = 16, cex = 20)

ggplot(data = hexgrid_list[[t]] %>% filter(type != 'none')) + 
  geom_point(aes(x = temp, y = prec, color = resloess_pd_rich), size = 0.8) +
  scale_color_gradient2(low = crad_col, # blue
                       mid = mid_col,
                       high = mus_col, # red
                       midpoint = 0, 
                       name = 'res') +
  labs(title = paste0(t, ' climatic space')) +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.height = unit(0.2,"cm"),
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'),
        legend.text = element_text(angle = 45, hjust = 1))

# plot cradle and museum climatic space ----
climate_plot <- vector('list', length(taxa))
names(climate_plot) <- taxa

pointsize <- 0.3
transparency <- 0.6

xmin_pic <- setNames(c(-19, -18, -18, -19), taxa)
xmax_pic <- setNames(c(-5, -10, -10, -5), taxa)
ymin_pic <- setNames(c(4500, 4200, 4000, 4500), taxa)
ymax_pic <- setNames(c(6000, 6000, 6000, 6000), taxa)

for (t in taxa){
  
  taxapic_fn <- paste0('../taxa_images/', t, '.png')
  img <- readPNG(taxapic_fn)
  g <- rasterGrob(img, interpolate = TRUE)
  
  climate_plot[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
         aes(x = temp, y = prec)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type != 'none'), 
               aes(color = type), size = pointsize, alpha = transparency) +
    scale_color_manual(name = 'residuals', 
                       values = c(cradle = crad_col,
                                  museum = mus_col)) +
    lims(x = c(-20, 30)) +
    labs(x = 'temperature (ºC)', 
         y = 'precipitation (mm)') +
    annotation_custom(grob = g, 
                      xmin = xmin_pic[t], xmax = xmax_pic[t], 
                      ymin = ymin_pic[t], ymax = ymax_pic[t]) +
    theme_bw() +
    theme(element_blank(), 
          legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

wrap_plots(climate_plot)
ggsave('plots/climate_plots.pdf', plot = wrap_plots(climate_plot))
ggsave('plots/climate_plots.png', plot = wrap_plots(climate_plot))

# climate plot cradles only ----
climate_cradles_plot <- vector('list', length(taxa))
names(climate_cradles_plot) <- taxa
for (t in taxa){
  climate_cradles_plot[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
                              aes(x = temp, y = prec)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type == 'cradle'), 
               color = crad_col,
               size = pointsize, alpha = transparency) +
    lims(x = c(-20, 30)) +
    labs(title = paste0(t, ' climatic space')) +
    theme_minimal() +
    theme(legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

wrap_plots(climate_cradles_plot)
ggsave('plots/climate_cradles_plot.pdf', plot = last_plot())

# climate plot museums only ----
climate_museums_plot <- vector('list', length(taxa))
names(climate_museums_plot) <- taxa
for (t in taxa){
  climate_museums_plot[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
                                      aes(x = temp, y = prec)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type == 'museum'), 
               color = mus_col,
               size = pointsize, alpha = transparency) +
    lims(x = c(-20, 30)) +
    labs(title = paste0(t, ' climatic space')) +
    theme_minimal() +
    theme(legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

wrap_plots(climate_museums_plot)
ggsave('plots/climate_museums_plot.pdf', plot = last_plot())

# Residual PD vs temperature ----
# LM res~temp
fit.res_temp <- vector('list', length(taxa))
rsq_res_temp <- vector('numeric', length(taxa))
names(rsq_res_temp) <- names(fit.res_temp) <- taxa
for (t in taxa){
  fit.res_temp[[t]] <- lm.rrpp(resloess_pd_rich ~ temp, data = hexgrid_list[[t]])
  fit.sum <- summary(fit.res_temp[[t]])
  fit.sum$table$Rsq
  rsq_res_temp[t] <- fit.sum$table$Rsq
  
}

lapply(fit.res_temp, summary)


# Weak relationship between residuals and temperature

# Plot
res_temp_plots <- vector('list', length(taxa))
names(res_temp_plots) <- taxa

x.annot_temp <- setNames(c(0, -30, -10, -5), taxa)
y.annot_temp <- setNames(c(-1500, -1000, -375, -750), taxa)

for (t in taxa){
  
  res_temp_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                       aes(x = temp, y = resloess_pd_rich)) +
    geom_point(color = 'gray80', size = pointsize) + 
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth", color = 'black') +
    annotate(geom = 'text', x = x.annot_temp[t], y = y.annot_temp[t], size = 3,
             label = paste0('R^2 == ', round(rsq_res_temp[t], 3)), 
             parse = TRUE) +
#    annotate(geom = 'text', label = paste0('p = 0.001'), size = 3) +
    labs(title = paste0(t), x = 'temperature', y = 'res PD') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'), 
          legend.position = 'none')
  
}

wrap_plots(res_temp_plots)
ggsave('plots/res_temp_plots.pdf', plot = last_plot())




# Residual PD vs precipitation ----
# LM res~prec
fit.res_prec <- vector('list', length(taxa))
rsq_res_prec <- vector('numeric', length(taxa))
names(rsq_res_prec) <- names(fit.res_prec) <- taxa
for (t in taxa){
  fit.res_prec[[t]] <- lm.rrpp(resloess_pd_rich ~ prec, data = hexgrid_list[[t]])
  fit.sum <- summary(fit.res_prec[[t]])
  fit.sum$table$Rsq
  rsq_res_prec[t] <- fit.sum$table$Rsq
}
# Weak relationship between residuals and precipitation
lapply(fit.res_prec, summary)

# Plot
res_prec_plots <- vector('list', length(taxa))
names(res_prec_plots) <- taxa

x.annot_prec <- setNames(c(4000, 5000, 4000, 4000), taxa)
y.annot_prec <- setNames(c(-1500, -1000, -375, -750), taxa)

for (t in taxa){
  
  res_prec_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                aes(x = prec, y = resloess_pd_rich)) +
    geom_point(color = 'gray80', size = pointsize) + 
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth", color = 'black') +
    annotate(geom = 'text', x = x.annot_prec[t], y = y.annot_prec[t], size = 3,
             label = paste0('R^2 == ', round(rsq_res_prec[t], 3)), 
             parse = TRUE) +
    labs(title = paste0(t), x = 'precipitation', y = 'res PD') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'), 
          legend.position = 'none')
  
}

wrap_plots(res_prec_plots)
ggsave('plots/res_prec_plots.pdf', plot = last_plot())


# 2. TEMP SEASONALITY VS PREC SEASONALITY ----

# Get temperature and precipitation seasonality rasters
tempseas_raster <- worldclim$wc2.1_10m_bio_04
precseas_raster <- worldclim$wc2.1_10m_bio_15

# Reproject rasters ----
tempseas_ea <- projectRaster(tempseas_raster, crs = crs(hexgrid_list[[1]]))
precseas_ea <- projectRaster(precseas_raster, crs = crs(hexgrid_list[[1]]))

# Plot raster ----
tempseas_ea_df <- as.data.frame(tempseas_ea, xy = TRUE)
head(tempseas_ea_df)

tempseas_ea_df %>% 
  filter(!is.na(wc2.1_10m_bio_04)) %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = wc2.1_10m_bio_04)) +
  #geom_sf(data = squam_sf, fill = 'red', alpha = 0.2, color = 'transparent') +
  coord_sf()

# Extract climatic values ----
for (t in taxa){
  print(toupper(t))
  
  # get temperature seasonality values per grid cell
  print(paste0('extracting temperature values for ', t))
  tempseas_vals <- raster::extract(x = tempseas_ea, y = hexgrid_list[[t]], 
                               fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$tempseas <- as.vector(tempseas_vals)
  
  # get precipitation seasonality values per grid cell 
  print(paste0('extracting precipitation values for ', t))
  precseas_vals <- raster::extract(x = precseas_ea, y = hexgrid_list[[t]], 
                               fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$precseas <- as.vector(precseas_vals)
  
  print('##############################')
}

head(hexgrid_list[[t]])
saveRDS(hexgrid_list, '../objects/hexgrid_list_geo.rds')
hexgrid_list <- readRDS('../objects/hexgrid_list_geo.rds')
colnames(hexgrid_list[[t]])

ggplot(data = hexgrid_list[[t]] %>% filter(type != 'none')) + 
  geom_point(aes(x = tempseas, y = precseas, color = resloess_pd_rich), size = 0.8) +
  scale_color_gradient2(low = crad_col, # blue
                        mid = mid_col,
                        high = mus_col, # red
                        midpoint = 0, 
                        name = 'res') +
  labs(title = paste0(t, ' climatic seasonality space')) +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.height = unit(0.2,"cm"),
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'),
        legend.text = element_text(angle = 45, hjust = 1))

# plot cradle and museum climatic seasonality space ----
climate_seasonality_plot <- vector('list', length(taxa))
names(climate_seasonality_plot) <- taxa

pointsize <- 0.3
transparency <- 0.6

for (t in taxa){
  climate_seasonality_plot[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
                              aes(x = tempseas, y = precseas)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type != 'none'), 
               aes(color = type), size = pointsize, alpha = transparency) +
    scale_color_manual(name = 'residuals', 
                       values = c(cradle = crad_col,
                                  museum = mus_col)) +
    #lims(x = c(-20, 30)) +
    labs(title = paste0(t, ' seasonality'), 
         x = 'temp seasonality', y = 'prec seasonality') +
    theme_minimal() +
    theme(legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

wrap_plots(climate_seasonality_plot)
ggsave('plots/climate_seasonality_plots.pdf', plot = last_plot())

# Residual PD vs temperature seasonality ----
# LM res~tempseas
fit.res_tempseas <- vector('list', length(taxa))
rsq_res_tempseas <- vector('numeric', length(taxa))
names(rsq_res_tempseas) <- names(fit.res_tempseas) <- taxa
for (t in taxa){
  fit.res_tempseas[[t]] <- lm.rrpp(resloess_pd_rich ~ tempseas, 
                                   data = hexgrid_list[[t]], 
                                   print.progress = TRUE)
  fit.sum <- summary(fit.res_tempseas[[t]])
  fit.sum$table$Rsq
  rsq_res_tempseas[t] <- fit.sum$table$Rsq
  
}
# Weak relationship between residuals and temperature seasonality

# Plot
res_tempseas_plots <- vector('list', length(taxa))
names(res_tempseas_plots) <- taxa
x.annot_tempseas <- setNames(c(1000, 1000, 1250, 1500), taxa)
y.annot_tempseas <- setNames(c(-1500, -1000, -375, -750), taxa)

for (t in taxa){
  
  res_tempseas_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                aes(x = tempseas, y = resloess_pd_rich)) +
    geom_point(color = 'gray80', size = pointsize) + 
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth", color = 'black') +
    annotate(geom = 'text', x = x.annot_tempseas[t], 
             y = y.annot_tempseas[t], size = 3,
             label = paste0('R^2 == ', round(rsq_res_tempseas[t], 3)), 
             parse = TRUE) +
    labs(title = paste0(t), x = 'temp. seas.', y = 'res PD') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'), 
          legend.position = 'none')
  
}

wrap_plots(res_tempseas_plots)
ggsave('plots/res_tempseas_plots.pdf', plot = last_plot())

# Residual PD vs precipitation seasonality ----
# LM res~precseas
fit.res_precseas <- vector('list', length(taxa))
rsq_res_precseas <- vector('numeric', length(taxa))
names(rsq_res_precseas) names(fit.res_precseas) <- taxa

for (t in taxa){
  fit.res_precseas[[t]] <- lm.rrpp(resloess_pd_rich ~ precseas, 
                                   data = hexgrid_list[[t]])
  fit.sum <- summary(fit.res_precseas[[t]])
  fit.sum$table$Rsq
  rsq_res_precseas[t] <- fit.sum$table$Rsq
}
# Weak relationship between residuals and precipitation seasonality

# Plot
res_precseas_plots <- vector('list', length(taxa))
names(res_precseas_plots) <- taxa

x.annot_precseas <- setNames(c(100, 150, 150, 150), taxa)
y.annot_precseas <- setNames(c(-1500, -1000, -375, -750), taxa)

for (t in taxa){
  
  res_precseas_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                    aes(x = precseas, y = resloess_pd_rich)) +
    geom_point(color = 'gray80', size = pointsize) + 
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth", color = 'black') +
    annotate(geom = 'text', x = x.annot_precseas[t], 
             y = y.annot_precseas[t], size = 3,
             label = paste0('R^2 == ', round(rsq_res_precseas[t], 3)), 
             parse = TRUE) +
    labs(title = paste0(t), x = 'prec. seas.', y = 'res PD') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'), 
          legend.position = 'none')
  
}

wrap_plots(res_precseas_plots)
ggsave('plots/res_precseas_plots.pdf', plot = last_plot())





# NPP AND TRI ----
# Reproject rasters ----
npp_ea <- projectRaster(npp_raster, crs = crs(hexgrid_list[[1]]))
tri_current_ea <- projectRaster(tri_current_raster, crs = crs(hexgrid_list[[1]]))
tri_holo_ea <- projectRaster(tri_holo_raster, crs = crs(hexgrid_list[[1]]))
tri_lgm_ea <- projectRaster(tri_lgm_raster, crs = crs(hexgrid_list[[1]]))

# Extract npp and tri values ----
for (t in taxa){
  print(toupper(t))
  
  # get npp values per grid cell
  print(paste0('extracting npp values for ', t))
  npp_vals <- raster::extract(x = npp_ea, y = hexgrid_list[[t]], 
                               fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$npp <- as.vector(npp_vals)
  
  # get tri_current values per grid cell
  print(paste0('extracting tri_current values for ', t))
  tri_current_vals <- raster::extract(x = tri_current_ea, y = hexgrid_list[[t]], 
                              fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$tri_current <- as.vector(tri_current_vals)
  
  # get tri_holo values per grid cell
  print(paste0('extracting tri_holo values for ', t))
  tri_holo_vals <- raster::extract(x = tri_holo_ea, y = hexgrid_list[[t]], 
                                      fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$tri_holo <- as.vector(tri_holo_vals)
  
  # get tri_lgm values per grid cell
  print(paste0('extracting tri_lgm values for ', t))
  tri_lgm_vals <- raster::extract(x = tri_lgm_ea, y = hexgrid_list[[t]], 
                                      fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$tri_lgm <- as.vector(tri_lgm_vals)
  
  print('##############################')
}

saveRDS(hexgrid_list, '../objects/hexgrid_list_geo_v2.rds')

# plot cradle and museum npp - tri current space ----
npp_tri_current_plot <- vector('list', length(taxa))
names(npp_tri_current_plot) <- taxa

pointsize <- 0.3
transparency <- 0.6

for (t in taxa){
  npp_tri_current_plot[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
                                          aes(x = npp, y = tri_current)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type != 'none'), 
               aes(color = type), size = pointsize, alpha = transparency) +
    scale_color_manual(name = 'residuals', 
                       values = c(cradle = crad_col,
                                  museum = mus_col)) +
    lims(y = c(0, 300)) +
    labs(title = paste0(t, ' npp ~ tri (current)'), 
         x = 'primary productivity', y = 'topographic complexity') +
    theme_minimal() +
    theme(legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

#plot(hexgrid_list[[t]]['tri_current'], border = 'transparent')

wrap_plots(npp_tri_current_plot)
ggsave('plots/npp_tri_current_plot.pdf', plot = last_plot())

# only cradles npp ~ tri current 
npp_tri_cradles <- vector('list', length(taxa))
names(npp_tri_cradles) <- taxa

for (t in taxa){
  npp_tri_cradles[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
                                      aes(x = npp, y = tri_current)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type == 'cradle'), 
               color = col_crad, size = pointsize, alpha = transparency) +
    lims(y = c(0, 300)) +
    labs(title = paste0(t), 
         x = 'NPP', y = 'TRI') +
    theme_minimal() +
    theme(legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

wrap_plots(npp_tri_cradles)
ggsave('plots/npp_tri_cradles.pdf', plot = last_plot())

# only museums npp ~ tri current 
npp_tri_museums <- vector('list', length(taxa))
names(npp_tri_museums) <- taxa

for (t in taxa){
  npp_tri_museums[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
                                 aes(x = npp, y = tri_current)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type == 'museum'), 
               color = col_mus, size = pointsize, alpha = transparency) +
    lims(y = c(0, 300)) +
    labs(title = paste0(t), 
         x = 'NPP', y = 'TRI') +
    theme_minimal() +
    theme(legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

wrap_plots(npp_tri_museums)
ggsave('plots/npp_tri_museums.pdf', plot = last_plot())

# plot cradle and museum npp - tri holocene space ----
npp_tri_holo_plot <- vector('list', length(taxa))
names(npp_tri_holo_plot) <- taxa

pointsize <- 0.3
transparency <- 0.6

for (t in taxa){
  npp_tri_holo_plot[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
                                      aes(x = npp, y = tri_holo)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type != 'none'), 
               aes(color = type), size = pointsize, alpha = transparency) +
    scale_color_manual(name = 'residuals', 
                       values = c(cradle = crad_col,
                                  museum = mus_col)) +
    lims(y = c(0, 300)) +
    labs(title = paste0(t, ' productivity ~ topography (holocene)'), 
         x = 'primary productivity', y = 'topographic complexity') +
    theme_minimal() +
    theme(legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

wrap_plots(npp_tri_holo_plot)
ggsave('plots/npp_tri_holo_plot.pdf', plot = last_plot())

# plot cradle and museum npp - tri lgm space ----
npp_tri_lgm_plot <- vector('list', length(taxa))
names(npp_tri_lgm_plot) <- taxa

pointsize <- 0.3
transparency <- 0.6

for (t in taxa){
  npp_tri_lgm_plot[[t]] <- ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
                                      aes(x = npp, y = tri_lgm)) + 
    geom_point(color = 'gray80', size = pointsize) +
    geom_point(data = hexgrid_list[[t]] %>% filter(type != 'none'), 
               aes(color = type), size = pointsize, alpha = transparency) +
    scale_color_manual(name = 'residuals', 
                       values = c(cradle = crad_col,
                                  museum = mus_col)) +
    lims(y = c(0, 300)) +
    labs(title = paste0(t, ' productivity ~ topography (lgm)'), 
         x = 'primary productivity', y = 'topographic complexity') +
    theme_minimal() +
    theme(legend.position = 'none', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
}

wrap_plots(npp_tri_lgm_plot)
ggsave('plots/npp_tri_lgm_plot.pdf', plot = last_plot())

# Residual PD vs npp ----
# LM res~npp
fit.res_npp <- vector('list', length(taxa))
rsq_res_npp <- vector('numeric', length(taxa))
names(rsq_res_npp) <- names(fit.res_npp) <- taxa
for (t in taxa){
  rm(fit.sum)
  fit.res_npp[[t]] <- lm.rrpp(resloess_pd_rich ~ npp, 
                                   data = hexgrid_list[[t]], 
                              print.progress = TRUE)
  fit.sum <- summary(fit.res_npp[[t]])
  rsq_res_npp[t] <- fit.sum$table$Rsq
}
# No relationship between residuals and npp

lapply(fit.res_npp, summary)

# Plot
res_npp_current_plots <- vector('list', length(taxa))
names(res_npp_current_plots) <- taxa

x.annot_npp <- setNames(c(3e11, 3e11, 7.5e11, 4.5e11), taxa)
y.annot_npp <- setNames(c(-1500, -1000, -375, -750), taxa)

for (t in taxa){
  res_npp_current_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                       aes(x = npp, y = resloess_pd_rich)) +
    geom_point(color = 'gray80', size = pointsize) + 
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth", color = 'black') +
    annotate(geom = 'text', x = x.annot_npp[t], 
             y = y.annot_npp[t], size = 3,
             label = paste0('R^2 == ', round(rsq_res_npp[t], 3)), 
             parse = TRUE) +
    labs(title = paste0(t), x = 'NPP', y = 'res PD') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'), 
          legend.position = 'none')
  
}

wrap_plots(res_npp_current_plots)
ggsave('plots/res_npp_current_plots.pdf', 
       plot = wrap_plots(res_npp_current_plots))



# Residual PD vs tri ----
# ____ tri current ----
# LM res~tri
fit.res_tri <- vector('list', length(taxa))
rsq_res_tri <- vector('numeric', length(taxa))
names(rsq_res_tri) <- names(fit.res_tri) <- taxa

for (t in taxa){
  rm(fit.sum)
  fit.res_tri[[t]] <- lm.rrpp(resloess_pd_rich ~ tri_current, 
                              data = hexgrid_list[[t]], 
                              print.progress = TRUE)
  fit.sum <- summary(fit.res_tri[[t]])
  rsq_res_tri[t] <- fit.sum$table$Rsq
}
# No relationship between residuals and topographic complexity

lapply(fit.res_tri, summary)

# Plot
res_tri_current_plots <- vector('list', length(taxa))
names(res_tri_current_plots) <- taxa

x.annot_tri <- setNames(c(150, 200, 250, 200), taxa)
y.annot_tri <- setNames(c(-1500, 500, -375, -750), taxa)

for (t in taxa){
  res_tri_current_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                    aes(x = tri_current, y = resloess_pd_rich)) +
    geom_point(color = 'gray80', size = pointsize) + 
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth", color = 'black') +
    annotate(geom = 'text', x = x.annot_tri[t], 
             y = y.annot_tri[t], size = 3,
             label = paste0('R^2 == ', round(rsq_res_tri[t], 4)), 
             parse = TRUE) +
    labs(title = paste0(t), x = 'TRI', y = 'res PD') +
    lims(x = c(0, 300)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'), 
          legend.position = 'none')
  
}

wrap_plots(res_tri_current_plots)
ggsave('plots/res_tri_current_plots.pdf', plot = last_plot())


# ____ tri holocene (last 11,700 years) ----


# ____ tri lgm (21,000 years ago) ----


# LM res~richness ----
rsq_res_sr <- vector('numeric', length(taxa))
names(rsq_res_sr) <- taxa
for (t in taxa){
  rsq_res_sr[t] <- summary(lm(resloess_pd_rich ~ spRichness, 
                              data = hexgrid_list[[t]]))$r.squared
  
}
# No relationship between residuals and species richness


# LATITUDINAL GRADIENT ----
# get centroid longitude (x) and latitude (y) 
for (t in taxa){
  print(toupper(t))
  xy_coords <- st_coordinates(st_centroid(hexgrid_list[[t]]))
  hexgrid_list[[t]]$long <- xy_coords[,'X']
  hexgrid_list[[t]]$lat <- xy_coords[, 'Y']
}

head(hexgrid_list[[t]])

saveRDS(hexgrid_list, '../objects/hexgrid_list_geo_v3.rds')

richness_lat_plots <- vector('list', length(taxa))
names(richness_lat_plots) <- taxa

pd_lat_plots <- vector('list', length(taxa))
names(pd_lat_plots) <- taxa

res_lat_plots <- vector('list', length(taxa))
names(res_lat_plots) <- taxa

res_richness_plots <- vector('list', length(taxa))
names(res_richness_plots) <- taxa

for (t in taxa){
  print(toupper(t))
  
  # richness vs latitude
  richness_lat_plots[[t]] <- ggplot(data = hexgrid_list[[t]], aes(x = spRichness, y = lat)) +
    geom_point(size = 0.4, aes(fill = resloess_pd_rich), color = 'transparent', 
               pch = 21) +
    scale_fill_gradient2(low = crad_col, # blue
                         mid = mid_col,
                         high = mus_col, # red
                         midpoint = 0, 
                         name = 'res') +
    labs(title = paste0('richness ', t)) +
    theme_classic() +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
  
  
  # PD vs latitude 
  pd_lat_plots[[t]] <- ggplot(data = hexgrid_list[[t]], aes(x = pd, y = lat)) +
    geom_point(size = 0.4, aes(fill = resloess_pd_rich), color = 'transparent', 
               pch = 21) +
    scale_fill_gradient2(low = crad_col, # blue
                         mid = mid_col,
                         high = mus_col, # red
                         midpoint = 0, 
                         name = 'res') +
    labs(title = paste0('PD ', t)) +
    theme_classic() +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
  
  # residuals vs latitude
  res_lat_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                               aes(x = resloess_pd_rich, y = lat)) +
    geom_point(size = 0.4, aes(fill = spRichness), color = 'transparent', 
               pch = 21) +
    scale_fill_viridis() +
    labs(title = paste0('residuals ', t)) +
    theme_classic() +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          legend.text = element_text(angle = 0, hjust = 1))
  
  # residual PD vs richness
  res_richness_plots[[t]] <- ggplot(data = hexgrid_list[[t]], 
                                    aes(x = spRichness, y = resloess_pd_rich)) +
    geom_point(size = pointsize, color = 'gray80') +                                     
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth", color = 'black') +
    labs(title = paste0(t)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'))
  
}


wrap_plots(richness_lat_plots)
ggsave('plots/lat_richness_plots.pdf', plot = last_plot())

wrap_plots(pd_lat_plots)
ggsave('plots/lat_pd_plots.pdf', plot = last_plot())

wrap_plots(res_lat_plots)
ggsave('plots/lat_residuals_plots.pdf', plot = last_plot())

wrap_plots(res_richness_plots)
ggsave('plots/res_richness_plots.pdf', plot = last_plot())





# Residual PD vs temp*prec ----
# LM res~temp*prec
fit.res_temprec <- vector('list', length(taxa))
rsq_res_temprec <- vector('numeric', length(taxa))
names(rsq_res_temprec) <- names(fit.res_temprec) <- taxa
for (t in taxa){
  fit.res_temprec[[t]] <- lm.rrpp(resloess_pd_rich ~ temp*prec, SS.type = "III",
                                  data = hexgrid_list[[t]])
  fit.sum <- summary(fit.res_temprec[[t]])
  fit.sum$table$Rsq
  rsq_res_temprec[t] <- fit.sum$table$Rsq
  
}

fit.res_temprec[[t]]$LM$fitted
lapply(fit.res_temprec, summary)
lapply(fit.res_temprec, anova)

# 
fit.res_temprec <- vector('list', length(taxa))
rsq_res_temprec <- vector('numeric', length(taxa))
names(rsq_res_temprec) <- names(fit.res_temprec) <- taxa
for (t in taxa){
  fit.res_temprec[[t]] <- lm.rrpp(resloess_pd_rich ~ temp + prec + temp:prec, 
                                  SS.type = "I",
                                  data = hexgrid_list[[t]])
  fit.sum <- summary(fit.res_temprec[[t]])
  fit.sum$table$Rsq
  rsq_res_temprec[t] <- fit.sum$table$Rsq
  
}
lapply(fit.res_temprec, summary)
lapply(fit.res_temprec, anova)



# Residual PD vs temp*prec*logdr ----
# LM res~temp*prec*logdr
fit.res_temprecdr <- vector('list', length(taxa))
rsq_res_temprecdr <- vector('numeric', length(taxa))
names(rsq_res_temprecdr) <- names(fit.res_temprecdr) <- taxa
for (t in taxa){
  fit.res_temprecdr[[t]] <- lm.rrpp(resloess_pd_rich ~ temp*prec*logdr, 
                                  data = hexgrid_list[[t]])
  fit.sum <- summary(fit.res_temprecdr[[t]])
  fit.sum$table$Rsq
  rsq_res_temprecdr[t] <- fit.sum$table$Rsq
  
}


lapply(fit.res_temprecdr, summary)
lapply(fit.res_temprecdr, anova)



?lm.rrpp

fit.resdr_temprec <- vector('list', length(taxa))
rsq_resdr_temprec <- vector('numeric', length(taxa))
names(rsq_resdr_temprec) <- names(fit.resdr_temprec) <- taxa
for (t in taxa){
  dat_temp <-rrpp.data.frame(x = cbind(resloess_pd_rich = hexgrid_list[[t]]$resloess_pd_rich, logdr= hexgrid_list[[t]]$logdr), temp = hexgrid_list[[t]]$temp, prec = hexgrid_list[[t]]$prec)
  fit.resdr_temprec[[t]] <- lm.rrpp(x ~ temp*prec, data= dat_temp)
  fit.sum <- summary(fit.resdr_temprec[[t]])
  fit.sum$table$Rsq
  rsq_resdr_temprec[t] <- fit.sum$table$Rsq
  
}

lapply(fit.resdr_temprec, summary)
rsq_resdr_temprec

