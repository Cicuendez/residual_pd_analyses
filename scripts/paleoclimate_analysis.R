

# packages ----
library(terra)
library(sf)
library(tidyverse)
library(patchwork)
#library(tidyterra)

# show progress
terraOptions(progress = 1)

# import hexagons ----
if ('hexgrid_list_paleoclimate.rds' %in% list.files('../objects/')){
  hexgrid_list <- readRDS('../objects/hexgrid_list_paleoclimate.rds')
  taxa <- names(hexgrid_list)
}


if (!'hexgrid_list_paleoclimate.rds' %in% list.files('../objects/')){
  
  # import paleoclimate variables ----
  # These are derived from the PALEO-PGEM paleoclimatic layers spanning 
  # from 5 Ma to 1950.
  # Resolution: 1 degree x 1 degree. 
  
  ## standard deviation ----
  temp_sd <- readRDS('output/temp_sd.rds')
  prec_sd <- readRDS('output/prec_sd.rds')
  
  ## net change ----
  temp_netchange <- readRDS('output/temp_netchange.rds')
  prec_netchange <- readRDS('output/prec_netchange.rds')
  names(temp_netchange) <- names(prec_netchange) <- 'netchange'
  
  ## cumulative change ----
  temp_cumchange <- readRDS('output/temp_cumchange.rds')
  prec_cumchange <- readRDS('output/prec_cumchange.rds')
  
  ## slope ----
  temp_slope <- readRDS('output/temp_slope.rds')
  prec_slope <- readRDS('output/prec_slope.rds')
  names(temp_slope) <- names(prec_slope) <- 'slope'
  
  #par(mfrow = c(2,2))
  #plot(log(temp_sd), main = 'Temp SD')
  #plot(temp_netchange, main = 'Temp Net change')
  #plot(log(temp_cumchange), main = 'Temp Cumulative change')
  #plot(paleotemp_slope*10000, main = 'Temp slope')
  #
  #plot(log(prec_sd), main = 'prec SD')
  #plot(prec_netchange, main = 'prec Net change')
  #plot(log(prec_cumchange), main = 'prec Cumulative change')
  #plot(paleoprec_slope*10000, main = 'prec slope')
  #
  #terra::res(temp_sd)
  
  
  
  
  hexgrid_list <- readRDS('../objects/hexgrid_list_geo_v3.rds')
  proj_hexgrid <- crs(hexgrid_list[[1]])
  # resolution: 100 km (~ 1 degree)
  taxa <- names(hexgrid_list)
  
  # reproject rasters ----
  temp_sd_proj <- terra::project(temp_sd, proj_hexgrid)
  prec_sd_proj <- terra::project(prec_sd, proj_hexgrid)
  temp_netchange_proj <- terra::project(temp_netchange, proj_hexgrid)
  prec_netchange_proj <- terra::project(prec_netchange, proj_hexgrid)
  temp_cumchange_proj <- terra::project(temp_cumchange, proj_hexgrid)
  prec_cumchange_proj <- terra::project(prec_cumchange, proj_hexgrid)
  temp_slope_proj <- terra::project(temp_slope, proj_hexgrid)
  prec_slope_proj <- terra::project(prec_slope, proj_hexgrid)
  
  
  #world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  #world_projected <- st_transform(world, crs = proj_hexgrid)
  #
  #temp_sd_sf <- as.data.frame(temp_sd_reprojected, xy = TRUE, na.rm = TRUE)
  #
  #ggplot() +
  #  geom_sf(data = world_projected,
  #          col = 'transparent',
  #          fill = 'gray80') +
  #  geom_raster(data = temp_sd_sf,
  #              aes(x = x, y = y, fill = sd)) +
  #  
  #  geom_sf(data = hexgrid_list[[t]],
  #          aes(fill = resloess_pd_rich),
  #          col = 'transparent')
  
  
  for (t in taxa){
    print(toupper(t))
    
    # get paleotemperature SD values per grid cell
    print(paste0('extracting temp SD values for ', t))
    
    paleotemp_sd <- terra::extract(x = temp_sd_proj, 
                                   y = hexgrid_list[[t]][, c('long', 'lat')], 
                                   fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleotemp_sd <- paleotemp_sd$sd
    
    # get paleoprecipitation SD values per grid cell 
    print(paste0('extracting prec SD values for ', t))
    paleoprec_sd <- terra::extract(x = prec_sd_proj, 
                                   y = hexgrid_list[[t]][, c('long', 'lat')], 
                                   fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleoprec_sd <- paleoprec_sd$sd
    
    # get paleotemperature net change values per grid cell
    print(paste0('extracting temp net change values for ', t))
    
    paleotemp_netchange <- terra::extract(x = temp_netchange_proj, 
                                          y = hexgrid_list[[t]][, c('long', 'lat')], 
                                          fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleotemp_netchange <- paleotemp_netchange$netchange
    
    # get paleoprecipitation net change values per grid cell
    print(paste0('extracting prec net change values for ', t))
    
    paleoprec_netchange <- terra::extract(x = prec_netchange_proj, 
                                          y = hexgrid_list[[t]][, c('long', 'lat')], 
                                          fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleoprec_netchange <- paleoprec_netchange$netchange
    
    # get paleotemperature cumulative change values per grid cell
    print(paste0('extracting temp cumulative change values for ', t))
    
    paleotemp_cumchange <- terra::extract(x = temp_cumchange_proj, 
                                          y = hexgrid_list[[t]][, c('long', 'lat')], 
                                          fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleotemp_cumchange <- paleotemp_cumchange$sum
    
    # get paleoprecipitation cumulative change values per grid cell
    print(paste0('extracting prec cumulative change values for ', t))
    
    paleoprec_cumchange <- terra::extract(x = prec_cumchange_proj, 
                                          y = hexgrid_list[[t]][, c('long', 'lat')], 
                                          fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleoprec_cumchange <- paleoprec_cumchange$sum
    
    # get paleotemperature slope values per grid cell
    print(paste0('extracting temp slope values for ', t))
    
    paleotemp_slope <- terra::extract(x = temp_slope_proj, 
                                      y = hexgrid_list[[t]][, c('long', 'lat')], 
                                      fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleotemp_slope <- paleotemp_slope$slope
    
    # get paleoprecipitation slope values per grid cell
    print(paste0('extracting prec slope values for ', t))
    
    paleoprec_slope <- terra::extract(x = prec_slope_proj, 
                                      y = hexgrid_list[[t]][, c('long', 'lat')], 
                                      fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleoprec_slope <- paleoprec_slope$slope
    
    print('##############################')
  }
  
  saveRDS(hexgrid_list, '../objects/hexgrid_list_paleoclimate.rds')
  
}


# PALEOCLIMATE VIOLIN PLOTS ----


# import data sp in high and low resPD regions.
sp_cramus <- readRDS('../objects/sp_cramus.rds')
names(sp_cramus)
names(sp_cramus$squamates$cradle)
names(sp_cramus$squamates$museum)



cradles_and_museums <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  cradles_and_museums[[t]] <- setNames(vector('list', length = 2), c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    cradles_and_museums[[t]][[typ]] <- names(sp_cramus[[t]][[typ]])
  }
}

# this is to keep only the cradle or the museum cells appropriate according to our 
# designation of global regions (for instance, there are some cradle cells in the 
# overall museum region of south_africa, so we want to keep only the museum cells)
x <- data.frame(hexgrid_list[[t]] %>% filter(type == typ & geo %in% cradles_and_museums[[t]][[typ]]))
x[x$geo == 'south_africa',]$type  



# PALEOCLIMATE SPACES ----
colnames(hexgrid_list[[t]])[grep('paleo', colnames(hexgrid_list[[t]]))]

crad_col <- '#2F6BA8' # blue
mus_col <- '#A72E37' # red
mid_col <- 'gray80'

typ_colors <- setNames(c(crad_col, mus_col), c('cradle', 'museum'))


pointsize <- 1
transparency <- 0.6

## > Paleoclimate SD ----
paleoclimate_sd_plot <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  paleoclimate_sd_plot[[t]] <- setNames(vector('list', length = 2), 
                                        c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    paleoclimate_sd_plot[[t]][[typ]] <- 
      ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
             aes(x = paleotemp_sd, y = paleoprec_sd)) + 
      geom_point(color = 'gray80', size = pointsize) +
      geom_point(data = hexgrid_list[[t]] %>% filter(type == typ), 
                 color = typ_colors[typ],
                 size = pointsize, alpha = transparency) +
      #lims(x = c(-20, 30)) +
      labs(title = paste0(t, ' paleoclimatic SD - ', typ)) +
      theme_minimal() +
      theme(legend.position = 'none', 
            legend.key.height = unit(0.2,"cm"),
            plot.title = element_text(hjust = 0.5, 
                                      face = 'bold'),
            legend.text = element_text(angle = 0, hjust = 1))
  }
  
  
}

paleoclimate_sd_plot <- unlist(paleoclimate_sd_plot, recursive = FALSE)
wrap_plots(paleoclimate_sd_plot, ncol = 2)

ggplot(data = hexgrid_list[['squamates']] %>% filter(type %in% c('cradle', 'museum'))) +
  geom_violin(aes(x = geo, y = log(paleotemp_sd), fill = type)) +
  scale_fill_manual(values = typ_colors) + 
  theme_classic()



ggplot(data = hexgrid_list[['birds']]) +
  geom_point(aes(x = log(paleotemp_sd), y = logdr, col = resloess_pd_rich)) +
  scale_color_gradient2(low = '#2F6BA8', # blue
                       mid = "gray90",
                       high = '#A72E37', # red
                       midpoint = 0, 
                       name = 'residual PD') +
  theme_classic()

colnames(hexgrid_list[['birds']])
unique(hexgrid_list[['birds']]$geo)


## > Paleoclimate net change ----
paleoclimate_netchange_plot <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  paleoclimate_netchange_plot[[t]] <- setNames(vector('list', length = 2), 
                                        c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    paleoclimate_netchange_plot[[t]][[typ]] <- 
      ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
             aes(x = paleotemp_netchange, y = paleoprec_netchange)) + 
      geom_point(color = 'gray80', size = pointsize) +
      geom_point(data = hexgrid_list[[t]] %>% filter(type == typ), 
                 color = typ_colors[typ],
                 size = pointsize, alpha = transparency) +
      #lims(x = c(-20, 30)) +
      labs(title = paste0(t, ' clim net change - ', typ)) +
      theme_minimal() +
      theme(legend.position = 'none', 
            legend.key.height = unit(0.2,"cm"),
            plot.title = element_text(hjust = 0.5, 
                                      face = 'bold'),
            legend.text = element_text(angle = 0, hjust = 1))
  }
  
  
}

paleoclimate_netchange_plot <- unlist(paleoclimate_netchange_plot, recursive = FALSE)
wrap_plots(paleoclimate_netchange_plot, ncol = 2)

## > Paleoclimate net change ----
paleoclimate_cumchange_plot <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  paleoclimate_cumchange_plot[[t]] <- setNames(vector('list', length = 2), 
                                               c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    paleoclimate_cumchange_plot[[t]][[typ]] <- 
      ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
             aes(x = paleotemp_cumchange, y = paleoprec_cumchange)) + 
      geom_point(color = 'gray80', size = pointsize) +
      geom_point(data = hexgrid_list[[t]] %>% filter(type == typ), 
                 color = typ_colors[typ],
                 size = pointsize, alpha = transparency) +
      #lims(x = c(-20, 30)) +
      labs(title = paste0(t, ' clim cum change - ', typ)) +
      theme_minimal() +
      theme(legend.position = 'none', 
            legend.key.height = unit(0.2,"cm"),
            plot.title = element_text(hjust = 0.5, 
                                      face = 'bold'),
            legend.text = element_text(angle = 0, hjust = 1))
  }
  
  
}

paleoclimate_cumchange_plot <- unlist(paleoclimate_cumchange_plot, recursive = FALSE)
wrap_plots(paleoclimate_cumchange_plot, ncol = 2)

## > Paleoclimate slope ----
paleoclimate_slope_plot <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  paleoclimate_slope_plot[[t]] <- setNames(vector('list', length = 2), 
                                               c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    paleoclimate_slope_plot[[t]][[typ]] <- 
      ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
             aes(x = paleotemp_slope, y = paleoprec_slope)) + 
      geom_point(color = 'gray80', size = pointsize) +
      geom_point(data = hexgrid_list[[t]] %>% filter(type == typ), 
                 color = typ_colors[typ],
                 size = pointsize, alpha = transparency) +
      #lims(x = c(-20, 30)) +
      labs(title = paste0(t, ' paleoclim slope - ', typ)) +
      theme_minimal() +
      theme(legend.position = 'none', 
            legend.key.height = unit(0.2,"cm"),
            plot.title = element_text(hjust = 0.5, 
                                      face = 'bold'),
            legend.text = element_text(angle = 0, hjust = 1))
  }
  
  
}

paleoclimate_slope_plot <- unlist(paleoclimate_slope_plot, recursive = FALSE)
wrap_plots(paleoclimate_slope_plot, ncol = 2)















