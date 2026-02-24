


# packages ----
library(terra)
library(sf)
library(tidyverse)
#library(tidyterra)

# show progress
terraOptions(progress = 1)

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



# import hexagons ----
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








