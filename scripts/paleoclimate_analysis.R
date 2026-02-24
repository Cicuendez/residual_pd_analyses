




# packages ----
library(terra)

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

## cumulative change ----
temp_cumchange <- readRDS('output/temp_cumchange.rds')
prec_cumchange <- readRDS('output/prec_cumchange.rds')

## slope ----
paleotemp_slope <- readRDS('output/temp_slope.rds')
paleoprec_slope <- readRDS('output/prec_slope.rds')

par(mfrow = c(2,2))
plot(log(temp_sd), main = 'Temp SD')
plot(temp_netchange, main = 'Temp Net change')
plot(log(temp_cumchange), main = 'Temp Cumulative change')
plot(paleotemp_slope*10000, main = 'Temp slope')

plot(log(prec_sd), main = 'prec SD')
plot(prec_netchange, main = 'prec Net change')
plot(log(prec_cumchange), main = 'prec Cumulative change')
plot(paleoprec_slope*10000, main = 'prec slope')

terra::res(temp_sd)



# import hexagons ----
hexgrid_list <- readRDS('../objects/hexgrid_list_geo_v3.rds')
# resolution: 100 km (~ 1 degree)
taxa <- names(hexgrid_list)

names(hexgrid_list[['squamates']][c('long', 'lat')])[1] <- 'lon'
head(hexgrid_list[['squamates']][c('long', 'lat')])

colnames(hexgrid_list$squamates)[19] <- 'lon'

nrow(hexgrid_list$squamates)

crs(temp_sd)
crs(hexgrid_list[['squamates']])

?project
temp_sd_reprojected <- terra::project(temp_sd, crs(hexgrid_list[[1]]))


terra::extract()
?extract
?terra::extract
class(hexgrid_list[[t]])


for (t in taxa){
  print(toupper(t))

  # get paleotemperature SD values per grid cell
  print(paste0('extracting temperature values for ', t))

  paleotemp_sd <- terra::extract(x = temp_sd_reprojected, 
                                 y = hexgrid_list[[t]][1:10, c('long', 'lat')], 
                               fun = 'mean', method = 'bilinear', na.rm = TRUE)
  hexgrid_list[[t]]$paleotemp_sd <- as.vector(paleotemp_sd)
  
  # get precipitation values per grid cell 
  print(paste0('extracting precipitation values for ', t))
  prec_vals <- raster::extract(x = prec_ea, y = hexgrid_list[[t]], 
                               fun = mean, na.rm = TRUE)
  hexgrid_list[[t]]$prec <- as.vector(prec_vals)
  
  print('##############################')
}








