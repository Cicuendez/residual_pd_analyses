




# packages ----
library(terra)

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









