


packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal',
              'scico', 'viridis', 'terra', 'phytools',
              'geiger', 'picante', 'patchwork', 'doParallel', 'lwgeom')
lapply(packages, require, character.only = TRUE)



highdr_dist_birds <- readRDS('../../SPATIAL_DATA/Birds/highDR_birds/highdr_dist_birds.rds')
highdr_birds_spatial <- as(highdr_dist_birds[-c(511),], 'Spatial')

grid_spatial <- readRDS('../../SPATIAL_DATA/Birds/highDR_birds/landbird_grid_spatial.rds')

# intersect
int <- gIntersects(grid_spatial, highdr_birds_spatial[1:50,], byid = TRUE, returnDense = FALSE)


plot(highdr_birds_spatial[66,])





