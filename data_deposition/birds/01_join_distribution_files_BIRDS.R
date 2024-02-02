

packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')
easypackages::libraries(packages)


# Distribution data ----
dist.dir <- '../../SPATIAL_DATA/Birds/'
shp.files <- list.files(dist.dir, full.names = TRUE)[grep('.shp$', list.files(dist.dir))]
dist.list <- lapply(shp.files, st_read)
dist_sf <- bind_rows(dist.list)

all_sp_dist_binomial <- dist_sf$sci_name
dist_sf$species <- gsub(' ', '_', all_sp_dist_binomial)
all_sp_dist <- dist_sf$species
saveRDS(dist_sf, 'data/bird_dist.rds')
