

# date ----
# 16/11/2022 Wednesday, Prague

# purpose ----
# Get the 20% of species with highest DR and get the species richness map 
# for them. 

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal',
              'scico', 'viridis', 'terra', 'phytools',
              'geiger', 'picante', 'patchwork', 'doParallel', 'lwgeom')
lapply(packages, require, character.only = TRUE)

# dr data ----
dr_objects <- list.files('../objects', full.names = TRUE)[grep('dr_mean_', list.files('../objects'))]

taxa <- c('amphibians', 'birds', 'mammals', 'squamates')
dr_list <- vector('list', length = length(taxa))
names(dr_list) <- taxa

'%nin%' <- Negate('%in%')

nobirds_taxa <- taxa[taxa %nin% c('birds')]
birds_taxa <- 'birds'

# grep('squamates', dr_objects, ignore.case = TRUE)

for (t in birds_taxa){
  fn <- dr_objects[grep(t, dr_objects, ignore.case = TRUE)]
  dr_list[[t]] <- readRDS(fn)
}

# if you could see the order of the files you can just do it with lapply
#dr_list <- lapply(dr_objects, readRDS)
#names(dr_list) <- taxa

# distribution data ----
dist_objects <- list.files('data/distribution', full.names = TRUE)[grep('dist_clean', list.files('data/distribution'))]

dist_list <- vector('list', length(taxa))
names(dist_list) <- taxa

for (t in birds_taxa){
  fn <- dist_objects[grep(t, dist_objects, ignore.case = TRUE)]
  dist_list[[t]] <- readRDS(fn)
}

Sys.setenv('R_MAX_VSIZE'=64000000000)

# get high-dr species names ----
# first take only the species present in distribution, 
# and, from that, select those with 20% highest DR

highdr_sp <- vector('list', length(taxa))
names(highdr_sp) <- taxa

for (t in birds_taxa){
  dr_df <- dr_list[[t]]
  dr_vector <- dr_df$dr
  names(dr_vector) <- rownames(dr_df)
  sp_both <- dist_list[[t]]$species[dist_list[[t]]$species %in% names(dr_vector)]
  dr <- dr_vector[sp_both]
  
  # sort by dr
  dr_sorted <- sort(dr, decreasing = TRUE)
  
  # how many species is the 20%
  n20 <- round(0.2*length(dr))
  n10 <- round(0.1*length(dr))
  n5 <- round(0.05*length(dr))
  
  # get names for high-dr species
  highdr_sp[[t]] <- names(dr_sorted[1:n5])
  #highdr_sp[[t]] <- names(dr_sorted[(n10+1):n20])
  
}

saveRDS(highdr_sp, '../objects/highdr_sp_list.rds')

# filter distribution data by dr ----
highdr_dist <- vector('list', length(taxa))
names(highdr_dist) <- taxa

for (t in birds_taxa){
  highdr_dist[[t]] <- dist_list[[t]] %>%
    filter(species %in% highdr_sp[[t]])
}

# set resolution ----
res <- 100000

# set projection ----
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# n cores ----
number_of_cores <- detectCores()

# make epm ----
# set the extent of the whole distribution (to compare it with the 
# richness map of all species)
library(lwgeom)
sf_use_s2(FALSE)

highdr_epm <- vector('list', length(taxa))
names(highdr_epm) <- taxa

for (t in birds_taxa){
  print(t)
  dist_sf <- highdr_dist[[t]]

  # spList ----
  # Make a list of separate species range polygons
  species_names <- dist_sf$species
  spList <- vector('list', length(species_names))
  names(spList) <- species_names
  
  for (i in 1:length(species_names)) {
    spList[[i]] <- dist_sf[i,]
  }
  
  # CHECK FOR GEOMETRY ISSUES ----
  for (i in 1:length(spList)) {
    print(paste0('poly ', i))
    if (!any(st_is_valid(spList[[i]]))) {
      message('\trepairing poly ', i)
      spList[[i]] <- st_make_valid(spList[[i]])
      if (!any(st_is_valid(spList[[i]]))) {
        message('\t\tstill broken...')
      }
    }
  }
  
  # PROJECTION ----
  # Transform data to an equal area projection
  spListEA <- lapply(spList, function(x) st_transform(x, crs = behrmann))
  # I think there's a problem with species 511
  spListEA2 <- spListEA[-511]
  length(spListEA2)

  print('.........CREATING EPM OBJECT............')
  print(t)

  highdr_epm[[t]] <- createEPMgrid(spListEA2, 
                                   resolution = res, 
                                   retainSmallRanges = TRUE, 
                                   extent = dist_list[[t]], 
                                   method = 'centroid', 
                                   cellType = 'hexagon', 
                                   nThreads = number_of_cores, 
                                   checkValidity = FALSE, 
                                   verbose = TRUE)
}


# BIRDS ----
highdr_dist_birds <- readRDS('../../SPATIAL_DATA/Birds/highDR_birds/highdr_dist_birds.rds')
highdr_birds_sp <- highdr_dist_birds$species
bird_EPM <- readRDS('../objects/EPM_birds.rds')

# Manually drop species from each cell
#birdlist <- epm::expandSpeciesCellList(bird_EPM)
#indr <- function(x){
#  length(x[x %in% highdr_birds_sp])
#}
#
#highdr_bird_richness <- sapply(birdlist, indr)
#bird_EPM_highdr_grid <- bird_EPM$grid
#bird_EPM_highdr_grid$spRichness <- highdr_bird_richness
#richness_grid <- bird_EPM_highdr_grid

# Drop species with epm function
sp_to_drop <- bird_EPM$geogSpecies[bird_EPM$geogSpecies %nin% highdr_birds_sp]
bird_EPM_highdr <- epm::dropSpecies(bird_EPM, sp = sp_to_drop)
richness_grid <- bird_EPM_highdr$grid

highdr_epm[['birds']] <- bird_EPM_highdr

saveRDS(highdr_epm, '../objects/EPM_highdr.rds', version = 2)

highdr_epm <- readRDS('objects/EPM_highdr.rds')

# Get world data for plotting ----
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_behrmann <- st_transform(world, crs = behrmann)

library(lwgeom)
sf_use_s2(FALSE)

# Dissolve world countries layer to crop bird grid ----
world_united <- world_behrmann %>% 
  st_union() %>% # unite to a geometry object
  st_sf() # make the geometry a data frame object

saveRDS(world_united, 'objects/world_dissolved.rds')

# Get the grid cells that intersect with land ----
richness_grid_birds <- highdr_epm[['birds']]$grid['spRichness']
landbird_int <- st_intersects(richness_grid_birds, world_united)

# zeros and ones (no intersects and intersects)
zao <- sapply(landbird_int, length)
richness_grid_landbirds <- richness_grid_birds[zao == 1, ]

nrow(richness_grid_landbirds)


highdr_richness_grid_list <- vector('list', length(taxa))
names(highdr_richness_grid_list) <- taxa

for (t in nobirds_taxa){
  highdr_richness_grid_list[[t]] <- highdr_epm[[t]]$grid['spRichness']
}

highdr_richness_grid_list[['birds']] <- richness_grid_landbirds

saveRDS(highdr_richness_grid_list, 'objects/highdr_richness_grid_list.rds')

highdr_richness_grid_list <- readRDS('../objects/highdr_richness_grid_list.rds')

# plot richness ----

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









