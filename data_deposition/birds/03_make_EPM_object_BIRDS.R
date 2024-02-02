
# Purpose ----
# Make the object EPM for all birds 

# Date: 2022/11/06
#setwd('/Users/imac2016/Dropbox/hector_working/richness_pd_dr')
setwd('/home/panthera/hector/richness_pd_dr')

output_path_to_save <- 'output/epm_output_birds.txt'
sink(file=output_path_to_save, 
     append=F, split=TRUE)

starting_time <- Sys.time()
print("Starting! On date and time:")
print(starting_time)
print("Let's go!")

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'terra', 'phytools',
              'geiger', 'picante', 'doParallel')
lapply(packages, require, character.only = TRUE)

number_of_cores <- detectCores()
print(paste0("cores available: ", number_of_cores))
registerDoParallel(cores=number_of_cores)
Sys.setenv('R_MAX_VSIZE'=64000000000)


# Distribution ----
print('Loading distribution file')
dist_file <- 'data/distribution/landbirds_dist_clean.rds'
dist_sf <- readRDS(dist_file)
print("distribution file loaded")

# spList ----
# Make a list of separate species range polygons
species_names <- dist_sf$species
spList <- vector('list', length(species_names))
names(spList) <- species_names

for (i in 1:length(species_names)) {
  spList[[i]] <- dist_sf[i,]
}


# CHECK FOR GEOMETRY ISSUES ----
# Although we won't go into detail here as to the how and why, 
# polygons can potentially have geometry issues -- that is, there may be issues 
# with the topology of the polygons, and this can have undesirable effects 
# downstream. We will demonstrate one way to address this potential problem 
# using the lwgeom package. Here, we will test each range polygon for problems, 
# and attempt to repair the polygon if need be.
library(lwgeom)


sf_use_s2(FALSE)

for (i in 1:length(spList)) {
  print(paste0('polygon ', i))
  if (!any(st_is_valid(spList[[i]]))) {
    message('\trepairing poly ', i)
    spList[[i]] <- st_make_valid(spList[[i]])
    if (!any(st_is_valid(spList[[i]]))) {
      message('\t\tstill broken...')
    }
  }
}

saveRDS(spList, 'objects/spList_landbirds.rds')

# PROJECTION ----
# Transform data to an equal area projection
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
#spListEA <- lapply(spList, function(x) st_transform(x, st_crs(behrmann)))
print('Transforming polygons to equal area projection')
print('...........................')
spListEA <- lapply(spList, function(x) st_transform(x, crs = behrmann))
print('Projection finished!')

# RESOLUTION ----
# We will set the resolution to 100km (= 100000 m).
res <- 100000

# Creating an epmGrid object ----
#?createEPMgrid
print('.........CREATING EPM OBJECT............')
bird_EPM <- createEPMgrid(spListEA, 
                           resolution = res, 
                           retainSmallRanges = TRUE, 
                           extent = 'auto', 
                           method = 'centroid', 
                           cellType = 'hexagon', 
                           nThreads = number_of_cores)

print('EPM object CREATED!')
print('.........SAVING EPM OBJECT.............')
object_path_to_save <- 'objects/EPM_landbirds.rds'
saveRDS(bird_EPM, object_path_to_save)
print("EPM object saved")

finish_time <- Sys.time()
print("Finishing! On date and time:")
print(finish_time)
print("---------")
total_time <- finish_time - starting_time
print("Total time running:")
print(total_time)





