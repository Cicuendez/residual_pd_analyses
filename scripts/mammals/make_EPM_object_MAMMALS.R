
# Purpose ----
# Make the object EPM for all mammals 
# 
# Date: 2022/11/01

setwd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1UUotV2ZfjWpi2pbuYPKkz5zwS_tRU-IO/Hector_Jiri_PROJECTS/richness-pd-rates')

output_path_to_save <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1UUotV2ZfjWpi2pbuYPKkz5zwS_tRU-IO/Hector_Jiri_PROJECTS/richness-pd-rates/richness_pd_dr/output/epm_output_mammals.txt'
sink(file=output_path_to_save, 
     append=F, split=F)

starting_time <- Sys.time()
print("Starting! On date and time:")
print(starting_time)
print("Let's go!")

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')
easypackages::libraries(packages)

# Distribution ----
dist_file <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1UUotV2ZfjWpi2pbuYPKkz5zwS_tRU-IO/Hector_Jiri_PROJECTS/richness-pd-rates/data/distribution/mammal_dist_clean.rds'
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

#head(spList)

# CHECK FOR GEOMETRY ISSUES ----
# Although we won't go into detail here as to the how and why, 
# polygons can potentially have geometry issues -- that is, there may be issues 
# with the topology of the polygons, and this can have undesirable effects 
# downstream. We will demonstrate one way to address this potential problem 
# using the lwgeom package. Here, we will test each range polygon for problems, 
# and attempt to repair the polygon if need be.
library(lwgeom)

#for (i in 1:length(spList)) {
#  if (!any(st_is_valid(spList[[i]]))) {
#    message('\trepairing poly ', i)
#    spList[[i]] <- st_make_valid(spList[[i]])
#    if (!any(st_is_valid(spList[[i]]))) {
#      message('\t\tstill broken...')
#    }
#  }
#}

sf_use_s2(FALSE)

for (i in 1:length(spList)) {
  if (!any(st_is_valid(spList[[i]]))) {
    message('\trepairing poly ', i)
    spList[[i]] <- st_make_valid(spList[[i]])
    if (!any(st_is_valid(spList[[i]]))) {
      message('\t\tstill broken...')
    }
  }
}

#st_is_valid(st_make_valid(spList[[8228]]))
#st_is_valid(spList[[9277]])



# PROJECTION ----
# Transform data to an equal area projection
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
#spListEA <- lapply(spList, function(x) st_transform(x, st_crs(behrmann)))
spListEA <- lapply(spList, function(x) st_transform(x, crs = behrmann))

# RESOLUTION ----
# We will set the resolution to 100km (= 100000 m).
res <- 100000

# Creating an epmGrid object ----
#?createEPMgrid
mam_EPM <- createEPMgrid(spListEA, 
                           resolution = res, 
                           retainSmallRanges = TRUE, 
                           extent = 'auto', 
                           method = 'centroid', 
                           cellType = 'hexagon', 
                           nThreads = 4)

object_path_to_save <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1UUotV2ZfjWpi2pbuYPKkz5zwS_tRU-IO/Hector_Jiri_PROJECTS/richness-pd-rates/objects/EPM_mam.rds'
saveRDS(mam_EPM, object_path_to_save)
print("EPM object saved")

finish_time <- Sys.time()
print("Finishing! On date and time:")
print(finish_time)
print("---------")
total_time <- finish_time - starting_time
print("Total time running:")
print(total_time)

sink()



