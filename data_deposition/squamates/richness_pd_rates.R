# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')
easypackages::libraries(packages)

# Phylogeny ----
tree_file <- 'data/phylogeny/squam_tree_clean.tre'
tree <- read.tree(tree_file)

# PROJECTION ----
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Get world data for plotting ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
ggplot(world) +
  geom_sf()
crs(world) <- behrmann


plot(world$geometry, col = 'gray90', border = 'gray90')

# Distribution ----
dist_file <- 'data/distribution/squam_dist_clean.rds'
dist_sf <- readRDS(dist_file)
#dist_sf <- st_read(dist_file)


# Import EPM object
squam_EPM <- readRDS('../objects/EPM_squamates.rds')

# create an interactive plot where you can click to see which species occur somewhere. 
identify(squam_EPM, pal = viridis)


# Add phylogeny ----
squam_EPM <- addPhylo(squam_EPM, tree)
?addPhylo

# Get species richness grid ----
sprichness_grid <- squam_EPM$grid['spRichness']
#
## Get DR grid ----
#starting_time <- Sys.time()
#print("Starting! On date and time:")
#print(starting_time)
#
#epm_dr <- gridMetrics(squam_EPM, metric = 'DR')
#
#finish_time <- Sys.time()
#print("Finishing! On date and time:")
#print(finish_time)
#print("---------")
#total_time <- finish_time - starting_time
#print("Total time running:")
#print(total_time)

# Add DR data ----
# Import DR data
dr_mean <- readRDS("../objects/dr_mean_SQUAMATES.rds")
nrow(dr_mean)
head(dr_mean)
rownames(dr_mean)
dr_vector <- dr_mean$dr
names(dr_vector) <- rownames(dr_mean)

squam_EPM$speciesList

squam_EPM <- addTraits(squam_EPM, dr_vector, replace = TRUE)
head(squam_EPM$data)

# Get DR standard deviation grid ----
epm_dr_sd <- gridMetrics(squam_EPM, metric = 'variance')
colnames(epm_dr_sd$grid)[colnames(epm_dr_sd$grid) == 'variance'] <- 'dr_sd'
dr_grid_sd <- epm_dr_sd$grid['dr_sd']

# Get DR grid ----
epm_dr <- gridMetrics(squam_EPM, metric = 'mean')
colnames(epm_dr$grid)[colnames(epm_dr$grid) == 'mean'] <- 'dr'
dr_grid <- epm_dr$grid['dr']
dr_grid$logdr <- log(dr_grid$dr)
range(dr_grid$dr)
hex_grid$logdr <- log(hex_grid$dr)


# Get PD grid ----
#epm_pd <- gridMetrics(squam_EPM, metric = 'pd')
#epm_pd <- readRDS('objects/epm_pd.rds')
?gridMetrics

#pd_grid <- epm_pd$grid['pd']


pd_mean <- readRDS('../objects/pd_mean.rds')

hex_grid <- cbind(sprichness_grid, 
                  dr_sd = dr_grid_sd$dr_sd,
                  dr = dr_grid$dr, 
                  logdr = dr_grid$logdr,
                  pd = pd_mean)
hex_grid$id <- rownames(hex_grid)
hex_grid$group <- 'squamates'

colnames(hex_grid)


world_behrmann <- st_transform(world, crs = behrmann)





# Save hex_grid ----
saveRDS(hex_grid, '../objects/hex_grid_SQUAMATES.rds')



