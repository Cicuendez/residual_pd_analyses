
# Generate the hexagonal grid object with all the info: 
# richness, DR, PD

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'terra', 'phytools',
              'geiger', 'picante', 'patchwork')
lapply(packages, require, character.only = TRUE)

# PROJECTION ----
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Get world data for plotting ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_behrmann <- st_transform(world, crs = behrmann)

# Import EPM object
bird_EPM <- readRDS('objects/EPM_birds.rds')

# Get species richness grid ----
sprichness_grid <- bird_EPM$grid['spRichness']

# Add DR data ----
# Import DR data
dr_mean <- readRDS("objects/dr_mean_BIRDS.rds")
nrow(dr_mean)
head(dr_mean)
rownames(dr_mean)
dr_vector <- dr_mean$dr
names(dr_vector) <- rownames(dr_mean)

bird_EPM <- addTraits(bird_EPM, dr_vector, replace = TRUE)
head(bird_EPM$data)

# Get DR standard deviation grid ----
epm_dr_sd <- gridMetrics(bird_EPM, metric = 'variance')
colnames(epm_dr_sd$grid)[colnames(epm_dr_sd$grid) == 'variance'] <- 'dr_sd'
dr_grid_sd <- epm_dr_sd$grid['dr_sd']

# Get DR grid ----
epm_dr <- gridMetrics(bird_EPM, metric = 'mean')
colnames(epm_dr$grid)[colnames(epm_dr$grid) == 'mean'] <- 'dr'
dr_grid <- epm_dr$grid['dr']
dr_grid$logdr <- log(dr_grid$dr)
range(dr_grid$dr)

# Get PD grid ----
pd_mean <- readRDS('objects/pd_mean_BIRDS.rds')

# Get hexagonal grid with everything ----
hex_grid <- cbind(sprichness_grid, 
                  dr_sd = dr_grid_sd$dr_sd,
                  dr = dr_grid$dr, 
                  logdr = dr_grid$logdr,
                  pd = pd_mean)

colnames(hex_grid)

# Save hex_grid ----
st_crs(hex_grid) <-  behrmann
saveRDS(hex_grid, 'objects/hex_grid_BIRDS.rds')

# Dissolve world countries layer ----
world_united <- world_behrmann %>% 
  st_union() %>% # unite to a geometry object
  st_sf() # make the geometry a data frame object

# Get the grid cells that intersect with land ----
hex_grid_int <- st_intersects(hex_grid, world_united)

length(hex_grid_int[[2]]) < 1
# zeros and ones (no intersects and intersects)
zao <- sapply(hex_grid_int, length)
hex_grid_ter <- hex_grid[zao == 1, ]

nrow(hex_grid_ter)
saveRDS(hex_grid_ter, 'objects/hex_grid_ter_BIRDS.rds')

