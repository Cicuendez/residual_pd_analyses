
# Generate the hexagonal grid object with all the info: 
# richness, DR, PD

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')
easypackages::libraries(packages)

# PROJECTION ----
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Get world data for plotting ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_behrmann <- st_transform(world, crs = behrmann)

# Import EPM object
amph_EPM <- readRDS('../objects/EPM_amphibians.rds')

# Get species richness grid ----
sprichness_grid <- amph_EPM$grid['spRichness']

# Add DR data ----
# Import DR data
dr_mean <- readRDS("../objects/dr_mean_AMPHIBIANS.rds")
nrow(dr_mean)
head(dr_mean)
rownames(dr_mean)
dr_vector <- dr_mean$dr
names(dr_vector) <- rownames(dr_mean)

amph_EPM <- addTraits(amph_EPM, dr_vector, replace = TRUE)
head(amph_EPM$data)

# Get DR standard deviation grid ----
epm_dr_sd <- gridMetrics(amph_EPM, metric = 'variance')
colnames(epm_dr_sd$grid)[colnames(epm_dr_sd$grid) == 'variance'] <- 'dr_sd'
dr_grid_sd <- epm_dr_sd$grid['dr_sd']

# Get DR grid ----
epm_dr <- gridMetrics(amph_EPM, metric = 'mean')
colnames(epm_dr$grid)[colnames(epm_dr$grid) == 'mean'] <- 'dr'
dr_grid <- epm_dr$grid['dr']
dr_grid$logdr <- log(dr_grid$dr)
range(dr_grid$dr)

# Get PD grid ----
pd_mean <- readRDS('../objects/pd_mean_AMPHIBIANS.rds')

# Get hexagonal grid with everything ----
hex_grid <- cbind(sprichness_grid, 
                  dr_sd = dr_grid_sd$dr_sd,
                  dr = dr_grid$dr, 
                  logdr = dr_grid$logdr,
                  pd = pd_mean)

colnames(hex_grid)

# Save hex_grid ----
saveRDS(hex_grid, '../objects/hex_grid_AMPHIBIANS.rds')



