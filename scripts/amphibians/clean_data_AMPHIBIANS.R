

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante')
easypackages::libraries(packages)

# Phylogeny ----
tree_file <- '../../PHYLOGENETIC_DATA/Amphibians/amph_shl_new_Consensus_7238.tre'
tree <- read.tree(tree_file)
sp_in_tree <- tree$tip.label

# Distribution data ----
dist_file <- '../../SPATIAL_DATA/Amphibians/AMPHIBIANS_living_native_dissolve.shp'
dist_sf <- st_read(dist_file)
all_sp_dist_binomial <- dist_sf$binomial
dist_sf$species <- gsub(' ', '_', all_sp_dist_binomial)
sp_in_dist <- dist_sf$species

# Shared species ----
# Get species present in both datasets: tree and distribution
length(sp_in_tree)
length(sp_in_dist)
head(sp_in_tree)
sp_in_dist[1]
grep(sp_in_dist[1], sp_in_tree)
sp_in_tree[grep(sp_in_dist[1], sp_in_tree)]

sp_in_both <- intersect(sp_in_tree, sp_in_dist)
length(sp_in_both) # 5832 species in both datasets

# Clean  tree ----
sp_to_drop_tree <- sp_in_tree[!(sp_in_tree %in% sp_in_both)]
tree_clean <- drop.tip(tree, sp_to_drop_tree)

# Clean distributions ----
dist_sf_amphibian_clean <- dist_sf[dist_sf$species %in% sp_in_both, ]

# Save data ----
write.tree(tree_clean, '../data/phylogeny/amphibian_tree_clean.tre')
saveRDS(dist_sf_amphibian_clean, 
        '../data/distribution/amphibian_dist_clean.rds', 
        version = 2)




