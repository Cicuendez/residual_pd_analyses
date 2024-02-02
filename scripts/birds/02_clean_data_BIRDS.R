

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante')
easypackages::libraries(packages)

# Phylogeny ----
tree_file <- '../PHYLOGENETIC_DATA/Birds/AllBirdsHackett1.tre'
trees <- read.tree(tree_file)
tree <- trees[[1]]
sp_in_tree <- tree$tip.label

# Distribution data ----
dist_sf <- readRDS('data/bird_dist.rds')
sp_in_dist <- dist_sf$species

# Shared species ----
# Get species present in both datasets: tree and distribution
length(sp_in_tree)
length(sp_in_dist)
head(sp_in_tree)
head(sp_in_dist)

sp_in_both <- intersect(sp_in_tree, sp_in_dist)
length(sp_in_both)

# Clean  tree ----
sp_to_drop_tree <- sp_in_tree[!(sp_in_tree %in% sp_in_both)]
tree_clean <- drop.tip(tree_renamed, sp_to_drop_tree)

# Clean distributions ----
dist_sf_bird_clean <- dist_sf[dist_sf$species %in% sp_in_both, ]

# Save data ----
dist_path_to_save <- '../data/distribution/bird_dist_clean.rds'
saveRDS(dist_sf_bird_clean, dist_path_to_save)






