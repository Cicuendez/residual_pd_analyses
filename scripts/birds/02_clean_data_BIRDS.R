

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante')
easypackages::libraries(packages)

# Phylogeny ----
#tree_file <- '../data/phylogeny/consensus/birds_all_tree.tre'
tree_file <- '/Users/cicu/Library/CloudStorage/OneDrive-UniversidadComplutensedeMadrid(UCM)/CIENCIA/PROJECTS/NICHE_BREADTH_JOAN/data/phylogeny/birds_all_tree.tre'
tree <- read.tree(tree_file) # 9993 species in phylogeny
#trees <- read.tree(tree_file)
#tree <- trees[[1]]
sp_in_tree <- tree$tip.label

# Distribution data ----
#dist_sf <- readRDS('../data/bird_dist.rds')
#dist_sf_landbirds <- readRDS('../../SPATIAL_DATA/Birds/all_birds_rds/landbirds.rds')
# landbirds: 7949 species

#dist_sf_clean <- readRDS('../../SPATIAL_DATA/Birds/all_birds_rds/birds_dist_clean.rds')
# clean: 7995 species

dist_sf_allbirds <- readRDS('../../SPATIAL_DATA/Birds/all_birds_rds/all_bird_dist.rds')
# all birds: 10993 species

dist_sf <- dist_sf_allbirds

sp_in_dist <- dist_sf$species

# Shared species ----
# Get species present in both datasets: tree and distribution
length(sp_in_tree) # 9993 species in phylo
length(sp_in_dist) # 10993 species in distribution data
head(sp_in_tree)
head(sp_in_dist)

sp_in_both <- intersect(sp_in_tree, sp_in_dist)
length(sp_in_both) # 7949

# Clean  tree ----
sp_to_drop_tree <- sp_in_tree[!(sp_in_tree %in% sp_in_both)]
tree_clean <- drop.tip(tree_renamed, sp_to_drop_tree)

# Clean distributions ----
dist_sf_bird_clean <- dist_sf[dist_sf$species %in% sp_in_both, ]

# Save data ----
dist_path_to_save <- '../data/distribution/bird_dist_clean.rds'
saveRDS(dist_sf_bird_clean, dist_path_to_save)






