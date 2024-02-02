

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante')
easypackages::libraries(packages)

# Phylogeny ----
tree_file <- '../../PHYLOGENETIC_DATA/Reptiles/Tonini/tonini_tree.tre'
tree <- read.tree(tree_file)
sp_in_tree <- tree$tip.label

# Distribution data ----
#dist_file <- '/Volumes/GoogleDrive/Mi unidad/CIENCIA/DATA_GENERAL/Roll_etal_2021_GARD/GARD_1_5_ranges.shp'
dist_file <- '../../SPATIAL_DATA/Reptiles/GARD_1_5_ranges.shp'
dist_sf <- st_read(dist_file)
all_sp_dist_binomial <- dist_sf$binomial
dist_sf$species <- gsub(' ', '_', all_sp_dist_binomial)
all_sp_dist <- dist_sf$species
length(dist_sf$Family[dist_sf$Family == 'Lacertidae'])

# Take Squamata and tuatara only
levels(dist_sf$group)
dist_sf_squamata <- dist_sf[!dist_sf$group %in% c('croc', 'turtle'), ]
sp_in_dist <- dist_sf_squamata$species

# Shared species ----
# Get species present in both dataset: tree and distribution
length(sp_in_tree)
length(sp_in_tree[sp_in_tree %in% sp_in_dist])

length(sp_in_dist)
length(sp_in_dist[sp_in_dist %in% sp_in_tree])

sp_in_both <- intersect(sp_in_tree, sp_in_dist)
length(sp_in_both)


# Clean  tree ----
sp_to_drop_tree <- sp_in_tree[!(sp_in_tree %in% sp_in_both)]
tree_clean <- drop.tip(tree, sp_to_drop_tree)

# Clean distributions ----
dist_sf_squamata_clean <- dist_sf_squamata[dist_sf_squamata$species %in% sp_in_both, ]

# Save data ----
dir.create('data/phylogeny', recursive = TRUE)
dir.create('data/distribution')

write.tree(tree_clean, 'data/phylogeny/squam_tree_clean.tre')
saveRDS(dist_sf_squamata_clean, '../data/distribution/squam_dist_clean.rds', 
        version = 2)

#st_write(dist_sf_squamata_clean, 
#         'data/distribution/squam_dist_clean.shp', 
#         append = FALSE)







