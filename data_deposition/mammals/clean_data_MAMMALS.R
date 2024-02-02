

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante')
easypackages::libraries(packages)

# Phylogeny ----
tree_file <- '../../PHYLOGENETIC_DATA/Mammals/MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_NDexp_MCC_v2_target.nex'
tree <- read.nexus(tree_file)
sp_in_tree <- tree$tip.label

# Distribution data ----
dist_file <- '../../SPATIAL_DATA/Mammals/MAMMALS_TERRESTRIAL_ONLY_living_native_dissolve.shp'
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

name_conversion <- data.frame(sp_dist = NA, sp_tree = NA)
for(s in 1:length(sp_in_dist)){
  sp_d <- sp_in_dist[s]
  print(paste0(s, ' / ', length(sp_in_dist), ' ---> ', sp_d))
  sp_t <- sp_in_tree[grep(sp_d, sp_in_tree)]
  name_conversion[s, 1] <- sp_d
  if(length(sp_t) > 0){
    name_conversion[s, 2] <- sp_t
  }
}

# Rename tiplabels ----
# (those with distribution data)
tree_renamed <- tree
head(name_conversion)
nrow(name_conversion)
name_conversion_nona <- name_conversion %>% 
  filter(!is.na(sp_tree))
nrow(name_conversion_nona)


for (t in 1:length(tree$tip.label)){
  if (tree$tip.label[t] %in% name_conversion_nona$sp_tree){
    tree_renamed$tip.label[t] <- 
      name_conversion_nona$sp_dist[name_conversion_nona$sp_tree == tree$tip.label[t]]
  }
}

write.tree(tree_renamed, '/Volumes/GoogleDrive/.shortcut-targets-by-id/1UUotV2ZfjWpi2pbuYPKkz5zwS_tRU-IO/Hector_Jiri_PROJECTS/richness-pd-rates/data/phylogeny/mammal_tree_renamed.tre')

sp_in_tree <- tree_renamed$tip.label
sp_in_both <- intersect(sp_in_tree, sp_in_dist)
length(sp_in_both)

# Clean  tree ----
sp_to_drop_tree <- sp_in_tree[!(sp_in_tree %in% sp_in_both)]
tree_clean <- drop.tip(tree_renamed, sp_to_drop_tree)

# Clean distributions ----
dist_sf_mammal_clean <- dist_sf[dist_sf$species %in% sp_in_both, ]

# Save data ----
dir.create('data/phylogeny', recursive = TRUE)
dir.create('data/distribution')


tree_path_to_save <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1UUotV2ZfjWpi2pbuYPKkz5zwS_tRU-IO/Hector_Jiri_PROJECTS/richness-pd-rates/data/phylogeny/mammal_tree_clean.tre'
write.tree(tree_clean, tree_path_to_save)
dist_path_to_save <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1UUotV2ZfjWpi2pbuYPKkz5zwS_tRU-IO/Hector_Jiri_PROJECTS/richness-pd-rates/data/distribution/mammal_dist_clean.rds'
saveRDS(dist_sf_mammal_clean, dist_path_to_save)

#st_write(dist_sf_squamata_clean, 
#         'data/distribution/squam_dist_clean.shp', 
#         append = FALSE)







