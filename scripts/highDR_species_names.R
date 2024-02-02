
# Purpose ----
# Get species names for the species with highest DR 

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal',
              'scico', 'viridis', 'terra', 'phytools',
              'geiger', 'picante', 'patchwork', 'doParallel', 'lwgeom')
lapply(packages, require, character.only = TRUE)

# dr data ----
dr_objects <- list.files('../objects', full.names = TRUE)[grep('dr_mean_', list.files('../objects'))]

taxa <- c('amphibians', 'birds', 'mammals', 'squamates')
dr_list <- vector('list', length = length(taxa))
names(dr_list) <- taxa

'%nin%' <- Negate('%in%')

for (t in taxa){
  fn <- dr_objects[grep(t, dr_objects, ignore.case = TRUE)]
  dr_list[[t]] <- readRDS(fn)
}

# distribution data ----
dist_objects <- list.files('data/distribution', full.names = TRUE)[grep('dist_clean', list.files('data/distribution'))]

dist_list <- vector('list', length(taxa))
names(dist_list) <- taxa

for (t in birds_taxa){
  fn <- dist_objects[grep(t, dist_objects, ignore.case = TRUE)]
  dist_list[[t]] <- readRDS(fn)
}

# get high-dr species names ----
# first take only the species present in distribution, 
# and, from that, select those with 20% highest DR

highdr_sp <- vector('list', length(taxa))
names(highdr_sp) <- taxa

for (t in birds_taxa){
  dr_df <- dr_list[[t]]
  dr_vector <- dr_df$dr
  names(dr_vector) <- rownames(dr_df)
  sp_both <- dist_list[[t]]$species[dist_list[[t]]$species %in% names(dr_vector)]
  dr <- dr_vector[sp_both]
  
  # sort by dr
  dr_sorted <- sort(dr, decreasing = TRUE)
  
  # how many species is the 20%
  n20 <- round(0.2*length(dr))
  n10 <- round(0.1*length(dr))
  n5 <- round(0.05*length(dr))
  
  # get names for high-dr species
  highdr_sp[[t]] <- names(dr_sorted[1:n20])
  #highdr_sp[[t]] <- names(dr_sorted[(n10+1):n20])
  
}

saveRDS(highdr_sp, '../objects/highdr_sp_list.rds')

