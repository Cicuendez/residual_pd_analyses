
# Purpose ----
# Get species names for the species with lowest DR 

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
nobirds_taxa <- taxa[taxa %nin% c('birds')]
birds_taxa <- 'birds'
taxa <- c('amphibians', 'mammals')

for (t in taxa){
  fn <- dr_objects[grep(t, dr_objects, ignore.case = TRUE)]
  dr_list[[t]] <- readRDS(fn)
}

# distribution data ----
dist_objects <- list.files('../data/distribution', full.names = TRUE)[grep('dist_clean', list.files('../data/distribution'))]

dist_list <- vector('list', length(taxa))
names(dist_list) <- taxa

for (t in taxa){
  fn <- dist_objects[grep(t, dist_objects, ignore.case = TRUE)]
  dist_list[[t]] <- readRDS(fn)
}

# get low-dr species names ----
# first take only the species present in distribution, 
# and, from that, select those with 20% lowest DR

lowdr_sp <- vector('list', length(taxa))
names(lowdr_sp) <- taxa

for (t in taxa){
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
  lowdr_sp[[t]] <- names(dr_sorted[1:n20])
  #lowdr_sp[[t]] <- names(dr_sorted[(n10+1):n20])
  
}

saveRDS(lowdr_sp, '../objects/lowdr_sp_list.rds')


# purpose ----
# CREATE LOW DR EPM OBJECTS

# lowdr species ----
lowdr_sp <- readRDS('../objects/lowdr_sp_list.rds')

# epm objects (all species) ----
epm_list <- vector('list', length(taxa))
names(epm_list) <- taxa

for (t in taxa){
  epm_list[[t]] <- readRDS(paste0('../objects/EPM_', t, '.rds'))
  print(paste0(t, ' EPM imported'))
}

# make lowdr EPM objects ----
# Drop species with epm function
lowdr_epm_list <- vector('list', length(taxa))
names(lowdr_epm_list) <- taxa

for (t in taxa){
  sp_to_drop <- epm_list[[t]]$geogSpecies[epm_list[[t]]$geogSpecies %nin% lowdr_sp[[t]]]
  print(paste0('dropping ', length(sp_to_drop), ' / ', length(epm_list[[t]]$geogSpecies), 
               ' species from ', t, ' EPM object...'))
  lowdr_epm_list[[t]] <- epm::dropSpecies(epm_list[[t]], sp = sp_to_drop)
  
}

nrow(lowdr_epm_list[[t]]$grid)
nrow(epm_list[[t]]$grid)

lowdr_epm_list[[t]]$grid
epm_list[[t]]$grid

# save lowdr EPM list
saveRDS(lowdr_epm_list, '../objects/EPM_lowdr_list.rds', version = 2)


lowdr_richness_grid_list <- vector('list', length(taxa))
names(lowdr_richness_grid_list) <- taxa

for (t in taxa){
  lowdr_richness_grid_list[[t]] <- lowdr_epm_list[[t]]$grid['spRichness']
}


saveRDS(lowdr_richness_grid_list, 'objects/lowdr_richness_grid_list.rds')


