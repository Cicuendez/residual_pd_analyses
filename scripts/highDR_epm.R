

# purpose ----
# CREATE HIGH DR EPM OBJECTS

# highDR species ----
highdr_sp <- readRDS('../objects/highdr_sp_list.rds')

taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# epm objects (all species) ----
epm_list <- vector('list', length(taxa))
names(epm_list) <- taxa

for (t in taxa){
  epm_list[[t]] <- readRDS(paste0('../objects/EPM_', t, '.rds'))
  print(paste0(t, ' EPM imported'))
}

# make highDR EPM objects ----
# Drop species with epm function
highdr_epm_list <- vector('list', length(taxa))
names(highdr_epm_list) <- taxa

for (t in taxa){
  sp_to_drop <- epm_list[[t]]$geogSpecies[epm_list[[t]]$geogSpecies %nin% highdr_sp[[t]]]
  print(paste0('dropping ', length(sp_to_drop), ' / ', length(epm_list[[t]]$geogSpecies), 
               ' species from ', t, ' EPM object...'))
  highdr_epm_list[[t]] <- epm::dropSpecies(epm_list[[t]], sp = sp_to_drop)
  
}

nrow(highdr_epm_list[[t]]$grid)
nrow(epm_list[[t]]$grid)

highdr_epm_list[[t]]$grid
epm_list[[t]]$grid

# save highDR EPM list
saveRDS(highdr_epm_list, '../objects/EPM_highdr_list.rds', version = 2)





















