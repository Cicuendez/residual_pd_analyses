


# Purpose ----
# Estimate the gamma statistic (Pybus and Harvey 2000 PRSB) from the 
# posterior regional phylogenies of regions with high and low residual PD. 
# - If gamma > 0: internal nodes are closer to the tips than expected under 
# the pure birth model.
# If gamma < 0: internal nodes are closer to the root than expected under 
# the pure birth model. 


# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 
              'scico', 'RColorBrewer', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'rlist')
lapply(packages, require, character.only = TRUE)

# taxa ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# sp in regions ----
# import data sp in high and low resPD regions.
sp_cramus <- readRDS('../objects/sp_cramus.rds')

# posterior trees ----
phylogeny_dir <- '../data/phylogeny'
list.files(phylogeny_dir, full.names = TRUE)[grep('tree_post_100', 
                                                  list.files(phylogeny_dir))]

post_list <- vector('list', length(taxa))
names(post_list) <- taxa

for (t in taxa){
  post_list[[t]] <- readRDS(paste0('../data/phylogeny/tree_post_100_', 
                                   toupper(t), '.rds'))
  print(paste0(t, ' posterior trees imported'))
}

# gamma object ----
# Create the object to store gamma values of each high and low resPD region
gamma_values <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  gamma_values[[t]] <- setNames(vector('list', length = 2), c('cradle', 'museum'))
}
for (t in taxa){
  for (xx in c('cradle', 'museum')){
    reg <- names(sp_cramus[[t]][[xx]])
  gamma_values[[t]][[xx]] <- setNames(vector('list', length = length(reg)), reg)
  }
}

npost <- length(post_list[[1]])

# gamma values ----
time0 <- Sys.time()
for (t in taxa){
  for (xx in c('cradle', 'museum')){
    reg <- names(sp_cramus[[t]][[xx]])
    for (r in reg){
      for (ntree in 1:npost){
        tree <- keep.tip(post_list[[t]][[ntree]], 
                         tip = sp_cramus[[t]][[xx]][[r]])
        tree_ultra <- force.ultrametric(tree)
        
        print(paste('calculating gamma stat -', t, r, ntree))
        gamma_values[[t]][[xx]][[r]][ntree] <- gammaStat(tree_ultra)
      }
    }
  }
}

time1 <- Sys.time()
print(paste0('finished estimating gamma stat in ', time1 - time0))





