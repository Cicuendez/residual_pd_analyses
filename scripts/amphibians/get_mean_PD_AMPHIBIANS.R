# Purpose ----
# Get the mean phylogenetic diversity (PD) with 100 posterior amphibian trees
# 
# Date: 2022/11/03
wd <- '/Users/imac2016/Dropbox/hector_working/richness_pd_dr'
setwd(wd)
sink(file="output/get_meanPD_output_AMPHIBIANS.txt", 
     append=F, split=TRUE)

starting_time <- Sys.time()
print("Starting! On date and time:")
print(starting_time)
print("Let's go!")

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'viridis', 'terra', 'phytools',
              'geiger', 'picante', 'patchwork')
easypackages::libraries(packages)

# Phylogeny ----
postrees_file <- 'objects/tree_post_100_AMPHIBIANS.rds'
postrees <- readRDS(postrees_file)
print("Posterior trees loaded")

# Import EPM object
amph_EPM <- readRDS('objects/EPM_amph.rds')
print("amph_EPM object loaded")

pd_list <- vector('list', length(postrees))

for (i in 1:length(postrees)){
     # Add phylogeny ----
     print(paste0('#### TREE ', i, '/', length(postrees), '... ####'))
     tree <- postrees[[i]]
     amph_EPM <- addPhylo(amph_EPM, tree, replace = TRUE)
     print("Phylogeny added to EPM object")

     print("running gridMetrics() to get PD...") 
     epm_pd <- gridMetrics(amph_EPM, metric = 'pd')
     print(paste0('epm_pd object ', i, '/', length(postrees), ' CREATED!'))

     # Get PD values ----
     pd_list[[i]] <- epm_pd$grid['pd']$pd

}

# Get mean PD ----
pd_mean <- rowMeans(rlist::list.cbind(pd_list))

# Save pd_mean object ----
saveRDS(pd_mean, 'objects/pd_mean_AMPHIBIANS.rds')
print("pd_mean object SAVED!")


finish_time <- Sys.time()
print("Finishing! On date and time:")
print(finish_time)
print("---------")
total_time <- finish_time - starting_time
print("Total time running:")
print(total_time)

sink()




