# Purpose ----
# Get the phylogenetic diversity (PD) from squamate EPM object
# 
# Date: 2022/06/10


#wd <- '/Users/imac2016/Google Drive/CIENCIA/PROJECTS/richness-pd-rates'
#setwd(wd)
sink(file="get_PD_output.txt", 
     append=F, split=TRUE)

starting_time <- Sys.time()
print("Starting! On date and time:")
print(starting_time)
print("Let's go!")

# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')
easypackages::libraries(packages)

# Phylogeny ----
tree_file <- 'data/phylogeny/squam_tree_clean.tre'
tree <- read.tree(tree_file)

print("Phylogeny loaded")

# PROJECTION ----
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Import EPM object
squam_EPM <- readRDS('objects/squam_EPM.rds')
print("squam_EPM object loaded")

# Add phylogeny ----
squam_EPM <- addPhylo(squam_EPM, tree)
print("Phylogeny added to EPM object")

print("running gridMetrics() to get PD...")
epm_pd <- gridMetrics(squam_EPM, metric = 'pd')
print("epm_pd object CREATED!")


saveRDS(epm_pd, 'objects/epm_pd.rds')
print("epm_pd object SAVED!")

finish_time <- Sys.time()
print("Finishing! On date and time:")
print(finish_time)
print("---------")
total_time <- finish_time - starting_time
print("Total time running:")
print(total_time)

sink()




