




# Load packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')
easypackages::libraries(packages)

# Import EPM object ----
squam_EPM <- readRDS('objects/squam_EPM.rds')

# Import phylogeny ----
tree_file <- 'data/phylogeny/squam_tree_clean.tre'
tree <- read.tree(tree_file)

# Import DR data ----
dr_mean <- readRDS("objects/dr_mean.rds")
nrow(dr_mean)


# Add phylogeny to EPM ----
squam_EPM <- addPhylo(squam_EPM, tree)

# Add DR data to EPM ----
squam_EPM_dr <- addTraits(squam_EPM, data = dr_mean)


epm_dr <- gridMetrics(squam_EPM, metric = 'mean')
?gridMetrics










