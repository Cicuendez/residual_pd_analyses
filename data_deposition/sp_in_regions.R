
# date ----
# 11/11/2022

# purpose ----
# Get the name of the species present in each region 

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')

lapply(packages, require, character.only = TRUE)

# region data ----
africa_sf <- readRDS('../data/continents/africa.rds')
australia_sf <- readRDS('../data/continents/australia.rds')
n.america_sf <- readRDS('../data/continents/n.america_sf.rds')
s.america_sf <- readRDS('../data/continents/s.america_sf.rds')
palearctic_sf <- readRDS('../data/continents/palearctic_sf.rds')
oriental_sf <- readRDS('../data/continents/oriental_sf.rds')
madagascar_sf <- readRDS('../data/continents/madagascar_sf.rds')

# distribution data ----
squamates <- readRDS('data/distribution/squam_dist_clean.rds')
mammals <- readRDS('data/distribution/mammal_dist_clean.rds')
amphibians <- readRDS('data/distribution/amphibian_dist_clean.rds')
birds <- readRDS('data/distribution/bird_dist_clean.rds')

# reprojection ----
crs_dist <- crs(squamates)
africa <- st_transform(africa_sf, crs = crs_dist)
australia <- st_transform(australia_sf, crs = crs_dist)
n_america <- st_transform(n.america_sf, crs = crs_dist)
s_america <- st_transform(s.america_sf, crs = crs_dist)
palearctic <- st_transform(palearctic_sf, crs = crs_dist)
oriental <- st_transform(oriental_sf, crs = crs_dist)
madagascar <- st_transform(madagascar_sf, crs = crs_dist)

# intersection ----
regions <- c('africa', 'australia', 'n_america', 's_america', 
             'palearctic', 'oriental', 'madagascar')
taxa <- c('squamates', 'mammals', 'amphibians', 'birds')

region_list <- list(africa, australia, n_america, s_america, 
                    palearctic, oriental, madagascar)
names(region_list) <- regions
taxa_list <- list(squamates, mammals, amphibians, birds)
names(taxa_list) <- taxa

sp_regions_list <- vector('list', length = length(regions))
names(sp_regions_list) <- regions

for (i in 1:length(sp_regions_list)){
    sp_regions_list[[i]] <- vector('list', length = length(taxa))
    names(sp_regions_list[[i]]) <- taxa
}


sf_use_s2(FALSE)

for (r in regions){
    print(r)
    for (t in taxa){
        print(paste0(r, ' -- ', t))
        inters <- st_intersects(taxa_list[[t]], region_list[[r]])
        zao <- sapply(inters, length)
        sp_regions_list[[r]][[t]] <- taxa_list[[t]]$species[zao == 1]
    }
}

saveRDS(sp_regions_list, 'objects/sp_regions_list.rds')






