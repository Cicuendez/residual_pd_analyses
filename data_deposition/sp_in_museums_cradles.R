




# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork')

easypackages::libraries(packages)

# taxa ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# hexgrids ----
hexgrid_list <- readRDS('../objects/hexgrid_list_v2.rds')
colnames(hexgrid_list[['birds']])

# create discrete column cradles & museums ----
for (t in taxa){
  quants <- quantile(hexgrid_list[[t]]$resloess_pd_rich, c(0.1, 0.9))
  hexgrid_list[[t]] <- hexgrid_list[[t]] %>% 
    mutate(type = case_when(resloess_pd_rich <= quants[1] ~ 'cradle', 
                            resloess_pd_rich >= quants[2] ~ 'museum', 
                            (quants[1] < resloess_pd_rich & 
                               resloess_pd_rich < quants[2]) ~ 'none'))
}

# region data ----
africa_sf <- readRDS('../data/continents/africa.rds')
australia_sf <- readRDS('../data/continents/australia.rds')
n.america_sf <- readRDS('../data/continents/n.america_sf.rds')
s.america_sf <- readRDS('../data/continents/s.america_sf.rds')
oriental_sf <- readRDS('../data/continents/oriental_sf.rds')
austocean_sf <- readRDS('../data/continents/austocean_sf.rds')
africa_mad_sf <- readRDS('../data/continents/africa_mad.rds')
s_africa_sf <- readRDS('../data/continents/s_africa.rds')
se_asia_sf <- readRDS('../data/continents/se_asia.rds')
austoceanasia_sf <- readRDS('../data/continents/austoceanasia.rds')

# Get world data for plotting ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
world_projected <- st_transform(world, crs = behrmann)

region_list <- list(africa_sf, africa_mad_sf, s_africa_sf, 
                    n.america_sf, s.america_sf, 
                    australia_sf, austocean_sf, austoceanasia_sf, 
                    se_asia_sf, oriental_sf)
regions <- c('africa', 'africa_madagascar', 'south_africa', 
             'north_america', 'south_america', 
             'australia', 'australia_oceania', 'australia_oceania_asia', 
             'southeast_asia', 'oriental')
names(region_list) <- regions

amphibian_regions <- c('south_america', 'africa', 'oriental', 'australia_oceania')
bird_regions <- c('north_america', 'south_america', 'africa_madagascar',
                  'australia_oceania_asia')
mammal_regions <- c('north_america', 'south_america', 'africa')
squamate_regions <- c('north_america', 'south_america', 'south_africa', 
                      'southeast_asia', 'australia')

taxa_regions <- list(amphibian_regions, bird_regions, mammal_regions,
                     squamate_regions)
names(taxa_regions) <- taxa

# create geography column ----
for (t in taxa){
  hexgrid_list[[t]]$geo <- NA
}

# intersect cells with regions ----
for (t in taxa){
  print(toupper(t))
  for (r in regions){
    if (r %in% taxa_regions[[t]]){
      print(paste0(toupper(t), ' --- ', r))
      a <- hexgrid_list[[t]]
      b <- region_list[[r]]
      x <- st_intersects(a, st_transform(b, crs = st_crs(a)), sparse = FALSE)
      hexgrid_list[[t]]$geo[x[,1] == TRUE] <- r
    }
  }
}

saveRDS(hexgrid_list, '../objects/hexgrid_list_geo.rds', version = 2)


plot(world_projected$geometry, col = 'gray80', border = 'transparent')
plot(a$gridTemplate, add = TRUE)


t <- 'squamates'
taxa_regions[[t]]

# Filter data by cradles/museums and geography ----
hexgrid_list <- readRDS('../objects/hexgrid_list_geo.rds')

hexgrid_list[[t]]$type

hexgrid_list[[t]] %>% 
  filter(type =! 'none') %>%
  filter(type == 'cradle') %>% 
  filter(geo == '')

as.factor(hexgrid_list[[t]]$geo)
is.na(hexgrid_list[[t]]$geo)

cell_ids <- vector('list', length(taxa))
names(cell_ids) <- taxa

for (t in taxa){
  cell_ids[[t]] <- vector('list', length = 2)
  names(cell_ids[[t]]) <- c('cradle', 'museum')
}

regions

# Get cell ids cradles and museums ----
# __ amphibians ----
# ____ cradles ----
cell_ids[['amphibians']][['cradle']] <- vector('list', length = 3)
names(cell_ids[['amphibians']][['cradle']]) <- c('south_america', 
                                                  'oriental', 
                                                  'australia_oceania')
# ____ museums ----
cell_ids[['amphibians']][['museum']] <- vector('list', length = 1)
names(cell_ids[['amphibians']][['museum']]) <- 'africa'

# __ birds ----
# ____ cradles ----
cell_ids[['birds']][['cradle']] <- vector('list', length = 2)
names(cell_ids[['birds']][['cradle']]) <- c('north_america', 
                                            'south_america')

# ____ museums ----
cell_ids[['birds']][['museum']] <- vector('list', length = 2)
names(cell_ids[['birds']][['museum']]) <- c('africa_madagascar', 
                                            'australia_oceania_asia')

# __ mammals ----
# ____ cradles ----
cell_ids[['mammals']][['cradle']] <- vector('list', length = 2)
names(cell_ids[['mammals']][['cradle']]) <- c('north_america', 
                                            'south_america')

# ____ museums ----
cell_ids[['mammals']][['museum']] <- vector('list', length = 2)
names(cell_ids[['mammals']][['museum']]) <- c('north_america', 
                                              'africa')

# __ squamates ----
# ____ cradles ----
cell_ids[['squamates']][['cradle']] <- vector('list', length = 3)
names(cell_ids[['squamates']][['cradle']]) <- c('north_america', 
                                              'south_america', 
                                              'australia')

# ____ museums ----
cell_ids[['squamates']][['museum']] <- vector('list', length = 2)
names(cell_ids[['squamates']][['museum']]) <- c('south_africa', 
                                                'southeast_asia')


for (t in taxa){
  print(toupper(t))
  for (xx in c('cradle', 'museum')){
    for (r in regions){
      if (r %in% names(cell_ids[[t]][[xx]])){
        cell_ids[[t]][[xx]][[r]] <- hexgrid_list[[t]] %>%
          filter(type == xx) %>% 
          filter(geo == r) %>% 
          .$id
      }
    }
  }
}


# epm objects ----
epm_list <- vector('list', length(taxa))
names(epm_list) <- taxa

for (t in taxa){
  epm_list[[t]] <- readRDS(paste0('../objects/EPM_', t, '.rds'))
  print(paste0(t, ' EPM imported'))
}

# Get SPECIES cradles and museums ----
sp_cramus <- vector('list', length(taxa))
names(sp_cramus) <- taxa

for (t in taxa){
  sp_cramus[[t]] <- vector('list', length = 2)
  names(sp_cramus[[t]]) <- c('cradle', 'museum')
}

# __ amphibians ----
# ____ cradles ----
sp_cramus[['amphibians']][['cradle']] <- vector('list', length = 3)
names(sp_cramus[['amphibians']][['cradle']]) <- c('south_america', 
                                                 'oriental', 
                                                 'australia_oceania')
# ____ museums ----
sp_cramus[['amphibians']][['museum']] <- vector('list', length = 1)
names(sp_cramus[['amphibians']][['museum']]) <- 'africa'

# __ birds ----
# ____ cradles ----
sp_cramus[['birds']][['cradle']] <- vector('list', length = 2)
names(sp_cramus[['birds']][['cradle']]) <- c('north_america', 
                                            'south_america')

# ____ museums ----
sp_cramus[['birds']][['museum']] <- vector('list', length = 2)
names(sp_cramus[['birds']][['museum']]) <- c('africa_madagascar', 
                                            'australia_oceania_asia')

# __ mammals ----
# ____ cradles ----
sp_cramus[['mammals']][['cradle']] <- vector('list', length = 2)
names(sp_cramus[['mammals']][['cradle']]) <- c('north_america', 
                                              'south_america')

# ____ museums ----
sp_cramus[['mammals']][['museum']] <- vector('list', length = 2)
names(sp_cramus[['mammals']][['museum']]) <- c('north_america', 
                                              'africa')

# __ squamates ----
# ____ cradles ----
sp_cramus[['squamates']][['cradle']] <- vector('list', length = 3)
names(sp_cramus[['squamates']][['cradle']]) <- c('north_america', 
                                                'south_america', 
                                                'australia')

# ____ museums ----
sp_cramus[['squamates']][['museum']] <- vector('list', length = 2)
names(sp_cramus[['squamates']][['museum']]) <- c('south_africa', 
                                                'southeast_asia')



for (t in taxa){
  print(toupper(t))
  for (xx in c('cradle', 'museum')){
    for (r in regions){
      if (r %in% names(sp_cramus[[t]][[xx]])){
        zz <- cell_ids[[t]][[xx]][[r]]
        sp_cell_list <- epm::expandSpeciesCellList(epm_list[[t]])
        names(sp_cell_list) <- epm_list[[t]]$grid$grid_id
#        l <- epm_list[[t]]$speciesList[zz]
        l <- sp_cell_list[zz]
        sp_cramus[[t]][[xx]][[r]] <- unique(unlist(l))
      }
    }
  }
}

saveRDS(sp_cramus, '../objects/sp_cramus.rds')

sp_cramus[['birds']]$cradle$north_america
sp_cramus[['birds']]$cradle$south_america









