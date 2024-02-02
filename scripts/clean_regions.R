
# purpose ----
# Get a vector layer for each region 

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal',
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'RColorBrewer')

easypackages::libraries(packages)
sf_use_s2(FALSE)
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# data Africa ----
africa_file <- '../../SPATIAL_DATA/Continents/Africa.shp'
africa_sf <- st_read(africa_file)

africa_united_sf <- st_union(africa_sf)
saveRDS(africa_united_sf, '../data/continents/africa.rds')

# data south africa ----
s_africa_file <- '../../SPATIAL_DATA/Continents/S_Africa.shp'
s_africa_sf <- st_read(s_africa_file)
s_africa_sf <- st_transform(s_africa_sf, crs = behrmann)
saveRDS(s_africa_sf, '../data/continents/s_africa.rds')

# data se asia ----
se_asia_file <- '../../SPATIAL_DATA/Continents/SE_Asia.shp'
se_asia_sf <- st_read(se_asia_file)
se_asia_sf <- st_transform(se_asia_sf, crs = behrmann)
saveRDS(se_asia_sf, '../data/continents/se_asia.rds')

# data continents ----
continent_file <- '../../SPATIAL_DATA/Continents/All_continents.shp'
continent_sf <- st_read(continent_file)
plot(continent_sf$geometry, col = rainbow(8))
continent_sf$CONTINENT
nrow(continent_sf)

# extract Africa + Madagascar ----
africa_mad_sf <- continent_sf %>% 
  filter(CONTINENT == 'Africa')
plot(africa_mad_sf$geometry)
saveRDS(africa_mad_sf, '../data/continents/africa_mad.rds')


# extract Australia ----
australia_sf <- continent_sf %>% 
  filter(CONTINENT == 'Australia')
plot(australia_sf$geometry)
saveRDS(australia_sf, '../data/continents/australia.rds')

# extract North America ----
n.america_sf <- continent_sf %>% 
  filter(CONTINENT == 'North America')
plot(n.america_sf$geometry)
# Note: it includes Greenland! 
saveRDS(n.america_sf, '../data/continents/n.america_sf.rds')

# extract South America ----
s.america_sf <- continent_sf %>% 
  filter(CONTINENT == 'South America')
plot(s.america_sf$geometry)
saveRDS(s.america_sf, '../data/continents/s.america_sf.rds')

# data palearctic ----
palearctic_file <- '../../SPATIAL_DATA/Holt_Zoogeo_regions/Palearctic.shp'
palearctic_sf <- st_read(palearctic_file)
palearctic_sf$Realm <- 'Palearctic'
plot(palearctic_sf$geometry)
saveRDS(palearctic_sf, '../data/continents/palearctic_sf.rds')

# data new realms ----
newrealms_file <- '../../SPATIAL_DATA/Holt_Zoogeo_regions/newRealms.shp'
newrealms_sf <- st_read(newrealms_file)
# crop to avoid problems with reprojection: reduce the extent a little
e <- st_bbox(c(xmin = -179.5, xmax = 179.5, ymax = 83, ymin = -55), 
             crs = st_crs(newrealms_sf))
newrealms_cropped <- st_crop(newrealms_sf, e)
plot(newrealms_sf$geometry, col = 'red')

newrealms_cropped <- st_transform(newrealms_cropped, crs = behrmann)
newrealms_sf$Realm
plot(newrealms_sf$geometry[newrealms_sf$Realm == 'Oriental'])
plot(newrealms_sf$geometry[newrealms_sf$Realm == 'Australian'], border = 'transparent', 
     col = 'gray70')
plot(newrealms_sf$geometry, col = rainbow(11), border = 'transparent')

# Extract Oceanina ----
oceanina_sf <- newrealms_cropped %>% 
  filter(Realm == 'Oceanina')
oceanina_sf <- oceanina_sf['Realm']
plot(oceanina_sf$geometry)

# join Australia + Oceanina ----
colnames(oceanina_sf)[1] <- 'CONTINENT'
plot(australia_sf$geometry)
austocean_sf <- rbind(oceanina_sf, 
                      st_transform(australia_sf['CONTINENT'], st_crs(oceanina_sf)))
plot(austocean_sf$geometry)
austocean_sf <- st_union(austocean_sf)
saveRDS(austocean_sf, '../data/continents/austocean_sf.rds')

# join Australia + Oceanina + SE Asia ----
se_asia_sf$Name <- 'se_asia'
colnames(se_asia_sf)[1] <- 'CONTINENT'
austoceanasia_sf <- rbind(austocean_sf, se_asia_sf)
austoceanasia_sf <- st_union(austoceanasia_sf)
saveRDS(austoceanasia_sf, '../data/continents/austoceanasia.rds')

# extract Oriental ----
oriental_sf <- newrealms_sf %>%
  filter(Realm == 'Oriental')
plot(oriental_sf$geometry)
saveRDS(oriental_sf, '../data/continents/oriental_sf.rds')

# extract Madagascan ----
madagascar_sf <- newrealms_sf %>%
  filter(Realm == 'Madagascan')
plot(madagascar_sf$geometry)
saveRDS(madagascar_sf, '../data/continents/madagascar_sf.rds')



