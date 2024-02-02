
# data ----
bird_sf <- readRDS('birds_dist_clean.rds')

# PROJECTION ----
behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Get world data for plotting ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_behrmann <- st_transform(world, crs = behrmann)

# Dissolve world countries layer ----
world_united <- world_behrmann %>% 
  st_union() %>% # unite to a geometry object
  st_sf() # make the geometry a data frame object

# Get the polygons that intersect with land, use st_intersects
# To do the actual intersection, st_intersection: 

landbirds_sf <- st_intersection(bird_sf, world_united)
saveRDS(landbirds_sf, 'landbirds.rds')

