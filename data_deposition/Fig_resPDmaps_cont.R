

packages <- c('tidyverse', 'sf', 'raster', 'epm', 
              'terra', 'patchwork', 'rnaturalearth')
easypackages::libraries(packages)

# taxa ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# Import data ----
hexgrid_list <- readRDS('../../objects/hexgrid_list_geo_v3.rds')
proj_hexgrid <- crs(hexgrid_list[[1]])

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_projected <- st_transform(world, crs = proj_hexgrid)


# MAP RESIDUALS (LOESS) ----
resmaps <- vector('list', length(taxa))
names(resmaps) <- taxa

for (t in taxa){
  resmaps[[t]] <- ggplot() +
    geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
    geom_sf(data = hexgrid_list[[t]], aes(fill = resloess_pd_rich), 
            col = 'transparent') +
    scale_fill_gradient2(low = '#2F6BA8', # blue
                         mid = "white",
                         high = '#A72E37', # red
                         midpoint = 0, 
                         name = 'residual PD') +
    labs(title = paste0(t)) +
    theme_void() +
    theme(legend.position = 'bottom', 
          legend.key.height = unit(0.2,"cm"),
          plot.title = element_text(hjust = 0.5, 
                                    face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5, 
                                    face = 'bold', size = 10),
          legend.text = element_text(angle = 0, vjust = 2, size = 5), 
          legend.title = element_text(vjust = 1, face = 'bold', size = 8))
}
ggsave('plots/Figure_resPDmaps_cont.png', plot = wrap_plots(resmaps), width = )
ggsave('plots/Figure_resPDmaps_cont.pdf', plot = wrap_plots(resmaps))



wrap_plots(resmaps)


