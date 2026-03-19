

# packages ----
library(terra)
library(sf)
library(tidyverse)
library(patchwork)
library(data.table)
library(forcats)
library(tidytext)
library(viridis)
#library(tidyterra)

# show progress
terraOptions(progress = 1)

# import hexagons ----
if ('hexgrid_list_paleoclimate.rds' %in% list.files('../objects/')){
  hexgrid_list <- readRDS('../objects/hexgrid_list_paleoclimate.rds')
  taxa <- names(hexgrid_list)
}


if (!'hexgrid_list_paleoclimate.rds' %in% list.files('../objects/')){
  
  # import paleoclimate variables ----
  # These are derived from the PALEO-PGEM paleoclimatic layers spanning 
  # from 5 Ma to 1950.
  # Resolution: 1 degree x 1 degree. 
  
  ## standard deviation ----
  temp_sd <- readRDS('output/temp_sd.rds')
  prec_sd <- readRDS('output/prec_sd.rds')
  
  ## net change ----
  temp_netchange <- readRDS('output/temp_netchange.rds')
  prec_netchange <- readRDS('output/prec_netchange.rds')
  names(temp_netchange) <- names(prec_netchange) <- 'netchange'
  
  ## cumulative change ----
  temp_cumchange <- readRDS('output/temp_cumchange.rds')
  prec_cumchange <- readRDS('output/prec_cumchange.rds')
  
  ## slope ----
  temp_slope <- readRDS('output/temp_slope.rds')
  prec_slope <- readRDS('output/prec_slope.rds')
  names(temp_slope) <- names(prec_slope) <- 'slope'
  
  #par(mfrow = c(2,2))
  #plot(log(temp_sd), main = 'Temp SD')
  #plot(temp_netchange, main = 'Temp Net change')
  #plot(log(temp_cumchange), main = 'Temp Cumulative change')
  #plot(paleotemp_slope*10000, main = 'Temp slope')
  #
  #plot(log(prec_sd), main = 'prec SD')
  #plot(prec_netchange, main = 'prec Net change')
  #plot(log(prec_cumchange), main = 'prec Cumulative change')
  #plot(paleoprec_slope*10000, main = 'prec slope')
  #
  #terra::res(temp_sd)
  
  
  
  
  hexgrid_list <- readRDS('../objects/hexgrid_list_geo_v3.rds')
  proj_hexgrid <- crs(hexgrid_list[[1]])
  # resolution: 100 km (~ 1 degree)
  taxa <- names(hexgrid_list)
  
  # reproject rasters ----
  temp_sd_proj <- terra::project(temp_sd, proj_hexgrid)
  prec_sd_proj <- terra::project(prec_sd, proj_hexgrid)
  temp_netchange_proj <- terra::project(temp_netchange, proj_hexgrid)
  prec_netchange_proj <- terra::project(prec_netchange, proj_hexgrid)
  temp_cumchange_proj <- terra::project(temp_cumchange, proj_hexgrid)
  prec_cumchange_proj <- terra::project(prec_cumchange, proj_hexgrid)
  temp_slope_proj <- terra::project(temp_slope, proj_hexgrid)
  prec_slope_proj <- terra::project(prec_slope, proj_hexgrid)
  
  
  #world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  #world_projected <- st_transform(world, crs = proj_hexgrid)
  #
  #temp_sd_sf <- as.data.frame(temp_sd_reprojected, xy = TRUE, na.rm = TRUE)
  #
  #ggplot() +
  #  geom_sf(data = world_projected,
  #          col = 'transparent',
  #          fill = 'gray80') +
  #  geom_raster(data = temp_sd_sf,
  #              aes(x = x, y = y, fill = sd)) +
  #  
  #  geom_sf(data = hexgrid_list[[t]],
  #          aes(fill = resloess_pd_rich),
  #          col = 'transparent')
  
  
  for (t in taxa){
    print(toupper(t))
    
    # get paleotemperature SD values per grid cell
    print(paste0('extracting temp SD values for ', t))
    
    paleotemp_sd <- terra::extract(x = temp_sd_proj, 
                                   y = hexgrid_list[[t]][, c('long', 'lat')], 
                                   fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleotemp_sd <- paleotemp_sd$sd
    
    # get paleoprecipitation SD values per grid cell 
    print(paste0('extracting prec SD values for ', t))
    paleoprec_sd <- terra::extract(x = prec_sd_proj, 
                                   y = hexgrid_list[[t]][, c('long', 'lat')], 
                                   fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleoprec_sd <- paleoprec_sd$sd
    
    # get paleotemperature net change values per grid cell
    print(paste0('extracting temp net change values for ', t))
    
    paleotemp_netchange <- terra::extract(x = temp_netchange_proj, 
                                          y = hexgrid_list[[t]][, c('long', 'lat')], 
                                          fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleotemp_netchange <- paleotemp_netchange$netchange
    
    # get paleoprecipitation net change values per grid cell
    print(paste0('extracting prec net change values for ', t))
    
    paleoprec_netchange <- terra::extract(x = prec_netchange_proj, 
                                          y = hexgrid_list[[t]][, c('long', 'lat')], 
                                          fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleoprec_netchange <- paleoprec_netchange$netchange
    
    # get paleotemperature cumulative change values per grid cell
    print(paste0('extracting temp cumulative change values for ', t))
    
    paleotemp_cumchange <- terra::extract(x = temp_cumchange_proj, 
                                          y = hexgrid_list[[t]][, c('long', 'lat')], 
                                          fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleotemp_cumchange <- paleotemp_cumchange$sum
    
    # get paleoprecipitation cumulative change values per grid cell
    print(paste0('extracting prec cumulative change values for ', t))
    
    paleoprec_cumchange <- terra::extract(x = prec_cumchange_proj, 
                                          y = hexgrid_list[[t]][, c('long', 'lat')], 
                                          fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleoprec_cumchange <- paleoprec_cumchange$sum
    
    # get paleotemperature slope values per grid cell
    print(paste0('extracting temp slope values for ', t))
    
    paleotemp_slope <- terra::extract(x = temp_slope_proj, 
                                      y = hexgrid_list[[t]][, c('long', 'lat')], 
                                      fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleotemp_slope <- paleotemp_slope$slope
    
    # get paleoprecipitation slope values per grid cell
    print(paste0('extracting prec slope values for ', t))
    
    paleoprec_slope <- terra::extract(x = prec_slope_proj, 
                                      y = hexgrid_list[[t]][, c('long', 'lat')], 
                                      fun = 'mean', method = 'bilinear', na.rm = TRUE)
    hexgrid_list[[t]]$paleoprec_slope <- paleoprec_slope$slope
    
    print('##############################')
  }
  
  saveRDS(hexgrid_list, '../objects/hexgrid_list_paleoclimate.rds')
  
}



# dataframes per region ----

# import data sp in high and low resPD regions.
sp_cramus <- readRDS('../objects/sp_cramus.rds')
#names(sp_cramus)
#names(sp_cramus$squamates$cradle)
#names(sp_cramus$squamates$museum)



cradles_and_museums <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  cradles_and_museums[[t]] <- setNames(vector('list', length = 2), c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    cradles_and_museums[[t]][[typ]] <- names(sp_cramus[[t]][[typ]])
  }
}


# Create the object to separate each high and low resPD region
hexgrid_per_region <- setNames(vector('list', length(taxa)), taxa)

for (t in taxa){
  hexgrid_per_region[[t]] <- setNames(vector('list', length = 2), c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    reg <- names(sp_cramus[[t]][[typ]])
    hexgrid_per_region[[t]][[typ]] <- setNames(vector('list', length = length(reg)), reg)
  }
}

# this is to keep only the cradle or the museum cells appropriate according to our 
# designation of global regions (for instance, there are some cradle cells in the 
# overall museum region of south_africa, so we want to keep only the museum cells)

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    reg <- cradles_and_museums[[t]][[typ]]
    
    for (r in reg){
      hexgrid_per_region[[t]][[typ]][[r]] <- 
        hexgrid_list[[t]] %>% filter(type == typ & geo %in% r)
    }
  }
}

# This way we keep only the cells that have low or high PD (categorized as 
# cradles or museums) in the larger regions categorized as global 
# regions of low or high PD.

# combine dataframes per region ----
hexgrid_per_region_combined <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  hexgrid_per_region_combined[[t]] <- rbindlist(lapply(hexgrid_per_region[[t]], rbindlist))
}



# CLIMATE VIOLIN PLOTS ----
# colors ----
col_cradle <- '#2F6BA8' # blue
col_museum <- '#A72E37' # red

# > Violin main plot ----
#paleoclim_vars <- colnames(hexgrid_list[[t]])[grep('paleo', colnames(hexgrid_list[[t]]))]

# let's take the main variables used for the main text: 
# - present temperature and present precipitation; 
# - cumulative change of temp and prec in the past 5 Ma (preindustrial). 

clim_vars <- c('temp', 'prec', 'paleotemp_cumchange', 'paleoprec_cumchange')

# Long dataframe
df_long <- bind_rows(lapply(names(hexgrid_per_region_combined), function(g) {
  hexgrid_per_region_combined[[g]] %>%
    mutate(group = g)
})) %>%
  select(group, geo, type, all_of(clim_vars)) %>%
  pivot_longer(cols = all_of(clim_vars),
               names_to = "variable",
               values_to = "value") %>%
  filter(is.finite(value)) %>%
  mutate(
    group = factor(group,
                   levels = c("amphibians","birds","mammals","squamates")),
    type = factor(type,
                  levels = c("cradle","museum"))
  )

# new labels
geo_labels <- c(
  "africa_madagascar" = "Africa - Madag.",
  "south_america" = "S America",
  "north_america" = "N America",
  "southeast_asia" = "SE Asia",
  "south_africa" = "S Africa",
  "australia_oceania_asia" = "Austral-Asia",
  "australia_oceania" = "Austral. - Oceania",
  "africa" = "Africa", 
  "oriental" = "India - SE Asia", 
  "australia" = "Australia"
)

df_long <- df_long %>%
  mutate(
    geo = recode(geo, !!!geo_labels), 
    variable = factor(
      variable,
      levels = clim_vars
      )
    )


var_labels <- c(
  "temp" = "Temperature",
  "prec" = "Precipitation",
  "paleotemp_cumchange" = "Temp. change",
  "paleoprec_cumchange" = "Precip. change"
)

taxa_labels <- c(
  "amphibians" = "Amphibians",
  "birds" = "Birds",
  "mammals" = "Mammals",
  "squamates" = "Squamates"
)

# separation in cases of regions with both factors
pd <- position_dodge(width = 0.85) # when one region contains cradle and museum

# shade first and third columns of the plots (taxa separation)
shade_df <- data.frame(
  group = levels(df_long$group)[c(1, 3)]
)

# main plot
p_4x4_main <- df_long %>%
  filter(variable %in% clim_vars) %>%
  filter(is.finite(value)) %>%
  ggplot(aes(x = geo, y = value, fill = type)) +
  
  # background for columns
  geom_rect(
    data = shade_df,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#F7F7F7"
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "gray60") +
  
  geom_violin(trim = TRUE, scale = "width", position = pd, color = NA, alpha = 0.7) +
  
  geom_boxplot(width = 0.15, position = pd, 
               outlier.shape = 16, outlier.size = 0.7, 
               outlier.stroke = 0, alpha = 0.9, 
               color = 'black', 
               linewidth = 0.2) +
  
  scale_fill_manual(values = c(cradle = col_cradle, museum = col_museum)) +
  
  facet_grid(variable ~ group, scales = "free", 
             labeller = labeller(variable = var_labels, 
                                 group = taxa_labels)) +   # 4 taxa (rows) × 4 variables (columns)
  
  labs(x = NULL, y = NULL) +
  
  theme_classic() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"), # text in the panel boxes
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(), 
    
    #strip.background.x = element_rect(fill = "#EAEAEA", color = NA),
    #strip.background.y = element_rect(fill = "#F5F5F5", color = NA)
    
    #strip.background = element_blank()
    
    strip.background.x = element_rect(
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    ),
    
    strip.background.y = element_rect(
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    )
  )

#p_4x4

# add some more space between taxa panels
paleoclim_plot_main <- p_4x4_main +
  theme(
    panel.spacing.x = unit(1.2, "lines"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
paleoclim_plot_main

ggsave('plots/paleoclim_violin_main.pdf', paleoclim_plot_main)

# add squares separating panels
#p_4x4 +
#  theme(
#    panel.spacing.x = unit(0.8, "lines"),
#    panel.background = element_rect(fill = "white"),
#    plot.background = element_rect(fill = "white"),
#    strip.background = element_rect(fill = "grey92"),
#    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
#  )

#p_4x4 +
#  theme(
#    panel.spacing.x = unit(1.2, "lines"),
#    
#    # strips: the titles of rows and columns (y = variables, x = taxa)
#    strip.background.x = element_rect(fill = "white", color = 'white'),
#    strip.background.y = element_rect(fill = "white", color = 'white'),
#    strip.text = element_text(face = "bold"),
#    
#    panel.background = element_rect(fill = "white")
#  )


#plot_one_variable <- function(varname){
#  
#  pd <- position_dodge(width = 0.85)
#  
#  df_plot <- df_long %>%
#    filter(variable == varname) %>%
#    group_by(group, geo) %>%
#    mutate(med = median(value, na.rm = TRUE)) %>%
#    ungroup()
#    #mutate(geo_group = reorder_within(geo, med, group)) # sort by median
#    
#
#  ggplot(df_plot,
#         aes(x = geo, y = value, fill = type)) +
#    
#    geom_hline(yintercept = 0,
#               linetype = "dashed",
#               linewidth = 0.4,
#               color = "gray60") +
#    
#    geom_violin(trim = TRUE,
#                scale = "width",
#                position = pd,
#                color = NA) +
#    
#    scale_fill_manual(values = c(
#      cradle = col_cradle,
#      museum = col_museum
#    )) +
#    
#    #scale_x_reordered() +
#    
#    facet_wrap(~ group,
#               ncol = 2,
#               scales = "free_x") +
#    
#    labs(title = varname,
#         x = NULL,
#         y = NULL) +
#    
#    theme_classic() +
#    theme(
#      legend.position = "none",
#      strip.text = element_text(face = "bold"),
#      axis.text.x = element_text(angle = 45,
#                                 hjust = 1),
#      axis.ticks.x = element_blank()
#    )
#}
#
#
#climate_violin_plots <- setNames(
#  lapply(clim_vars, plot_one_variable),
#  clim_vars
#)
#
## Example
#climate_violin_plots$paleotemp_cumchange
#wrap_plots(climate_violin_plots, nrow = 4)
#ggsave('plots/paleoclimate_violin.pdf', wrap_plots(paleoclimate_violin_plots, ncol = 2), 
#       height = 20, width = 12)
#

# > Violin supp plot ----
## >> Current variables ----
clim_vars_supp_current <- c('tempseas', 'precseas', 'npp', 'tri_current')

df_long <- bind_rows(lapply(names(hexgrid_per_region_combined), function(g) {
  hexgrid_per_region_combined[[g]] %>%
    mutate(group = g)
})) %>%
  select(group, geo, type, all_of(clim_vars_supp_current)) %>%
  pivot_longer(cols = all_of(clim_vars_supp_current),
               names_to = "variable",
               values_to = "value") %>%
  filter(is.finite(value)) %>%
  mutate(
    group = factor(group,
                   levels = c("amphibians","birds","mammals","squamates")),
    type = factor(type,
                  levels = c("cradle","museum"))
  )

df_long <- df_long %>%
  mutate(
    geo = recode(geo, !!!geo_labels), 
    variable = factor(
      variable,
      levels = clim_vars_supp_current
    )
  )

var_labels <- c(
  "tempseas" = "T. seasonality",
  "precseas" = "P. seasonality",
  "npp" = "NPP",
  "tri_current" = "TRI"
)

taxa_labels <- c(
  "amphibians" = "Amphibians",
  "birds" = "Birds",
  "mammals" = "Mammals",
  "squamates" = "Squamates"
)

# separation in cases of regions with both factors
pd <- position_dodge(width = 0.85) # when one region contains cradle and museum

# shade first and third columns of the plots (taxa separation)
shade_df <- data.frame(
  group = levels(df_long$group)[c(1, 3)]
)

# main plot
p_4x4_supp_current <- df_long %>%
  filter(variable %in% clim_vars_supp_current) %>%
  filter(is.finite(value)) %>%
  ggplot(aes(x = geo, y = value, fill = type)) +
  
  # background for columns
  geom_rect(
    data = shade_df,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#F7F7F7"
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "gray60") +
  
  geom_violin(trim = TRUE, scale = "width", position = pd, color = NA, alpha = 0.7) +
  
  geom_boxplot(width = 0.15, position = pd, 
               outlier.shape = 16, outlier.size = 0.8, 
               outlier.stroke = 0, alpha = 0.9, 
               color = 'black', 
               linewidth = 0.2) +
  
  scale_fill_manual(values = c(cradle = col_cradle, museum = col_museum)) +
  
  facet_grid(variable ~ group, scales = "free", 
             labeller = labeller(variable = var_labels, 
                                 group = taxa_labels)) +   # 4 taxa (rows) × 4 variables (columns)
  
  labs(x = NULL, y = NULL) +
  
  theme_classic() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"), # text in the panel boxes
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(), 
    
    #strip.background.x = element_rect(fill = "#EAEAEA", color = NA),
    #strip.background.y = element_rect(fill = "#F5F5F5", color = NA)
    
    #strip.background = element_blank()
    
    strip.background.x = element_rect(
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    ),
    
    strip.background.y = element_rect(
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    )
  )

# add some more space between taxa panels
paleoclim_plot_supp_current <- p_4x4_supp_current +
  theme(
    panel.spacing.x = unit(1.2, "lines"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
paleoclim_plot_supp_current

ggsave('plots/paleoclim_violin_supp_current.pdf', paleoclim_plot_supp_current)


## >> Paleo variables ----
clim_vars_supp_paleo <- c('paleotemp_sd', 'paleoprec_sd', 'paleotemp_netchange', 'paleoprec_netchange', 
                          'paleotemp_slope', 'paleoprec_slope')

df_long <- bind_rows(lapply(names(hexgrid_per_region_combined), function(g) {
  hexgrid_per_region_combined[[g]] %>%
    mutate(group = g)
})) %>%
  select(group, geo, type, all_of(clim_vars_supp_paleo)) %>%
  pivot_longer(cols = all_of(clim_vars_supp_paleo),
               names_to = "variable",
               values_to = "value") %>%
  filter(is.finite(value)) %>%
  mutate(
    group = factor(group,
                   levels = c("amphibians","birds","mammals","squamates")),
    type = factor(type,
                  levels = c("cradle","museum"))
  )

df_long <- df_long %>%
  mutate(
    geo = recode(geo, !!!geo_labels), 
    variable = factor(
      variable,
      levels = clim_vars_supp_paleo
    )
  )

var_labels <- c(
  "paleotemp_sd" = "Temp. SD",
  "paleoprec_sd" = "Prec. SD",
  "paleotemp_netchange" = "T. net change",
  "paleoprec_netchange" = "P. net change", 
  "paleotemp_slope" = "Temp. slope", 
  "paleoprec_slope" = "Prec. slope"
)

taxa_labels <- c(
  "amphibians" = "Amphibians",
  "birds" = "Birds",
  "mammals" = "Mammals",
  "squamates" = "Squamates"
)

# separation in cases of regions with both factors
pd <- position_dodge(width = 0.85) # when one region contains cradle and museum

# shade first and third columns of the plots (taxa separation)
shade_df <- data.frame(
  group = levels(df_long$group)[c(1, 3)]
)

# main plot
p_4x4_supp_paleo <- df_long %>%
  filter(variable %in% clim_vars_supp_paleo) %>%
  filter(is.finite(value)) %>%
  ggplot(aes(x = geo, y = value, fill = type)) +
  
  # background for columns
  geom_rect(
    data = shade_df,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#F7F7F7"
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "gray60") +
  
  geom_violin(trim = TRUE, scale = "width", position = pd, color = NA, alpha = 0.7) +
  
  geom_boxplot(width = 0.15, position = pd, 
               outlier.shape = 16, outlier.size = 0.8, 
               outlier.stroke = 0, alpha = 0.9, 
               color = 'black', 
               linewidth = 0.2) +
  
  scale_fill_manual(values = c(cradle = col_cradle, museum = col_museum)) +
  
  facet_grid(variable ~ group, scales = "free", 
             labeller = labeller(variable = var_labels, 
                                 group = taxa_labels)) +   # 4 taxa (rows) × 4 variables (columns)
  
  labs(x = NULL, y = NULL) +
  
  theme_classic() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"), # text in the panel boxes
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(), 
    
    #strip.background.x = element_rect(fill = "#EAEAEA", color = NA),
    #strip.background.y = element_rect(fill = "#F5F5F5", color = NA)
    
    #strip.background = element_blank()
    
    strip.background.x = element_rect(
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    ),
    
    strip.background.y = element_rect(
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    )
  )

# add some more space between taxa panels
paleoclim_plot_supp_paleo <- p_4x4_supp_paleo +
  theme(
    panel.spacing.x = unit(1.2, "lines"),
    panel.spacing.y = unit(1.2, "lines"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
paleoclim_plot_supp_paleo

ggsave('plots/paleoclim_violin_supp_paleo.pdf', paleoclim_plot_supp_paleo, 
       width = 7, height = 10)


 ###    #   #   ###    #      #   #    ###    #####   ###
#   #   ##  #  #   #   #       # #    #         #    #   
#####   # # #  #####   #        #      ###      #     ### 
#   #   #  ##  #   #   #        #         #     #        #
#   #   #   #  #   #   #####    #      ###    #####   ###


#       A     N   N     A     L     Y   Y    SSS    IIIII   SSS
#      A A    NN  N    A A    L      Y Y    S         I    S
#     AAAAA   N N N   AAAAA   L       Y      SSS      I     SSS
#     A   A   N  NN   A   A   L       Y         S     I        S
#     A   A   N   N   A   A   LLLLL   Y      SSS    IIIII   SSS

# ANALYSIS ----
# rationale: 
# The working hypothesis (H1) here is that regions of low resPD ("cradles") and 
# regions of high resPD ("museums") differ climatically / environmentally. 
# We have current variables and paleo variables (temperature and precipitation 
# change during the past 5 million years), so we will treat them 
# separately. Therefore the hypotheses we test are: 
# 1. There are differences in current environment between cradles and museums. 
# 2. There are differences in climatic change in the last 5 million years. 
# 
# With our dataset and the big number of grid cells, the test will likely be 
# significant even if we don't see a clear difference between cradles and 
# museums in the violin plots. So we can compare the results of the tests using 
# the 'type' column (cradles vs. museums) with the results of the tests using 
# the 'geo' column (the specific regions: North America, SE Asia, Africa, 
# Australia, etc.; these are specific for each taxa group). These will likely 
# show a stronger effect of the geographic context than the cradle-museum 
# category.
# 
# For the paleoclimate: 
# - cumulative change and standard deviation are very 
# similar, so we only use cumulative change. 
# - net change is not really informative because it can hide great change 
# with a net of zero. So we do not use net change in the analysis. 
# - we use slope as a proxy for the general climate trend. It is kind of similar 
# to cumulative change in the violin plot patterns, but since it may show a 
# different thing (trend insted of specific amount of change) we use it. 


hexgrid_per_region_combined # this is the data list

vars.current <- c("temp", "prec", "tempseas", "precseas", "npp", "tri_current")
vars.paleo <- c("paleotemp_cumchange", "paleoprec_cumchange", "paleotemp_slope", "paleoprec_slope")

## empty results table
results_manova <- data.frame(
  taxon = character(),
  block = character(),     # "current" or "paleo"
  effect = character(),    # "type" or "geo"
  pillai = numeric(),
  approx_F = numeric(),
  num_df = numeric(),
  den_df = numeric(),
  p = numeric(),
  stringsAsFactors = FALSE
)

results_anova <- data.frame(
  taxon = character(),
  variable = character(),
  effect = character(),    # "type" or "geo"
  ss_effect = numeric(), 
  ss_resid = numeric(),
  df1 = numeric(),
  df2 = numeric(),
  F.stat = numeric(),
  p = numeric(),
  eta2 = numeric(),
  stringsAsFactors = FALSE
)

for (t in taxa){
  
  dat.test <- as.data.frame(hexgrid_per_region_combined[[t]] %>% 
                              select(all_of(c(vars.current, vars.paleo, 'type', 'geo', 'group'))))
  
  ## current ----
  Y_current <- scale(log(dat.test[, vars.current]))
  
  ### MANOVA current - type ----
  manova.current.type <- manova(Y_current ~ type, data = dat.test)
  manova.current.type$coefficients
  manova.current.type$coefficients
  sum.manova.current.type <- summary(manova.current.type, test = 'Pillai')
  sum.manova.current.type$stats
  # Pillai's trace ranges from 0 to 1 (with 1 degree of freedom). 
  # Higher Pillai's trace values indicate 
  # stronger evidence that the explanatory variable (type; cradle vs museum) 
  # has an effect on the response variables (climate). In other words in this case, 
  # higher Pillai's traces show larger climatic differences between high resPD
  # regions and low resPD regions.
  
  results_manova <- rbind(
    results_manova,
    data.frame(
      taxon = t, 
      block = 'current', 
      effect = "type",
      pillai = sum.manova.current.type$stats[1, "Pillai"],
      approx_F = sum.manova.current.type$stats[1, "approx F"],
      num_df = sum.manova.current.type$stats[1, "num Df"],
      den_df = sum.manova.current.type$stats[1, "den Df"],
      p = sum.manova.current.type$stats[1, "Pr(>F)"],
      stringsAsFactors = FALSE
    )
  )
  
  ### ANOVAs current - type ----
  
  aov.current.type <- summary.aov(manova.current.type)
  names(aov.current.type) <- vars.current
  aov.current.type$temp
  
  for (v in vars.current) {
    
    tab <- aov.current.type[[v]]
    
    ss_effect <- tab$`Sum Sq`[1]
    ss_resid  <- tab$`Sum Sq`[2]
    eta2 <- ss_effect / (ss_effect + ss_resid)
    
    df1 <- tab$Df[1]
    df2 <- tab$Df[2]
    F.stat <- tab$`F value`[1]
    pval <- tab$`Pr(>F)`[1]
    
    # save to table
    results_anova <- rbind(
      results_anova, 
      data.frame(
        taxon = t,
        variable = v,
        effect = 'type',    # "type" or "geo"
        ss_effect = ss_effect, 
        ss_resid = ss_resid, 
        df1 = df1,
        df2 = df2,
        F.stat = F.stat,
        p = pval,
        eta2 = eta2,
        stringsAsFactors = FALSE
      )
    )
  }
  
  ### MANOVA current - geo ----
  
  manova.current.geo <- manova(Y_current ~ geo, data = dat.test)
  manova.current.geo$coefficients
  sum.manova.current.geo <- summary(manova.current.geo, test = 'Pillai')
  sum.manova.current.geo$stats
  
  results_manova <- rbind(
    results_manova,
    data.frame(
      taxon = t, 
      block = 'current', 
      effect = "geo",
      pillai = sum.manova.current.geo$stats[1, "Pillai"],
      approx_F = sum.manova.current.geo$stats[1, "approx F"],
      num_df = sum.manova.current.geo$stats[1, "num Df"],
      den_df = sum.manova.current.geo$stats[1, "den Df"],
      p = sum.manova.current.geo$stats[1, "Pr(>F)"],
      stringsAsFactors = FALSE
    )
  )
  
  ### ANOVA current - geo ----
  
  aov.current.geo <- summary.aov(manova.current.geo)
  names(aov.current.geo) <- vars.current
  aov.current.geo$temp
  
  for (v in vars.current) {
    
    tab <- aov.current.geo[[v]]
    
    ss_effect <- tab$`Sum Sq`[1]
    ss_resid  <- tab$`Sum Sq`[2]
    eta2 <- ss_effect / (ss_effect + ss_resid)
    
    df1 <- tab$Df[1]
    df2 <- tab$Df[2]
    F.stat <- tab$`F value`[1]
    pval <- tab$`Pr(>F)`[1]
    
    # save to table
    results_anova <- rbind(
      results_anova, 
      data.frame(
        taxon = t,
        variable = v,
        effect = 'geo',
        ss_effect = ss_effect, 
        ss_resid = ss_resid, 
        df1 = df1,
        df2 = df2,
        F.stat = F.stat,
        p = pval,
        eta2 = eta2,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  ## paleo ----
  
  # since slopes have negative values, don't apply the log for them
  Y_paleo <- cbind(
    scale(log(dat.test[, vars.paleo[1:2]])), 
    scale(dat.test[, vars.paleo[3:4]])
  )
  
  ### MANOVA paleo - type ----
  
  manova.paleo.type <- manova(Y_paleo ~ type, data = dat.test)
  manova.paleo.type$coefficients
  sum.manova.paleo.type <- summary(manova.paleo.type, test = 'Pillai')
  sum.manova.paleo.type$stats
  
  results_manova <- rbind(
    results_manova,
    data.frame(
      taxon = t, 
      block = 'paleo', 
      effect = "type",
      pillai = sum.manova.paleo.type$stats[1, "Pillai"],
      approx_F = sum.manova.paleo.type$stats[1, "approx F"],
      num_df = sum.manova.paleo.type$stats[1, "num Df"],
      den_df = sum.manova.paleo.type$stats[1, "den Df"],
      p = sum.manova.paleo.type$stats[1, "Pr(>F)"],
      stringsAsFactors = FALSE
    )
  )
  
  
  
  ### ANOVAs paleo - type ----
  
  aov.paleo.type <- summary.aov(manova.paleo.type)
  names(aov.paleo.type) <- vars.paleo
  aov.paleo.type$paleotemp_cumchange
  
  for (v in vars.paleo) {
    
    tab <- aov.paleo.type[[v]]
    
    ss_effect <- tab$`Sum Sq`[1]
    ss_resid  <- tab$`Sum Sq`[2]
    eta2 <- ss_effect / (ss_effect + ss_resid)
    
    df1 <- tab$Df[1]
    df2 <- tab$Df[2]
    F.stat <- tab$`F value`[1]
    pval <- tab$`Pr(>F)`[1]
    
    # save to table
    results_anova <- rbind(
      results_anova, 
      data.frame(
        taxon = t,
        variable = v,
        effect = 'type',
        ss_effect = ss_effect, 
        ss_resid = ss_resid, 
        df1 = df1,
        df2 = df2,
        F.stat = F.stat,
        p = pval,
        eta2 = eta2,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  
  ### MANOVA paleo - geo ----
  
  manova.paleo.geo <- manova(Y_paleo ~ geo, data = dat.test)
  manova.paleo.geo$coefficients
  sum.manova.paleo.geo <- summary(manova.paleo.geo, test = 'Pillai')
  sum.manova.paleo.geo$stats
  
  results_manova <- rbind(
    results_manova,
    data.frame(
      taxon = t, 
      block = 'paleo', 
      effect = "geo",
      pillai = sum.manova.paleo.geo$stats[1, "Pillai"],
      approx_F = sum.manova.paleo.geo$stats[1, "approx F"],
      num_df = sum.manova.paleo.geo$stats[1, "num Df"],
      den_df = sum.manova.paleo.geo$stats[1, "den Df"],
      p = sum.manova.paleo.geo$stats[1, "Pr(>F)"],
      stringsAsFactors = FALSE
    )
  )
  
  
  
  ### ANOVAs paleo - geo ----
  
  aov.paleo.geo <- summary.aov(manova.paleo.geo)
  names(aov.paleo.geo) <- vars.paleo
  aov.paleo.geo$paleotemp_cumchange
  
  for (v in vars.paleo) {
    
    tab <- aov.paleo.geo[[v]]
    
    ss_effect <- tab$`Sum Sq`[1]
    ss_resid  <- tab$`Sum Sq`[2]
    eta2 <- ss_effect / (ss_effect + ss_resid)
    
    df1 <- tab$Df[1]
    df2 <- tab$Df[2]
    F.stat <- tab$`F value`[1]
    pval <- tab$`Pr(>F)`[1]
    
    # save to table
    results_anova <- rbind(
      results_anova, 
      data.frame(
        taxon = t,
        variable = v,
        effect = 'geo',
        ss_effect = ss_effect, 
        ss_resid = ss_resid, 
        df1 = df1,
        df2 = df2,
        F.stat = F.stat,
        p = pval,
        eta2 = eta2,
        stringsAsFactors = FALSE
      )
    )
  }
  
  
}

results_manova
results_anova

hexgrid_per_region_combined

dat.test <- hexgrid_per_region_combined[[t]]

Y_current <- scale(dat.test[, c("temp", "prec", "tempseas", "precseas", "npp", "tri_current")])
mod_current.type <- manova(Y_current ~ type, data = dat.test)
mod_current.geo <- manova(Y_current ~ geo, data = dat.test)
summary(mod_current.type, test = "Pillai")
summary.aov(mod_current.type)

summary(mod_current.geo,  test = "Pillai")
summary.aov(mod_current.geo)

# Multivariate analyses revealed significant differences in climatic conditions between regions classified as cradles and museums (Pillai’s trace = 0.34, p < 0.001). However, geographic variation among regions explained substantially more variation in climate (Pillai’s trace = 1.93, p < 0.001), with effect sizes consistently higher across all variables. This indicates that climatic differences between cradles and museums largely reflect underlying regional environmental differences rather than a consistent climatic signature associated with cradle or museum dynamics.

library(RRPP)
Y_paleo <- scale(dat.test[, c("paleotemp_cumchange", "paleoprec_cumchange")])
mod_paleo.type <- manova(Y_paleo ~ type, data = dat.test)
mod_paleo.geo <- manova(Y_paleo ~ geo, data = dat.test)

mod_paleo.type_rrpp <- lm.rrpp(Y_paleo ~ type, data = dat.test, iter = 2000)
summary.manova.rrpp_paleo <- summary(mod_paleo.type_rrpp)
summary.manova.rrpp_paleo$table

hist(dat.test$paleotemp_cumchange)
hist(log(dat.test$paleotemp_cumchange))

mod_paleotemp_cumchange_rrpp <- lm.rrpp(paleotemp_cumchange ~ type, data = dat.test)
anova_paleotemp_cumchange_rrpp <- anova.lm.rrpp(mod_paleotemp_cumchange_rrpp)


summary(mod_paleotemp_cumchange_rrpp)

summary(mod_paleo.type, test = "Pillai")
summary.aov(mod_paleo.type)

summary(mod_paleo.geo,  test = "Pillai")
summary.aov(mod_paleo.geo)

# Multivariate analyses of paleoclimatic variables also revealed significant differences between cradle and museum regions (Pillai’s trace = 0.17, p < 0.001), although effect sizes were notably smaller than for current climatic conditions. Univariate analyses showed that these differences were driven primarily by cumulative changes in precipitation, whereas temperature change did not differ between cradles and museums. In contrast, geographic variation among regions remained consistently strong (Pillai’s trace = 0.51, p < 0.001), with all paleoclimatic variables showing significant differences among regions. Together, these results indicate that climatic differences between cradles and museums are variable and dependent on specific variables and time periods, whereas geographic structure is consistently dominant.









# PALEOCLIMATE REGION MAPS ----

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
proj_hexgrid <- crs(hexgrid_list[[1]])
world_projected <- st_transform(world, crs = proj_hexgrid)

maps <- list()

for (t in taxa) {
  
  maps[[t]] <- list()
  
  # collect all pieces for this taxon: all types x geos
  # hexgrid_per_region[[tx]] is a list of types; each type is a list of geos (sf-ish tables)
  dat_list <- list()
  
  for (typ in c('cradle', 'museum')) {
    for (r in names(hexgrid_per_region[[t]][[typ]])) {
      
      dat_list[[paste(typ, r, sep = "__")]] <- hexgrid_per_region[[t]][[typ]][[r]] %>%
        mutate(type = typ, geo = r)
    }
  }
  
  dat <- bind_rows(dat_list)
  


  
  # Crear contornos por región (geo) y tipo (cradle/museum)
  #    -> st_union une todas las celdas de esa región en una geometría
  outlines <- dat %>%
    group_by(type, geo) %>%
    summarise(geometry = st_union(gridTemplate), .groups = "drop")
  
  for (v in paleoclim_vars) {
    
    # keep only non-missing values for this variable
    dat_sf_v <- dat %>% filter(is.finite(.data[[v]]))
    
    p <- ggplot() +
      geom_sf(data = world_projected, fill = "gray80", color = "gray80", linewidth = 0.2) +
      geom_sf(data = dat_sf_v, aes(fill = .data[[v]]), color = NA) +
      
      geom_sf(data = outlines,
              color = "white",
              fill = NA,
              linewidth = 0.5) + 
      
      geom_sf(data = outlines, aes(color = type), fill = NA, linewidth = 0.3) +
      
      scale_fill_viridis(
        option = "viridis",
        direction = 1
      ) +
      
      scale_color_manual(values = c(cradle = col_cradle, museum = col_museum)) +
      coord_sf(crs = proj_hexgrid) +
      labs(title = paste(t, "-", v), fill = v, color = "type") +
      theme_classic() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = 'bottom'
      )
    
    maps[[t]][[v]] <- p
  }
}

# Example: show one
maps[["mammals"]][["paleotemp_cumchange"]]




# PALEOCLIMATE SPACES ----
colnames(hexgrid_list[[t]])[grep('paleo', colnames(hexgrid_list[[t]]))]

crad_col <- '#2F6BA8' # blue
mus_col <- '#A72E37' # red
mid_col <- 'gray80'

typ_colors <- setNames(c(crad_col, mus_col), c('cradle', 'museum'))


pointsize <- 1
transparency <- 0.6

## > Paleoclimate SD ----
paleoclimate_sd_plot <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  paleoclimate_sd_plot[[t]] <- setNames(vector('list', length = 2), 
                                        c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    paleoclimate_sd_plot[[t]][[typ]] <- 
      ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
             aes(x = paleotemp_sd, y = paleoprec_sd)) + 
      geom_point(color = 'gray80', size = pointsize) +
      geom_point(data = hexgrid_list[[t]] %>% filter(type == typ), 
                 color = typ_colors[typ],
                 size = pointsize, alpha = transparency) +
      #lims(x = c(-20, 30)) +
      labs(title = paste0(t, ' paleoclimatic SD - ', typ)) +
      theme_minimal() +
      theme(legend.position = 'none', 
            legend.key.height = unit(0.2,"cm"),
            plot.title = element_text(hjust = 0.5, 
                                      face = 'bold'),
            legend.text = element_text(angle = 0, hjust = 1))
  }
  
  
}

paleoclimate_sd_plot <- unlist(paleoclimate_sd_plot, recursive = FALSE)
wrap_plots(paleoclimate_sd_plot, ncol = 2)

ggplot(data = hexgrid_list[['squamates']] %>% filter(type %in% c('cradle', 'museum'))) +
  geom_violin(aes(x = geo, y = log(paleotemp_sd), fill = type)) +
  scale_fill_manual(values = typ_colors) + 
  theme_classic()



ggplot(data = hexgrid_list[['birds']]) +
  geom_point(aes(x = log(paleotemp_sd), y = logdr, col = resloess_pd_rich)) +
  scale_color_gradient2(low = '#2F6BA8', # blue
                       mid = "gray90",
                       high = '#A72E37', # red
                       midpoint = 0, 
                       name = 'residual PD') +
  theme_classic()

colnames(hexgrid_list[['birds']])
unique(hexgrid_list[['birds']]$geo)


## > Paleoclimate net change ----
paleoclimate_netchange_plot <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  paleoclimate_netchange_plot[[t]] <- setNames(vector('list', length = 2), 
                                        c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    paleoclimate_netchange_plot[[t]][[typ]] <- 
      ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
             aes(x = paleotemp_netchange, y = paleoprec_netchange)) + 
      geom_point(color = 'gray80', size = pointsize) +
      geom_point(data = hexgrid_list[[t]] %>% filter(type == typ), 
                 color = typ_colors[typ],
                 size = pointsize, alpha = transparency) +
      #lims(x = c(-20, 30)) +
      labs(title = paste0(t, ' clim net change - ', typ)) +
      theme_minimal() +
      theme(legend.position = 'none', 
            legend.key.height = unit(0.2,"cm"),
            plot.title = element_text(hjust = 0.5, 
                                      face = 'bold'),
            legend.text = element_text(angle = 0, hjust = 1))
  }
  
  
}

paleoclimate_netchange_plot <- unlist(paleoclimate_netchange_plot, recursive = FALSE)
wrap_plots(paleoclimate_netchange_plot, ncol = 2)

## > Paleoclimate cumulative change ----
paleoclimate_cumchange_plot <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  paleoclimate_cumchange_plot[[t]] <- setNames(vector('list', length = 2), 
                                               c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    paleoclimate_cumchange_plot[[t]][[typ]] <- 
      ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
             aes(x = paleotemp_cumchange, y = paleoprec_cumchange)) + 
      geom_point(color = 'gray80', size = pointsize) +
      geom_point(data = hexgrid_list[[t]] %>% filter(type == typ), 
                 color = typ_colors[typ],
                 size = pointsize, alpha = transparency) +
      #lims(x = c(-20, 30)) +
      labs(title = paste0(t, ' clim cum change - ', typ)) +
      theme_minimal() +
      theme(legend.position = 'none', 
            legend.key.height = unit(0.2,"cm"),
            plot.title = element_text(hjust = 0.5, 
                                      face = 'bold'),
            legend.text = element_text(angle = 0, hjust = 1))
  }
  
  
}

paleoclimate_cumchange_plot <- unlist(paleoclimate_cumchange_plot, recursive = FALSE)
wrap_plots(paleoclimate_cumchange_plot, ncol = 2)

## > Paleoclimate slope ----
paleoclimate_slope_plot <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  paleoclimate_slope_plot[[t]] <- setNames(vector('list', length = 2), 
                                               c('cradle', 'museum'))
}

for (t in taxa){
  for (typ in c('cradle', 'museum')){
    paleoclimate_slope_plot[[t]][[typ]] <- 
      ggplot(data = hexgrid_list[[t]] %>% filter(type == 'none'), 
             aes(x = paleotemp_slope, y = paleoprec_slope)) + 
      geom_point(color = 'gray80', size = pointsize) +
      geom_point(data = hexgrid_list[[t]] %>% filter(type == typ), 
                 color = typ_colors[typ],
                 size = pointsize, alpha = transparency) +
      #lims(x = c(-20, 30)) +
      labs(title = paste0(t, ' paleoclim slope - ', typ)) +
      theme_minimal() +
      theme(legend.position = 'none', 
            legend.key.height = unit(0.2,"cm"),
            plot.title = element_text(hjust = 0.5, 
                                      face = 'bold'),
            legend.text = element_text(angle = 0, hjust = 1))
  }
  
  
}

paleoclimate_slope_plot <- unlist(paleoclimate_slope_plot, recursive = FALSE)
wrap_plots(paleoclimate_slope_plot, ncol = 2)















