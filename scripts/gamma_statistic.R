


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
              'geiger', 'picante', 'patchwork', 'rlist', 'data.table')
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

# save gamma values ----
saveRDS(gamma_values, 'output/gamma_stat_values.rds')
gamma_values <- readRDS('output/gamma_stat_values.rds')

# Plot ----
t <- 'squamates'
xx <- 'cradle'
r <- 'australia'
ntree <- 1

density(gamma_values[[t]][[xx]][[r]])
densityplot(gamma_values[[t]][[xx]][[r]], col = 'black')

density_data <- density(gamma_values[[t]][[xx]][[r]])
plot(density_data, type = 'n')
polygon(density_data, 
        col = "#963448",  # Fill color
        border = "transparent")
density_data$x

# Using the vioplot package
library(vioplot)
vioplot(gamma_values[[t]][[xx]][[reg[1]]], 
        gamma_values[[t]][[xx]][[reg[2]]], 
        gamma_values[[t]][[xx]][[reg[3]]],
        names = c("Group 1", "Group 2", 'Group 3'), col = c("lightblue", "pink", 'purple'), 
        border = NA)

# make dataframes of gamma values
gamma_df <- gamma_values

for (t in taxa){
  for (xx in c('cradle', 'museum')){
    reg <- names(gamma_values[[t]][[xx]])
    for (r in reg){
      gamma_df[[t]][[xx]][[r]] <- data.frame(taxa = t, region_name = r, resPD = xx, 
                                             gamma = gamma_values[[t]][[xx]][[r]])
    }
  }
}

# combine dataframes
gamma_df_combined <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  gamma_df_combined[[t]] <- rbindlist(lapply(gamma_df[[t]], rbindlist))
}

# Gamma comparison high vs low resPD ----


# colors ----
col_lowrespd <- '#2F6BA8' # blue
col_highrespd <- '#A72E37' # red

# Plot per region ----
gamma.plot.regions <- setNames(vector('list', length(taxa)), taxa)
for (t in taxa){
  
  gamma.plot.regions[[t]] <- ggplot(data = gamma_df_combined[[t]], 
         aes(x = region_name, y = gamma, fill = resPD)) +
    geom_violin(color = 'transparent') +
    scale_fill_manual(values = c(cradle = col_lowrespd, museum = col_highrespd)) +
#    stat_summary(geom = 'pointrange', fun = 'median', na.rm = TRUE, 
#                 color = 'black', fill = 'white', pch = 21) +
    geom_hline(yintercept = 0, linewidth = 0.2, color = 'gray40', linetype = 'dashed') +
    labs(x = '', y = 'gamma value', title = toupper(t)) +
    theme_classic() +
    theme(legend.position = 'none', 
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 10), 
          plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 8), 
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  
}
wrap_plots(gamma.plot.regions)
ggsave('plots/gamma_regions.png', wrap_plots(gamma.plot.regions))

# Plot all regions together ----
# coordinates for taxa pics: amphibians, birds, mammals, squamates
xmin_pic <- setNames(c(1.7, 1.7, 0.7, 1.6), taxa)
xmax_pic <- setNames(c(2.3, 2.3, 1.4, 2.4), taxa)
ymin_pic <- setNames(c(-2.5, 5.5, 0.5, 4), taxa)
ymax_pic <- setNames(c(-5.0, 10.0, 4, 11), taxa)


gamma.plot <- setNames(vector('list', length(taxa)), taxa)


for (t in taxa){
  # import taxa pic
  taxapic_fn <- paste0('../taxa_images/', t, '.png')
  img <- png::readPNG(taxapic_fn)
  g <- grid::rasterGrob(img, interpolate = TRUE)
  
  gamma.plot[[t]] <- ggplot(data = gamma_df_combined[[t]], 
                                    aes(x = resPD, y = gamma, fill = resPD)) +
    geom_violin(color = 'transparent') +
    scale_fill_manual(values = c(cradle = col_lowrespd, museum = col_highrespd)) +
    stat_summary(geom = 'pointrange', fun = 'median', na.rm = TRUE, 
                 color = 'black', fill = 'white', pch = 21) +
    geom_hline(yintercept = 0, linewidth = 0.2, color = 'gray40', linetype = 'dashed') +
    scale_x_discrete(labels=c('low resPD', 'high resPD')) +
    labs(x = '', y = 'gamma value', title = toupper(t)) +
    annotation_custom(grob = g, 
                      xmin = xmin_pic[t], xmax = xmax_pic[t], 
                      ymin = ymin_pic[t], ymax = ymax_pic[t]) +
    theme_classic() +
    theme(legend.position = 'none', 
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 10), 
          plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 8), 
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7))
  
}
wrap_plots(gamma.plot)
ggsave(filename = 'plots/gamma.png', 
       plot = wrap_plots(gamma.plot))

ggsave(filename = 'plots/gamma.pdf', 
       plot = wrap_plots(gamma.plot))

