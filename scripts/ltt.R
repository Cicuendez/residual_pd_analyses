

# date ----
# 14/11/2022

# purpose ----
# LTT plots from posterior distribution of each vertebrate group 
# for each main region

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 
              'scico', 'RColorBrewer', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'rlist')

lapply(packages, require, character.only = TRUE)

# data: species in regions ----
sp_regions_list <- readRDS('objects/sp_regions_list.rds')
sp_regions_list[['australia']]$squamates

# data: phylogeny ----
trees_squamates <- readRDS('data/phylogeny/tree_post_100_SQUAMATES.rds')
trees_mammals <- readRDS('data/phylogeny/tree_post_100_MAMMALS.rds')
trees_amphibians <- readRDS('data/phylogeny/tree_post_100_AMPHIBIANS.rds')
trees_birds <- readRDS('data/phylogeny/tree_post_100_BIRDS.rds')

regions <- c('africa', 'australia', 'n_america', 's_america', 
             'palearctic', 'oriental', 'madagascar')
taxa <- c('squamates', 'mammals', 'amphibians', 'birds')

postree_list <- list(trees_squamates, trees_mammals, trees_amphibians, 
                     trees_birds)
names(postree_list) <- taxa

# regional trees ----
# generate regional phylogenies: trees with the species present 
# in each region, for each taxa group
region_trees_list <- vector('list', length(regions))
names(region_trees_list) <- regions
for (i in 1:length(regions)){
    region_trees_list[[i]] <- vector('list', length(taxa))
    names(region_trees_list[[i]]) <- taxa
}

'%nin%' <- Negate('%in%')

for (r in regions){
    for (t in taxa){
        # Show progress
        print(paste0(r, ' -- ', t))

        # Get the species that are not in the region
        tips_to_drop <- postree_list[[t]][[1]]$tip.label[postree_list[[t]][[1]]$tip.label %nin% sp_regions_list[[r]][[t]]]

        # Drop those species
        region_trees_list[[r]][[t]] <- drop.tip.multiPhylo(postree_list[[t]], tip = tips_to_drop)
    }
}

# save regional trees ----
saveRDS(region_trees_list, 'objects/region_trees_list.rds')

# import regional trees ----
region_trees_list <- readRDS('../objects/region_trees_list.rds')

regions <- c('africa', 'australia', 'n_america', 's_america', 
             'palearctic', 'oriental', 'madagascar')
taxa <- c('squamates', 'mammals', 'amphibians', 'birds')
'%nin%' <- Negate('%in%')

# ltt plots ----
ltt.plot(region_trees_list[['africa']][['squamates']][[1]])
mltt.plot(region_trees_list[['oriental']][['squamates']], 
          dcol = FALSE, log = 'y', legend = FALSE)
mltt.plot(region_trees_list[['africa']][['mammals']], 
          dcol = FALSE, log = 'y', legend = FALSE)
ltt.plot.coords(region_trees_list[['africa']][['mammals']][[1]])
as.data.frame(ltt.plot.coords(region_trees_list[['africa']][['mammals']][[1]]))
?ltt.plot

ltt_coords_list <- vector('list', length(regions))
names(ltt_coords_list) <- regions
for (i in 1:length(ltt_coords_list)){
  ltt_coords_list[[i]] <- vector('list', length(taxa))
  names(ltt_coords_list[[i]]) <- taxa
}

for (r in regions){
  for (t in taxa){
    ltt_coords_list[[r]][[t]] <- vector('list', length = 100)
    names(ltt_coords_list[[r]][[t]]) <- paste0('tree', c(1:100))
  }
}

for (r in regions){
  for (t in taxa){
    for (i in 1:100){
      print(paste0(r, ' -- ', t, ' -- tree ', i))
      ltt_coords_list[[r]][[t]][[i]] <- as.data.frame(ltt.plot.coords(region_trees_list[[r]][[t]][[i]]))
      ltt_coords_list[[r]][[t]][[i]]$tree <- i
      ltt_coords_list[[r]][[t]][[i]]$taxa <- t
      ltt_coords_list[[r]][[t]][[i]]$region <- r
      
    }
  }
}
head(ltt_coords_list[['s_america']])
nrow(ltt_coords_list[['africa']][[t]][[i]])

for (r in regions){
  for (t in taxa){
    ltt_coords_list[[r]][[t]] <- rlist::list.rbind(ltt_coords_list[[r]][[t]])
  }
}

for ( r in regions){
  ltt_coords_list[[r]] <- rlist::list.rbind(ltt_coords_list[[r]])
}
str(ltt_coords_list)
# Now we have a list, one element per region, each element is a 
# dataframe with all taxa, 100 trees per taxa, with the time points and 
# number of lineages per tree.

?list.rbind
rlist::list.cbind(pd_list)

ggplot(data = ltt_coords_list[[r]], aes(x = time, y = log(N))) +
  geom_line(data = . %>% filter(taxa == 'squamates'), aes(color = tree)) +
  scale_color_gradient(low = 'gray80', high = 'red')

ltt_coords_list[[r]]$tree <- as.factor(ltt_coords_list[[r]]$tree)

ltt_coords_list[['s_america']] %>% 
  filter(taxa == 'squamates') %>%
  ggplot(data = ., aes(x = time, y = log(N))) +
  geom_line(aes(color = tree)) +
  geom_line(data = ltt_coords_list[[r]][ltt_coords_list[[r]]$taxa == 'mammals',], 
            aes(color = tree)) +
  scale_color_manual(values = rep('gray80', 100)) +
  theme_bw() +
  theme(legend.position = 'none')

taxa_colors <- c(mammals = '#003f5c', squamates = '#7a5195', 
                    amphibians = '#ef5675', birds = '#ffa600')


plot_ltt_by_region_list <- vector('list', length(regions))
names(plot_ltt_by_region_list) <- regions

levels(plot_ltt_by_region_list[[r]]$taxa)

for (r in regions){
  plot_ltt_by_region_list[[r]] <- ltt_coords_list[[r]] %>% 
    ggplot(data = ., 
           aes(x = time, y = log(N))) +
    geom_line(aes(color = interaction(tree, taxa, sep=':')), 
              alpha = 0.2, size = 0.5) +
    scale_color_manual(values = c(rep('#ef5675', 100), # amphibians
                                  rep('#ffa600', 100), # birds
                                  rep('#003f5c', 100), # mammals
                                  rep('#7a5195', 100))) + # squamates
    lims(x = c(-350, 0)) +
    labs(title = r) +
    theme_bw() +
    theme(legend.position = 'none')
  
}

wrap_plots(plot_ltt_by_region_list)
ggsave('plots/all_ltt_by_region.png', plot = last_plot())



# one single dataframe to rule them all ----
df <- rlist::list.rbind(ltt_coords_list)
nrow(df)
df$region <- as.factor(df$region)
levels(df$region)

ltt_by_taxa_plots <- vector('list', length(taxa))
names(ltt_by_taxa_plots) <- taxa


for (t in taxa){
  ltt_by_taxa_plots[[t]] <- df %>% 
    filter(taxa == t) %>%
    filter(region %nin% c('madagascar', 's_america')) %>%
    ggplot(data = ., 
           aes(x = time, y = log(N))) +
    geom_line(aes(color = interaction(tree, region, sep=':')), 
              alpha = 0.2, size = 0.3) +
    scale_color_manual(values = c(rep('#F7B172', 100), # africa
                                  rep('#EB6662', 100), # australia
                                  rep('#82C881', 100), # namerica
                                  rep('#F7D37E', 100), # oriental
                                  rep('#1D8F94', 100) # palearctic
 #                                 rep('#203D85', 100) # samerica
                                  )) + 
    lims(x = c(-350, 0)) +
    labs(title = t) +
    theme_bw() +
    theme(legend.position = 'none')
  
}

region_colors <- c(africa = '#F7B172', 
                   australia = '#EB6662', 
                   namerica = '#82C881', 
                   oriental = '#F7D37E', 
                   palearctic = '#1D8F94', 
                   samerica = '#203D85')
plot(1:6, 1:6, cex = 8, pch = 16, col = region_colors)

wrap_plots(ltt_by_taxa_plots)
ggsave('plots/all_ltt_by_taxa.png', plot = last_plot())
ggsave('plots/all_ltt_by_taxa_noSA.png', plot = last_plot())

ltt_by_taxa_plots$squamates





