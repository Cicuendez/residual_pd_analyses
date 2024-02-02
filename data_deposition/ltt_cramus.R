

# purpose ----
# LTT plots from posterior distribution of each vertebrate group 
# for each main cradle and museum

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 
              'scico', 'RColorBrewer', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'rlist')
lapply(packages, require, character.only = TRUE)

# taxa ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# import data sp in cradles and museums ----
sp_cramus <- readRDS('../objects/sp_cramus.rds')

sp_cramus$mammals$cradle$north_america[!sp_cramus$mammals$cradle$north_america %in% sp_cramus$mammals$museum$north_america]
sp_cramus$mammals$museum$north_america[!sp_cramus$mammals$museum$north_america %in% sp_cramus$mammals$cradle$north_america]
intersect(sp_cramus$mammals$museum$north_america, sp_cramus$mammals$cradle$north_america)

str(sp_cramus)

# data: phylogeny ----
#trees_squamates <- readRDS('../data/phylogeny/tree_post_100_SQUAMATES.rds')
#trees_mammals <- readRDS('../data/phylogeny/tree_post_100_MAMMALS.rds')
#trees_amphibians <- readRDS('../data/phylogeny/tree_post_100_AMPHIBIANS.rds')
#trees_birds <- readRDS('../data/phylogeny/tree_post_100_BIRDS.rds')

# posterior trees
phylogeny_dir <- '../data/phylogeny'
list.files(phylogeny_dir, full.names = TRUE)[grep('tree_post_100', 
                                                  list.files(phylogeny_dir))]

post_list <- vector('list', length(taxa))
names(post_list) <- taxa

for (t in taxa){
  post_list[[t]] <- readRDS(paste0('../data/phylogeny/tree_post_100_', 
                                               toupper(t), '.rds'))
  print(paste0(t, ' consensus tree imported'))
}

# consensus trees
tree_list <- vector('list', length(taxa))
names(tree_list) <- taxa

for (t in taxa){
  tree_list[[t]] <- treeio::read.newick(paste0('../data/phylogeny/consensus/', 
                                               t, '_all_tree.tre'))
  print(paste0(t, ' consensus tree imported'))
}
# the bird tree is the tree #1 of the posterior distribution based on Hackett

# the outgroup of mammals ("_Anolis_carolinensis") makes the root very old 
# relative to mammal diversification, so we remove it for better visualization.
tree_list[['mammals']] <- drop.tip(tree_list[['mammals']], '_Anolis_carolinensis')
tree_list

# LTT DATAFRAMES: PREPARATION ----
ltt_df <- vector('list', length(taxa))
names(ltt_df) <- taxa

for (t in taxa){
  ltt_df[[t]] <- vector('list', length = 2)
  names(ltt_df[[t]]) <- c('cradle', 'museum')
}

# __ amphibians ----
# ____ cradles ----
ltt_df[['amphibians']][['cradle']] <- vector('list', length = 3)
names(ltt_df[['amphibians']][['cradle']]) <- c('south_america', 
                                                   'oriental', 
                                                   'australia_oceania')
# ____ museums ----
ltt_df[['amphibians']][['museum']] <- vector('list', length = 1)
names(ltt_df[['amphibians']][['museum']]) <- 'africa'

# __ birds ----
# ____ cradles ----
ltt_df[['birds']][['cradle']] <- vector('list', length = 2)
names(ltt_df[['birds']][['cradle']]) <- c('north_america', 
                                              'south_america')

# ____ museums ----
ltt_df[['birds']][['museum']] <- vector('list', length = 2)
names(ltt_df[['birds']][['museum']]) <- c('africa_madagascar', 
                                              'australia_oceania_asia')

# __ mammals ----
# ____ cradles ----
ltt_df[['mammals']][['cradle']] <- vector('list', length = 2)
names(ltt_df[['mammals']][['cradle']]) <- c('north_america', 
                                                'south_america')

# ____ museums ----
ltt_df[['mammals']][['museum']] <- vector('list', length = 2)
names(ltt_df[['mammals']][['museum']]) <- c('north_america', 
                                                'africa')

# __ squamates ----
# ____ cradles ----
ltt_df[['squamates']][['cradle']] <- vector('list', length = 3)
names(ltt_df[['squamates']][['cradle']]) <- c('north_america', 
                                                  'south_america', 
                                                  'australia')

# ____ museums ----
ltt_df[['squamates']][['museum']] <- vector('list', length = 2)
names(ltt_df[['squamates']][['museum']]) <- c('south_africa', 
                                                  'southeast_asia')

npost <- length(post_list[[1]])
for (t in taxa){
  for (xx in c('cradle', 'museum')){
    reg <- names(ltt_df[[t]][[xx]])
    for (r in reg){
      ltt_df[[t]][[xx]][[r]] <- vector('list', length = npost)
    }
  }
}

# Now we have a list of nested lists: 
# - 4 lists corresponding to different taxa. 
# - 2 lists per taxa with cradles and museums respectively. 
# - a variable number of lists corresponding to each cradle and museum per taxa. 
# - 100 lists per cradle/museum corresponding to each tree from the posterior. 
# For each of the trees, we will have a dataframe with the LTT coordinates.
length(ltt_df$amphibians$cradle$south_america)
ltt_df$amphibians$cradle$south_america[[33]]

ntree

ltt_plots <- ltt_df # prepare another list of lists for the actual ltt plots

# LTT DATAFRAMES: CREATION ----
t
xx
r <- 'south_africa'
ntree <- 1
for (t in taxa){
  print(toupper(t))
  for (xx in c('cradle', 'museum')){
    reg <- names(sp_cramus[[t]][[xx]])
    for (r in reg){
      for (ntree in 1:npost){
        print(paste0(t, ' -- ', xx, ' -- ', r, ' -- tree ', ntree))
        ltt_df[[t]][[xx]][[r]][[ntree]] <- 
          as.data.frame(ltt.plot.coords(keep.tip(post_list[[t]][[ntree]], 
                                                 tip = sp_cramus[[t]][[xx]][[r]])))
      }
    }
  }
}

saveRDS(ltt_df, '../objects/ltt_df_list.rds')
ltt_df <- readRDS('../objects/ltt_df_list.rds')

ltt_df$amphibians$cradle$south_america[[33]]

# add columns with the information on number of tree, region name, 
# type (cradle or museum), and taxa.
for (t in taxa){
  for (xx in c('cradle', 'museum')){
    for (r in names(ltt_df[[t]][[xx]])){
      for (ntree in 1:npost){
        print(paste0(t, ' -- ', xx, ' -- ', r, ' -- tree ', ntree))
        ltt_df[[t]][[xx]][[r]][[ntree]]$tree <- ntree
        ltt_df[[t]][[xx]][[r]][[ntree]]$geo <- r
        ltt_df[[t]][[xx]][[r]][[ntree]]$type <- xx
        ltt_df[[t]][[xx]][[r]][[ntree]]$taxa <- t
      }
    }
  }
}

ltt_df$amphibians$cradle$south_america[[33]]

# Let's bind the dataframes for the 100 trees of each region. 
for (t in taxa){
  for (xx in c('cradle', 'museum')){
    for (r in names(ltt_df[[t]][[xx]])){
      ltt_df[[t]][[xx]][[r]] <- rlist::list.rbind(ltt_df[[t]][[xx]][[r]])
    }
  }
}

ltt_df$amphibians$cradle$south_america
unique(ltt_df$amphibians$cradle$south_america$tree)

# LTT DATAFRAMES FULL TREES ----
# We get the ltt for the full trees to plot in the background
ltt_fulltrees_df <- vector('list', length(taxa))
names(ltt_fulltrees_df) <- taxa

for (t in taxa){
  ltt_fulltrees_df[[t]] <- vector('list', length = npost)
}

for (t in taxa){
  for (ntree in 1:npost){
    ltt_fulltrees_df[[t]][[ntree]] <- as.data.frame(ltt.plot.coords(post_list[[t]][[ntree]]))
    ltt_fulltrees_df[[t]][[ntree]]$tree <- ntree
    ltt_fulltrees_df[[t]][[ntree]]$taxa <- t
  }
}

for (t in taxa){
  ltt_fulltrees_df[[t]] <- rlist::list.rbind(ltt_fulltrees_df[[t]])
}

# COLORS cradle & museum ----
col_crad <- '#2F6BA8' # blue
col_mus <- '#A72E37' # red
cramus_col <- c(cradle = '#2F6BA8', museum = '#A72E37')



# LTT PLOTS ----
ltt_plots <- ltt_df

for (t in taxa){
  for (xx in c('cradle', 'museum')){
    for (r in names(ltt_plots[[t]][[xx]])){
      print(paste0(t, ' -- ', xx, ' -- ', r))
      
      # plot
      ltt_plots[[t]][[xx]][[r]] <- ggplot() +
        # first we plot the ltt distribution of the full trees
        geom_line(data = ltt_fulltrees_df[[t]], 
                  aes(x = time, y = log(N), group = tree), 
                  color = 'gray80', size = 0.1) +
        
        # then we add the ltt for the region
        geom_line(data = ltt_df[[t]][[xx]][[r]], 
                  aes(x = time, y = log(N), group = tree),
                  color = cramus_col[[xx]], size = 0.1) +
        labs(title = paste0(t, ' in ', r)) +
        theme_minimal() +
        theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 12))
      
    }
  }
}

ltt_plots[[t]][[xx]][[r]]

# save the tree plots ----
for (t in taxa){
  print(t)
  i <- 1
  np <- sum(lengths(ltt_plots[[t]]))
  p <- vector('list', length = np)
  for (xx in 1:length(ltt_plots[[t]])){
    for (r in 1:length(ltt_plots[[t]][[xx]])){
      p[[i]] <- ltt_plots[[t]][[xx]][[r]]
      i <- i + 1
    }
  }
  wrap_plots(p)
  fn <- paste0('plots/ltt_', t, '.pdf')
  ggsave(fn, plot = last_plot())
}




###################
###################
###################
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







ltt_df$squamates$cradle$australia




# 







