






# cradles vs museums 

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'png', 'grid')

easypackages::libraries(packages)

# taxa ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# DATA: hexgrids ----
hexgrid_list <- readRDS('../objects/hexgrid_list_v2.rds')
colnames(hexgrid_list[['birds']])

# create discrete column cradles & museums
for (t in taxa){
  quants <- quantile(hexgrid_list[[t]]$resloess_pd_rich, c(0.1, 0.9))
  hexgrid_list[[t]] <- hexgrid_list[[t]] %>% 
    mutate(type = case_when(resloess_pd_rich <= quants[1] ~ 'cradle', 
                            resloess_pd_rich >= quants[2] ~ 'museum', 
                            (quants[1] < resloess_pd_rich & 
                               resloess_pd_rich < quants[2]) ~ 'none'))
}

# DATA: epm objects ----
epm_list <- vector('list', length(taxa))
names(epm_list) <- taxa

for (t in taxa){
  epm_list[[t]] <- readRDS(paste0('../objects/EPM_', t, '.rds'))
  print(paste0(t, ' EPM imported'))
}

# DATA: DR rates ----
dr_objects <- list.files('../objects', 
                         full.names = TRUE)[grep('dr_mean_', 
                                                 list.files('../objects'))]
dr_list <- vector('list', length = length(taxa))
names(dr_list) <- taxa

for (t in taxa){
  fn <- dr_objects[grep(t, dr_objects, ignore.case = TRUE)]
  dr_df <- readRDS(fn)
  dr_list[[t]] <- dr_df$dr
  names(dr_list[[t]]) <- rownames(dr_df)
}

# DATA: trees ----
phylogeny_dir <- '../data/phylogeny'
list.files(phylogeny_dir, full.names = TRUE)[grep('tree_post_100', 
                                                  list.files(phylogeny_dir))]

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


# get cell IDs of cradles and museums ----
cell_cradles_museums <- vector('list', length(taxa))
names(cell_cradles_museums) <- taxa

for (t in taxa){
  cell_cradles_museums[[t]] <- vector('list', length = 2)
  names(cell_cradles_museums[[t]]) <- c('cradle', 'museum')
}

for (t in taxa){
  for (xx in c('cradle', 'museum'))
  cell_cradles_museums[[t]][[xx]] <- hexgrid_list[[t]] %>%
    dplyr::filter(type == xx) %>% 
    .$id
}

# get species in cradles and museums from cell IDs ----
sp_in_cradles_and_museums <- vector('list', length(taxa))
names(sp_in_cradles_and_museums) <- taxa
for (t in taxa){
  sp_in_cradles_and_museums[[t]] <- vector('list', length = 2)
  names(sp_in_cradles_and_museums[[t]]) <- c('cradle', 'museum')
}


for (t in taxa){
  for (xx in c('cradle', 'museum')){
    sp_cell_list <- epm::expandSpeciesCellList(epm_list[[t]])
    names(sp_cell_list) <- epm_list[[t]]$grid$grid_id
    
    cells <- cell_cradles_museums[[t]][[xx]]
    sp_in_cradles_and_museums[[t]][[xx]] <- unique(unlist(sp_cell_list[cells]))
    
  }
}

saveRDS(sp_in_cradles_and_museums, '../objects/sp_cramus_alltogether.rds')
sp_in_cradles_and_museums <- readRDS('../objects/sp_cramus_alltogether.rds')

# DR in cradles and museums ----
dr_cramus_all <- vector('list', length(taxa))
names(dr_cramus_all) <- taxa
for (t in taxa){
  dr_cramus_all[[t]] <- vector('list', length = 2)
  names(dr_cramus_all[[t]]) <- c('cradle', 'museum')
}

for (t in taxa){
  for (xx in c('cradle', 'museum')){
    species <- sp_in_cradles_and_museums[[t]][[xx]]
    dr_values <- dr_list[[t]][species]
    dr_values <- dr_values[!is.na(dr_values)]
    dr_cramus_all[[t]][[xx]] <- data.frame(sp = names(dr_values), 
                                           dr = dr_values, 
                                           type = xx, 
                                           taxa = t)
    
  }
  dr_cramus_all[[t]] <- data.table::rbindlist(dr_cramus_all[[t]])
}

# duplicated species
nrow(dr_cramus_all[[t]][duplicated(dr_cramus_all[[t]]$sp),])


# ANOVA all with RRPP ----
library(RRPP)
aovrrpp_all_list <- vector('list', length(taxa))
names(aovrrpp_all_list) <- taxa

summary_all_list <- vector('list', length(taxa))
names(summary_all_list) <- taxa

for (t in taxa){
  print(t)
  aovrrpp_all_list[[t]] <- RRPP::lm.rrpp(f1 = dr ~ type, 
                                         data = dr_cramus_all[[t]],
                                         iter = 999, 
                                         RRPP = TRUE,
                                         print.progress = TRUE)
  summary_all_list[[t]] <- summary(aovrrpp_all_list[[t]])
  print(paste0('p-value ', t, ' = ', summary_all_list[[t]]$table$`Pr(>F)`))
  print(paste0('Z ', t, ' = ', summary_all_list[[t]]$table$`Z (from F)`))
  print('##################################')
}



for (t in taxa){
  summary_all_list[[t]] <- summary(aovrrpp_all_list[[t]])
  print(paste0('p-value ', t, ' = ', summary_all_list[[t]]$table$`Pr(>F)`))
  print(paste0('Z ', t, ' = ', summary_all_list[[t]]$table$`Z (from F)`))
}

# VIOLIN PLOT DR ALL CRADLES VS MUSEUMS ----
boxplot_dr_all_list <- vector('list', length(taxa))
names(boxplot_dr_all_list) <- taxa

col_crad <- '#2F6BA8' # blue
col_mus <- '#A72E37' # red

xmin_pic <- setNames(c(2.0, 2.0, 2.0, 2.0), taxa)
xmax_pic <- setNames(c(2.7, 2.7, 2.7, 2.7), taxa)
ymin_pic <- setNames(c(-1.5, 0, -0.5, -1), taxa)
ymax_pic <- setNames(c(-0.9, 1.2, 0.5, 0), taxa)

for (t in taxa){
  taxapic_fn <- paste0('../taxa_images/', t, '.png')
  img <- readPNG(taxapic_fn)
  g <- rasterGrob(img, interpolate = TRUE)
  
  boxplot_dr_all_list[[t]] <- ggplot(dr_cramus_all[[t]], 
                                     aes(x = type, y = log(dr))) +
    geom_violin(aes(fill = type), col = 'transparent') +
    scale_fill_manual(values = c(cradle = col_crad, museum = col_mus)) +
    stat_summary(geom = 'pointrange', fun = 'median', na.rm = TRUE, 
                 color = 'black', fill = 'white', pch = 21) +
    labs(x = '', y = 'log(DR)') +
    scale_x_discrete(labels=c('low resPD', 'high resPD')) +
    annotate(geom = 'text', x = 1.5, y = - 3.5, size = 3,
             label = paste0('p = ', summary_all_list[[t]]$table$`Pr(>F)`)) +
    annotate(geom = 'text', x = 1.5, y = - 4, size = 3,
             label = paste0('Z = ', 
                            round(summary_all_list[[t]]$table$`Z (from F)`, 3))) +
    annotation_custom(grob = g, 
                      xmin = xmin_pic[t], xmax = xmax_pic[t], 
                      ymin = ymin_pic[t], ymax = ymax_pic[t]) +
    theme_classic() +
    theme(legend.position = 'none', 
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 10), 
          plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 8))
  
}

wrap_plots(boxplot_dr_all_list)
ggsave(filename = 'plots/boxplot_all_3.pdf')
ggsave(filename = 'plots/boxplot_all_3.png', 
       plot = wrap_plots(boxplot_dr_all_list))

for (t in taxa){
  n_sp_cradles <- nrow(dr_cramus_all[[t]][dr_cramus_all[[t]]$type == 'cradle',])
  n_sp_museums <- nrow(dr_cramus_all[[t]][dr_cramus_all[[t]]$type == 'museum',])
  n_sp_total <- length(unique(dr_cramus_all[[t]]$sp))
  dup <- nrow(dr_cramus_all[[t]][duplicated(dr_cramus_all[[t]]$sp),])
  ntips <- Ntip(tree_list[[t]])
  perc <- n_sp_total/ntips * 100
  print(paste0(t, ' species in CRADLES: ', n_sp_cradles, ' / ', ntips))
  print(paste0(t, ' species in MUSEUMS: ', n_sp_museums, ' / ', ntips))
  print(paste0(t, ' species TOTAL: ', n_sp_total, ' / ', ntips, ' -- ', round(perc, 2), '%'))
  print(paste0(dup, ' ', t, ' species (', round(dup/n_sp_total*100, 2), '%) are BOTH in cradles and museums'))
}







