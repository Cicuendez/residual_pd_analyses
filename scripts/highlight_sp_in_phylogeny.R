

# packages ----
packages <- c('tidyverse', 'sf', 'raster', 'epm', 'rgeos', 'rgdal', 'RColorBrewer', 
              'scico', 'ggthemes', 'viridis', 'terra', 'treeio', 'phytools',
              'geiger', 'picante', 'patchwork', 'ggtree', 'treeio')

easypackages::libraries(packages)

# taxa ----
taxa <- c('amphibians', 'birds', 'mammals', 'squamates')

# regions
regions <- c('africa', 'africa_madagascar', 'south_africa', 
             'north_america', 'south_america', 
             'australia', 'australia_oceania', 'australia_oceania_asia', 
             'southeast_asia', 'oriental')

# PHYLOGENIES cradles and museums ----
sp_cramus <- readRDS('../objects/sp_cramus.rds')

# import trees ----
phylogeny_dir <- '../data/phylogeny'
list.files(phylogeny_dir, full.names = TRUE)[grep('tree_post_100', 
                                                  list.files(phylogeny_dir))]

post_list <- vector('list', length(taxa))
names(post_list) <- taxa

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
tree_list[['squamates']]
tree_list[['amphibians']]
tree_list[['birds']]
plot.phylo(tree_list[['squamates']], type = 'fan', show.tip.label = FALSE, 
           edge.width = 0.1)

ggtree(tree_list[['squamates']], size = 0.1, layout = 'fan', open.angle = 10)

# PLOT TREES ----
tree_plots <- vector('list', length(taxa))
names(tree_plots) <- taxa

for (t in taxa){
  tree_plots[[t]] <- vector('list', length = 2)
  names(tree_plots[[t]]) <- c('cradle', 'museum')
}

# __ amphibians ----
# ____ cradles ----
tree_plots[['amphibians']][['cradle']] <- vector('list', length = 3)
names(tree_plots[['amphibians']][['cradle']]) <- c('south_america', 
                                                  'oriental', 
                                                  'australia_oceania')
# ____ museums ----
tree_plots[['amphibians']][['museum']] <- vector('list', length = 1)
names(tree_plots[['amphibians']][['museum']]) <- 'africa'

# __ birds ----
# ____ cradles ----
tree_plots[['birds']][['cradle']] <- vector('list', length = 2)
names(tree_plots[['birds']][['cradle']]) <- c('north_america', 
                                             'south_america')

# ____ museums ----
tree_plots[['birds']][['museum']] <- vector('list', length = 2)
names(tree_plots[['birds']][['museum']]) <- c('africa_madagascar', 
                                             'australia_oceania_asia')

# __ mammals ----
# ____ cradles ----
tree_plots[['mammals']][['cradle']] <- vector('list', length = 2)
names(tree_plots[['mammals']][['cradle']]) <- c('north_america', 
                                               'south_america')

# ____ museums ----
tree_plots[['mammals']][['museum']] <- vector('list', length = 2)
names(tree_plots[['mammals']][['museum']]) <- c('north_america', 
                                               'africa')

# __ squamates ----
# ____ cradles ----
tree_plots[['squamates']][['cradle']] <- vector('list', length = 3)
names(tree_plots[['squamates']][['cradle']]) <- c('north_america', 
                                                 'south_america', 
                                                 'australia')

# ____ museums ----
tree_plots[['squamates']][['museum']] <- vector('list', length = 2)
names(tree_plots[['squamates']][['museum']]) <- c('south_africa', 
                                                 'southeast_asia')



col_crad <- '#2F6BA8' # blue
col_mus <- '#A72E37' # red

cramus_col <- c(cradle = '#2F6BA8', museum = '#A72E37')

for (t in taxa){
  print(toupper(t))
  for (xx in c('cradle', 'museum')){
    for (r in regions){
      if (r %in% names(sp_cramus[[t]][[xx]])){
        print(paste0(t, ' -- ', xx, ' -- ', r))
        nsp <- length(sp_cramus[[t]][[xx]][[r]]) # number of species
        grp <- list(g = sp_cramus[[t]][[xx]][[r]])
        tree <- groupOTU(tree_list[[t]], grp)
        tree_plots[[t]][[xx]][[r]] <- ggtree(tree, layout = "fan", 
                                             open.angle = 3, 
                                             aes(color = group, size = group, 
                                                 alpha = group)) +
          scale_color_manual(values = c('gray80', cramus_col[[xx]])) +
          scale_size_manual(values = c(0.1, 0.2)) +
          scale_alpha_manual(values = c(0.5, 1)) +
          labs(title = paste0(toupper(t), ' (n = ', nsp, ')'), 
               subtitle = paste(xx, r)) +
          theme(legend.position = 'none', 
                plot.title = element_text(face = 'bold', hjust = 0.5, size = 9), 
                plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 9))
      }
    }
  }
}

sp_cramus[[t]][['cradle']][['south_america']]

tree_plots[['squamates']][['museum']][['south_africa']]
tree_plots[['squamates']][['cradle']][['australia']]

tree_plots[['birds']][['cradle']][['south_america']]


# save the tree plots ----
for (t in taxa){
  print(t)
  i <- 1
  np <- sum(lengths(tree_plots[[t]]))
  p <- vector('list', length = np)
  for (xx in 1:length(tree_plots[[t]])){
    for (r in 1:length(tree_plots[[t]][[xx]])){
      p[[i]] <- tree_plots[[t]][[xx]][[r]]
      i <- i + 1
    }
  }
  wrap_plots(p)
  fn <- paste0('plots/cramus_', t, '.pdf')
  ggsave(fn, plot = last_plot())
}



# DR HISTOGRAMS ----
# import dr objects
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


dr_cramus <- vector('list', length(taxa))
names(dr_cramus) <- taxa

for (t in taxa){
  dr_cramus[[t]] <- vector('list', length = 2)
  names(dr_cramus[[t]]) <- c('cradle', 'museum')
}

# __ amphibians ----
# ____ cradles ----
dr_cramus[['amphibians']][['cradle']] <- vector('list', length = 3)
names(dr_cramus[['amphibians']][['cradle']]) <- c('south_america', 
                                                   'oriental', 
                                                   'australia_oceania')
# ____ museums ----
dr_cramus[['amphibians']][['museum']] <- vector('list', length = 1)
names(dr_cramus[['amphibians']][['museum']]) <- 'africa'

# __ birds ----
# ____ cradles ----
dr_cramus[['birds']][['cradle']] <- vector('list', length = 2)
names(dr_cramus[['birds']][['cradle']]) <- c('north_america', 
                                              'south_america')

# ____ museums ----
dr_cramus[['birds']][['museum']] <- vector('list', length = 2)
names(dr_cramus[['birds']][['museum']]) <- c('africa_madagascar', 
                                              'australia_oceania_asia')

# __ mammals ----
# ____ cradles ----
dr_cramus[['mammals']][['cradle']] <- vector('list', length = 2)
names(dr_cramus[['mammals']][['cradle']]) <- c('north_america', 
                                                'south_america')

# ____ museums ----
dr_cramus[['mammals']][['museum']] <- vector('list', length = 2)
names(dr_cramus[['mammals']][['museum']]) <- c('north_america', 
                                                'africa')

# __ squamates ----
# ____ cradles ----
dr_cramus[['squamates']][['cradle']] <- vector('list', length = 3)
names(dr_cramus[['squamates']][['cradle']]) <- c('north_america', 
                                                  'south_america', 
                                                  'australia')

# ____ museums ----
dr_cramus[['squamates']][['museum']] <- vector('list', length = 2)
names(dr_cramus[['squamates']][['museum']]) <- c('south_africa', 
                                                  'southeast_asia')

lim_dr <- list(amphibians = c(-5, 0), 
                birds = c(-5, 1), 
                mammals = c(-5, 1), 
                squamates = c(-5, 0))


for (t in taxa){
  print(toupper(t))
  for (xx in c('cradle', 'museum')){
    for (r in regions){
      if (r %in% names(sp_cramus[[t]][[xx]])){
        species <- sp_cramus[[t]][[xx]][[r]]
        dr_values <- dr_list[[t]][species]
        dr_cramus[[t]][[xx]][[r]] <- ggplot(data = as.data.frame(dr_values)) + 
          geom_density(aes(x = log(dr_values)), fill = cramus_col[[xx]], 
                       color = 'transparent') +
          lims(x = lim_dr[[t]]) +
          labs(title = paste0(toupper(t), ' DR rate'), 
               subtitle = paste(xx, r), 
               x = 'DR', y = 'n') +
          theme_classic() +
          theme(legend.position = 'none', 
                plot.title = element_text(face = 'bold', hjust = 0.5, size = 10), 
                plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 8))

      }
    }
  }
}


# save the histograms ----
for (t in taxa){
  print(t)
  i <- 1
  np <- sum(lengths(dr_cramus[[t]]))
  p <- vector('list', length = np)
  for (xx in 1:length(dr_cramus[[t]])){
    for (r in 1:length(dr_cramus[[t]][[xx]])){
      p[[i]] <- dr_cramus[[t]][[xx]][[r]]
      i <- i + 1
    }
  }
  wrap_plots(p)
  fn <- paste0('plots/histogram_', t, '.png')
  ggsave(fn, plot = last_plot())
}


# save the density plots ----
for (t in taxa){
  print(t)
  i <- 1
  np <- sum(lengths(dr_cramus[[t]]))
  p <- vector('list', length = np)
  for (xx in 1:length(dr_cramus[[t]])){
    for (r in 1:length(dr_cramus[[t]][[xx]])){
      p[[i]] <- dr_cramus[[t]][[xx]][[r]]
      i <- i + 1
    }
  }
  wrap_plots(p)
  fn <- paste0('plots/density_', t, '.pdf')
  ggsave(fn, plot = last_plot())
}

# save squamate density plot wider
wrap_plots(p)
ggsave(fn, plot = last_plot(), width = 7.76/2*3, height = 5.92)

# COMPARE DR VALUES CRADLES VS MUSEUMS ----

# prepare data for anova ----
# extract the data from the density plots:
# for each taxa, the data will be a dataframe with the DR values in 
# one column, and whether they are from a cradle or from a museum in 
# another column. 
# With this we will perform ANOVA
dr_cramus_data <- vector('list', length(taxa))
names(dr_cramus_data) <- taxa

for (t in taxa){
  
  # get the cradles and museums of this taxa group
  reg_cra <- names(dr_cramus[[t]][['cradle']])
  reg_mus <- names(dr_cramus[[t]][['museum']])
  
  # get the DR data from the cradles and stack them in a single dataframe
  data_cra <- data.frame()
  
  for (rcra in reg_cra){
    
    # create a column with the region and sp names
    dcr <- dr_cramus[[t]][['cradle']][[rcra]][['data']]
    dcr$region <- rcra
    dcr$sp <- rownames(dcr)
    data_cra <- rbind(data_cra, dcr)
  }
  data_cra$cramus <- 'cradle' # create the column cramus to specify 'cradle'
  
  # get the DR data from the cradles and stack them in a single dataframe
  data_mus <- data.frame()
  
  for (rmus in reg_mus){
    
    # create a column with the region and sp names
    dmr <- dr_cramus[[t]][['museum']][[rmus]][['data']]
    dmr$region <- rmus
    dmr$sp <- rownames(dmr)
    data_mus <- rbind(data_mus, dmr)
  }
  
  data_mus$cramus <- 'museum' # create the column cramus to specify 'museum'
  
  # bind the dataframes of cradles and museums
  dr_cramus_data[[t]] <- rbind(data_cra, data_mus)
}

# create a column combining cramus and region. 
# This allows us to compare a cradle and a museum in the same region (as is 
# the case in North American mammals)
for (t in taxa) {
  dr_cramus_data[[t]]$region_cramus <- paste(dr_cramus_data[[t]]$cramus, 
                                             dr_cramus_data[[t]]$region,
                                             sep = '_')
  
}

saveRDS(dr_cramus_data, '../objects/dr_cramus_data.rds')


# DR BOXPLOT / VIOLIN PLOT ----
boxplot_dr_list <- vector('list', length(taxa))
names(boxplot_dr_list) <- taxa

for (t in taxa){
  boxplot_dr_list[[t]] <- ggplot(dr_cramus_data[[t]], 
                                 aes(x = region, y = log(dr_values))) +
    geom_violin(aes(fill = cramus), col = 'transparent') +
    scale_fill_manual(values = c(cradle = col_crad, museum = col_mus)) +
    labs(title = t, x = '', y = 'log(DR)') +
    theme_classic() +
    theme(legend.position = 'none', 
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 10), 
          plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 8), 
          axis.text.x = element_text(hjust = 1, angle = 45))
  
}

wrap_plots(boxplot_dr_list)
ggsave(filename = 'plots/boxplot_cradles_museums_v2.pdf')
ggsave(filename = 'plots/boxplot_cradles_museums_v2.png')

# mean and standard deviation ----
stats_data <- vector('list', length(taxa))
names(stats_data) <- taxa
for (t in taxa){
  stats_data[[t]] <- aggregate(log(dr_values) ~ region,
            data = dr_cramus_data[[t]],
            function(x) round(c(mean = mean(x), sd = sd(x), median = median(x)), 
                              2)
  ) 
}
stats_data

stats_data2 <- vector('list', length(taxa))
names(stats_data2) <- taxa
for (t in taxa){
  stats_data2[[t]] <- group_by(dr_cramus_data[[t]], region) %>%
    summarise(
      mean = mean(log(dr_values), na.rm = TRUE),
      sd = sd(log(dr_values), na.rm = TRUE), 
      median = median(log(dr_values), na.rm = TRUE)
    )
}



# anova ----
aov_list <- vector('list', length(taxa))
names(aov_list) <- taxa

for (t in taxa){
  print(t)
  aov_list[[t]] <- aov(dr_values ~ region_cramus, data = dr_cramus_data[[t]])
  print(summary(aov_list[[t]]))
  print('#########################')
}

# post-hoc tests ----
# We want to test if the DR rates are generally lower in the species present 
# in museums than in those present in cradles. Therefore, we set the 
# 'alternative' parameter to 'less'.

dir.create('results/')
file.create('results/posthoc_dr.txt')

sink(file = 'results/posthoc_dr.txt', append = TRUE, split = TRUE)
for (t in taxa){
  print(toupper(t))
  posthoc_test <- pairwise.t.test(x = dr_cramus_data[[t]]$dr_values, 
                                  g = dr_cramus_data[[t]]$region_cramus, 
                                  p.adjust.method = 'holm', 
                                  alternative = 'less', 
                                  paired = FALSE, 
                                  pool.sd = FALSE)
  print(posthoc_test)
  print('##################################')
}
sink()

# ANOVA with RRPP ----

library(RRPP)
aovrrpp_list <- vector('list', length(taxa))
names(aovrrpp_list) <- taxa


for (t in taxa){
  print(t)
  aovrrpp_list[[t]] <- RRPP::lm.rrpp(f1 = dr_values ~ region_cramus, 
                                     data = dr_cramus_data[[t]],
                                     iter = 999, 
                                     RRPP = TRUE,
                                     print.progress = TRUE)
  print('##################################')
}

summary_list <- vector('list', length(taxa))
names(summary_list) <- taxa

for (t in taxa){
  summary_list[[t]] <- summary(aovrrpp_list[[t]])
  print(paste0('p-value ', t, ' = ', summary_list[[t]]$table$`Pr(>F)`))
  print(paste0('Z ', t, ' = ', summary_list[[t]]$table$`Z (from F)`))
}


# post-hoc with RRPP ----
?posthoc.rrpp

posthocrrpp_list <- vector('list', length(taxa))
names(posthocrrpp_list) <- taxa

for (t in taxa){
  print(t)
  posthocrrpp_list[[t]] <- RRPP::pairwise(aovrrpp_list[[t]], 
                                          groups = dr_cramus_data[[t]]$region_cramus)
}


summary_posthoc_list <- vector('list', length(taxa))
names(summary_posthoc_list) <- taxa

for (t in taxa){
  summary_posthoc_list[[t]] <- summary(posthocrrpp_list[[t]], type = 'dist', 
                                       stat.table = TRUE)
}

colnames(summary_posthoc_list[[t]]$summary.table)
posthoc_table <- data.frame(d = NULL, "UCL (95%)" = NULL, Z = NULL, 
                            "Pr > d" = NULL, taxa = NULL)

for (t in taxa){
  summary_posthoc_list[[t]]$summary.table$taxa <- t
  posthoc_table <- rbind(posthoc_table, 
                         summary_posthoc_list[[t]]$summary.table)
}

write.table(posthoc_table, 'manuscript/drafts/Tables/posthoc_DR.csv', 
            sep = ';', dec = '.', quote = FALSE, col.names = NA)


# effect size (Z) ----


unlist(strsplit(rownames(summary_posthoc_list[[t]]$summary.table), ":"))

substr(rn, 1, 6)
substr(rn, regexpr(':', rn)+1, regexpr(':', rn)+6)

z_data <- data.frame()
for (t in taxa){
  rn <- rownames(summary_posthoc_list[[t]]$summary.table)
  z_df <- data.frame(case = paste0(substr(rn, 1, 6), 
                                     '_', 
                                     substr(rn, regexpr(':', rn)+1, regexpr(':', rn)+6)), 
                       Z = summary_posthoc_list[[t]]$summary.table$Z, 
                       taxa = t)
  z_data <- rbind(z_data, z_df)
  
}

taxa_colors <- c(mammals = '#003f5c', squamates = '#7a5195', 
                 amphibians = '#ef5675', birds = '#ffa600')
taxa_colors <- setNames(c('#ef5675', '#ffa600', '#003f5c', '#7a5195'), 
                        nm = taxa)

plot_zeffect <- ggplot(z_data, aes(x = case, y = Z)) +
  geom_point(aes(fill = taxa), color = 'black', 
             size = 8, alpha = 0.9, pch = 21) +
  scale_fill_manual(values = taxa_colors) +
  scale_x_discrete(labels = c('low_low', 'low_high', 'high_high')) +
  labs() +
  theme_classic() +
  theme()

summary(lm.rrpp(Z~case, data = z_data))

ggsave('plots/speciation_cramus_effect.pdf', plot = plot_zeffect)



summary(summary_posthoc_list)
summary_posthoc_list
heatmap(summary_posthoc_list$pairwise.tables$Z, symm = TRUE)



# anova all ----
# Look at the script 'dr_ALL_cradles_vs_museums.R' for the final analysis. 
# Compare DR in species in cradles and museums (all together)
# Remove duplicate species (but keep two rows for the species that are both 
# in cradles and museums)
anova_all_data <- aov_all_list <- vector('list', length(taxa))
names(anova_all_data) <- names(aov_all_list) <- taxa

for (t in taxa){
  anova_all_data[[t]] <- dr_cramus_data[[t]] %>% 
    dplyr::select(-c(region_cramus, region)) %>% 
    distinct()
}

anova_all_data$amphibians$cramus

for (t in taxa){
  print(t)
  aov_all_list[[t]] <- aov(dr_values ~ cramus, data = anova_all_data[[t]])
  print(summary(aov_all_list[[t]]))
  print('#########################')
}


# ANOVA all with RRPP ----
# Look at the script 'dr_ALL_cradles_vs_museums.R' for the final analysis. 
library(RRPP)
aovrrpp_all_list <- vector('list', length(taxa))
names(aovrrpp_all_list) <- taxa

summary_all_list <- vector('list', length(taxa))
names(summary_all_list) <- taxa

for (t in taxa){
  print(t)
  aovrrpp_all_list[[t]] <- RRPP::lm.rrpp(f1 = dr_values ~ cramus, 
                                     data = anova_all_data[[t]],
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
# Look at the script 'dr_ALL_cradles_vs_museums.R' for the final analysis. 
boxplot_dr_all_list <- vector('list', length(taxa))
names(boxplot_dr_all_list) <- taxa

for (t in taxa){
  boxplot_dr_all_list[[t]] <- ggplot(anova_all_data[[t]], 
                                     aes(x = cramus, y = log(dr_values))) +
    geom_violin(aes(fill = cramus), col = 'transparent') +
    scale_fill_manual(values = c(cradle = col_crad, museum = col_mus)) +
    stat_summary(geom = 'pointrange', fun = 'median', na.rm = TRUE, 
                 color = 'black', fill = 'white', pch = 21) +
    labs(title = t, x = '', y = 'log(DR)') +
    scale_x_discrete(labels=c('low resPD', 'high resPD')) +
    annotate(geom = 'text', x = 1.5, y = - 3.5, size = 3,
             label = paste0('p = ', summary_all_list[[t]]$table$`Pr(>F)`)) +
    annotate(geom = 'text', x = 1.5, y = - 4, size = 3,
             label = paste0('Z = ', 
                            round(summary_all_list[[t]]$table$`Z (from F)`, 3))) +
    theme_classic() +
    theme(legend.position = 'none', 
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 10), 
          plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 8))
  
}

wrap_plots(boxplot_dr_all_list)
ggsave(filename = 'plots/boxplot_all.pdf')

sp_cramus

for (t in taxa){
  n_sp_cradles <- nrow(anova_all_data[[t]][anova_all_data[[t]]$cramus == 'cradle',])
  n_sp_museums <- nrow(anova_all_data[[t]][anova_all_data[[t]]$cramus == 'museum',])
  n_sp_total <- length(unique(anova_all_data[[t]]$sp))
  dup <- nrow(anova_all_data[[t]][duplicated(anova_all_data[[t]]$sp),])
  ntips <- Ntip(tree_list[[t]])
  perc <- n_sp_total/ntips * 100
  print(paste0(t, ' species in CRADLES: ', n_sp_cradles, ' / ', ntips))
  print(paste0(t, ' species in MUSEUMS: ', n_sp_museums, ' / ', ntips))
  print(paste0(t, ' species TOTAL: ', n_sp_total, ' / ', ntips, ' -- ', round(perc, 2)))
  print(paste0(dup, ' ', t, ' species are BOTH in cradles and museums'))
}
nrow(anova_all_data[[t]][anova_all_data[[t]]$cramus == 'cradle',])
nrow(anova_all_data[[t]][anova_all_data[[t]]$cramus == 'museum',])
length(unique(anova_all_data[[t]]$sp))




sp_cramus[['mammals']]$cradle$north_america
sp_cramus[['mammals']]$museum$north_america

sp_cramus[['mammals']]$cradle$north_america[!sp_cramus[['mammals']]$cradle$north_america %in% sp_cramus[['mammals']]$museum$north_america]




# BRANCH LENGTHS ----
# Get species branch lengths ----
for (t in taxa){
  df_blength <- data.frame(sp = tree_list[[t]]$tip.label, 
                           blength = tree_list[[t]]$edge.length[which(tree_list[[t]]$edge[, 2] %in% 
                                                                        1:Ntip(tree_list[[t]]))])
  anova_all_data[[t]] <- anova_all_data[[t]] %>% 
    left_join(df_blength)
  
  dr_cramus_data[[t]] <- dr_cramus_data[[t]] %>% 
    left_join(df_blength)
}

# Anova RRPP branch length cradles vs museums ----
aovrrpp_blength_list <- vector('list', length(taxa))
names(aovrrpp_blength_list) <- taxa

summary_blength_list <- vector('list', length(taxa))
names(summary_blength_list) <- taxa

for (t in taxa){
  print(t)
  aovrrpp_blength_list[[t]] <- RRPP::lm.rrpp(f1 = blength ~ cramus, 
                                             data = anova_all_data[[t]],
                                             iter = 999, 
                                             RRPP = TRUE,
                                             print.progress = TRUE)
  summary_blength_list[[t]] <- summary(aovrrpp_blength_list[[t]])
  print(paste0('p-value ', t, ' = ', summary_blength_list[[t]]$table$`Pr(>F)`))
  print(paste0('Z ', t, ' = ', summary_blength_list[[t]]$table$`Z (from F)`))
  print('##################################')
}



for (t in taxa){
  summary_blength_list[[t]] <- summary(aovrrpp_blength_list[[t]])
  print(paste0('p-value ', t, ' = ', summary_blength_list[[t]]$table$`Pr(>F)`))
  print(paste0('Z ', t, ' = ', summary_blength_list[[t]]$table$`Z (from F)`))
}

# plot branch length cradles vs museums ----
blength_plots <- vector('list', length(taxa))
names(blength_plots) <- taxa

for (t in taxa){
  blength_plots[[t]] <- ggplot(data = anova_all_data[[t]], aes(x = cramus, y = log(blength))) +
    geom_violin(aes(fill = cramus)) +
    scale_fill_manual(values = c(cradle = col_crad, museum = col_mus)) +
    stat_summary(geom = 'pointrange', fun = 'median', na.rm = TRUE, 
                 color = 'black', fill = 'white', pch = 21) +
    labs(title = t, x = '', y = 'log(branch length)') +
    theme_classic() +
    theme(legend.position = 'none', 
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 10), 
          plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 8))
}
wrap_plots(blength_plots)
ggsave('plots/branchlengths_all.pdf')

for (t in taxa){
  print(anova_all_data[[t]] %>% 
          summarize(.by = cramus, 
                    median_blength = median(blength), 
                    mean_blength = mean(blength)))
}


# Individual cradles and museums ----

blength_ind_plots <- vector('list', length(taxa))
names(blength_ind_plots) <- taxa

for (t in taxa){
  blength_ind_plots[[t]] <- ggplot(dr_cramus_data[[t]], 
                                   aes(x = region_cramus, y = log(blength))) +
    geom_violin(aes(fill = cramus), col = 'black') +
    scale_fill_manual(values = c(cradle = col_crad, museum = col_mus)) +
    stat_summary(geom = 'pointrange', fun = 'median', na.rm = TRUE, 
                 color = 'black', fill = 'white', pch = 21) +
    labs(title = t, x = '', y = 'blength') +
    theme_classic() +
    theme(legend.position = 'none', 
          plot.title = element_text(face = 'bold', hjust = 0.5, size = 10), 
          plot.subtitle = element_text(face = 'plain', hjust = 0.5, size = 8), 
          axis.text.x = element_text(angle = 45, hjust = 1))
  
}
wrap_plots(blength_ind_plots)
ggsave('plots/branchlengths_ind.pdf')

