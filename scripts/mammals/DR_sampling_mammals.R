

# DR RATES ESTIMATION IN A SAMPLE OF MAMMAL TREES ----

sink(file="", append=F, split=F)
starting_time <- Sys.time()
print("Starting! On date and time:")
print(starting_time)
setwd("")

##### Load the packages #####
libs <- c("doParallel", "phytools")
lapply(libs, require, character.only = TRUE)

number_of_cores <- detectCores()
print(paste0("cores available: ", number_of_cores))
registerDoParallel(cores=number_of_cores)


# Select the folder with the posterior trees ----
#post_folder <- '../../PHYLOGENETIC_DATA/Mammals/Completed_5911sp_topoCons_NDexp'
#list.files(post_folder)
#prefix <- 'MamPhy_BDvr_Completed_5911sp_topoCons_NDexp_v2_tree'
#head(list.files(post_folder))
#
## Sample nsamp trees ----
nsamp <- 100
#set.seed(20221102)
#sampling100 <- sample(1:length(list.files(post_folder)), nsamp)
#
## Take the random sample of tree files
#tree_samp_files <- list.files(post_folder)[sampling100]
#
## Read the sample of trees into a list
#tree_post_100 <- vector('list', length = nsamp)
#
#for (i in 1:nsamp){
#  print(paste0('reading tree ', i, ' / 100'))
#  tree_post_100[[i]] <- read.tree(paste0(post_folder, 
#                                         '/', 
#                                         tree_samp_files[i]))
#}
#
## Make the list a multiPhylo object
#class(tree_post_100) <- 'multiPhylo'
#
## Save sample of 100 trees ----
#saveRDS(tree_post_100, '../objects/tree_post_100_MAMMALS.rds')
#print('Sampling of 100 posterior trees saved')

##### Estimate DR rates in the sampled trees #####
# Source DR function
source('scripts/DRstat_function.R')
print("DR function sourced")

tree_post_100 <- readRDS('objects/tree_post_100_MAMMALS.rds')

print("Estimating DR rates in sampled trees")
dr_list100 <- list()
i <- 1
for (i in 1:nsamp){
  dr_list100[[i]] <- DRstat(tree_post_100[[i]])
  print(paste(i, "/", nsamp))
}

saveRDS(dr_list100, "objects/dr_list100_MAMMALS.rds")

# Data frame with the DR rates for each species in each of the posterior trees ----


# Calculate the mean DR rate for each species ----
# Go to script DR_average_sampling.R 

finish_time <- Sys.time()
print("Finishing! On date and time:")
print(finish_time)
print("---------")
total_time <- finish_time - starting_time
print("Total time running:")
print(total_time)


