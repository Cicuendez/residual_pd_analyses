

# DR RATES ESTIMATION IN A SAMPLE OF POSTERIOR TREES ----
setwd('~/Dropbox/hector_working/richness_pd_dr')
sink(file='output/DR_sampling_amphibians.txt', append=F, split=TRUE)
starting_time <- Sys.time()
print("Starting! On date and time:")
print(starting_time)

# Packages & cores ----
libs <- c("doParallel", "phytools")
lapply(libs, require, character.only = TRUE)

number_of_cores <- detectCores()
print(paste0("cores available:", number_of_cores))
registerDoParallel(cores=number_of_cores)

# Load the posterior trees ----
file_all_post <- 'data/phylogeny/all_post_amphibians.rds'
all_post <- readRDS(file_all_post)
print("Object all_post_amphibians.rds imported")

##### Sample nsamp trees #####
set.seed(20221103)
nsamp <- 100
sampling100 <- sample(1:length(all_post), nsamp)
tree_samp100 <- all_post[sampling100]

# Save sample of 100 trees ----
saveRDS(tree_samp100, 'data/phylogeny/tree_post_100_AMPHIBIANS.rds', version = 2)
print('Sampling of 100 posterior trees saved')

##### Estimate DR rates in the sampled trees #####
# Source DR function
source("scripts/DRstat_function.R")
print("DR function sourced")

print("Estimating DR rates in sampled trees")
dr_list100 <- list()
for (i in 1:nsamp){
  dr_list100[[i]] <- DRstat(tree_samp100[[i]])
  print(paste(i, "/", nsamp, "--> tree", sampling100[i]))
}
names(dr_list100) <- sampling100

head(dr_list100)

saveRDS(dr_list100, "objects/dr_list100_AMPHIBIANS.rds")

##### Data frame with the DR rates for each species in each of the posterior trees #####


##### Calculate the mean DR rate for each species #####

finish_time <- Sys.time()
print("Finishing! On date and time:")
print(finish_time)
print("---------")
total_time <- finish_time - starting_time
print("Total time running:")
print(total_time)


