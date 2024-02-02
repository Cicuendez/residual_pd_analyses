


# Purpose ----
# Get all the posterior trees in one single file

# Packages ----
libs <- c("doParallel", "phytools")
lapply(libs, require, character.only = TRUE)

# Select the folder with the posterior trees ----
post_folder <- '../../PHYLOGENETIC_DATA/Amphibians/amph_shl_new_Posterior_7238.1000-10000.trees/'
setwd(post_folder)
# The first file is the consensus tree, so we exclude it
post_files <- list.files(post_folder)[-1]
# There are 10 posterior files, each with 1000 trees. 
# We want to stack them together in one file. 

# Read posterior files ----
post1 <- read.tree(post_files[1])
post_list <- lapply(post_files, read.tree)

# Make a single object with all the posterior ----
all_post <- list()
n <- 1
for (i in 1:length(post_list)){
  for (t in 1:length(post_list[[i]])){
    all_post[[n]] <- post_list[[i]][[t]]
    n <- n + 1
  }
}
class(all_post) <- "multiPhylo"

# Save object ----
saveRDS(all_post, "../all_post_amphibians.rds")











