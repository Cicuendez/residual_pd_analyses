
# Packages ----
library(geiger)
library(phytools)

# Links to the zip files ----
prefix <- 'https://data.vertlife.org/birdtree/Stage2/HackettStage2_'
sufix <- '.zip'

post_zip_files <- c()
post_zip_files[1] <- paste0(prefix, '0001_1000', sufix)
post_zip_files[2] <- paste0(prefix, '1001_2000', sufix)
post_zip_files[3] <- paste0(prefix, '2001_3000', sufix)
post_zip_files[4] <- paste0(prefix, '3001_4000', sufix)
post_zip_files[5] <- paste0(prefix, '4001_5000', sufix)
post_zip_files[6] <- paste0(prefix, '5001_6000', sufix)
post_zip_files[7] <- paste0(prefix, '6001_7000', sufix)
post_zip_files[8] <- paste0(prefix, '7001_8000', sufix)
post_zip_files[9] <- paste0(prefix, '8001_9000', sufix)
post_zip_files[10] <- paste0(prefix, '9001_10000', sufix)

# Download zip files ----
for (i in 01:length(post_zip_files)){
    tmp_fn <- paste0('tmp_files_post/post_', i, '.zip')
    download.file(post_zip_files[i], destfile = tmp_fn)
}

# Unzip files ----
for (i in 1:length(list.files('tmp_files_post'))){
  fn <- list.files('tmp_files_post', full.names = TRUE)[i]
  unzip(fn)
}

# Import posterior trees ----
post_files <- list.files('post_files', full.names = TRUE)
post_list <- lapply(post_files, read.tree)

# All in one file ----
all_post <- list()
n <- 1
for (i in 1:length(post_list)){
  for (t in 1:length(post_list[[i]])){
    all_post[[n]] <- post_list[[i]][[t]]
    n <- n + 1
  }
}
class(all_post) <- "multiPhylo"
saveRDS(all_post, "data/all_post_birds.rds")




