
##### DR SAMPLING AVERAGE #####

# Import object dr_list100 ----
# a list with vectors of the DR rates for all sampled trees
dr_list100 <- readRDS('../objects/dr_list100_AMPHIBIANS.rds')

# df list ----
# Create a list of data frames (instead of vectors)
dr_df_list <- list()
for (i in 1:length(dr_list100)){
  dr_df_list[[i]] <- as.data.frame(dr_list100[[i]])
}
names(dr_df_list) <- names(dr_list100)

# single df ----
# Create a single data frame with as many columns as trees sampled, 
# and as many rows as species.
species <- rownames(dr_df_list[[1]])
species <- species[order(species)] # alphabetic order

dr_df <- data.frame(matrix(nrow=length(species), ncol=length(dr_df_list)))
rownames(dr_df) <- species

for(i in 1:ncol(dr_df)){
  dr_df[,i] <- dr_df_list[[i]][species,]
}
colnames(dr_df) <- names(dr_df_list)

# Get the average DR rate for each species
dr_mean <- as.data.frame(rowMeans(dr_df))
head(dr_mean)
colnames(dr_mean)[1] <- "dr"

# Save both dataframes (all DR values and mean DR)
saveRDS(dr_df, "../objects/dr_df_AMPHIBIANS.rds")
saveRDS(dr_mean, "../objects/dr_mean_AMPHIBIANS.rds")

