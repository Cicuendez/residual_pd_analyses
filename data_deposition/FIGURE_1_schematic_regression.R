

library('tidyverse')
library('sf')
library('terra')
setwd('/Users/cicu/Google Drive/CIENCIA/Hector_Jiri_PROJECTS/richness-pd-rates/richness_pd_dr/')

# DATA: hexgrids ----
hexgrid_list <- readRDS('../objects/hexgrid_list_v2.rds')
colnames(hexgrid_list[['birds']])
proj_hexgrid <- crs(hexgrid_list[['birds']])

grid_df <- hexgrid_list[['birds']]
grid_df$id

# Get world data for plotting ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_projected <- st_transform(world, crs = proj_hexgrid)

# Set seed for reproducibility
set.seed(123)

# Number of data points to generate
n_points <- length(grid_df$id)
n_points <- 1000

# Simulate correlated data
correlation <- 0.7  # Set your desired correlation value
data <- MASS::mvrnorm(n_points, mu = c(0, 0), 
                      Sigma = matrix(c(1, correlation, correlation, 1), 
                                     ncol = 2))

v1 <- rnorm(n = n_points, mean = 20, sd = 5)
v2 <- rnorm(n = n_points, mean = 1, sd = 20)
plot(v2~v1, pch = 16, col = 'gray70')
df <- data.frame(
  id = grid_df$id,
  Variable1 = v1,
  Variable2 = v2
)

# Create a data frame
df <- data.frame(
#  id = grid_df$id,
  Variable1 = data[, 1],
  Variable2 = data[, 2]
)
#head(df)

df <- df %>% 
  mutate(res_values = resid(lm(Variable2 ~ Variable1, data = df)))

map_data <- merge(grid_df, df, by = "id", all.x = TRUE)
map_data <- map_data %>% 
  mutate(res_values = resid(lm(Variable2 ~ Variable1, data = map_data)))
plot(Variable2~Variable1, data = map_data, pch = 16, col = 'gray70')

# scatterplot
ggplot(data = df, aes(x = Variable1, y = Variable2, 
                            color = res_values)) + 
  geom_point(size = 2) +
  scale_color_gradient2(low = '#2F6BA8',
                        mid = 'gray90',
                        high = '#A72E37',
                        midpoint = 0, 
                        name = 'residual PD') +
  geom_smooth(method = "lm", se = FALSE, color = 'gray50', 
              linetype = "dashed", linewidth = 0.5) +
  labs(x = 'richness', y = 'PD') +
  theme_classic() +
  theme(legend.position = 'bottom', 
        legend.key.height = unit(0.2,"cm"), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.text = element_blank(), 
        legend.title = element_text(angle = 0, vjust = 2))
ggsave('plots/figure_schematic_regression.pdf')
ggsave('plots/figure_schematic_regression.png')

# Plot residuals
ggplot() +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = map_data, 
          aes(fill = res_values), 
          col = 'transparent') +
  scale_fill_gradient2(low = '#2F6BA8',
                       mid = 'gray95',
                       high = '#A72E37',
                       midpoint = 0, 
                       name = 'loess resid') +
  labs(title = 'residuals V2 ~ V1') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.height = unit(0.2,"cm"),
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'))

# plot V1
ggplot() +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = map_data, aes(fill = Variable1), col = 'transparent') +
  scale_fill_viridis_c(option = 'viridis', name = 'Variable1') +
  labs(title = 'Variable1') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.height = unit(0.2,"cm"),
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'))

# Plot V2
ggplot() +
  geom_sf(data = world_projected, col = 'transparent', fill = 'gray80') +
  geom_sf(data = map_data, aes(fill = Variable2), col = 'transparent') +
  scale_fill_viridis_c(option = 'viridis', name = 'Variable1') +
  labs(title = 'Variable2') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.height = unit(0.2,"cm"),
        plot.title = element_text(hjust = 0.5, 
                                  face = 'bold'))



hist(map_data$Variable1)
hist(map_data$Variable2)
hist(res_values)

