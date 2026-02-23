

#install.packages('pastclim')
#library(pastclim)

# packages ----
library(terra)

# import paleoclimate (5 Ma until 1950; preindustrial) ----
paleotemp <- rast('../data/paleo_pgem/PALEO-PGEM-Series_bio1_mean.nc')
paleoprec <- rast('../data/paleo_pgem/PALEO-PGEM-Series_bio12_mean.nc')

# show progress
terraOptions(progress = 1)

# time in years
#time(paleotemp)
#time(paleoprec)
#identical(time(paleoprec), time(paleotemp))
#time(paleotemp[[1]])
#
#nlyr(paleotemp)
#nlyr(paleotemp[[c(1,4)]])

# reduce dataset ----
# select layers
idx <- seq(1, nlyr(paleotemp), by = 10)
paleotemp_selected <- paleotemp[[idx]]
paleoprec_selected <- paleoprec[[idx]]
#time(paleotemp_selected)

# WAYS OF CAPTURING CLIMATE VARIABILITY ----

## 1. standard deviation ----
# SD of temperature in the last 5 Ma
if ('temp_sd.rds' %in% list.files('output/')){
  temp_sd <- readRDS('output/temp_sd.rds')
  prec_sd <- readRDS('output/prec_sd.rds')
}

if (!'temp_sd.rds' %in% list.files('output/')){
  temp_sd <- app(paleotemp_selected, fun = sd, na.rm = TRUE)
  plot(log(temp_sd))
  
  prec_sd <- app(paleoprec_selected, fun = sd, na.rm = TRUE)
  plot(log(prec_sd))
  plot(prec_sd)
  
  saveRDS(temp_sd, 'output/temp_sd.rds')
  saveRDS(prec_sd, 'output/prec_sd.rds')
}

hist(temp_sd)
hist(log(temp_sd))
hist(prec_sd)
hist(log(prec_sd))

## 2. net change ----
temp_netchange <- paleotemp[[nlyr(paleotemp)]] - paleotemp[[1]]
prec_netchange <- paleoprec[[nlyr(paleoprec)]] - paleoprec[[1]]
plot(paleotemp[[1]])
plot(paleotemp[[nlyr(paleotemp)]])
plot(paleotemp[[round(nlyr(paleotemp)/2)]])

plot(temp_netchange)
plot(prec_netchange)


## 3. cumulative change ----
# Differences between consecutive layers

if ('temp_cumchange.rds' %in% list.files('output/')){
  temp_cumchange <- readRDS('output/temp_cumchange.rds')
  prec_cumchange <- readRDS('output/prec_cumchange.rds')
}

if (!'temp_cumchange.rds' %in% list.files('output/')){
  system.time(d_temp <- diff(paleotemp_selected))   # computes layer_t - layer_(t-1)
  # diff() reduces the number of layers from 5001 → 5000.
  d_temp
  
  # Cumulative change (sum of absolute changes per cell)
  system.time(temp_cumchange <- app(abs(d_temp), sum, na.rm = TRUE))
  plot(temp_cumchange)
  
  system.time(d_prec <- diff(paleoprec_selected))
  prec_cumchange <- app(abs(d_prec), sum, na.rm = TRUE)
  plot(prec_cumchange)
  
  saveRDS(temp_cumchange, 'output/temp_cumchange.rds')
  saveRDS(prec_cumchange, 'output/prec_cumchange.rds')
}

plot(temp_cumchange)
plot(log(temp_cumchange))
plot(prec_cumchange)
plot(log(prec_cumchange))


plot(paleotemp[[1]])
plot(paleotemp[[3000]])
nlyr(paleotemp)
ncell(paleotemp[[1]])


## 4. slope per cell ----
# apply closed-form slope formula instead of fitting linear models for each cell
if ('temp_slope.rds' %in% list.files('output/')){
  paleotemp_slope <- readRDS('output/temp_slope.rds')
  paleoprec_slope <- readRDS('output/prec_slope.rds')
}

if (!'temp_slope.rds' %in% list.files('output/')){
  t_steps <- as.numeric(time(paleotemp_selected))
  stopifnot(length(t_steps) == nlyr(paleotemp_selected))
  
  
  #t_centered <- t_steps - mean(t_steps, na.rm = TRUE)
  #den <- sum(t_centered^2, na.rm = TRUE)
  
  slope_fun <- function(x){
    ok <- is.finite(x) & is.finite(t_steps)
    if (sum(ok) < 2) return(NA_real_)
    xc <- x[ok] - mean(x[ok])
    tc <- t_steps[ok] - mean(t_steps[ok])
    sum(tc * xc) / sum(tc^2)
  }
  
  system.time(paleotemp_slope <- app(paleotemp_selected, slope_fun))
  system.time(paleoprec_slope <- app(paleoprec_selected, slope_fun))
  plot(paleotemp_slope*100000)
  plot(paleoprec_slope*10000)
  
  saveRDS(paleotemp_slope, 'output/temp_slope.rds')
  saveRDS(paleoprec_slope, 'output/prec_slope.rds')
  
}

paleotemp_selected

### > slope verification ----
# verify, for a given cell_id, that the closed-form slope matches the lm() slope

# extract values of each cell in each layer
v <- values(paleotemp_selected)
ncol(v) # 501 columns; each column represents one layer (one time step). 
colnames(v)
nrow(v) # 53640 rows; this is the number of cells in each layer. 



# for example, for cell 103, 883... (see that it's not NA)
v[,1] # look for the ones that are not NA
cell <- 883
v[cell,] # temp values in each time step for cell 103

temp_cell <- data.frame(time = time(paleotemp_selected), temp = v[cell,])
par(mar=c(5,5,2,2))
plot(x = temp_cell$time, y = temp_cell$temp, pch = 16, col = 'gray60', main = 'Cell 103')

lm_cell <- lm(temp_cell$temp ~ temp_cell$time)
coef(lm_cell)[2] # slope from the linear model

values(paleotemp_slope, mat = FALSE)[cell] # slope from the closed-form slope formula

# slope values are the same with both methods







# cell by coordinates
x <- 10.5
y <- 45.2

ts <- extract(paleotemp, cbind(x, y))[1, -1]  # drop the ID column
ts

# add time values (e.g., for plotting)
t <- as.numeric(time(paleotemp))

df <- data.frame(time = t, value = as.numeric(ts))
head(df)



#Here’s a straightforward way to verify, for a given cell_id, that the 
# closed-form slope matches the lm() slope (using the same handling of 
# missing values).

cell_id <- 123456  # <-- your cell

# 1) Extract the time series for that cell (one value per layer)
y <- as.numeric(values(paleotemp_selected, cells = cell_id))

# 2) Get the time vector (must align with layers)
t <- as.numeric(time(paleotemp_selected))

stopifnot(length(y) == length(t))

# 3) Keep only valid pairs (same NA handling as in the slope formula)
ok <- is.finite(y) & is.finite(t)
y2 <- y[ok]
t2 <- t[ok]

# ---- A) slope from lm() ----
fit <- lm(y2 ~ t2)
slope_lm <- unname(coef(fit)[2])

# ---- B) slope from the closed-form formula ----
tc <- t2 - mean(t2)
yc <- y2 - mean(y2)
slope_formula <- sum(tc * yc) / sum(tc^2)

# 4) Compare
c(slope_lm = slope_lm, slope_formula = slope_formula, diff = slope_lm - slope_formula)
all.equal(slope_lm, slope_formula, tolerance = 1e-12)





