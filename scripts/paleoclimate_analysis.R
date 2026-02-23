

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
  plot(paleotemp_slope)
  plot(paleoprec_slope)
  
  saveRDS(paleotemp_slope, 'output/temp_slope.rds')
  saveRDS(paleoprec_slope, 'output/prec_slope.rds')
  
}






