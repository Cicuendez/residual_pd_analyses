

#install.packages('pastclim')
#library(pastclim)

# packages ----
library(terra)

# import paleoclimate ----
paleotemp <- rast('../data/paleo_pgem/PALEO-PGEM-Series_bio1_mean.nc')
paleoprec <- rast('../data/paleo_pgem/PALEO-PGEM-Series_bio12_mean.nc')

# show progress
terraOptions(progress = 1)

# time in years
time(paleotemp)
time(paleoprec)
identical(time(paleoprec), time(paleotemp))
time(paleotemp[[1]])

nlyr(paleotemp)
nlyr(paleotemp[[c(1,4)]])





# reduce dataset ----
# select layers
idx <- seq(1, nlyr(paleotemp), by = 10)
paleotemp_selected <- paleotemp[[idx]]
paleoprec_selected <- paleoprec[[idx]]
time(paleotemp_selected)

# WAYS OF CAPTURING CLIMATE VARIABILITY ----

## 1. standard deviation ----
# SD of temperature in the last 5 Ma
temp_sd <- app(paleotemp_selected, fun = sd, na.rm = TRUE)
plot(temp_sd)

prec_sd <- app(paleoprec_selected, fun = sd, na.rm = TRUE)
plot(prec_sd)

saveRDS(temp_sd, 'output/temp_sd.rds')
saveRDS(prec_sd, 'output/prec_sd.rds')

## 2. net change ----
temp_netchange <- paleotemp[[nlyr(paleotemp)]] - paleotemp[[1]]
plot(paleotemp[[1]])
plot(paleotemp[[nlyr(paleotemp)]])
plot(paleotemp[[round(nlyr(paleotemp)/2)]])

plot(temp_netchange)

## 3. cumulative change ----
# Differences between consecutive layers
d <- diff(paleotemp_selected)   # computes layer_t - layer_(t-1)
# diff() reduces the number of layers from 5001 → 5000.
d

# Cumulative change (sum of absolute changes per cell)



temp_cumchange <- app(abs(d), sum, na.rm = TRUE)
plot(temp_cumchange)




plot(paleotemp[[1]])
plot(paleotemp[[3000]])
nlyr(paleotemp)
ncell(paleotemp[[1]])








## 4. slope per cell ----
idx <- seq(1, nlyr(paleotemp), by = 500)
paleotemp_onlyten <- paleotemp[[idx]]
time(paleotemp_onlyten)

t_steps <- as.numeric(time(paleotemp_onlyten))
stopifnot(length(t_steps) == nlyr(paleotemp_onlyten))


t_centered <- t_steps - mean(t_steps, na.rm = TRUE)
den <- sum(t_centered^2, na.rm = TRUE)

slope_fun <- function(x) {
  ok <- is.finite(x) & is.finite(t_steps)
  if (sum(ok) < 2) return(NA_real_)
  xc <- x[ok] - mean(x[ok])
  tc <- t_steps[ok] - mean(t_steps[ok])
  sum(tc * xc) / sum(tc^2)
}

system.time(slope_r <- app(paleotemp_onlyten, slope_fun))
plot(slope_r)
slope_r
