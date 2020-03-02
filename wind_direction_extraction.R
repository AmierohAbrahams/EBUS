
library(ncdf4) # library for processing netCDFs
library(ncdf4)
library(data.table)
library(tidyverse)
library(reshape2)
library(plyr)
library(lubridate)
library(stringr)
library(doMC); doMC::registerDoMC(cores = 4)
library(heatwaveR)


# Extracting wind
ncFile <- '/home/amieroh/Documents/data/cc/wind_165.nc'
nc <- nc_open(ncFile)
u_10 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
u_10_df <- as_tibble(melt(u_10, value.name = "u_10"))
  u_10_df$time <- as.POSIXct(u_10_df$time * 60 * 60, origin = "1900-01-01")
tidy <- u_10_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,u_10)
u_10_df_BC <- new %>%
  unite(year, month, day, col = "date", sep = "-")


##########################################################

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily81-99_166.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 13)
v_10 <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
v_10_df <- as_tibble(melt(v_10, value.name = "v_10"))
v_10_df$time <- as.POSIXct(v_10_df$time * 60 * 60, origin = "1900-01-01")
tidy <- v_10_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,v_10)
v_10_df_BC <- new %>%
  unite(year, month, day, col = "date", sep = "-")

# save(v_10_df_BC , file = "data/v_10_df_BC.RData")