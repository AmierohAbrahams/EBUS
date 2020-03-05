library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
# library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 4)

HC_30yr <- HC%>%
  slice(1:15800999)
#save(HC_30yr , file = "data/HC_30yr.RData")

### EXtracting and creating the wind script
ncFile <- '/home/amieroh/HC/165/wind_2012_165b.nc'

nc <- nc_open(ncFile)
u_10.2 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10.2) <- list(lon = nc$dim$lon$vals,
                         lat = nc$dim$lat$vals,
                         time = nc$dim$time$vals)
nc_close(nc)
u_10.2_df <- as_tibble(melt(u_10.2, value.name = "u_10"))
u_10.2_df$time <- as.POSIXct(u_10.2_df$time * 60 * 60, origin = "1900-01-01")
tidy <- u_10.2_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
  t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,u_10)
u_10.2df_hc <- new %>%
  unite(year, month, day, col = "date", sep = "-")

HC_wind <- rbind(u_10.2df_hc1,u_10.2df_hc)
# save(u_10.2df_hc2 , file = "data/u_10.2df_hc2.RData")
load("~/Documents/EBUS/data/u_10.2df_hc1.RData")
# BC_wind <- cbind(BC_vwind,BC_wind)

# Matching it with temperature 

HC_wind_season <- u_10.2df_hc1 %>% 
  mutate(lat_new = lat + 0.125,
         lon_new = lon + 0.125)%>% 
  mutate(lon_newest =lon_new +360)

u_10.2df_hc1_test  <- u_10.2df_hc1_test  %>% 
  mutate(date = as.Date(as.character(date)))

u_10.2df_hc1_test2 <- u_10.2df_hc1_test %>% 
  select(-lon,-lat,-lon_new) %>% 
  rename(lat = lat_new,
         lon = lon_newest)

match_func <- function(df){
  match <- df  %>%  
    left_join(u_10.2df_hc1_test3, by = c("lat","lon","date")) #%>% 
  #na.trim()
  return(match)
}

wind_temp_match <- match_func(df = HC_30yr)

############################
# Calafornia current

library(tidyverse)
CalC_30yr <- CalC%>%
  slice(1:21461937)

save(CalC_30yr , file = "data/CalC_30yr.RData")
##
ncFile <- '/home/amieroh/HC/165/wind_2012_165b.nc'

nc <- nc_open(ncFile)
u_10.2 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10.2) <- list(lon = nc$dim$lon$vals,
                         lat = nc$dim$lat$vals,
                         time = nc$dim$time$vals)
nc_close(nc)
u_10.2_df <- as_tibble(melt(u_10.2, value.name = "u_10"))
u_10.2_df$time <- as.POSIXct(u_10.2_df$time * 60 * 60, origin = "1900-01-01")
tidy <- u_10.2_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,u_10)
u_10.2df_hc <- new %>%
  unite(year, month, day, col = "date", sep = "-")

HC_wind <- rbind(u_10.2df_hc1,u_10.2df_hc)
# save(u_10.2df_hc2 , file = "data/u_10.2df_hc2.RData")
load("~/Documents/EBUS/data/u_10.2df_hc1.RData")
# BC_wind <- cbind(BC_vwind,BC_wind)

HC_wind_season <- u_10.2df_hc1 %>% 
  mutate(lat_new = lat + 0.125,
         lon_new = lon + 0.125)%>% 
  mutate(lon_newest =lon_new +360)

u_10.2df_hc1_test  <- u_10.2df_hc1_test  %>% 
  mutate(date = as.Date(as.character(date)))

u_10.2df_hc1_test2 <- u_10.2df_hc1_test %>% 
  select(-lon,-lat,-lon_new) %>% 
  rename(lat = lat_new,
         lon = lon_newest)

match_func <- function(df){
  match <- df  %>%  
    left_join(u_10.2df_hc1_test3, by = c("lat","lon","date")) #%>% 
  #na.trim()
  return(match)
}

wind_temp_match <- match_func(df = HC_30yr)

