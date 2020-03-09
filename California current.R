### California current

library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
# library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 1)

CalC_30yr <- CalC%>%
  slice(1:21461937)
#save(CalC_30yr , file = "data/CalC_30yr.RData")

### EXtracting and creating the wind script
ncFile <- '/home/amieroh/Documents/EBUS/data/CalC/165/wind_1981_165.nc'

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
u_10.2df_CalC <- new %>%
  unite(year, month, day, col = "date", sep = "-")


save(u_10.2df_CalC , file = "data/u_10.2df_CalC.RData")
load("~/Documents/EBUS/data/u_10.2df_CalC.RData")
# 

ncFile <- '/home/amieroh/Documents/EBUS/data/CalC/165/wind_1981_165b.nc'

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
u_10.2df_CalC2 <- new %>%
  unite(year, month, day, col = "date", sep = "-")

# Matching it with temperature 

CalC_wind_season <- u_10.2df_CalC2 %>% 
  mutate(lat_new = lat + 0.125,
       lon_new = lon + 0.125) %>% 
   mutate(lon_newest =lon_new +360)

u_10.2df_Calc1_test  <- CalC_wind_season  %>% 
  mutate(date = as.Date(as.character(date)))

u_10.2df_hc1_test2 <- u_10.2df_Calc1_test %>% 
  select(-lon,-lat,-lon_new) %>% 
  rename(lat = lat_new,
         lon = lon_newest)

match_func <- function(df){
  match <- df  %>%  
    left_join(u_10.2df_hc1_test2, by = c("lat","lon","date")) #%>% 
  #na.trim()
  return(match)
}

wind_temp_match_CalC <- match_func(df = CalC_30yr)
save(wind_temp_match_CalC , file = "data/wind_temp_match_CalC.RData")
