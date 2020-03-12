library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
# library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 4) 

#ncFile <- '~/Desktop/EBUS/data/CalC/166/wind_1981_165.nc'
#ncFile <- '/home/amieroh/Documents/EBUS/data/CalC/165/wind_1981_165b.nc'
ncFile <- '~/Desktop/EBUS/data/CC/166/wind_2012_166b.nc'
# CC_30years <- CC %>% 
#   slice(1:17239455)

#save(CC_30years , file = "data/CC_30years.RData")

nc <- nc_open(ncFile)
u_10.2 <- ncvar_get(nc, varid = "v10") %>%
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
#save(u_10.2df_hc , file = "data/u_10.2df_hc.RData")


HC_wind_season.test <- u_10.2df_hc %>% 
  mutate(lat_new = lat + 0.125,
         lon_new = lon + 0.125)%>% 
  mutate(lon_newest =lon_new +360) %>%
  select(date,u_10,lat_new,lon_newest)

# rm(u_10.2df_hc)
# rm(nc)
# rm(t2)
# rm(tidy)
# rm(new)
#save(HC_wind_season.test, file = "data/HC_wind_season.test.RData")


HC_wind_season  <- HC_wind_season.test  %>% 
  mutate(date = as.Date(as.character(date)))

HC_wind_season <- HC_wind_season %>% 
  rename(lat = lat_new,
         lon = lon_newest)

# save(HC_wind_season , file = "data/HC_wind_season.RData")

match_func <- function(df){
  match <- df  %>%  
    left_join(HC_wind_season, by = c("lat","lon","date")) #%>% 
  #na.trim()
  return(match)
}

#load("~/Desktop/EBUS/data/CalC_30yr.RData")
#rm(HC_wind_season.test)

#wind_temp_match_CC <- match_func(df = CC_semi_complete) #use this after combining the big dataset at end
wind_temp_match_CC <- match_func(df = wind_temp_match_CC)
save(wind_temp_match_CC, file = "data/wind_temp_match_CC.RData")

CC_semi_complete <- wind_temp_match_CC %>%
  rename(ua = u_10.x,
         ub = u_10.y,
         uc= u_10.x.x,
         ud = u_10.y.y,
         ue= u_10.x.x.x,
        uf= u_10.y.y.y) %>% 
  ungroup() # %>% 
# gather(ua, ub, uc,ud, key = "10", value = "U")
CC_semi_complete$v <- rowMeans(CC_semi_complete[c("v","ua","ub","uc","ud","ue","uf")], na.rm = T)

CC_semi_complete <- CC_semi_complete %>% 
  select(lon,lat,temp,date,u,v)
save(CC_semi_complete , file = "data/CC_semi_complete.RData")

# test <- unite(wind_temp_match_tester,ua,ub,uc,ud)
#wind_temp_match1<- rbind(wind_temp_match, HC_wind_season)
# wind_temp_matchA <- merge(wind_temp_match,HC_wind_season,by=c("lat","lon","date")) 
#################################################
