library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
# library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 1) 

ncFile <- '/home/amieroh/Documents/EBUS/data/HC/166/wind_1981_166.nc'
nc <- nc_open(ncFile)
v_10.2 <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10.2) <- list(lon = nc$dim$lon$vals,
                         lat = nc$dim$lat$vals,
                         time = nc$dim$time$vals)
nc_close(nc)
v_10.2_df <- as_tibble(melt(v_10.2, value.name = "u_10"))
v_10.2_df$time <- as.POSIXct(v_10.2_df$time * 60 * 60, origin = "1900-01-01")
tidy <- v_10.2_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,u_10)
v_10.2df_hc <- new %>%
  unite(year, month, day, col = "date", sep = "-")
save(v_10.2df_hc , file = "data/v_10.2df_hc.RData")


HC_wind_season.test <- v_10.2df_hc %>% 
  mutate(lat_new = lat + 0.125,
         lon_new = lon + 0.125)%>% 
  mutate(lon_newest =lon_new +360) %>% 
  select(date,v_10,lat_new,lon_newest)
rm(v_10.2df_hc) 

save(HC_wind_season.test, file = "data/HC_wind_season.test.RData")

rm(nc)
rm(new)
rm(t2)
rm(tidy)
rm(v_10.2_df) 

HC_wind_season  <- HC_wind_season.test  %>% 
  mutate(date = as.Date(as.character(date)))

HC_wind_season <- HC_wind_season %>% 
  rename(lat = lat_new,
         lon = lon_newest)

save(HC_wind_season , file = "data/HC_wind_season.RData")

match_func <- function(df){
  match <- df  %>%  
    left_join(HC_wind_season, by = c("lat","lon","date")) #%>% 
  #na.trim()
  return(match)
}

#load("~/Documents/EBUS/data/wind_temp_match_tester.RData")
rm(HC_wind_season.test)
wind_temp_match_HC <- match_func(df = wind_temp_match_HC)
save(wind_temp_match_HC, file = "data/wind_temp_match_HC.RData")

wind_temp_match_tester <- wind_temp_match_HC %>%
  rename(ua = u_10.x,
         ub = u_10.y,
         uc= u_10.x.x,
         ud = u_10.y.y) %>% 
  ungroup() # %>% 
# gather(ua, ub, uc,ud, key = "10", value = "U")
wind_temp_match_tester$mean <- rowMeans(wind_temp_match_tester[c("mean","ua","ub","uc","ud")], na.rm = T)
wind_temp_match_tester <- wind_temp_match_tester %>% 
  select(lon,lat,temp,date,mean)
save(wind_temp_match_tester , file = "data/wind_temp_match_tester.RData")

# test <- unite(wind_temp_match_tester,ua,ub,uc,ud)
#wind_temp_match1<- rbind(wind_temp_match, HC_wind_season)
# wind_temp_matchA <- merge(wind_temp_match,HC_wind_season,by=c("lat","lon","date")) 
#################################################

wind_temp_match_tester <- wind_temp_match_tester %>% 
  rename(u =mean)
