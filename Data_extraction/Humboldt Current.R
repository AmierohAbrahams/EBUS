library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
# library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 4) 

ncFile <- '~/Desktop/EBUS/data/HC/166/wind_2005_166b.nc'
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

#rm(u_10.2df_hc) 

#save(HC_wind_season.test, file = "data/HC_wind_season.test.RData")

# 

HC_wind_season  <- HC_wind_season.test  %>% 
  mutate(date = as.Date(as.character(date)))

HC_wind_season <- HC_wind_season %>% 
  rename(lat = lat_new,
         lon = lon_newest)

#save(HC_wind_season , file = "data/HC_wind_season.RData")

match_func <- function(df){
  match <- df  %>%  
    left_join(HC_wind_season, by = c("lat","lon","date")) #%>% 
  #na.trim()
  return(match)
}

#load("~/Desktop/EBUS/data/wind_temp_match_tester.RData")
# load("~/Desktop/EBUS/data/HC_semi_complete.RData")
rm(HC_wind_season.test)
#wind_temp_match_HC <- match_func(df = HC_semi_complete)  #use this after combining the big dataset at end
wind_temp_match_HC <- match_func(df = wind_temp_match_HC)
save(wind_temp_match_HC, file = "data/wind_temp_match_HC.RData")

HC_semi_complete <- wind_temp_match_HC %>%
  rename(ua = u_10.x,
         ub = u_10.y,
         uc= u_10.x.x,
         ud = u_10.y.y) %>% 
  ungroup() # %>% 
 # gather(ua, ub, uc,ud, key = "10", value = "U")
HC_semi_complete$v <- rowMeans(HC_semi_complete[c("v","ua","ub","uc","ud")], na.rm = T)

HC_semi_complete <- HC_semi_complete %>% 
  select(lon,lat,temp,date,v,u)
save(HC_semi_complete , file = "data/HC_semi_complete.RData")

HC_semi_complete <- left_join(wind_temp_match_tester_v,wind_temp_match_tester, by = c("lon","lat","date","temp"))

save(HC_semi_complete, file = "data/HC_semi_complete.RData")
  # test <- unite(wind_temp_match_tester,ua,ub,uc,ud)
#wind_temp_match1<- rbind(wind_temp_match, HC_wind_season)
# wind_temp_matchA <- merge(wind_temp_match,HC_wind_season,by=c("lat","lon","date")) 
#################################################

wind_temp_match_tester <- wind_temp_match_tester %>% 
  rename(u =mean)

wind_temp_match_HC <- wind_temp_match_HC%>% 
  select(lon,lat,temp,date,v,u)


###########################
#converting u and v to wind dir and speed
#
HC_semi_complete$u_squared ='^'(HC_semi_complete$u,2)
HC_semi_complete$v_squared ='^'(HC_semi_complete$v,2)
HC_semi_complete <- HC_semi_complete %>% 
  mutate(speed = sqrt(u_squared + v_squared))
# Wind direction 

HC_semi_complete <- HC_semi_complete %>% 
  mutate(wind_dir_trig_to = atan2(u/speed, v/speed),
         wind_dir = wind_dir_trig_to * 180/pi)

HC_semi_complete_fin <- HC_semi_complete %>% 
  select(lon,lat,temp,date,speed,wind_dir)
save(HC_semi_complete_fin, file = "data/HC_semi_complete_fin.RData")

