# Python script to download wind data

# 10m u-component of wind	
# m s-1	
# Eastward component of the 10m wind. It is the horizontal speed of air 
# moving towards the east, at a height of ten metres above the surface of 
#the Earth, in metres per second. Care should be taken when comparing this
# variable with observations, because wind observations vary on small space and time
#scales and are affected by the local terrain, vegetation and buildings that are represented only on 
#average in the ECMWF Integrated Forecasting System. This variable can be combined with the V component of 10m
#wind to give the speed and direction of the horizontal 10m wind.



# 10m v-component of wind	
# m s-1	
# Northward component of the 10m wind. It is the horizontal speed of air moving towards the north, at a height of ten metres above 
#the surface of the Earth, in metres per second. Care should be taken when comparing this variable with observations, because wind observations
#vary on small space and time scales and are affected by the local terrain, vegetation and buildings that are represented only on average in the ECMWF 
#Integrated Forecasting System. This variable can be combined with the U component of 10m wind to give the speed and direction of the horizontal 10m wind.

#################################################################################################################################################################3
library(tidyverse)
library(lubridate)

# Converting U and V wind variables to wind speed and direction

# load("data/BC_wind.RData")
# load("data/BC_vwind.RData")
# 
BC_vwind <- BC_vwind %>%
  select(v_10)
BC_wind <- cbind(BC_vwind,BC_wind)
BC_wind_fin <- BC_wind %>%
  select(lon,lat,date,u_10,v_10)

# save(BC_wind_fin , file = "data/BC_wind_fin.RData")

# load("data/BC_wind_fin.RData")
# Wind speed
BC_wind_fin$u_squared ='^'(BC_wind_fin$u_10,2)
BC_wind_fin$v_squared ='^'(BC_wind_fin$v_10,2)
BC_wind_fin <- BC_wind_fin %>% 
  mutate(speed = sqrt(u_squared + v_squared))
# Wind direction 

BC_wind_fin <- BC_wind_fin %>% 
  mutate(wind_dir_trig_to = atan2(u_10/speed, v_10/speed),
         wind_dir = wind_dir_trig_to * 180/pi)

BC_wind_complete <- BC_wind_fin %>% 
  select(lon,lat,date,u_10,v_10,speed,wind_dir)

BC_wind_complete <- BC_wind_complete %>% 
  mutate(date = as.Date(as.character(date)))

# save(BC_wind_complete , file = "data/BC_wind_complete.RData")
# Creating seasonal column and comparing changes in wind patterns over years
load("~/Documents/EBUS/data/BC_wind_complete.RData")
BC_wind_season <- BC_wind_complete %>% 
  mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
         year = year(date)) %>% 
  mutate(season = ifelse(month %in% c("Jan", "Feb", "Mar"), "Summer",        
                         ifelse(month %in% c("Apr", "May", "Jun"), "Autumn",
                                ifelse(month %in% c("Jul", "Aug", "Sep"), "Winter",
                                       ifelse(month %in% c("Oct", "Nov", "Dec"), "Spring","Error")))))

# save(BC_wind_season, file = "data/BC_wind_season.RData")

BC_wind_season <- BC_wind_season %>% 
  mutate(lat_new = lat + 0.125,
         lon_new = lon + 0.125)

BC_wind_season <- BC_wind_season %>% 
  select(-lon,-lat) %>% 
  rename(lat = lat_new,
         lon = lon_new)

# save(BC_wind_season , file = "data/BC_wind_season.RData")


# Match the wind with the BC temperature
match_func <- function(df){
  match <- df  %>%  
    left_join(BC_wind_season, by = c("lat","lon","date")) #%>% 
    #na.trim()
  return(match)
}

wind_temp_match <- match_func(df = BC)
wind_temp_match <- wind_temp_match %>% 
  select(lon,lat,temp,date,speed,wind_dir,year,month,season)
# save(wind_temp_match , file = "data/wind_temp_match.RData")
############
load("data/wind_temp_match.RData")
wind_func <- function(df){
  wind <- df %>%  
    mutate(dir = ifelse(wind_dir < 0, wind_dir+360, wind_dir)) %>%
    dplyr::rename(spd = speed) %>%
    filter(spd > 0)
}

BC_wind_temp <- wind_func(df = wind_temp_match )
# save(BC_wind_temp, file = "data/BC_wind_temp.RData")
# First filter out only the SE data
SE_renamed <-BC_data %>% # Chnaged the names with the data 
  filter(dir >= 180, dir <= 270)
# Then create diifferent temporal results
SE_annual <- SE_renamed %>% 
  group_by(year) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
SE_summer <- SE_renamed %>% 
  filter(season == "Summer") %>% 
  group_by(year, season) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
SE_monthly <- SE_renamed %>% 
  filter(season == "Summer") %>% 
  group_by(year, season, month) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
           mean_temp = mean(temp, na.rm = T))

ggplot(data = SE_annual, aes(x = year, y = count)) +
  geom_line() +
  geom_smooth(method = "lm") 
## Annual count of SE wind in Summer
### The trends between annual and summer SE wind counts are remarkably similar
ggplot(data = SE_summer, aes(x = year, y = count)) +
  geom_line() +
  geom_smooth(method = "lm") #+
 # facet_wrap(~site)
## Summer month count of SE winds
ggplot(data = SE_monthly, aes(x = year, y = count)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") #+
  #facet_wrap(~site)

######




