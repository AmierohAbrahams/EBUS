# Obtaining the wind and temperature data for the EBUS
# Temperature data: OISST
# Wind data: ERA 5 u and v variables
# The extracting scripts from netCDF to CSV can be found in the folder called Data_extraction

#############################################################################################################################

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

# #################################################################################################################################################################
# Loading Libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
source("functions/theme.R")

# Converting U and V wind variables to wind speed and direction
# The wind data (u and v) for all EBUS are found in the folder "data_wind_uv"
# This is repeated for the BC, HC, CC and CalC

BC_vwind <- BC_vwind %>%
  select(v_10)
BC_wind <- cbind(BC_vwind,BC_wind)
BC_wind_fin <- BC_wind %>% # This is a complete dataset of wind u and v variables for the Benguela current
  select(lon,lat,date,u_10,v_10)


# Wind speed
BC_wind_fin$u_squared ='^'(BC_wind_fin$u,2)
BC_wind_fin$v_squared ='^'(BC_wind_fin$v,2)
BC_wind_fin <- BC_wind_fin %>%
  mutate(speed = sqrt(u_squared + v_squared))

# Wind direction
BC_wind_fin <- BC_wind_fin %>%
  mutate(wind_dir_trig_to = atan2(u/speed, v/speed),
         wind_dir = wind_dir_trig_to * 180/pi)

#####################################################################################################################################################################
# The data being loaded here is the wind data and the temperature data. 
# This data was matched by lat,lon and date
# Additionally a year and season column was created 
# This was repeated for all EBUS

BC_temp <- read_csv("data_complete/BC_temp.csv") # BC_temp created in the Data_extraction folder. Where I extract netCDFs to CSV
colnames(BC_temp ) <- c("lon", "lat", "temp","date")

match_func <- function(df){
  match <- BC_wind_fin  %>% 
    left_join(df, by = c("lon", "date", "lat")) %>% 
    na.trim()
  return(match)
}

BC_match <- match_func(df = BC_temp) # Matching the wind data with the 30yr time series OISST temperature data for the benguela current

# Seasons for the souhern hemisphere
seasons_func <- function(df){
  BC_seaons <- df %>% 
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
           year = year(date)) %>% 
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",        
                           ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}


#Seasons for the Northern Hemisphere
seasons_func <- function(df){
  seasons <- df %>% 
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
           year = year(date)) %>% 
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Winter",        
                           ifelse(month %in% c("Mar", "Apr", "May"), "Spring",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Summer",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Autumn","Error")))))
}


# Below is the complete datasets for each of the EBUS with wind speed and wind direction variables
load("data_complete/CalC_complete.RData")
load("data_complete/CC_complete.RData")
load("data_complete/BC_complete.RData")
load("data_complete/HC_complete.RData")

BC_complete <- BC_complete %>% 
  rename(speed = spd) 
HC_complete <- HC_complete %>%
  mutate(lon = lon - 360)
CC_complete <- CC_complete %>%
  mutate(lon = lon - 360)
CalC_complete <- CalC_complete %>%
  mutate(lon = lon - 360)

# Wind should not have negative values hence this forumla was used

wind_renamed_func <- function(df){
  wind_renamed <- df %>% 
    mutate(wind_dir = ifelse(wind_dir < 0, wind_dir+360, wind_dir)) %>%
    dplyr::rename(wind_spd = speed) %>%
    dplyr::rename(wind_dir = wind_dir) %>% 
    filter(spd > 0)
}

BC_final<- wind_renamed_func(BC_complete)
HC_final<- wind_renamed_func(HC_complete)
CC_final<- wind_renamed_func(CC_complete)
CalC_final<- wind_renamed_func(CalC_complete)


# save(BC_final, file = "data_complete/BC_final.RData")
# save(HC_final, file = "data_complete/HC_final.RData")
# save(CC_final, file = "data_complete/CC_final.RData")
# save(CalC_final, file = "data_complete/CalC_final.RData")








