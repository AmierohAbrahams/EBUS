# Obtaining the wind and temperature data for the EBUS
# Temperature data: OISST
# Wind data: ERA 5 u and v variables
# The extracting scripts from netCDF to CSV can be found in the folder called Data_extraction

#############################################################################################################################

# 10m u-component of wind
# m s-1
# Eastward component of the 10m wind. It is the horizontal speed of air
# moving towards the east, at a height of ten metres above the surface of
# the Earth, in metres per second. Care should be taken when comparing this
# variable with observations, because wind observations vary on small space and time
# scales and are affected by the local terrain, vegetation and buildings that are represented only on
# average in the ECMWF Integrated Forecasting System. This variable can be combined with the V component of 10m
# wind to give the speed and direction of the horizontal 10m wind.


# 10m v-component of wind
# m s-1
# Northward component of the 10m wind. It is the horizontal speed of air moving towards the north, at a height of ten metres above
# the surface of the Earth, in metres per second. Care should be taken when comparing this variable with observations, 
# because wind observations vary on small space and time scales and are affected by the local terrain, vegetation and buildings 
# that are represented only on average in the ECMWF Integrated Forecasting System. This variable can be combined with 
# the U component of 10m wind to give the speed and direction of the horizontal 10m wind.

# #################################################################################################################################################################
# Loading Libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(fasttime)
source("functions/theme.R")

# Converting U and V wind variables to wind speed and direction
# The wind data (u and v) for all EBUS are found in the folder "data_wind_uv" # RWS: This folder is not on GitHub
# This is repeated for the BC, HC, CC and CalC

# RWS: Where do BC_vwind and BC_wind come from? Code must be included here that shows how these files are loaded

BC_vwind <- BC_vwind %>%
  select(v_10)
BC_wind <- cbind(BC_vwind, BC_wind)
BC_wind_fin <- BC_wind %>% # This is a complete dataset of wind u and v variables for the Benguela current
  select(lon, lat, date, u_10, v_10)

# Load wind U V data frames
load("data_wind_uv/BC_wind_fin.RData")
BC_wind_fin <- BC_wind_fin %>% 
  mutate(date = as.Date(fastPOSIXct(date, tz = "GMT")))

# Calculate wind speed and direction
# RWS: I'd prefer if wind speed and direction were calculated like in this post:
# https://stackoverflow.com/questions/21484558/how-to-calculate-wind-direction-from-u-and-v-wind-components-in-r
# Note difference in the wind direction coming FROM vs where it is going TO
# I've produced both of them here for ease of use later on
BC_wind_dir <- BC_wind_fin %>% 
  mutate(wind_spd = round(sqrt(u_10^2 + v_10^2), 2),
         wind_dir_from = round((270-(atan2(v_10, u_10)*(180/pi)))%%360),
         wind_dir_to = ifelse(wind_dir_from >= 180, wind_dir_from-180, wind_dir_from+180))
save(BC_wind_dir, file = "data_wind_uv/BC_wind_dir.RData")

#####################################################################################################################################################################
# The data being loaded here are the wind data and the temperature data. 
# These data were matched by lat,lon and date
# Additionally a year and season column was created 
# This was repeated for all EBUS

BC_temp <- read_csv("Data_extraction/BC_temp.csv", col_names = c("lon", "lat", "temp", "date"))

match_func <- function(temp_df, wind_df){
  match <- wind_df  %>%
    left_join(temp_df, by = c("lon",  "lat", "date")) %>%
    na.trim()
  return(match)
}

BC_match <- match_func(temp_df = BC_temp, wind_df = BC_wind_dir) # Matching the wind data with the 30yr time series OISST temperature data for the Benguela current

# Seasons for the southern hemisphere
seasons_S_func <- function(df){
  df_seasons <- df %>% 
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
           year = year(date)) %>% 
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",        
                           ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}

# RWS: These functions were named the same

# Seasons for the Northern Hemisphere
seasons_N_func <- function(df){
  seasons <- df %>% 
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
           year = year(date)) %>% 
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Winter",        
                           ifelse(month %in% c("Mar", "Apr", "May"), "Spring",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Summer",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Autumn","Error")))))
}

# RWS: Where is the code in which these functions are used to create the 'data_complete/x_complete.RData' files?

# Below is the complete datasets for each of the EBUS with wind speed and wind direction variables
load("data_complete/CalC_complete.RData")
load("data_complete/CC_complete.RData")
load("data_complete/BC_complete.RData")
load("data_complete/HC_complete.RData")

BC_complete <- BC_complete %>% 
  rename(speed = spd) 
HC_complete <- HC_complete %>%
  mutate(lon = lon - 360) # RWS: Why is it not necessary to change the name of the speed column for the other files?
CC_complete <- CC_complete %>%
  mutate(lon = lon - 360)
CalC_complete <- CalC_complete %>%
  mutate(lon = lon - 360)

# Wind should not have negative values hence this formula was used
wind_renamed_func <- function(df){
  wind_renamed <- df %>% 
    mutate(wind_dir = ifelse(wind_dir < 0, wind_dir+360, wind_dir)) %>% # RWS:  I'm not certain this is correct to do
    dplyr::rename(wind_spd = speed) %>%
    dplyr::rename(wind_dir = wind_dir) %>% 
    filter(wind_spd > 0)
}

BC_final <- wind_renamed_func(BC_complete)
HC_final <- wind_renamed_func(HC_complete)
CC_final <- wind_renamed_func(CC_complete)
CalC_final <- wind_renamed_func(CalC_complete)


# save(BC_final, file = "data_complete/BC_final.RData")
# save(HC_final, file = "data_complete/HC_final.RData")
# save(CC_final, file = "data_complete/CC_final.RData")
# save(CalC_final, file = "data_complete/CalC_final.RData")








