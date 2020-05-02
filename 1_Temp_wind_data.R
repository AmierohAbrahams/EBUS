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
library(fasttime)
library(zoo)
source("functions/theme.R")

# Converting U and V wind variables to wind speed and direction
# The wind data (u and v) for all EBUS are found in the folder "data_wind_uv". This folder is too large for GITHUB
# This is repeated for the BC, HC, CC and CalC


# Load wind U V data frames
load("data_wind_uv/BC_wind_fin.RData") # This is the extracted u and v wind variables. See extraction folder
BC_wind_fin <- BC_wind_fin %>% 
  mutate(date = as.Date(fastPOSIXct(date, tz = "GMT")),
         lat = lat - 0.125,
         lon = lon + 0.125)

# Calculate wind speed and direction
BC_wind_dir <- BC_wind_fin %>% 
  mutate(wind_spd = round(sqrt(u_10^2 + v_10^2), 2),
         wind_dir_from = round((270-(atan2(v_10, u_10)*(180/pi)))%%360),
         wind_dir_to = ifelse(wind_dir_from >= 180, wind_dir_from-180, wind_dir_from+180))
# save(BC_wind_dir, file = "data_wind_uv/BC_wind_dir.RData")
rm(BC_wind_fin); gc()

#####################################################################################################################################################################
# The data being loaded here are the wind data and the temperature data. 
# These data are matched by lat, lon, and date
# Additionally a year and season column is created 
# This is repeated for all EBUS

# Load temperature data
BC_temp <- read_csv("data_complete/BC_temp.csv", col_names = c("lon", "lat", "temp", "date"))

# Function for matching temps and wind  
match_func <- function(temp_df, wind_df){
  match <- wind_df  %>%
    left_join(temp_df, by = c("lon",  "lat", "date")) %>%
    na.trim()
  return(match)
}

# Matching the wind data with the 30yr time series OISST temperature data for the Benguela current
BC_match <- match_func(temp_df = BC_temp, wind_df = BC_wind_dir) 
rm(BC_wind_dir, BC_temp); gc()

# Seasons for the southern hemisphere
seasons_S_func <- function(df){
  df_seasons <- df %>% 
    mutate(month = month(date, abbr = T, label = T),
           year = year(date)) %>% 
    mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Summer", 
                              month %in% c("Mar", "Apr", "May") ~ "Autumn",
                              month %in% c("Jun", "Jul", "Aug") ~ "Winter",
                              month %in% c("Sep", "Oct", "Nov") ~ "Spring"))
}


# Seasons for the Northern Hemisphere
seasons_N_func <- function(df){
  df_seasons <- df %>% 
    mutate(month = month(date, abbr = T, label = T),
           year = year(date)) %>% 
    mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Winter", 
                              month %in% c("Mar", "Apr", "May") ~ "Spring",
                              month %in% c("Jun", "Jul", "Aug") ~ "Summer",
                              month %in% c("Sep", "Oct", "Nov") ~ "Autumn"))
}

# Add the seasons to the wind + temp dataframes
BC_complete <- seasons_S_func(BC_match)
rm(BC_match); gc()

# Save
save(BC_complete, file = "data_complete/BC_complete.RData")

