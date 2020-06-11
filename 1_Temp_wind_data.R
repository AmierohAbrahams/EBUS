# 1_Temp_wind_data.R
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Match wind and temperature data
# 3: Determine wind speed and direction


# 1: Setup environment ----------------------------------------------------
library(tidyverse)
library(lubridate)
library(fasttime)
library(zoo)
source("functions/theme.R")


# : Match wind and temperature data ----------------------------------------------------
# This is the wind u and v data for each current (See extraction folder)

# The data loaded here were extracted in the netCDF2CSV script in the data extraction folder
load("data/CC_wind.RData")
load("data/CalC_wind.RData")
load("data/HC_wind.RData")
load("data/BC_wind.RData") 

BC_wind_fin <- BC_wind %>% 
  mutate( lat = lat - 0.125,
         lon = lon + 0.125) %>% 
  rename(date = t)

wind_func <- function(df){
  wind <- df %>% 
    mutate(lat = lat - 0.125,
           lon = lon + 360, #Adding 360 so that it will match the temperature data. 
           lon = lon + 0.125) %>% 
    rename(date = t)
}

CC_wind_fin <- wind_func(df = CC_wind)
CalC_wind_fin <- wind_func(df = CalC_wind)
HC_wind_fin <- wind_func(df = HC_wind)

# Function for matching temps and wind
# Loading the temperature data this is the OISST data extracted to the regions (See netCDF2CSVscript in the data extraction folder)
load("~/Documents/EBUS/data_complete/BC.RData")
load("~/Documents/EBUS/data_complete/HC.RData")
load("~/Documents/EBUS/data_complete/CC.RData")
load("~/Documents/EBUS/data_complete/CalC.RData")

match_func <- function(temp_df, wind_df){
  match <- wind_df  %>%
    left_join(temp_df, by = c("lon",  "lat", "date")) %>%
    na.trim()
  return(match)
}

# Matching the wind data with the 30yr time series OISST temperature data 
CC_match <- match_func(temp_df = CC, wind_df = CC_wind_fin)
CalC_match <- match_func(temp_df = CalC, wind_df = CalC_wind_fin)
HC_match <- match_func(temp_df = HC, wind_df = HC_wind_fin)
BC_match <- match_func(temp_df = BC, wind_df = BC_wind_fin) 

# save(CalC_match, file = "data_complete/CalC_match.RData") 
# save(CC_match, file = "data_complete/CC_match.RData") 
# save(HC_match, file = "data_complete/HC_match.RData") 
# save(BC_match, file = "data_complete/BC_match.RData") 

# 3: Calculating wind speed and direction ----------------------------------------------------

# Calculate wind speed and direction
load("data_complete/CC_match.RData")
load("data_complete/HC_match.RData")
load("data_complete/CalC_match.RData")
load("data_complete/BC_match.RData")


wind_dir_func <- function(df){
  wind_dir <- df %>% 
    mutate(wind_spd = round(sqrt(u10^2 + v10^2), 2),
           wind_dir_from = round((270-(atan2(v10, u10)*(180/pi)))%%360),
           wind_dir_to = ifelse(wind_dir_from >= 180, wind_dir_from-180, wind_dir_from+180))
}

CC_wind <- wind_dir_func(df = CC_match)
#save(CC_wind, file = "data/CC_wind.RData")
CalC_wind <- wind_dir_func(df = CalC_match)
#save(CalC_wind, file = "data/CalC_wind.RData")
HC_wind <- wind_dir_func(df = HC_match)
#save(HC_wind, file = "data/HC_wind.RData")
BC_wind <- wind_dir_func(df = BC_match)
# save(BC_wind, file = "data/BC_wind.RData")

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
CC_complete <- seasons_N_func(CC_wind)
CalC_complete <- seasons_N_func(CalC_wind)
HC_complete <- seasons_S_func(HC_wind)
BC_complete <- seasons_S_func(BC_wind)

# save(HC_complete, file = "data/HC_complete.RData")
# save(CC_complete, file = "data/CC_complete.RData")
# save(CalC_complete, file = "data/CalC_complete.RData")
# save(BC_complete, file = "data/BC_complete.RData")
