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

load("data_wind_uv/CC_wind_fin.RData")
load("data_wind_uv/CalC_wind_fin.RData")
load("data_wind_uv/HC_wind_fin.RData")
load("data_wind_uv/BC_wind_fin.RData") 

BC_wind_fin <- BC_wind_fin %>% 
  mutate(date = as.Date(fastPOSIXct(date, tz = "GMT")),
         lat = lat - 0.125,
         lon = lon + 0.125)

wind_func <- function(df){
  wind <- df %>% 
    mutate(date = as.Date(fastPOSIXct(date, tz = "GMT")),
           lat = lat - 0.125,
           lon = lon + 360, #Adding 360 so that it will match the temperature data. 
           lon = lon + 0.125)
}

CC_wind_fin <- wind_func(df = CC_wind_fin)
CalC_wind_fin <- wind_func(df = CalC_wind_fin)
HC_wind_fin <- wind_func(df = HC_wind_fin)

# Function for matching temps and wind
# Loading the temperature data this is the OISST data extracted to the regions (See extraction folder)
CC_temp <- read_csv("data_complete/CC_temp.csv", col_names = c("lon", "lat", "temp", "date"))
CalC_temp <- read_csv("data_complete/CalC_temp.csv", col_names = c("lon", "lat", "temp", "date"))
HC_temp <- read_csv("data_complete/HC_temp.csv", col_names = c("lon", "lat", "temp", "date"))
BC_temp <- read_csv("data_complete/BC_temp.csv", col_names = c("lon", "lat", "temp", "date"))

match_func <- function(temp_df, wind_df){
  match <- wind_df  %>%
    left_join(temp_df, by = c("lon",  "lat", "date")) %>%
    na.trim()
  return(match)
}

# Matching the wind data with the 30yr time series OISST temperature data 
CC_match <- match_func(temp_df = CC_temp, wind_df = CC_wind_fin)
CalC_match <- match_func(temp_df = CalC_temp, wind_df = CalC_wind_fin)
HC_match <- match_func(temp_df = HC_temp, wind_df = HC_wind_fin)
BC_match <- match_func(temp_df = BC_temp, wind_df = BC_wind_fin) 

# save(CalC_match, file = "data_complete/CalC_match.RData") 
# save(CC_match, file = "data_complete/CC_match.RData") 
# save(HC_match, file = "data_complete/HC_match.RData") 
# save(BC_match, file = "data_complete/BC_match.RData") 

# 3: Calculating wind speed and direction ----------------------------------------------------

# Calculate wind speed and direction
load("data_complete/CC_match.RData")
load("data_complete/HC_match.RData")
load("data_complete/CalC_matched2.RData")
load("data_complete/BC_match.RData")

BC_match <- BC_match %>% 
  rename(u = u_10,
         v = v_10)

CalC_matched2 <- wind_temp_match_test
rm(wind_temp_match_test)

wind_dir_func <- function(df){
  wind_dir <- df %>% 
    mutate(wind_spd = round(sqrt(u^2 + v^2), 2),
           wind_dir_from = round((270-(atan2(v, u)*(180/pi)))%%360),
           wind_dir_to = ifelse(wind_dir_from >= 180, wind_dir_from-180, wind_dir_from+180))
}

CC_wind <- wind_dir_func(df = CC_match)
#save(CC_wind, file = "data_complete/CC_wind.RData")
#CalC_wind <- wind_dir_func(df = CalC_match)
CalC_wind <- wind_dir_func(df = CalC_matched2)
#save(CalC_wind, file = "data_complete/CalC_wind.RData")
HC_wind <- wind_dir_func(df = HC_match)
#save(HC_wind, file = "data_complete/HC_wind.RData")
BC_wind <- wind_dir_func(df = BC_match)
# save(BC_wind, file = "data_complete/BC_wind.RData")

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

# save(HC_complete, file = "data_complete/HC_complete.RData")
# save(CC_complete, file = "data_complete/CC_complete.RData")
# save(CalC_complete, file = "data_complete/CalC_complete.RData")
# save(BC_complete, file = "data_complete/BC_complete.RData")

