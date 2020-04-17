# Loading Libraries
library(circular)
## devtools::install_github("robwschlegel/coastR")
library(gridExtra)
library(geosphere)
library(tidyverse)
library(heatwaveR)
library(coastR)
source("functions/theme.R")

# Data loaded and created in "1_Temp_wind_data"
load("data_complete/CC_complete.RData")
load("data_complete/CalC_complete.RData")
load("data_complete/HC_complete.RData")
load("data_complete/BC_complete.RData")

BC_complete <- BC_complete %>% 
  rename(speed = spd) %>% 
  mutate(site = "BC")

HC_complete <- HC_complete %>%
  mutate(lon = lon - 360)
CC_complete <- CC_complete %>%
  mutate(lon = lon - 360)
CalC_complete <- CalC_complete %>%
  mutate(lon = lon - 360)

# Now it was decided to get the mean values over the polygon region as the data above has values for each pixel 

final_dataset <- function(df){
  final <- df %>%
    group_by(date) %>% 
    summarise(mean_temp = mean(temp),
              mean_speed = mean(speed), # change spd to speed
              mean_wind = mean(wind_dir),
              mean_lat = mean(lat),
              mean_lon = mean(lon)) %>% 
    rename(temp = mean_temp,
           speed = mean_speed,
           wind = mean_wind,
           lat = mean_lat,
           lon = mean_lon)
}
# 
# CC_final <- final_dataset(df = CC_complete)
# HC_final <- final_dataset(df = HC_complete)
# CalC_final <- final_dataset(df = CalC_complete)
BC_final <- final_dataset(df = BC_complete)

wind_renamed_func <- function(df){
  wind_renamed <- df %>%
    mutate(wind = ifelse(wind < 0, wind+360, wind)) %>%
    #dplyr::rename(spd = speed) %>%
    dplyr::rename(dir_wind = wind) %>%
    filter(spd > 0)
}

BC_final <- wind_renamed_func(df = BC_final)
HC_final <- wind_renamed_func(df = HC_final)
CC_final <- wind_renamed_func(df = CC_final)
CalC_final <- wind_renamed_func(df = CalC_final)


# This works well running the heatwaveR package however, the upwelling index formula is dependant on the angel from the coastline,
# given that the lats and lon and now averagedd I will just have one angle from the coastline?

# Code to obtain the angle from the coastline

BC_transect <- coastR::transects(BC_final, spread = 30)

# Determining the upwelling index
upwelling_func <- function(df){
  UI<- df %>%  
    mutate(ui = speed * (cos(dir_wind - coast_angle))) %>%
    drop_na 
}
UI_BC <- upwelling_func(df= BC_final)

# This upwelling index is later used in this formula in order to obtain the upwelling metrics
UI_trim <- function(df){
  UI_trim <- df %>% 
    select(date,speed,dir_wind,coast_angle,ui) %>% # Removed lat and lon
    rename(t = date) %>% 
    rename(temp= ui)
}

BC_UI_trim <- UI_trim(df = UI_BC)

exceed_func <- function(df){ 
  df_upwell <- df %>%
    nest() %>% 
    mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1981-09-01", "2012-12-29")),
           exceed = purrr::map(clim, exceedance, minDuration = 1, threshold = 1)) %>%  
    select(-data, -clim) %>% 
    unnest() %>%
    filter(row_number() %% 2 == 1) %>%
    unnest() %>% # creates a column for each variables
    dplyr::rename(ui = temp) %>% # rename upwelling index vale to temp so that it could work with the function
    select(t, ui, exceedance) # selecting only these variables
}

BC_exceed <- exceed_func(df = BC_UI_trim)

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 3, coldSpells = T)
  return(res)
}

# Calculate the upwelling event metrics
upwell_base_BC <- BC_final %>% 
  dplyr::rename(t = date) %>% 
  nest() %>% 
  mutate(clim = purrr::map(data, ts2clm, pctile = 25, climatologyPeriod = c("1982-01-01", "2011-12-31"))) %>%
  select(-data) %>% 
  unnest(cols = clim) %>%
  left_join(CalC_exceed, by = c("t")) %>%
  filter(!is.na(exceedance)) %>%
  nest() %>% 
  mutate(exceed = purrr::map(data, detect_event_custom)) %>% 
  select(-data) %>% 
  unnest(cols = exceed) %>%
  filter(row_number() %% 2 == 0) %>% # Select event summary metrics
  # filter(row_number() %% 2 == 1) %>% # Select daily values
  unnest(cols = exceed)


#############################################################################################################################################
#### Given all this should I just maybe take it at a distance maybe 10km from the coastline?
# Why 10km and why not 5km? Because of the resolution of the data?

#Creating the transects
BC_trans <- transects(BC_complete)

transect.pixel <- function(site, distances){
  # Extract coordinates
  coords <- data.frame(lon = site$lon, lat = site$lat)
  # Find lon/ lats every X metres 
  pixels <- data.frame()
  for(i in 1:length(distances)){
    coords2 <- as.data.frame(destPoint(p = coords, b = site$heading, d = distances[i]))
    sitesIdx <- knnx.index(BC_complete[,1:2],as.matrix(coords2), k = 1)
    bathy1 <- data.frame(site = site$site,
                         heading = site$heading, 
                         distance = distances[i])
    pixels <- rbind(pixels, bathy1)
    coords <- coords2
  }
  if(nrow(pixels) < 1){
    pixels <- data.frame(site, depth = NA)
  }else{
    pixels <- pixels
  }
  return(pixels)
}

# Pixel points
site_pixels <- data.frame()
for(i in 1:length(BC_complete$site)){
  site <- BC_trans[i,]
  site_pixel <- transect.pixel(site, c(10000))
  site_pixels <- rbind(site_pixels, site_pixel)
}

