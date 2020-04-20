# 2_upwelling_identification.R
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Find the coastal pixels


# 1: Setup environment ----------------------------------------------------

# Loading Libraries
library(circular)
library(gridExtra)
library(geosphere)
library(tidyverse)
library(heatwaveR)
## devtools::install_github("robwschlegel/coastR")
library(coastR)
library(FNN)
source("functions/theme.R")

# Load data

# Data loaded and created in "1_Temp_wind_data"
# load("data_complete/CC_complete.RData")
# load("data_complete/CalC_complete.RData")
# load("data_complete/HC_complete.RData")
load("data_complete/BC_complete.RData")


# 2: Find the coastal pixels ----------------------------------------------

# RWS: NB: Figures need to be made for each step below to ensure that it is behaving as expected

# Load the temperature data as the wind data has pixels over land
BC_temp <- read_csv("data_complete/BC_temp.csv", col_names = c("lon", "lat", "temp", "date"))

# Isolate the unique pixel coordinates
BC_coords <- BC_temp %>% 
  dplyr::select(lon, lat) %>% 
  unique()

# Take coastal coordinates from a global map
BC_coastline <- fortify(maps::map(xlim = c(min(BC_coords$lon, na.rm = T), 
                                           max(BC_coords$lon, na.rm = T)), 
                                  ylim = c(min(BC_coords$lat, na.rm = T), 
                                           max(BC_coords$lat, na.rm = T)), 
                                  plot = F, interior = F, fill = T, lforce = "e", map = "world"))

# Find which of the EBUS pixels match closest to the coastal pixels
BC_coastal_index <- as.vector(knnx.index(as.matrix(BC_coords[, c("lon", "lat")]),
                                         as.matrix(BC_coastline[ ,c("long", "lat")]), k = 1))
BC_coastal_coords <- unique(BC_coords[BC_coastal_index,])

# Find the coastal angle for each point
BC_transects <- transects(BC_coastal_coords, spread = 2) %>% 
  dplyr::rename(coastal_angle = heading) %>% 
  mutate(coastal_angle = round(coastal_angle))

# Bind it all together
BC_coastal <- left_join(BC_coastal_coords, BC_complete, by = c("lon", "lat")) %>% 
  left_join(BC_transects, by = c("lon", "lat"))

# Determining the upwelling index per coastal pixel
upwelling_func <- function(df){
  UI <- df %>%  
    mutate(ui = wind_spd * (cos(wind_dir_from - coastal_angle)),
           ui_TF = ifelse(ui > 0, TRUE, FALSE)) #%>%
    # drop_na()
}

BC_UI <- upwelling_func(df = BC_coastal)

# The custom function for detecting upwelling and extracting only the metrics
detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$ui_TF, minDuration = 1, coldSpells = T) # I thought the min duration was 1 day, not 3?
  return(res)
}

# Calculate the upwelling event metrics
BC_upwell_base <- BC_coastal %>% 
  dplyr::rename(t = date) %>% 
  group_by(lon, lat) %>% 
  nest() %>% 
  mutate(clim = purrr::map(data, ts2clm, pctile = 25, climatologyPeriod = c("1982-01-01", "2011-12-31")), 
         exceed = purrr::map(clim, detect_event_custom))# %>%
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

