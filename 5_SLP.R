# 5_SLP
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Match the SLP data with the SST and wind data (This is the same as script 1 and 2 but matching the wind and temp data to the msl data here)
# 3: Determine SLP trends

# 1: Setup environment ----------------------------------------------------

# Loading Libraries
# library(circular)
library(gridExtra)
library(geosphere)
library(tidyverse)
library(heatwaveR)
## devtools::install_github("robwschlegel/coastR")
library(coastR)
library(FNN)
source("functions/theme.R")
source("functions/earthdist.R")
options(scipen=999)

# 2: Matching the SLP ------------------------------------------------------------------
load("data/SLP_BC.RData")  # Extracted from netCDF, see netCDF2CSV.R scipt in the Data_extraction folder
load("data/BC_match.RData") # Created in script 1_Temp_wind_data.R

# Rename date column for matching
date_func <- function(df){
  SLP <- df %>% 
   rename(date = t)
  return(SLP)
}

SLP_BC<- match_func(df = SLP_BC) 
SLP_CC<- match_func(df = SLP_CC) 
SLP_CalC<- match_func(df = SLP_CalC) 
SLP_HC<- match_func(df = SLP_HC) 

# Function for joining low-res and high-res data
match_func <- function(match_df, SLP_df){

  # NB: Finding the high-res unique coords in small steps to save RAM
  match_lon <- unique(match_df$lon)
  match_lat <- unique(match_df$lat)
  match_coords <- expand.grid(match_lon, match_lat) %>% 
    dplyr::rename(lon = Var1, lat = Var2)
  
  # Get the unique coordinates from the low-res data
  SLP_coords <- SLP_df %>%
    dplyr::select(lon, lat) %>%
    unique() %>%
    mutate(row_index = 1:n())
  
  # Find the high-res pixels that match to the low-res data
  match_coords_SLP <- match_coords %>%
    mutate(row_index = as.vector(knnx.index(as.matrix(SLP_coords[,c("lon", "lat")]),
                                            as.matrix(match_coords[,c("lon", "lat")]), k = 1))) %>%
    left_join(SLP_coords, by = "row_index") %>%
    dplyr::rename(lon = lon.x, lat = lat.x, lon_SLP = lon.y, lat_SLP = lat.y)
  
  # Join and exit
  match <- match_df  %>%
    left_join(match_coords_SLP, by = c("lon", "lat")) %>% 
    left_join(SLP_df, by = c("lon_SLP" = "lon",  "lat_SLP" = "lat", "date")) %>% 
    dplyr::select(-row_index) # This column is no longer necessary
  gc()
  return(match)
}

# Match up all of the data
BC_match_SLP <- match_func(match_df = BC_match, SLP_df = SLP_BC) 
rm(BC_match, SLP_BC); gc()
CC_match_SLP <- match_func(match_df = CC_match, SLP_df = SLP_CC) 
rm(CC_match, SLP_CC); gc()
CalC_match_SLP <- match_func(match_df = CalC_match, SLP_df = SLP_CalC) 
rm(CalC_match, SLP_CalC); gc()
HC_match_SLP <- match_func(match_df = HC_match, SLP_df = SLP_HC) 
rm(HC_match, SLP_HC); gc()

#save(BC_match_SLP, file = "data/BC_match_SLP.RData")
#save(CC_match_SLP, file = "data/CC_match_SLP.RData")
#save(CalC_match_SLP, file = "data/CalC_match_SLP.RData")
#save(HC_match_SLP, file = "data/HC_match_SLP.RData")

# Calculate wind speed and direction
load("data/CC_match_SLP.RData")
load("data/HC_match_SLP.RData")
load("data/CalC_match_SLP.RData")
load("data/BC_match_SLP.RData")


wind_dir_func <- function(df){
  wind_dir <- df %>% 
    mutate(wind_spd = round(sqrt(u10^2 + v10^2), 2),
           wind_dir_from = round((270-(atan2(v10, u10)*(180/pi)))%%360),
           wind_dir_to = ifelse(wind_dir_from >= 180, wind_dir_from-180, wind_dir_from+180))
}

CC_wind_SLP <- wind_dir_func(df = CC_match_SLP)
#save(CC_wind_SLP, file = "data/CC_wind_SLP.RData")
CalC_wind_SLP <- wind_dir_func(df = CalC_match_SLP)
#save(CalC_wind_SLP, file = "data/CalC_wind_SLP.RData")
HC_wind_SLP <- wind_dir_func(df = HC_match_SLP)
#save(HC_wind_SLP, file = "data/HC_wind_SLP.RData")
BC_wind_SLP <- wind_dir_func(df = BC_match_SLP)
# save(BC_wind_SLP, file = "data/BC_wind_SLP.RData")

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
CC_complete_SLP <- seasons_N_func(CC_wind_SLP)
CalC_complete_SLP <- seasons_N_func(CalC_wind_SLP)
HC_complete_SLP <- seasons_S_func(HC_wind_SLP)
BC_complete_SLP <- seasons_S_func(BC_wind_SLP)

# save(HC_complete_SLP, file = "data/HC_complete_SLP.RData")
# save(CC_complete_SLP, file = "data/CC_complete_SLP.RData")
# save(CalC_complete_SLP, file = "data/CalC_complete_SLP.RData")
# save(BC_complete_SLP, file = "data/BC_complete_SLP.RData")

# Load data
# Data loaded here was created in "1_Temp_wind_data.R"
load("data/CC_complete_SLP.RData")
load("data/CalC_complete_SLP.RData")
load("data/HC_complete_SLP.RData")
load("data/BC_complete_SLP.RData")

CC_complete_SLP <- CC_complete_SLP %>% 
  mutate(lon = lon - 360)

CalC_complete_SLP <- CalC_complete_SLP %>% 
  mutate(lon = lon - 360)

HC_complete_SLP <- HC_complete_SLP %>% 
  mutate(lon = lon - 360)

# Loading the temperature data this is the OISST data extracted to the regions (See netCDF2CSVscript in the data extraction folder)
load("~/Documents/EBUS/data/BC.RData")
load("~/Documents/EBUS/data/HC.RData")
load("~/Documents/EBUS/data/CC.RData")
load("~/Documents/EBUS/data/CalC.RData")

# 2: Find the coastal pixels ----------------------------------------------
# Isolate the unique pixel coordinates

coord_func <- function(df){
  coords <- df %>% 
    dplyr::select(lon, lat) %>%
    mutate(lon = lon - 360) %>% 
    unique()
}

CC_coords_SLP <- coord_func(df = CC)
CalC_coords_SLP <- coord_func(df = CalC)
HC_coords_SLP <- coord_func(df = HC)

BC_coords_SLP <- BC %>% 
  dplyr::select(lon, lat) %>% 
  unique()

# Take coastal coordinates from a global map
coastline_func <- function(df){
  coastline <- fortify(maps::map(xlim = c(min(df$lon, na.rm = T), 
                                          max(df$lon, na.rm = T)), 
                                 ylim = c(min(df$lat, na.rm = T), 
                                          max(df$lat, na.rm = T)), 
                                 plot = F, interior = F, fill = T, lforce = "e", map = "world"))
}

CC_coastline_SLP <- coastline_func(df = CC_coords_SLP)
CalC_coastline_SLP <- coastline_func(df = CalC_coords_SLP)
HC_coastline_SLP <- coastline_func(df = HC_coords_SLP)
BC_coastline_SLP <- coastline_func(df = BC_coords_SLP)


# Find which of the EBUS pixels match closest to the coastal pixels

coastal_index_func <- function(coord_df,coastline_df){
  BC_coastal_index <- as.vector(knnx.index(as.matrix(coord_df[, c("lon", "lat")]),
                                           as.matrix(coastline_df[ ,c("long", "lat")]), k = 1))
}

CC_coastal_index_SLP <- coastal_index_func(coord_df = CC_coords_SLP, coastline_df = CC_coastline_SLP)
CalC_coastal_index_SLP <- coastal_index_func(coord_df = CalC_coords_SLP, coastline_df = CalC_coastline_SLP)
HC_coastal_index_SLP <- coastal_index_func(coord_df = HC_coords_SLP, coastline_df = HC_coastline_SLP)
BC_coastal_index_SLP <- coastal_index_func(coord_df = BC_coords_SLP, coastline_df = BC_coastline_SLP)

BC_coastal_coords_SLP <- unique(BC_coords[BC_coastal_index_SLP,])
CC_coastal_coords_SLP <- unique(CC_coords[CC_coastal_index_SLP,])
CalC_coastal_coords_SLP <- unique(CalC_coords_SLP[CalC_coastal_index_SLP,])
HC_coastal_coords_SLP <- unique(HC_coords[HC_coastal_index_SLP,])

# Find the coastal angle for each point

BC_transects_SLP <- transects(BC_coastal_coords_SLP, spread = 2, alongshore = T) %>% 
  dplyr::rename(coastal_angle = heading) %>% 
  mutate(coastal_angle = round(coastal_angle),
         coastal_angle = coastal_angle+180,
         coastal_angle = ifelse(coastal_angle > 360, coastal_angle-360, coastal_angle))

transect_func <- function(df){
  transects <- transects(df, spread = 2) %>% 
    dplyr::rename(coastal_angle = heading) %>% 
    mutate(coastal_angle = round(coastal_angle))
}

CC_transects_SLP <- transect_func(df = CC_coastal_coords_SLP)
CalC_transects_SLP <- transect_func(df = CalC_coastal_coords_SLP)
HC_transects_SLP <- transect_func(df = HC_coastal_coords_SLP)

# Bind it all together

CC_coastal_SLP <- left_join(CC_coastal_coords_SLP, CC_complete_SLP, by = c("lon", "lat")) %>% 
  left_join(CC_transects_SLP, by = c("lon", "lat"))
# save(CC_coastal_SLP, file = "data/CC_coastal_SLP.RData")
rm(CC_complete_SLP, CC); gc()

CalC_coastal_SLP <- left_join(CalC_coastal_coords_SLP, CalC_complete_SLP, by = c("lon", "lat")) %>% 
  left_join(CalC_transects_SLP, by = c("lon", "lat"))
# save(CalC_coastal_SLP, file = "data/CalC_coastal_SLP.RData")
rm(CalC_complete_SLP, CalC); gc()


HC_coastal_SLP <- left_join(HC_coastal_coords_SLP, HC_complete_SLP, by = c("lon", "lat")) %>% 
  left_join(HC_transects_SLP, by = c("lon", "lat"))
# save(HC_coastal_SLP, file = "data/HC_coastal_SLP.RData")
rm(HC_complete, HC); gc()

BC_coastal_SLP <- left_join(BC_coastal_coords_SLP, BC_complete_SLP, by = c("lon", "lat")) %>% 
  left_join(BC_transects_SLP, by = c("lon", "lat"))
# save(BC_coastal_SLP, file = "data/BC_coastal_SLP.RData")

# Calculating SLP Gradient (SLPG)
# The pressure gradient can be determined mathematically by taking the difference in pressure between two locations (in Pascals) 
# and dividing it by the distance between the two locations (in meters).

# Determining distance
load("data/CalC_coastal_SLP.RData")

CalC_coastal_SLP_prep <- CalC_coastal_SLP %>% 
  mutate(site = 1:nrow(.),
         lon = deg2rad(lon),
         lat = deg2rad(lat)) %>% 
  select(site, lon, lat)

CalC_coastal_SLP_dist <- as.data.frame(round(PairsDists(CalC_coastal_SLP_prep), 2))
  dplyr::rename(dist = V1) %>%
  select(lon, lat, dist) %>% 
  mutate(cum_dist = cumsum(dist)) %>% 
  mutate(dist = lag(dist),
         cum_dist = lag(cum_dist))
  
# Determining sea level pressure gradient
# Difference in p1-p2/distance



