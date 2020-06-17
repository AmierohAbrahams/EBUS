# 4_SLP
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Match the SLP data with the SST and wind data created
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

# 2: Matching the SLP ------------------------------------------------------------------

wind_func <- function(df){
  wind <- df %>% 
    mutate(lat = lat - 0.125,
           lon = lon + 360, #Adding 360 so that it will match the temperature data. 
           lon = lon + 0.125,
           msl = msl/100) %>% # To convert Pa to hPa
    rename(date = t)
}

CC_SLP <- wind_func(df = CC_wind)
CalC_SLP <- wind_func(df = CalC_wind)
HC_SLP <- wind_func(df = HC_wind)


load("~/Documents/EBUS/data/BC_match.RData")
load("~/Documents/EBUS/data/HC_match.RData")
load("~/Documents/EBUS/data/CC_match.RData")
load("~/Documents/EBUS/data/CalC_match.RData")


match_func <- function(match_df, SLP_df){
  match <- SLP_df  %>%
    left_join(match_df, by = c("lon",  "lat", "date")) %>%
    na.trim()
  return(match)
}

# Matching the wind data with the 30yr time series OISST temperature data 
CC_match_SLP <- match_func(match_df = CC_match, wind_df = CC_wind_fin)
CalC_match_SLP <- match_func(match_df = CalC_match, wind_df = CC_wind_fin)
BC_match_SLP <- match_func(match_df = BC_match, wind_df = CC_wind_fin)
HC_match_SLP <- match_func(match_df = HC_match, wind_df = CC_wind_fin)

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

CC_wind_SLP <- wind_dir_func(df = CC_match)
#save(CC_wind, file = "data/CC_wind.RData")
CalC_wind_SLP <- wind_dir_func(df = CalC_match)
#save(CalC_wind, file = "data/CalC_wind.RData")
HC_wind_SLP <- wind_dir_func(df = HC_match)
#save(HC_wind, file = "data/HC_wind.RData")
BC_windSLP <- wind_dir_func(df = BC_match)
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
CC_complete_SLP <- seasons_N_func(CC_windc)
CalC_complete_SLP <- seasons_N_func(CalC_windc)
HC_complete_SLP <- seasons_S_func(HC_windc)
BC_complete_SLP <- seasons_S_func(BC_windc)

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

plot_func <- function(df){
  ggplot(data = df, aes(x = long, y = lat)) +
    geom_polygon(colour = "black", fill = "grey70", aes(group = group))
}

plot_func(df = CC_coastline_SLP)
plot_func(df = CalC_coastline_SLP)
plot_func(df = HC_coastline_SLP)
plot_func(df = BC_coastline_SLP)

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
CalC_coastal_coords_SLP <- unique(CalC_coords[CalC_coastal_index_SLP,])
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
# save(CC_coastal, file = "data/CC_coastal.RData")
rm(CC_complete_SLP, CC); gc()

CalC_coastal_SLP <- left_join(CalC_coastal_coords_SLP, CalC_complete_SLP, by = c("lon", "lat")) %>% 
  left_join(CalC_transects_SLP, by = c("lon", "lat"))
# save(CalC_coastal, file = "data/CalC_coastal.RData")
rm(CalC_complete_SLP, CalC); gc()

HC_coastal_SLP <- left_join(HC_coastal_coords_SLP, HC_complete_SLP, by = c("lon", "lat")) %>% 
  left_join(HC_transects_SLP, by = c("lon", "lat"))
# save(HC_coastal, file = "data/HC_coastal.RData")
rm(HC_complete, HC); gc()

BC_coastal_SLP <- left_join(BC_coastal_coords_SLP, BC_complete_SLP, by = c("lon", "lat")) %>% 
  left_join(BC_transects_SLP, by = c("lon", "lat"))
# save(BC_coastal, file = "data/BC_coastal.RData")
