# 2_upwelling_identification.R
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Find the coastal pixels
# 3: Calculate upwelling and the metrics

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

# Load data

# Data loaded and created in "1_Temp_wind_data"
# load("data_complete/CC_complete.RData")
# load("data_complete/CalC_complete.RData")
# load("data_complete/HC_complete.RData")
load("data_complete/BC_complete.RData")

# Load the temperature data as the wind data has pixels over land
BC_temp <- read_csv("data_complete/BC_temp.csv", col_names = c("lon", "lat", "temp", "date"))


# 2: Find the coastal pixels ----------------------------------------------

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


ggplot(data = BC_coastline, aes(x = long, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group))


# Find which of the EBUS pixels match closest to the coastal pixels
BC_coastal_index <- as.vector(knnx.index(as.matrix(BC_coords[, c("lon", "lat")]),
                                         as.matrix(BC_coastline[ ,c("long", "lat")]), k = 1))
BC_coastal_coords <- unique(BC_coords[BC_coastal_index,])

# Find the coastal angle for each point
BC_transects <- transects(BC_coastal_coords, spread = 2, alongshore = T) %>% 
  dplyr::rename(coastal_angle = heading) %>% 
  mutate(coastal_angle = round(coastal_angle),
         coastal_angle = coastal_angle+180,
         coastal_angle = ifelse(coastal_angle > 360, coastal_angle-360, coastal_angle))

# The mean coastal angle for BC
mean(BC_transects$coastal_angle) # 155
# Bind it all together
BC_coastal <- left_join(BC_coastal_coords, BC_complete, by = c("lon", "lat")) %>% 
  left_join(BC_transects, by = c("lon", "lat"))
rm(BC_complete, BC_temp); gc()

# Test visual of coastal headings

# Plotting the map
world_map <- ggplot() + 
  borders(fill = "grey40", colour = "black")

# Plotting function
plot_sites <- function(site_list, buffer, dist){
  
  # Find the point 200 km from the site manually to pass to ggplot
  heading2 <- data.frame(geosphere::destPoint(p = select(site_list, lon, lat),
                                              b = site_list$coastal_angle, d = dist))
  #
  # Add the new coordinates tot he site list
  site_list <- site_list %>%
    mutate(lon_dest = heading2$lon,
           lat_dest = heading2$lat)
  #
  # Visualise
  world_map +
    geom_segment(data = site_list, colour = "red4",
                 aes(x = lon, y = lat, xend = lon_dest, yend = lat_dest)) +
    geom_point(data = site_list, size = 3, colour = "black", aes(x = lon, y = lat)) +
    geom_point(data = site_list, size = 3, colour = "red", aes(x = lon_dest, y = lat_dest)) +
    coord_quickmap(xlim = c(min(site_list$lon - buffer),
                            max(site_list$lon + buffer)),
                   ylim = c(min(site_list$lat - buffer),
                            max(site_list$lat + buffer))) +
    labs(x = "", y = "", colour = "Site\norder")
}

# Shore normal transects
BC_coastal_unique <- BC_coastal %>% 
  dplyr::select(lon, lat, coastal_angle) %>% 
  unique()
BC_along <- plot_sites(BC_coastal_unique, 1, 100000)
BC_along

# Pixel closes to the coastline
ggplot(data = BC_coastal, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = temp)) 


# 3: Calculate upwelling and the metrics ----------------------------------

# Determining the upwelling index per coastal pixel
upwelling_func <- function(df){
  UI <- df %>%
    mutate(ui = wind_spd * (cos(deg_rad(wind_dir_from - coastal_angle))),
           ui_TF = ifelse(ui > 0, TRUE, FALSE)) #%>%
    # drop_na()
}

BC_UI <- upwelling_func(df = BC_coastal) %>% 
  dplyr::rename(t = date)

# The custom function for detecting upwelling and extracting only the metrics
detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$ui_TF, minDuration = 1, coldSpells = T)$event # I thought the min duration was 1 day, not 3?
  return(res)
}

# Calculate the upwelling event metrics
BC_clim <- BC_UI %>% 
  group_by(lon, lat) %>% 
  nest() %>% 
  mutate(clim = purrr::map(data, ts2clm, pctile = 25, climatologyPeriod = c("1982-01-01", "2011-12-31"))) %>% 
  select(-data) %>%
  unnest(cols = clim) %>% 
  ungroup()

# Calculate the upwelling metrics
BC_UI_metrics <- BC_UI %>% 
  left_join(BC_clim, by = c("lon", "lat", "t", "temp")) %>% 
  group_by(lon, lat) %>% 
  nest() %>% 
  mutate(event = purrr::map(data, detect_event_custom)) %>% 
  select(-data) %>%
  unnest(cols = event) %>% 
  ungroup()

# Save
# save(BC_UI_metrics, file = "data_complete/BC_UI_metrics.RData")

