# Wind analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Creating the new bounding boxes according to Varella (2018)

# Here I analyse the wind data - specifically wind the number of SE winds blown overtime and wind duration


# 1: Setup environment ------------------------------------------------------------------------------------------------------

library(gridExtra)
library(geosphere)
library(tidyverse)
library(lubridate)
library(heatwaveR)
## devtools::install_github("robwschlegel/coastR")
library(coastR)
library(mgcv) # for gam
library(FNN)
library(broom)
library(circular)
library(grid)
library(doParallel); registerDoParallel(cores = 7)
source("functions/theme.R")
options(scipen = 999) 

supp.labs <- c("Benguela Current South", "Benguela Current North", "Chile", "Peru",
               "California Current South", "California Current North", "Canary Current")
names(supp.labs) <- c('BC_south', 'BC_north', 'HC_chile', 'HC_peru',
                      'CalC_south', 'CalC_north', 'CC')

# Then create different temporal results
load("data_official/south_BC.RData")
load("data_official/north_BC.RData")
load("data_official/Canary_current.RData")
load("data_official/chile.RData")
load("data_official/peru.RData")
load("data_official/south_CalC.RData")
load("data_official/north_CalC.RData")

current_winds <- rbind(south_BC, north_BC, Canary_current, chile, peru, south_CalC,north_CalC)

# Then create different temporal results
temp_monthly <- current_winds %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_SLP = mean(slp, na.rm = T))

temp_monthly$month <- as.factor(temp_monthly$month)
temp_monthly$month <- factor(temp_monthly$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                            "Jul", "Aug", "Sep", "Oct", "Nov"))
temp_monthly$current <- factor(temp_monthly$current, levels = c('BC_south', 'BC_north', 'HC_chile', 'HC_peru',
                                                                'CalC_south', 'CalC_north', 'CC'))
# Monthly mean temperature
# This is Figure 2 in the manuscript
plot_0 <- ggplot(data = temp_monthly, aes(x = year, y = mean_temp)) +
  geom_line(aes(colour = month), size = 0.3) +
  geom_smooth(aes(colour = month), method = "lm", size = 0.2) +
  facet_wrap(~current, ncol = 1, scales = "free", labeller = labeller(current = supp.labs)) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "SST (°C)")+
  theme_bw() +
  labs(colour = "Month") +
  theme_minimal() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=6, family = "Palatino"),
    axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 5, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 10, family = "Palatino"),
    legend.position = "right",
    legend.text = element_text(size = 9, family = "Palatino"),
    #legend.key = element_rect(size = 0.8, colour = NA),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())

# 2: Creating the new bounding boxes according to Varella (2018) ------------------------------------------------------------

# #The datasets used here were created in script "5_SLP.R"
# load("data/CC_coastal_SLP.RData")
# load("data/CalC_coastal_SLP.RData")
# load("data/BC_coastal_SLP.RData")
# load("data/HC_coastal_SLP.RData")
# 
# # Filtering out to specific regions (as defined by Varela) within each current:
# 
# # # Benguela current: North and South Benguela
# south_BC <- BC_coastal_SLP %>%
#   filter(lat < -24) %>%
#   mutate(current = "BC_south")
# 
# north_BC <- BC_coastal_SLP %>%
#   #dplyr::filter(lat < -17) %>%
#   filter(lat >= -22, lat <= -17) %>%
#   mutate(current = "BC_north")
# 
# # Canary Current
# Canary_current <- CC_coastal_SLP %>%
#   filter(lat > 21) %>%
#   mutate(current = "CC")
# 
# # Humbold current: Chile and Peru
# chile <- HC_coastal_SLP %>%
#   filter(lat >= -41 , lat <= -17) %>%
#   mutate(current = "HC_chile")
# 
# peru <- HC_coastal_SLP %>%
#   filter(lat >= -18, lat <= -13) %>%
#   mutate(current = "HC_peru")
# 
# # California current: North and South California
# south_CalC <- CalC_coastal_SLP %>%
#   filter(lat >= 34, lat <= 37) %>%
#   mutate(current = "CalC_south")
# 
# north_CalC <- CalC_coastal_SLP %>%
#   filter(lat >= 38, lat <= 43) %>%
#   mutate(current = "CalC_north")
# 
# # save(south_BC, file = "data_official/south_BC.RData")
# # save(north_BC, file = "data_official/north_BC.RData")
# # save(Canary_current, file = "data_official/Canary_current.RData")
# # save(chile, file = "data_official/chile.RData")
# # save(peru, file = "data_official/peru.RData")
# # save(south_CalC, file = "data_official/south_CalC.RData")
# # save(north_CalC, file = "data_official/north_CalC.RData")

# 3: Detect winds ---------------------------------------------------------

# Load sub-domain data
load("data_official/south_BC.RData")
load("data_official/north_BC.RData")
load("data_official/chile.RData")
load("data_official/peru.RData")
load("data_official/south_CalC.RData")
load("data_official/north_CalC.RData")
load("data_official/Canary_current.RData")

# Convenience wrapper used in the follwing pipeline for detecting wind events
detect_wind <- function(df_sub){
  res <- detect_event(df_sub, y = wind_spd, threshClim2 = df_sub$wind_deg, minDuration = 1, maxGap = 0)$event
  return(res)
}

# Pipeline for detecting wind events
# This outputs annual summary statistics for: wind event count, duration, and intensity
# These values are averaged for the number of pixels in the EBUS sub-domain
# NB: Previously this was done over multiple steps, but if the calculations are very fast one should
# rather aim to combine the calculations into a single function.
# Keep in mind that the goal of efficient code is to repeat yourself as little as possible.
# Multiple individual steps should only be prioritised when the calculations are very RAM heavy
# testers...
# df <- peru
# wind_from <- "SE"
detect_wind_pipe <- function(df, wind_from){
  # Set the range of wind angles
  if(wind_from == "SE"){
    deg_min = 90; deg_max = 180
  } else if(wind_from == "NE"){
    deg_min = 0; deg_max = 90
  } else{
    stop("Enter a correct value for 'wind_from'")
  }
  
  # Find the number of distinct pixels
  df_distinct <- df %>% 
    dplyr::select(lon, lat) %>% 
    distinct() %>% 
    nrow()
  
  # Prep data for detection
  df_prep <- df %>% 
    distinct() %>% 
    dplyr::rename(t = date) %>% 
    dplyr::select(current, lon, lat, t, season, month, wind_spd, wind_dir_from) %>% 
    mutate(seas = 0, # By manually setting seas and thresh to 0 we can use the detect_event function 
           thresh = 0, # like the exceedance function, but with the increased functionality it has
           wind_deg = case_when(wind_dir_from >= deg_min & wind_dir_from <= deg_max ~ TRUE,
                                TRUE ~ FALSE), # Check the documentation to see why this is done like this if it seems odd to you
           wind_spd = case_when(season != "Summer" ~ 0,  # This is how we constrain the analysis to Summer only
                                TRUE ~ wind_spd))
  
  # Count the number of SE wind days per year+month
  df_days <- df_prep %>% 
    mutate(year = year(t)) %>% 
    group_by(lon, lat, year, month) %>% 
    summarise(wind_days = sum(wind_deg), .groups = "drop")
  
  # Calculate the summer wind events
  event_res <- plyr::ddply(df_prep, c("current", "lon", "lat"), detect_wind, .parallel = T)
  
  # If no events are detected in a year an NA dataframe is returned
  # But when combined with the results this value forces the dates into integers
  # This is a strange bug that we correct for here
  if(is.na(min(event_res$event_no))){
    event_res <- event_res %>% 
      filter(!is.na(event_no)) %>% 
      mutate(date_start = as.Date(date_start, origin = "1970-01-01"))
  }
  
  # Find annual summary statistics
  event_summary <- event_res %>% 
    mutate(year = year(date_start), # This is somewhat problematic because December is in the for summer in the Southern Hemisphere
           month = month(date_start, label = T, abbr = T)) %>%
    left_join(df_days, by = c("lon", "lat", "year", "month")) %>% 
    group_by(current, year, month) %>% 
    summarise(event_count = n()/df_distinct,
              wind_days = mean(wind_days),
              duration_mean = mean(duration),
              intensity_mean = mean(intensity_mean),
              intensity_max = mean(intensity_max), .groups = "drop") %>% 
    mutate(month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")))
  return(event_summary)
}

# Calculate the wind events
south_BC_wind <- detect_wind_pipe(south_BC, "SE")
north_BC_wind <- detect_wind_pipe(north_BC, "SE")
chile_wind <- detect_wind_pipe(chile, "SE")
peru_wind <- detect_wind_pipe(peru, "SE")
south_CalC_wind <- detect_wind_pipe(south_CalC, "NE")
north_CalC_wind <- detect_wind_pipe(north_CalC, "NE")
Canary_current_wind <- detect_wind_pipe(Canary_current, "NE")

winds <- rbind(south_BC_wind, north_BC_wind, chile_wind, peru_wind, south_CalC_wind, north_CalC_wind, Canary_current_wind)
# save(winds, file = "data_official/winds.RData")

winds$current <- factor(winds$current, levels = c("BC_south","BC_north","HC_chile","HC_peru","CalC_south","CalC_north","CC"))
# NE_winds <- rbind(south_CalC_wind, north_CalC_wind, Canary_current_wind)

# RWS: The results you need are produced above.
# Adapt the rest of your following code for your figures accordingly.

# # Plot showing the number of SE wind events
plotA <- ggplot(data = winds, aes(x = year, y = event_count)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current, ncol = 1, scales = "free",  labeller = labeller(current = supp.labs)) +  #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "Upwelling wind events (count)")+
  theme_minimal() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=6, family = "Palatino"),
    axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 5, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 10, family = "Palatino"),
    legend.position = "right",
    legend.text = element_text(size = 9, family = "Palatino"),
    #legend.key = element_rect(size = 0.8, colour = NA),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())

# Duration
plotB <- ggplot(data = winds, aes(x = year, y = duration_mean)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current, ncol = 1, scales = "free",  labeller = labeller(current = supp.labs)) +  #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "Duration of upwelling winds (Days)")+
  theme_minimal() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=6, family = "Palatino"),
    axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 5, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 10, family = "Palatino"),
    legend.position = "right",
    legend.text = element_text(size = 9, family = "Palatino"),
    #legend.key = element_rect(size = 0.8, colour = NA),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())

# Intensity
plotC <- ggplot(data = winds, aes(x = year, y = intensity_mean)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current, ncol = 1, scales = "free",  labeller = labeller(current = supp.labs)) +  #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "Mean intensity of winds")+
  theme_minimal() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=6, family = "Palatino"),
    axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 5, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 10, family = "Palatino"),
    legend.position = "right",
    legend.text = element_text(size = 9, family = "Palatino"),
    #legend.key = element_rect(size = 0.8, colour = NA),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())

New.Fig.2 <- ggpubr::ggarrange(
  plot_0,
  plotA,
  plotB,
  plotC,
  ncol = 4,
  common.legend = TRUE,
  labels = "AUTO"
)
New.Fig.2
ggplot2::ggsave(
  "New.Fig.3.jpg",
  width = 7.0 * (1 / 3),
  height = 5.2 * (1 / 3),
  scale = 3.7
)

ggsave(filename = "New.Fig.2.jpg", plot = New.Fig.2, width=180, height = 200, units = "mm",dpi = 300,  path = "figures/")

