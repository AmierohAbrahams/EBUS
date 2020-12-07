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
# df <- south_CalC
# wind_from <- "NE"
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
    # filter(season == "Summer") %>%
    dplyr::rename(t = date) %>% 
    dplyr::select(current, lon, lat, t, season, month, wind_spd, wind_dir_from) %>% 
    mutate(seas = 0, # By manually setting seas and thresh to 0 we can use the detect_event function 
           thresh = 0, # like the exceedance function, but with the increased functionality it has
           wind_deg = case_when(wind_dir_from >= deg_min & wind_dir_from <= deg_max ~ TRUE,
                                TRUE ~ FALSE), # Check the documentation to see why this is done like this if it seems odd to you
           wind_spd = case_when(season != "Summer" ~ 0,  # This is how we constrain the analysis to Summer only
                                TRUE ~ wind_spd))
  
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
    group_by(year, month) %>% 
    summarise(event_count = n()/df_distinct,
              wind_days = sum(duration)/df_distinct,
              duration_mean = mean(duration),
              intensity_mean = mean(intensity_mean),
              intensity_max = mean(intensity_max), .groups = "drop") %>% 
    mutate(month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")))
  return(event_summary)
}

# Calculate the wind events
# Note that the count of wind days per month may be more than 30 because the calculation is seeing 
# how long the events are on average that start in a given month
# So if events start in June, but then last 40 days, these 40 days will be attributed to June even though the event is ending in July
south_BC_wind <- detect_wind_pipe(south_BC, "SE")
north_BC_wind <- detect_wind_pipe(north_BC, "SE")
chile_wind <- detect_wind_pipe(chile, "SE")
peru_wind <- detect_wind_pipe(peru, "SE")
south_CalC_wind <- detect_wind_pipe(south_CalC, "NE") # This one is funny because there are almost never NE winds. You need to look into why this is.
north_CalC_wind <- detect_wind_pipe(north_CalC, "NE")
Canary_current_wind <- detect_wind_pipe(Canary_current, "NE")


# RWS: The results you need are produced above.
# Adapt the rest of your following code for your figures accordingly.

# # Plot showing the number of SE wind events
# plotA <- ggplot(data = SE_winds, aes(x = year, y = no_SE)) +
#   geom_line(aes(colour = month)) +
#   geom_smooth(aes(colour = month), method = "lm") +
#   facet_wrap(~current, ncol = 2) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
#   labs(x = "", y = "SE wind events (count)")+
#   theme_bw() +
#   labs(colour = "Month") +
#   theme_set(theme_grey()) +
#   theme_grey() +
#   theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
#     # panel.grid.major = element_line(size = 0.2, linetype = 2),
#     # panel.grid.minor = element_line(colour = NA),
#     strip.text = element_text(size=8, family = "Palatino"),
#     axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
#     axis.ticks.length = unit(0.2, "cm"),
#     panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
#     panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
#     axis.text = element_text(size = 8, colour = "black", family = "Palatino"),
#     plot.title = element_text(size = 15, hjust = 0),
#     legend.title = element_text(size = 10, family = "Palatino"),
#     legend.text = element_text(size = 9, family = "Palatino"),
#     # legend.key = element_rect(size = 0.8, colour = NA),
#     legend.key.size = unit(0.2, "cm"),
#     legend.background = element_blank())
# 
# 
#
#
# plotB <- ggplot(data = NE_winds, aes(x = year, y = no_SE)) +
#   geom_line(aes(colour = month)) +
#   geom_smooth(aes(colour = month), method = "lm") +
#   facet_wrap(~current, ncol = 1) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
#   labs(x = "", y = "NE wind events 
# (count)")+
#   theme_bw() +
#   labs(colour = "Month") +
#   theme_set(theme_grey()) +
#   theme_grey() +
#   theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
#     # panel.grid.major = element_line(size = 0.2, linetype = 2),
#     # panel.grid.minor = element_line(colour = NA),
#     strip.text = element_text(size=8, family = "Palatino"),
#     axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
#     axis.ticks.length = unit(0.2, "cm"),
#     panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
#     panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
#     axis.text = element_text(size = 8, colour = "black", family = "Palatino"),
#     plot.title = element_text(size = 15, hjust = 0),
#     legend.title = element_text(size = 10, family = "Palatino"),
#     legend.text = element_text(size = 9, family = "Palatino"),
#     # legend.key = element_rect(size = 0.8, colour = NA),
#     legend.key.size = unit(0.2, "cm"),
#     legend.background = element_blank())

plotB <- ggplot(data = wind_currents, aes(x = year, y = mean_dur)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current) #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "Duration of SE winds (Days)") +
  theme_bw() +
  labs(colour = "Month") +
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 20, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 20, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 15, hjust = 0),
    legend.title = element_text(size = 14, family = "Palatino"),
    legend.text = element_text(size = 9, family = "Palatino"),
    legend.key = element_rect(size = 1, colour = NA),
    legend.key.size = unit(0.8, "cm"),
    legend.background = element_blank())
  
  
# To observe changes in the duration, intensity and the count of upwelling favourable wind events in the southern and 
# northern hemisphere, we made use of the exceedance() function in the heatwaveR package, this function detects the consecutive days in exceedance of a given threshold.  
  
# Change in number of SE wind events over 27 yrs

ggplot(data = number_wind_events, aes(x = year, y = SE_count)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current) +#,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "SE wind event (count)") +
  theme_bw() +
  labs(colour = "Month") +
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 20, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 20, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 15, hjust = 0),
    legend.title = element_text(size = 14, family = "Palatino"),
    legend.text = element_text(size = 9, family = "Palatino"),
    legend.key = element_rect(size = 1, colour = NA),
    legend.key.size = unit(0.8, "cm"),
    legend.background = element_blank())


# Wind intensity
plotD <- ggplot(data = SE_monthly, aes(x = year, y = mean_wspd, colour = Month)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current, ncol = 4) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "SE wind intensity") +
  # (m.s^-1)") +
  # geom_smooth(aes(colour = month), method = "lm", se=FALSE, formula = my.formula) +
  # stat_poly_eq(formula = my.formula,
  #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
  #              parse = TRUE) +
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 20, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 20, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 15, hjust = 0),
    legend.title = element_text(size = 14, family = "Palatino"),
    legend.text = element_text(size = 9, family = "Palatino"),
    legend.key = element_rect(size = 1, colour = NA),
    legend.key.size = unit(0.8, "cm"),
    legend.background = element_blank())
