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
# NB: Previously this was done over multiple steps, but if the calculations are very fast one should
# rather aim to combine the calculations into a single function.
# Keep in mind that the goal of efficient code is to repeat as little as possible.
# Multiple individual steps should only be prioritised when the calculations are very RAM heavy
# testers...
# df <- north_BC
# wind_from <- "SE"
detect_wind_pipe <- function(df, wind_from){
  # Set the range of wind angles
  if(wind_from == "SE"){
    deg_min = 90; deg_max = 180
  } else if(wind_from == "NE"){
    deg_min = 0; deg_max = 90
  }
  
  # Find the number of distinct pixels
  df_distinct <- df %>% 
    dplyr::select(lon, lat) %>% 
    distinct() %>% 
    nrow()
  
  # Prep data for detection
  df_prep <- df %>% 
    # filter(season == "Summer") %>% # This is how we constrain the analysis to SUmmer only
    dplyr::rename(t = date) %>% 
    dplyr::select(lon, lat, t, season, wind_spd, wind_dir_from) %>% 
    mutate(seas = 0, # By manually setting seas and thresh to 0 we can use the detect_event function 
           thresh = 0, # like the exceedance function, but with the increased functionality it has
           wind_deg = case_when(wind_dir_from >= deg_min & wind_dir_from <= deg_max ~ TRUE,
                                TRUE ~ FALSE), # Check the documentation to see why this is done like this if it seems odd to you
           wind_spd = case_when(season != "Summer" ~ 0,
                                TRUE ~ wind_spd))
  
  # Calculate the summer wind events
  df_sub <- df_prep %>% 
    filter(lon == lon[1], lat == lat[1])
  
  res <- detect_event(df_sub, y = wind_spd, threshClim2 = df_sub$wind_deg, minDuration = 1, maxGap = 0)$event
}

detect_wind <- function(df){
  dur <- df %>% 
    rename(temp = wind_dir_from,
           t = date) %>% 
    select(t, temp)
}

BC_S_dur <- dur_prep(df = BC_S_prep)
BC_N_dur <- dur_prep(df = BC_N_prep)
chile_dur <- dur_prep(df = chile_prep) %>% 
  arrange(t)
peru_dur <- dur_prep(df = peru_prep) %>% 
  arrange(t)


exc_BC_S <- exceedance(BC_S_dur, minDuration = 1, threshold = 0)
exc_BC_N <- exceedance(BC_N_dur, minDuration = 1, threshold = 0)
exc_chile <- exceedance(chile_dur, minDuration = 1, threshold = 0)
exc_peru <- exceedance(peru_dur, minDuration = 1, threshold = 0)

wind_func <- function(df){
  wind_duration <- df$exceedance %>%
    ungroup() %>%
    select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) 
}

BC_S_dur <- wind_func(df = exc_BC_S)
BC_N_dur <- wind_func(df = exc_BC_N)
chile_dur <- wind_func(df = exc_chile)
peru_dur <- wind_func(df = exc_peru)

# Divide by the pixels
# south_BC_SE <- BC_S_dur %>% 
#   mutate(dur_SE = duration/nrow(BC_S_pixels)) 

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

# Changes in the duration of SE winds? ---------------------------------------------------------------------------------

# load("data_official/south_BC.RData")
# load("data_official/north_BC.RData")
# load("data_official/Canary_current.RData")
# load("data_official/chile.RData")
# load("data_official/peru.RData")
# load("data_official/south_CalC.RData")
# load("data_official/north_CalC.RData")

# RWS: Why was this being done as a separate step?
# wind_dur_func <- function(df){
#   wind <- df %>% 
#     select(date, wind_dir_from, lat, lon) %>% 
#     rename(date = date,
#            wind_se = wind_dir_from)
# }
# 
# BC_south_wind_dur <- wind_dur_func(df = south_BC)
# BC_north_wind_dur <- wind_dur_func(df = north_BC)
# CC_wind_dur <- wind_dur_func(df = Canary_current)
# CalC_south_wind_dur <- wind_dur_func(df = south_CalC)
# CalC_north_wind_dur <- wind_dur_func(df = north_CalC)
# chile_wind_dur <- wind_dur_func(df = chile)
# peru_wind_dur <- wind_dur_func(df = peru)

# Southern Hemisphere -----------------------------------------------------
# not sure what's happening here? Should you not first calculate the number of days with
# wind that blows from the range of directions across ALL THE PIXELS and then divide by pixels

# SE_wind_func <- function(df){
#   SE <- df %>% 
#     filter(wind_dir_from >= 90, wind_dir_from <= 180) %>% 
#     select(lat, lon, date, wind_dir_from, wind_spd) %>% 
#     distinct()
# }

# BC_S_SE <- SE_wind_func(df = south_BC)
# BC_N_SE <- SE_wind_func(df = north_BC)
# chile_SE <- SE_wind_func(df = chile)
# peru_SE <- SE_wind_func(df = peru)
# 
# BC_S_prep <- right_join(BC_S_SE, BC_south_wind_dur)
# BC_S_prep[is.na(BC_S_prep)] <- 0
# 
# BC_N_prep <- right_join(BC_N_SE, BC_north_wind_dur)
# BC_N_prep[is.na(BC_N_prep)] <- 0
# 
# chile_prep <- right_join(chile_SE, chile_wind_dur)
# chile_prep[is.na(chile_prep)] <- 0
# 
# peru_prep <- right_join(peru_SE, peru_wind_dur)
# peru_prep[is.na(peru_prep)] <- 0


seasons_S_func <- function(df){
  df_seasons <- df %>% 
    mutate(month = month(date_start, abbr = T, label = T),
           year = year(date_start)) %>% 
    mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Summer", 
                              month %in% c("Mar", "Apr", "May") ~ "Autumn",
                              month %in% c("Jun", "Jul", "Aug") ~ "Winter",
                              month %in% c("Sep", "Oct", "Nov") ~ "Spring"))
}

BC_S_wind<- seasons_S_func(df = south_BC_SE)
BC_S_wind <- BC_S_wind %>% 
  mutate(current = "BC_S")
BC_N_wind <- seasons_S_func(df = north_BC_SE)
BC_N_wind <- BC_N_wind %>% 
  mutate(current = "BC_N")
Chile_wind <- seasons_S_func(df = Chile_SE)
Chile_wind <- Chile_wind %>% 
  mutate(current = "Chile")
Peru_wind <- seasons_S_func(df = Peru_SE)
Peru_wind <- Peru_wind %>% 
  mutate(current = "Peru")

duration_wind_S <- rbind(BC_S_wind,BC_N_wind,Chile_wind,Peru_wind)


wind_currents <- duration_wind_S %>% 
  filter(season == "Summer") %>% 
  group_by(year, month, current) %>% 
  summarise(mean_dur = mean(duration))

wind_currents <- as.data.frame(wind_currents)
wind_currents$month <- as.factor(wind_currents$month)
wind_currents$month <- factor(wind_currents$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
# wind_currents$current = factor(wind_currents$current, levels=c('BC','HC','CalC','CC'))

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

number_wind_events <- duration_wind_S %>% 
  filter(season == "Summer") %>% 
  group_by(year, current, month) %>% 
  summarise(count = as.numeric(n())) %>% 
  rename(SE_count = count)

ggplot(data = number_wind_events, aes(x = year, y = SE_count)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current) #,  labeller = labeller(current = supp.labs), ncol = 4) +
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

# Wind intensity ----------------------------------------------------------
# Southern Hemisphere
load("data_official/south_BC.RData")
load("data_official/north_BC.RData")
load("data_official/chile.RData")
load("data_official/peru.RData")

current_S_winds <- rbind(south_BC, north_BC, chile,peru)

SE_winds <- current_S_winds %>% 
  filter(wind_dir_from >= 90, wind_dir_from <= 180) %>% 
  distinct()

# To get seasons and months
SE_monthly <- SE_winds %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(mean_wspd = mean(wind_spd))

SE_monthly$month <- as.factor(SE_monthly$month)
SE_monthly$month <- factor(SE_monthly$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                                        "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
# SE_monthly$current = factor(SE_monthly$current, levels=c('BC_south', 'BC_north', 'HC_chile','HC_peru'))

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
