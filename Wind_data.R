# Wind analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Creating the new bounding boxes according to Varella (2018)
# 3: ANOVA analyses
# 4: Linear models

# Here I analyse the wind data - specifically wind the number of SE winds blown overtime and wind duration
# 1: Setup environment ------------------------------------------------------------
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
source("functions/theme.R")
options(scipen=999) 

# 2: Creating the new bounding boxes according to Varella (2018) -------------------

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
# 


# 3: Identifying South easterly winds (SE)----------------------------
# Then create different temporal results
load("data_official/south_BC.RData")
load("data_official/north_BC.RData")
load("data_official/Canary_current.RData")
load("data_official/chile.RData")
load("data_official/peru.RData")
load("data_official/south_CalC.RData")
load("data_official/north_CalC.RData")

current_winds <- rbind(south_BC, north_BC, Canary_current, chile, peru, south_CalC, north_CalC)
# rm(south_BC, north_BC, Canary_current, chile, peru, south_CalC, north_CalC); gc()
# save(current_winds, file = "data_official/current_winds.RData")

# First filter out only the SE (South easterly) winds
SE_winds <- current_winds %>% 
  filter(wind_dir_from >= 180, wind_dir_from <= 270) %>% 
  unique()
rm(current_winds); gc()
#save(SE_renamed, file = "data/SE_renamed.RData")

# Then create different temporal results
# This is done to check if there are differences in the number of SE blown winds over time ... 
# funny, never thought of winds being discrete events! 
SE_monthly <- SE_winds %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            circ_dir = mean.circular(circular(wind_dir_from, units = "degrees")),
            circ_wspd = mean.circular(circular(wind_spd, units = "degrees")),
            mean_temp = mean(temp, na.rm = T),
            mean_SLP = mean(slp, na.rm = T))

# Determining the number of pixels within each current ----------
  
# BC_S_pixels <- SE_winds %>%
#   filter(current == "BC_south") %>%
#   dplyr::select(lon, lat) %>%
#   unique()# 45 pixels

# BC_N_pixels <- SE_winds %>%
#   filter(current == "BC_north") %>%
#   dplyr::select(lon, lat) %>%
#   unique() # 16 pixels

# CC_pixels <- SE_winds %>%
#   filter(current == "CC") %>%
#   dplyr::select(lon, lat) %>%
#   unique() # 82 pixels

# HC_pixels <- SE_winds %>%
#   filter(current == "HC_chile") %>%
#   dplyr::select(lon, lat) %>%
#   unique() # 99 pixels

# HC_pixels <- SE_winds %>%
#   filter(current == "HC_peru") %>%
#   dplyr::select(lon, lat) %>%
#   unique() # 25 pixels

# CalC_pixels <- SE_winds %>%
#   filter(current == "CalC_south") %>%
#   dplyr::select(lon, lat) %>%
#   unique() #24 pixels

# CalC_pixels <- SE_winds %>%
#   filter(current == "CalC_north") %>%
#   dplyr::select(lon, lat) %>%
#   unique() # 20 pixels

# Changes in the number of SE wind blown 
south_BC_SE <- SE_monthly %>% 
  filter(current == "BC_south") %>% 
  mutate(no_SE = count/45) # The value here is the number of pixels occuring within this region

north_BC_SE <- SE_monthly %>% 
  filter(current == "BC_north") %>% 
  mutate(no_SE = count/16)

CC_SE <- SE_monthly %>% 
  filter(current == "CC") %>% 
  mutate(no_SE = count/82)

south_CalC <- SE_monthly %>% 
  filter(current == "CalC_south") %>% 
  mutate(no_SE = count/24)

north_CalC <- SE_monthly %>% 
  filter(current == "CalC_north") %>% 
  mutate(no_SE = count/20)

Peru_SE <- SE_monthly %>% 
  filter(current == "HC_peru") %>% 
  mutate(no_SE = count/25)

Chile_SE <- SE_monthly %>% 
  filter(current == "HC_chile") %>% 
  mutate(no_SE = count/99)

SE_winds <- rbind(south_BC_SE, north_BC_SE, CC_SE, south_CalC, north_CalC, Peru_SE, Chile_SE)

# Plot showing the number of SE winds blown
plotA <- ggplot(data = SE_winds, aes(x = year, y = no_SE)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "SE wind events (count)") +
  theme_bw() +
  labs(colour = "Month") +
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=8, family = "Palatino"),
    axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 8, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 15, hjust = 0),
    legend.title = element_text(size = 10, family = "Palatino"),
    legend.text = element_text(size = 9, family = "Palatino"),
    # legend.key = element_rect(size = 0.8, colour = NA),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())

# Determining if there are changes in the duration of SE winds blown over the years

wind_dur_func <- function(df) {
  wind<- df %>% 
    select(date, wind_dir_from, lat, lon) %>% 
    rename(date = date,
           wind_se = wind_dir_from)
}

BC_south_wind_dur <- wind_dur_func(df = south_BC)
BC_north_wind_dur <- wind_dur_func(df = north_BC)
CC_wind_dur <- wind_dur_func(df = Canary_current)
CalC_south_wind_dur <- wind_dur_func(df = south_CalC)
CalC_north_wind_dur <- wind_dur_func(df = north_CalC)
chile_wind_dur <- wind_dur_func(df = chile)
peru_wind_dur <- wind_dur_func(df = peru)

SE_wind_func <- function(df) {
  SE<- df %>% 
    filter(wind_dir_from >= 180, wind_dir_from <= 270) %>% 
    unique() %>% 
    select(date,wind_dir_from,lat,lon)
}

BC_south_SE <- SE_wind_func(df = south_BC)
BC_north_SE <- SE_wind_func(df = north_BC)
CC_SE <- SE_wind_func(df = Canary_current)
CalC_south_SE <- SE_wind_func(df = south_CalC)
CalC_north_SE <- SE_wind_func(df = north_CalC)
chile_SE<- SE_wind_func(df = chile)
peru_SE <- SE_wind_func(df = peru)

BC_south_prep <- right_join(BC_south_SE, BC_south_wind_dur)
BC_south_prep[is.na(BC_south_prep)] <- 0

BC_north_prep <- right_join(BC_north_SE, BC_north_wind_dur)
BC_north_prep[is.na(BC_north_prep)] <- 0

CC_prep <- right_join(CC_SE, CC_wind_dur)
CC_prep[is.na(CC_prep)] <- 0

CalC_south_prep <- right_join(CalC_south_SE, CalC_south_wind_dur)
CalC_south_prep[is.na(CalC_south_prep)] <- 0

CalC_north_prep <- right_join(CalC_north_SE, CalC_north_wind_dur)
CalC_north_prep[is.na(CalC_north_prep)] <- 0

Chile_prep <- right_join(chile_SE, chile_wind_dur)
Chile_prep[is.na(Chile_prep)] <- 0

Peru_prep <- right_join(peru_SE, peru_wind_dur)
Peru_prep[is.na(Peru_prep)] <- 0

dur_prep <- function(df) {
  dur <- df %>% 
    rename(temp = wind_dir_from,
           t = date) %>% 
    select(temp,t)
}

BC_south_dur <- dur_prep(df = BC_south_prep)
BC_north_dur <- dur_prep(df = BC_north_prep)
CC_dur <- dur_prep(df = CC_prep)
CalC_south_dur <- dur_prep(df = CalC_south_prep)
CalC_north_dur <- dur_prep(df = CalC_north_prep)
Chile_dur <- dur_prep(df = Chile_prep)
Chile_dur <- Chile_dur %>% 
  arrange(t)
Peru_dur <- dur_prep(df = Peru_prep)
Peru_dur <- Peru_dur %>% 
  arrange(t)

exc_BC_south <- exceedance(BC_south_dur, minDuration = 1, threshold = 0)
exc_BC_north <- exceedance(BC_north_dur, minDuration = 1, threshold = 0)
exc_CC <- exceedance(CC_dur, minDuration = 1, threshold = 0)
exc_CalC_south <- exceedance(CalC_south_dur, minDuration = 1, threshold = 0)
exc_CalC_north <- exceedance(CalC_north_dur, minDuration = 1, threshold = 0)
exc_Chile <- exceedance(Chile_dur, minDuration = 1, threshold = 0)
exc_Peru <- exceedance(Peru_dur, minDuration = 1, threshold = 0)

wind_func <- function(df) {
  wind_duration <- df$exceedance %>%
    ungroup() %>%
    select(exceedance_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) 
}

BC_south_wind_duration <- wind_func(df = exc_BC_south)
BC_north_wind_duration <- wind_func(df = exc_BC_north)
CC_wind_duration <- wind_func(df = exc_CC)
CalC_south_wind_duration <- wind_func(df = exc_CalC_south)
CalC_north_wind_duration <- wind_func(df = exc_CalC_north)
Chile_wind_duration <- wind_func(df = exc_Chile)
Peru_wind_duration <- wind_func(df = exc_Peru)

# Seasons for the southern hemisphere
seasons_S_func <- function(df) {
  df_seasons <- df %>% 
    mutate(month = month(date_start, abbr = T, label = T),
           year = year(date_start)) %>% 
    mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Summer", 
                              month %in% c("Mar", "Apr", "May") ~ "Autumn",
                              month %in% c("Jun", "Jul", "Aug") ~ "Winter",
                              month %in% c("Sep", "Oct", "Nov") ~ "Spring"))
}

BC_south_wind<- seasons_S_func(df = BC_south_wind_duration)
BC_south_wind <- BC_south_wind %>% 
  mutate(current = "BC_south")

BC_north_wind<- seasons_S_func(df = BC_north_wind_duration)
BC_north_wind <- BC_north_wind %>% 
  mutate(current = "BC_north")

Chile_wind <- seasons_S_func(df = Chile_wind_duration)
Chile_wind <- Chile_wind %>% 
  mutate(current = "Chile")

Peru_wind <- seasons_S_func(df = Peru_wind_duration)
Peru_wind <- Peru_wind %>% 
  mutate(current = "Peru")

# Seasons for the Northern Hemisphere
seasons_N_func <- function(df) {
  df_seasons <- df %>% 
    mutate(month = month(date_start, abbr = T, label = T),
           year = year(date_start)) %>% 
    mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Winter", 
                              month %in% c("Mar", "Apr", "May") ~ "Spring",
                              month %in% c("Jun", "Jul", "Aug") ~ "Summer",
                              month %in% c("Sep", "Oct", "Nov") ~ "Autumn"))
}

CC_wind <- seasons_N_func(df = CC_wind_duration)
CC_wind <- CC_wind %>% 
  mutate(current = "CC")
CalC_south_wind <- seasons_N_func(df = CalC_south_wind_duration)
CalC_south_wind <- CalC_south_wind %>% 
  mutate(current = "CalC_south")

CalC_north_wind <- seasons_N_func(df = CalC_north_wind_duration)
CalC_north_wind <- CalC_north_wind %>% 
  mutate(current = "CalC_north")

duration_wind_currents <- rbind(BC_south_wind, BC_north_wind, Chile_wind,
                                Peru_wind, CC_wind, CalC_north_wind, CalC_south_wind)

wind_currents <- duration_wind_currents %>% 
  filter(season == "Summer") %>% 
  group_by(year, month, current) %>% 
  summarise(mean_dur = mean(duration))

wind_currents <- as.data.frame(wind_currents)

wind_currents$month <- as.factor(wind_currents$month)
wind_currents$month <- factor(wind_currents$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr",
                                                              "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
wind_currents$current = factor(wind_currents$current, levels = c('BC_south', 'BC_north', 'Chile',
                                                                 'Peru', 'CalC_south', 'CalC_north', 'CC'))

# Plotting duration of wind patterns over time
plotB <- ggplot(data = wind_currents, aes(x = year, y = mean_dur)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current)+#  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "Duration of SE winds (Days)")+
  theme_bw() +
  labs(colour = "Month") +
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=8, family = "Palatino"),
    axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 8, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 10, family = "Palatino"),
    legend.text = element_text(size = 9, family = "Palatino"),
    # legend.key = element_rect(size = 0.8, colour = NA),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())


#-------------------------------------------------------------
