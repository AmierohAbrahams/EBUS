# Wind analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Creating the new bounding boxes according to Varella (2018)
# 3: ANOVA analyses
# 4: Linear models

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
source("functions/theme.R")
options(scipen=999) 

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

# 3: Identifying South easterly winds (SE) ------------------------------------------------------------------------------

# Then create different temporal results
load("data_official/south_BC.RData")
load("data_official/north_BC.RData")
load("data_official/Canary_current.RData")
load("data_official/chile.RData")
load("data_official/peru.RData")
load("data_official/south_CalC.RData")
load("data_official/north_CalC.RData")

current_winds <- rbind(south_BC, north_BC, Canary_current, chile,peru, south_CalC, north_CalC)
# rm(south_BC, north_BC, Canary_current,chile,peru, south_CalC,north_CalC);gc()
# save(current_winds, file = "data_official/current_winds.RData")

# First filter out only the SE (South easterly) winds
# Southern Hemisphere
SE_winds <- current_winds %>% 
  filter(wind_dir_from >= 180, wind_dir_from <= 270) %>% 
  unique()
# rm(current_winds);gc()
#save(SE_renamed, file = "data/SE_renamed.RData")

# Then create diifferent temporal results
# This is done to check if there are differences in the number of SE blown winds over time
SE_monthly <- SE_winds %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            circ_dir = mean.circular(circular(wind_dir_from, units = "degrees")),
            circ_wspd = mean.circular(circular(wind_spd, units = "degrees")),
            mean_temp = mean(temp, na.rm = TRUE),
            mean_SLP = mean(slp, na.rm = TRUE))

# Determining the number of pixels within each current ---------------------------------------------------------------------

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

# SOUTHERN HEMISPHERE (Winds blow SE direction for upwelling)
# Changes in the number of SE wind blown 

# AJS: (number of winds? WTF?!)....Wind events. Yes. Number of winds. NO (Winds are not discrete)
south_BC_SE <- SE_monthly %>% 
  filter(current == "BC_south") %>% 
  mutate(no_SE = count/45) # The value here is the number of pixels occuring within this region

north_BC_SE <- SE_monthly %>% 
  filter(current == "BC_north") %>% 
  mutate(no_SE = count/16)

Peru_SE <- SE_monthly %>% 
  filter(current == "HC_peru") %>% 
  mutate(no_SE = count/25)

Chile_SE <- SE_monthly %>% 
  filter(current == "HC_chile") %>% 
  mutate(no_SE = count/99)

SE_winds <- rbind(south_BC_SE, north_BC_SE, Peru_SE, Chile_SE)

# Plot showing the number of SE winds blown (hahahaha!!)
plotA <- ggplot(data = SE_winds, aes(x = year, y = no_SE)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current, ncol = 2) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "SE wind events 
(count)")+
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

# NORTHERN HEMISPHERE (Winds blow NE direction for upwelling) -----------------------------------------------

NE_winds <- current_winds %>% 
  filter(wind_dir_from >= 0, wind_dir_from <= 180) %>% 
  unique()
# rm(current_winds);gc()
# save(NE_winds, file = "data_official/NE_winds.RData")

# Then create diifferent temporal results
# This is done to check if there are differences in the number of SE blown winds over time
NE_monthly <- NE_winds %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            circ_dir = mean.circular(circular(wind_dir_from, units = "degrees")),
            circ_wspd = mean.circular(circular(wind_spd, units = "degrees")),
            mean_temp = mean(temp, na.rm = T),
            mean_SLP = mean(slp, na.rm = T))

CC_NE <- NE_monthly %>% 
  filter(current == "CC") %>% 
  mutate(no_SE = count/82)

south_CalC <- NE_monthly %>% 
  filter(current == "CalC_south") %>% 
  mutate(no_SE = count/24)

north_CalC <- NE_monthly %>% 
  filter(current == "CalC_north") %>% 
  mutate(no_SE = count/20)

NE_winds <- rbind(CC_NE, south_CalC, north_CalC)

plotB <- ggplot(data = NE_winds, aes(x = year, y = no_SE)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current, ncol = 1) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "NE wind events 
(count)")+
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

# Changes in the duration of SE winds? ---------------------------------------------------------------------------------

load("data_official/south_BC.RData")
load("data_official/north_BC.RData")
load("data_official/Canary_current.RData")
load("data_official/chile.RData")
load("data_official/peru.RData")
load("data_official/south_CalC.RData")
load("data_official/north_CalC.RData")

wind_dur_func <- function(df){
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

# Southern Hemisphere -----------------------------------------------------
# not sure what's happening here? Should you not first calculate the number of days with
# wind that blows from the range of directions across ALL THE PIXELS and then divide by pixels

SE_wind_func <- function(df){
  SE<- df %>% 
    filter(wind_dir_from >= 180, wind_dir_from <= 270) %>% 
    unique() %>% 
    select(date,wind_dir_from,lat,lon)
}

BC_S_SE <- SE_wind_func(df = south_BC)
BC_N_SE <- SE_wind_func(df = north_BC)
chile_SE <- SE_wind_func(df = chile)
peru_SE<- SE_wind_func(df = peru)


BC_S_prep <- right_join(BC_S_SE, BC_south_wind_dur)
BC_S_prep[is.na(BC_S_prep)] <- 0

BC_N_prep <- right_join(BC_N_SE, BC_north_wind_dur)
BC_N_prep[is.na(BC_N_prep)] <- 0

chile_prep <- right_join(chile_SE, chile_wind_dur)
chile_prep[is.na(chile_prep)] <- 0

peru_prep <- right_join(peru_SE, peru_wind_dur)
peru_prep[is.na(peru_prep)] <- 0

dur_prep <- function(df){
  dur <- df %>% 
    rename(temp = wind_dir_from,
           t = date) %>% 
    select(temp,t)
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
    select(exceedance_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) 
}

BC_S_dur <- wind_func(df = exc_BC_S)
BC_N_dur <- wind_func(df = exc_BC_N)
chile_dur <- wind_func(df = exc_chile)
peru_dur <- wind_func(df = exc_peru)

# Divide by the pixels
south_BC_SE <- BC_S_dur %>% 
  mutate(dur_SE = duration/45) # The value here is the number of pixels occuring within this region

north_BC_SE <- BC_N_dur %>% 
  mutate(dur_SE = duration/16)

Chile_SE <- chile_dur %>% 
  mutate(dur_SE = duration/99)

Peru_SE <- peru_dur %>% 
  mutate(dur_SE = duration/25)

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
  labs(x = "", y = "Duration of SE winds
(Days)")+
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
SE_winds <- current_winds %>% 
  filter(wind_dir_from >= 180, wind_dir_from <= 270) %>% 
  unique()
# rm(current_winds);gc()
#save(SE_renamed, file = "data/SE_renamed.RData")

# Then create different temporal results
# This is done to check if there are differences in the number of SE blown winds over time
SE_monthly <- SE_winds %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            circ_dir = mean.circular(circular(wind_dir_from, units = "degrees")),
            circ_wspd = mean.circular(circular(wind_spd, units = "degrees")),
            mean_temp = mean(temp, na.rm = T),
            mean_SLP = mean(slp, na.rm = T))


south_BC_SE <- SE_monthly %>% 
  filter(current == "BC_south") 

north_BC_SE <- SE_monthly %>% 
  filter(current == "BC_north") 

Peru_SE <- SE_monthly %>% 
  filter(current == "HC_peru") 

Chile_SE <- SE_monthly %>% 
  filter(current == "HC_chile")

SE_monthly <- rbind(south_BC_SE, north_BC_SE, Peru_SE, Chile_SE)

SE_monthly$month <- as.factor(SE_monthly$month)
SE_monthly$month <- factor(SE_monthly$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                                        "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
SE_monthly$current = factor(SE_monthly$current, levels=c('BC_south', 'BC_north', 'HC_chile','HC_peru'))

# Wind intensity
plotD <- ggplot(data = SE_monthly, aes(x = year, y = circ_wspd, colour = Month)) +
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

# Northern Hemisphere
NE_winds <- current_winds %>% 
  filter(wind_dir_from >= 0, wind_dir_from <= 180) %>% 
  unique()
# rm(current_winds);gc()
#save(SE_renamed, file = "data/SE_renamed.RData")

# Then create different temporal results
# This is done to check if there are differences in the count of SE blown wind over time
NE_monthly <- NE_winds %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            circ_dir = mean.circular(circular(wind_dir_from, units = "degrees")),
            circ_wspd = mean.circular(circular(wind_spd, units = "degrees")),
            mean_temp = mean(temp, na.rm = T),
            mean_SLP = mean(slp, na.rm = T))

# Changes in the number of SE wind blown
CC_NE <- NE_monthly %>% 
  filter(current == "CC") 

north_CalC_NE <- NE_monthly %>% 
  filter(current == "CalC_north") 

south_CalC_NE <- NE_monthly %>% 
  filter(current == "CalC_south") 

NE_monthly <- rbind(CC_NE,north_CalC_NE, south_CalC_NE)

NE_monthly$month <- as.factor(NE_monthly$month)
NE_monthly$month <- factor(NE_monthly$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                                        "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
NE_monthly$current = factor(NE_monthly$current, levels=c('CalC_south', 'CalC_north', 'CC'))

# Wind intensity
plotE <- ggplot(data = NE_monthly, aes(x = year, y = circ_wspd, colour = Month)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current, ncol = 4) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "SE wind intensity")+
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
