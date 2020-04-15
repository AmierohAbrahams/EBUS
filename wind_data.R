# Python script to download wind data

# 10m u-component of wind
# m s-1
# Eastward component of the 10m wind. It is the horizontal speed of air
# moving towards the east, at a height of ten metres above the surface of
#the Earth, in metres per second. Care should be taken when comparing this
# variable with observations, because wind observations vary on small space and time
#scales and are affected by the local terrain, vegetation and buildings that are represented only on
#average in the ECMWF Integrated Forecasting System. This variable can be combined with the V component of 10m
#wind to give the speed and direction of the horizontal 10m wind.



# 10m v-component of wind
# m s-1
# Northward component of the 10m wind. It is the horizontal speed of air moving towards the north, at a height of ten metres above
#the surface of the Earth, in metres per second. Care should be taken when comparing this variable with observations, because wind observations
#vary on small space and time scales and are affected by the local terrain, vegetation and buildings that are represented only on average in the ECMWF
#Integrated Forecasting System. This variable can be combined with the U component of 10m wind to give the speed and direction of the horizontal 10m wind.

# #################################################################################################################################################################3
library(tidyverse)
library(lubridate)
library(ggpubr)

# Converting U and V wind variables to wind speed and direction
# load("data/BC_wind.RData")
# load("data/BC_vwind.RData")
#
BC_vwind <- BC_vwind %>%
  select(v_10)
BC_wind <- cbind(BC_vwind,BC_wind)
BC_wind_fin <- BC_wind %>%
  select(lon,lat,date,u_10,v_10)

# save(BC_wind_fin , file = "data/BC_wind_fin.RData")

# Wind speed
CalC_semi_complete$u_squared ='^'(CalC_semi_complete$u,2)
CalC_semi_complete$v_squared ='^'(CalC_semi_complete$v,2)
CalC_semi_complete <- CalC_semi_complete %>%
  mutate(speed = sqrt(u_squared + v_squared))

# Wind direction
CalC_semi_complete <- CalC_semi_complete %>%
  mutate(wind_dir_trig_to = atan2(u/speed, v/speed),
         wind_dir = wind_dir_trig_to * 180/pi)

CalC <- CalC_semi_complete %>%
  select(lon,lat,temp,date,speed,wind_dir)

BC_wind_complete <- BC_wind_fin %>%
  select(lon,lat,date,u_10,v_10,speed,wind_dir)

BC_wind_complete <- BC_wind_complete %>%
  mutate(date = as.Date(as.character(date)))

# save(BC_wind_complete , file = "data/BC_wind_complete.RData")
# Creating seasonal column and comparing changes in wind patterns over years
load("~/Documents/EBUS/data_complete/BC.RData")
load("~/Documents/EBUS/data_complete/HC.RData")
load("~/Documents/EBUS/data_complete/CalC.RData")
load("~/Documents/EBUS/data_complete/CC.RData")

### Southern Hemisphere

seasons_func <- function(df){
  BC_seaons <- df %>%
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
         year = year(date)) %>%
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",
                         ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                       ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}

BC_season <- seasons_func(df = BC)
HC_season <- seasons_func(df = HC)
save(BC_season, file = "data_complete/BC_season.RData")
save(HC_season, file = "data_complete/HC_season.RData")
####################### Northern Hemisphere
seasons_func <- function(df){
  seasons <- df %>%
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
         year = year(date)) %>%
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Winter",
                         ifelse(month %in% c("Mar", "Apr", "May"), "Spring",
                                ifelse(month %in% c("Jun", "Jul", "Aug"), "Summer",
                                       ifelse(month %in% c("Sep", "Oct", "Nov"), "Autumn","Error")))))
}

CC_season <- seasons_func(df = CC)
CalC_season <- seasons_func(df = CalC)
save(CC_season, file = "data_complete/CC_season.RData")
save(CalC_season, file = "data_complete/CalC_seasodn.RData")
##############################################################################################################################################
##############################################################################################################################################S

load("data_complete/CalC_complete.RData")
load("data_complete/CC_complete.RData")
load("data_complete/BC_complete.RData")
load("data_complete/HC_complete.RData")


# CC_complete <- CC_season %>%
#   na.omit()
# save(CC_complete, file = "data_complete/CC_complete.RData")
#

wind_func <- function(df){
  wind <- df %>%
    mutate(dir = ifelse(wind_dir < 0, wind_dir+360, wind_dir)) %>%
    #dplyr::rename(spd = speed) %>%
    filter(spd > 0)
}



ggarrange(CalC_plot, CC_plot, HC_plot, BC_plot,
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A","B", "C", "D")) # Create common legend
# ##########################################################################

source("functions/wind.rose.R")

wind_renamed_func <- function(df){
  wind_renamed <- df %>%
    mutate(wind_dir = ifelse(wind_dir < 0, wind_dir+360, wind_dir)) %>%
    dplyr::rename(spd = spd) %>%
    dplyr::rename(dir = wind_dir) %>%
    filter(spd > 0)
}

wind_BC <- wind_renamed_func(df = BC_complete)
wind_CalC <- wind_renamed_func(df = CalC_complete)
wind_CC <- wind_renamed_func(df = CC_complete)
wind_HC <- wind_renamed_func(df = HC_complete)

p.wr2 <- plot.windrose(data = wind_BC,
                       spd = "spd",
                       dir = "dir")
p.wr3 <- p.wr2 + facet_wrap(.~ site) +
  theme(strip.text.x = element_text(size = 25)) + theme(panel.spacing = unit(2, "lines"))
p.wr3

# ##############################################################################################
#
# currents_combined <- rbind(wind_BC,wind_CalC,wind_CC,wind_HC)

SE_renamed <-wind_CalC%>% # Chnaged the names with the data
  filter(dir >= 180, dir <= 270)
# Then create diifferent temporal results
SE_annual <- SE_renamed %>%
  group_by( year) %>%
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
SE_summer <- SE_renamed %>%
  filter(season == "Summer") %>%
  group_by(year, season) %>%
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
SE_monthly <- SE_renamed %>%
  filter(season == "Summer") %>%
  group_by(year, season, month) %>%
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
# Plots
## Annual count of SE wind
ggplot(data = SE_annual, aes(x = year, y = count)) +
  geom_line() +
  geom_smooth(method = "lm")
## Annual count of SE wind in Summer
### The trends between annual and summer SE wind counts are remarkably similar
ggplot(data = SE_summer, aes(x = year, y = count)) +
  geom_line() +
  geom_smooth(method = "lm")
## Summer month count of SE winds
CalC <- ggplot(data = SE_monthly, aes(x = year, y = count)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm")


ggarrange(BC, HC, CC, CalC,
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A","B", "C", "D")) # Create common legend


################################################################################
library(tidyverse)
library(lubridate)

# This is for each region and not for each pixel as done above

load("data_complete/BC_final.RData")
load("data_complete/HC_final.RData")
load("data_complete/CC_final.RData")
load("data_complete/CalC_final.RData")

options(scipen=999)

BC_final <- BC_final %>% 
  mutate(current = "BC")
HC_final <- HC_final %>% 
  mutate(current = "HC")
CC_final <- CC_final %>% 
  mutate(current = "CC")
CalC_final <- CalC_final %>% 
  mutate(current = "CalC")


seasons_func <- function(df){
  BC_seaons <- df %>% 
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
           year = year(date)) %>% 
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Summer",        
                           ifelse(month %in% c("Mar", "Apr", "May"), "Autumn",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Winter",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Spring","Error")))))
}

BC_final <- seasons_func(df = BC_final)
HC_final <- seasons_func(df = HC_final)
####################### Northern Hemisphere
seasons_func <- function(df){
  seasons <- df %>% 
    mutate(month = month(as.Date(as.character(date)), abbr = T, label = T),
           year = year(date)) %>% 
    mutate(season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Winter",        
                           ifelse(month %in% c("Mar", "Apr", "May"), "Spring",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"), "Summer",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"), "Autumn","Error")))))
}

CC_final <- seasons_func(df = CC_final)
CalC_final <- seasons_func(df = CalC_final)

complete_wind <- rbind(BC_final,HC_final,CC_final,CalC_final)

wind_renamed_func <- function(df){
  wind_renamed <- df %>% 
    mutate(wind = ifelse(wind < 0, wind+360, wind)) %>%
    dplyr::rename(spd = speed) %>%
    dplyr::rename(dir = wind) %>% 
    filter(spd > 0)
}

complete_wind <- wind_renamed_func(complete_wind)


SE_renamed <-complete_wind %>% # Chnaged the names with the data 
  filter(dir >= 180, dir <= 270)
# Then create diifferent temporal results
SE_annual <- SE_renamed %>% 
  group_by(current, year) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
SE_summer <- SE_renamed %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
SE_monthly <- SE_renamed %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
# Plots
## Annual count of SE wind
ggplot(data = SE_annual, aes(x = year, y = count)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~current)
## Annual count of SE wind in Summer
### The trends between annual and summer SE wind counts are remarkably similar
ggplot(data = SE_summer, aes(x = year, y = count)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~current)
## Summer month count of SE winds
ggplot(data = SE_monthly, aes(x = year, y = count)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current)












