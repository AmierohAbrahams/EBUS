library(circular)
## devtools::install_github("robwschlegel/coastR")
library(gridExtra)
library(geosphere)
library(tidyverse)
library(heatwaveR)
library(coastR)

load("data_complete/BC_complete.RData") 
load("data_complete/HC_complete.RData") 
load("data_complete/CC_complete.RData") 
load("data_complete/CalC_complete.RData") 


HC_complete <- HC_complete %>% 
  mutate(lon = lon - 360)

CC_complete <- CC_complete %>% 
  mutate(lon = lon - 360)

CalC_complete <- CalC_complete %>% 
  mutate(lon = lon - 360)

BC_angle <- coastR::transects(BC_complete, spread = 30)
CC_angle <- coastR::transects(CC_complete, spread = 30)
CalC_angle <- coastR::transects(CalC_complete, spread = 30)
HC_angle <- coastR::transects(HC_complete, spread = 30)

BC_angle_func <- function(df){
  BC_angle <- df %>% 
  select(heading)
}

BC_angle <- BC_angle_func(df = BC_angle)
HC_angle <- BC_angle_func(df = HC_angle)
CC_angle <- BC_angle_func(df = CC_angle)
CalC_angle <- BC_angle_func(df = CalC_angle)

BC_heading <- cbind(BC_complete, BC_angle)
HC_heading <- cbind(BC_complete, HC_angle)
CC_heading <- cbind(BC_complete, CC_angle)
CalC_heading <- cbind(BC_complete, CalC_angle)

upwelling_func <- function(df){
  UI<- df %>%  
    dplyr::rename(coast_angle = heading) %>% 
    mutate(ui = spd * (cos(wind_dir - coast_angle))) %>%
    drop_na 
}


UI_BC <- upwelling_func(df= BC_heading)
UI_HC <- upwelling_func(df= HC_heading)
UI_CC <- upwelling_func(df= CC_heading)
UI_CalC <- upwelling_func(df= CalC_heading)

save(UI_BC, file = "data_complete/UI_BC.RData")
save(UI_HC, file = "data_complete/UI_HC.RData")
save(UI_CC, file = "data_complete/UI_CC.RData")
save(UI_CalC, file = "data_complete/UI_CalC.RData")
############################################################

load("data_complete/UI_BC.RData") 
load("data_complete/UI_HC.RData") 
load("data_complete/UI_CC.RData") 
load("data_complete/UI_CalC.RData") 


BC_UI_trim <- UI_BC %>% 
  select(lon,lat,date,spd,wind_dir,coast_angle,ui) %>% 
  rename(t = date) %>% 
  rename(temp= ui)

exceed_func <- function(df){
df_upwell <- df %>%
  nest() %>% # apply the following functions to all of the variables in te dataset
  mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1981-09-01", "2012-12-31")), # creating a column of climaatologies. Column will be named clim
         # NB: A threshold of 3 appeared to be far to strict
         # purr::map - apllies a function to each element of a vector
         exceed = purrr::map(clim, exceedance, minDuration = 1, threshold = 1)) %>%  #Upwelling cannot be descrbed as an event. Upwelling can last for a few hours. Given that we have daily data, upwelling events minimum duration here will be 1day
  # Detect consecutive days in exceedance of a given threshold.
  # mutate() %>% 
  select(-data, -clim) %>% 
  unnest() %>%
  filter(row_number() %% 2 == 1) %>%
  unnest() %>% # creates a column for each variables
  dplyr::rename(ui = temp) %>% # rename upwelling index vale to temp so that it could work with the function
  select(lat,lon, t, ui, exceedance) # selecting only these variables
}

BC_exceed <- exceed_func(df = BC_UI_trim)
HC_exceed <- exceed_func(df = HC_UI_trim)
CC_exceed <- exceed_func(df = CC_UI_trim)
CalC_exceed <- exceed_func(df = CalC_UI_trim)

load("data_complete/BC_complete.RData") 
load("data_complete/HC_complete.RData") 
load("data_complete/CC_complete.RData") 
load("data_complete/CalC_complete.RData") 


HC_complete <- HC_complete %>% 
  mutate(lon = lon - 360)

CC_complete <- CC_complete %>% 
  mutate(lon = lon - 360)

CalC_complete <- CalC_complete %>% 
  mutate(lon = lon - 360)

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 3, coldSpells = T)
  return(res)
}

# Calculate the upwelling event metrics
upwell_base <- CalC_complete %>% 
  dplyr::rename(t = date) %>% 
  group_by(lon, lat) %>%
  nest() %>% 
  mutate(clim = purrr::map(data, ts2clm, pctile = 25, climatologyPeriod = c("1982-01-01", "2011-12-31"))) %>%
  select(-data) %>% 
  unnest(cols = clim) %>%
  left_join(CalC_exceed, by = c("lon", "lat", "t")) %>%
  filter(!is.na(exceedance)) %>%
  nest() %>% 
  mutate(exceed = purrr::map(data, detect_event_custom)) %>% 
  select(-data) %>% 
  unnest(cols = exceed) %>%
  filter(row_number() %% 2 == 0) %>% # Select event summary metrics
  # filter(row_number() %% 2 == 1) %>% # Select daily values
  unnest(cols = exceed)

#############################################################
library(tidyverse)
library(lubridate)
library(ggpubr)

load("~/Documents/EBUS/data_complete/upwell_base_BC_2012.RData")
load("~/Documents/EBUS/data_complete/upwell_base_CC_2012.RData")
load("~/Documents/EBUS/data_complete/upwell_base_CalC_2012.RData")
load("~/Documents/EBUS/data_complete/upwell_base_HC_2012.RData")


upwell_prep <- function(df){
  upwell_prep <- df %>% 
    mutate(date = date_start)
}

upwell_prep_BC = upwell_prep(df = upwell_base_BC_2012)
upwell_prep_HC = upwell_prep(df = upwell_base_HC_2012)
upwell_prep_CC = upwell_prep(df = upwell_base_CC_2012)
upwell_prep_CalC = upwell_prep(df = upwell_base_CalC_2012)

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

upwell_season_BC <- seasons_func(df = upwell_prep_BC)
upwell_season_HC <- seasons_func(df = upwell_prep_HC)
save(upwell_season_BC, file = "data_complete/upwell_season_BC.RData")
save(upwell_season_HC, file = "data_complete/upwell_season_HC.RData")
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

upwell_season_CC <- seasons_func(df = upwell_prep_CC)
upwell_season_CalC <- seasons_func(df = upwell_prep_CalC)
save(upwell_season_CC, file = "data_complete/upwell_season_CC.RData")
save(upwell_season_CalC, file = "data_complete/upwell_season_CalC.RData")

# write_csv(upwell_season_BC, path = "data_complete/upwell_season_BC.csv")
# write_csv(upwell_season_HC, path = "data_complete/upwell_season_HC.csv")
# write_csv(upwell_season_CalC, path = "data_complete/upwell_season_CalC.csv")
# write_csv(upwell_season_CC, path = "data_complete/upwell_season_CC.csv")
