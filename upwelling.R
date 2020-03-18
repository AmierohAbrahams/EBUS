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


###################################################
# Unpack the event metric reults



SACTN_upwell_events <- SACTN_upwell_base %>% 
  unnest() %>%
  filter(row_number() %% 2 == 0) %>%
  unnest()
# save(SACTN_upwell_events, file = "Data/SACTN_upwell_events.RData")
# Unpack the daily climatology results
SACTN_upwell_clims <- SACTN_upwell_base %>% 
  unnest() %>%
  filter(row_number() %% 2 == 1) %>% 
  unnest()
# save(SACTN_upwell_clims, file = "Data/SACTN_upwell_clims.RData")
# The above chunk of code appears to work as expected.
# What needs to be done now is that the thresholds for duration and UI strength need to be justified/decided uopn. 
# Min duration for an upwelling event - 1day and threshold -1
# What also needs to be decided is if we are interested in any upwelling results throughout the year,
# or only during the upwelling season, whenever that may be for each site.
# I think we have demonstrated the technical capability needed to answer the question of whether or not upwelling
# is changing over time.
# Now we really need to focus on which of these parameters need to best be tweaked to answer that question.

