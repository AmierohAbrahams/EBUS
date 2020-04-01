library(circular)
## devtools::install_github("robwschlegel/coastR")
library(gridExtra)
library(geosphere)
library(tidyverse)
library(heatwaveR)
library(coastR)


theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            #legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# load("data_complete/BC_complete.RData") 
# load("data_complete/HC_complete.RData") 
# load("data_complete/CC_complete.RData") 
# load("data_complete/CalC_complete.RData") 

# HC_complete <- HC_complete %>% 
#   mutate(lon = lon - 360)

# CC_complete <- CC_complete %>% 
#   mutate(lon = lon - 360)

# CalC_complete <- CalC_complete %>%
#   mutate(lon = lon - 360) 


load("data_complete/BC_final.RData")
load("data_complete/HC_final.RData")
load("data_complete/CC_final.RData")
load("data_complete/CalC_final.RData")



wind_renamed_func <- function(df){
  wind_renamed <- df %>% 
    mutate(dir_wind = ifelse(wind < 0, wind+360, wind)) %>%
   # dplyr::rename(mean_speed = mean_speed) %>%
    dplyr::rename(dir_wind = dir_wind) #%>% 
    #filter(mean_speed > 0)
}

BC_final <- wind_renamed_func(df = BC_final)
HC_final <- wind_renamed_func(df = HC_final)
CC_final <- wind_renamed_func(df = CC_final)
CalC_final <- wind_renamed_func(df = CalC_final)
# BC_angle <- coastR::transects(test1, spread = 30)
# CC_angle <- coastR::transects(CC_final, spread = 30)
# CalC_angle <- coastR::transects(CalC_final, spread = 30)
# HC_angle <- coastR::transects(HC_final, spread = 30)


# BC_angle_func <- function(df){
#   BC_angle <- df %>% 
#   select(heading)
# }
# 
# BC_angle <- BC_angle_func(df = BC_angle)
# HC_angle <- BC_angle_func(df = HC_angle)
# CC_angle <- BC_angle_func(df = CC_angle)
# CalC_angle <- BC_angle_func(df = CalC_angle)
# 
# BC_heading <- cbind(BC_complete, BC_angle)
# HC_heading <- cbind(BC_complete, HC_angle)
# CC_heading <- cbind(BC_complete, CC_angle)
# CalC_heading <- cbind(BC_complete, CalC_angle)

upwelling_func <- function(df){
  UI<- df %>%  
    mutate(ui = speed * (cos(dir_wind - coast_angle))) %>%
    drop_na 
}


UI_BC <- upwelling_func(df= BC_final)
UI_HC <- upwelling_func(df= HC_final)
UI_CC <- upwelling_func(df= CC_final)
UI_CalC <- upwelling_func(df= CalC_final)

save(UI_BC, file = "data_complete/UI_BC.RData")
save(UI_HC, file = "data_complete/UI_HC.RData")
save(UI_CC, file = "data_complete/UI_CC.RData")
save(UI_CalC, file = "data_complete/UI_CalC.RData")
############################################################

load("data_complete/UI_BC.RData") 
load("data_complete/UI_HC.RData") 
load("data_complete/UI_CC.RData") 
load("data_complete/UI_CalC.RData") 

UI_trim <- function(df){
UI_trim <- df %>% 
  select(date,speed,dir_wind,coast_angle,ui) %>% # Removed lat and lon
  rename(t = date) %>% 
  rename(temp= ui)
}

BC_UI_trim <- UI_trim(df = UI_BC)
HC_UI_trim <- UI_trim(df = UI_HC)
CC_UI_trim <- UI_trim(df = UI_CC)
CalC_UI_trim <- UI_trim(df = UI_CalC)

exceed_func <- function(df){ 
df_upwell <- df %>%
  nest() %>% # apply the following functions to all of the variables in te dataset
  mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1981-09-01", "2012-12-29")), # creating a column of climaatologies. Column will be named clim
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
  select(t, ui, exceedance) # selecting only these variables
}

BC_exceed <- exceed_func(df = BC_UI_trim)
HC_exceed <- exceed_func(df = HC_UI_trim)
CC_exceed <- exceed_func(df = CC_UI_trim)
CalC_exceed <- exceed_func(df = CalC_UI_trim)


save(BC_exceed, file = "data_complete/BC_exceed.RData")
save(HC_exceed, file = "data_complete/HC_exceed.RData")
save(CC_exceed, file = "data_complete/CC_exceed.RData")
save(CalC_exceed, file = "data_complete/CalC_exceed.RData")

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 3, coldSpells = T)
  return(res)
}


load("data_complete/BC_final.RData")
load("data_complete/HC_final.RData")
load("data_complete/CC_final.RData")
load("data_complete/CalC_final.RData")


# Calculate the upwelling event metrics
upwell_base_CalC <- CalC_final %>% 
  dplyr::rename(t = date) %>% 
  nest() %>% 
  mutate(clim = purrr::map(data, ts2clm, pctile = 25, climatologyPeriod = c("1982-01-01", "2011-12-31"))) %>%
  select(-data) %>% 
  unnest(cols = clim) %>%
  left_join(CalC_exceed, by = c("t")) %>%
  filter(!is.na(exceedance)) %>%
  nest() %>% 
  mutate(exceed = purrr::map(data, detect_event_custom)) %>% 
  select(-data) %>% 
  unnest(cols = exceed) %>%
  filter(row_number() %% 2 == 0) %>% # Select event summary metrics
  # filter(row_number() %% 2 == 1) %>% # Select daily values
  unnest(cols = exceed)

save(upwell_base_BC, file = "data_complete/upwell_base_BC.RData")
save(upwell_base_HC, file = "data_complete/upwell_base_HC.RData")
save(upwell_base_CC, file = "data_complete/upwell_base_CC.RData")
save(upwell_base_CalC, file = "data_complete/upwell_base_CalC.RData")

#############################################################
library(tidyverse)
library(lubridate)
library(ggpubr)



upwell_prep <- function(df){
  upwell_prep <- df %>% 
    mutate(date = date_start)
}

upwell_prep_BC = upwell_prep(df = upwell_base_BC)
upwell_prep_HC = upwell_prep(df = upwell_base_HC)
upwell_prep_CC = upwell_prep(df = upwell_base_CC)
upwell_prep_CalC = upwell_prep(df = upwell_base_CalC)

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


load("~/Documents/EBUS/data_complete/upwell_season_BC.RData")
load("~/Documents/EBUS/data_complete/upwell_season_CC.RData")
load("~/Documents/EBUS/data_complete/upwell_season_CalC.RData")
load("~/Documents/EBUS/data_complete/upwell_season_HC.RData")

### Adding wind speed and direction to the metrics


wind_speed_func <- function(df){
BC_final_test <- df %>% 
  rename(date_start = date) %>% 
  select(date_start, speed,dir_wind)
}

BC_final_test = wind_speed_func(df = BC_final)
HC_final_test = wind_speed_func(df = HC_final)
CC_final_test = wind_speed_func(df = CC_final)
CalC_final_test = wind_speed_func(df = CalC_final)


try2 <- function(df) {
  func2 <- df %>% 
    left_join(upwell_season_CalC, by = c("date_start")) %>% 
    drop_na()
  return(func2)
}


upwell_season_BC <- try2(df =BC_final_test)
upwell_season_HC <- try2(df =HC_final_test)
upwell_season_CC <- try2(df =CC_final_test)
upwell_season_CalC <- try2(df =CalC_final_test)


total_count_func <- function(df){
  total_count <- df %>% 
  group_by(year,season) %>% 
  summarise(n = n())
}

BC_total_count <- total_count_func(df = upwell_season_BC) %>% 
  mutate(current = "BC")
HC_total_count <- total_count_func(df = upwell_season_HC) %>% 
  mutate(current = "HC")
CC_total_count <- total_count_func(df = upwell_season_CC) %>% 
  mutate(current = "CC")
CalC_total_count <- total_count_func(df = upwell_season_CalC) %>% 
  mutate(current = "CalC")

total_count_current <- rbind(BC_total_count,HC_total_count,CC_total_count,CalC_total_count)

ggplot(data = total_count_current, aes(x = year, y = n, colour = current)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~season) +
  labs(x = "Year", y =" Number of signals detected") +
  theme_Publication()

#### Creating a tableS
mean_SD_func_1982 <- function(df){
  mean_SD<- df %>% 
    filter(year == 1982) %>% 
    group_by(season) %>% 
    summarise( mean_duration = mean(duration),
              mean_intensity = mean(intensity_mean),
              SD_duration = sd(duration),
              SD_intensity = sd(intensity_mean),
              mean_cum = mean(intensity_cumulative))
}

variance_BC <- mean_SD_func_1982(df = upwell_season_BC)
variance_HC <- mean_SD_func_1982(df = upwell_season_HC)
variance_CC <- mean_SD_func_1982(df = upwell_season_CC)
variance_CalC <- mean_SD_func_1982(df = upwell_season_CalC)
  
mean_SD_func_2012 <- function(df){
  mean_SD<- df %>% 
    filter(year == 2012) %>% 
    group_by(season) %>% 
    summarise( mean_duration = mean(duration),
               mean_intensity = mean(intensity_mean),
               SD_duration = sd(duration),
               SD_intensity = sd(intensity_mean),
               mean_cum = mean(intensity_cumulative))
}
  
  variance_BC_2012 <- mean_SD_func_2012(df = upwell_season_BC)
  variance_HC_2012 <- mean_SD_func_2012(df = upwell_season_HC)
  variance_CC_2012 <- mean_SD_func_2012(df = upwell_season_CC)
  variance_CalC_2012 <- mean_SD_func_2012(df = upwell_season_CalC)

#### Table showing the largest signal detected and the lowest signal detected
#### Anuual trends
  

totalCntFun <- function(df) {
  freq <- df %>%
    dplyr::group_by(year, season) %>%
    dplyr::summarise(y = n()) %>% 
    rename(total_count = y)
}

BC_totalCntFun <- totalCntFun(upwell_season_BC)
HC_totalCntFun <- totalCntFun(upwell_season_HC)
CC_totalCntFun <- totalCntFun(upwell_season_CC)
CalC_totalCntFun <- totalCntFun(upwell_season_CalC)

MeanInt <- function(df) {
  totMax <- df %>%
    dplyr::group_by(year, season) %>%
    dplyr::summarise(y = mean(intensity_mean, na.rm = TRUE)) %>% 
    rename(mean_intensity = y) 
}

BC_MeanInt <- MeanInt(upwell_season_BC)
HC_MeanInt <- MeanInt(upwell_season_HC)
CC_MeanInt <- MeanInt(upwell_season_CC)
CalC_MeanInt <- MeanInt(upwell_season_CalC)

MeanDur <- function(df) {
  totMax <- df %>%
    dplyr::group_by(year, season) %>%
    dplyr::summarise(y = mean(duration, na.rm = TRUE)) %>% 
    rename(mean_dur = y) 
}

BC_MeanDur <- MeanDur(upwell_season_BC)
HC_MeanDur<- MeanDur(upwell_season_HC)
CC_MeanDur<- MeanDur(upwell_season_CC)
CalC_MeanDur <- MeanDur(upwell_season_CalC)


MeanOns <- function(df) {
  totMax <- df %>%
    dplyr::group_by(year, season) %>%
    dplyr::summarise(y = mean(rate_onset, na.rm = TRUE)) %>% 
    rename(mean_ons = y) 
}

BC_MeanOns <- MeanOns(upwell_season_BC)
HC_MeanOns<- MeanOns(upwell_season_HC)
CC_MeanOns<- MeanOns(upwell_season_CC)
CalC_MeanOns <- MeanOns(upwell_season_CalC)

cumInt <- function(df) {
  totMax <- df %>%
    dplyr::group_by(year, season) %>%
    dplyr::summarise(y = mean(intensity_cumulative, na.rm = TRUE)) %>% 
    rename(cum_intensity = y) 
}

BC_cumInt<- cumInt(upwell_season_BC)
HC_cumInt<- cumInt(upwell_season_HC)
CC_cumInt<- cumInt(upwell_season_CC)
CalC_cumInt <- cumInt(upwell_season_CalC)

mean_speed <- function(df) {
  wind_speed <- df %>%
    dplyr::group_by(year, season) %>%
    dplyr::summarise(y = mean(speed, na.rm = TRUE)) %>% 
    rename(mean_speed = y) 
}

BC_speed<- mean_speed(upwell_season_BC)
HC_speed<- mean_speed(upwell_season_HC)
CC_speed<- mean_speed(upwell_season_CC)
CalC_speed <- mean_speed(upwell_season_CalC)

mean_wind <- function(df) {
  wind_direct <- df %>%
    dplyr::group_by(year, season) %>%
    dplyr::summarise(y = mean(dir_wind, na.rm = TRUE)) %>% 
    rename(dir_wind = y) 
}

BC_wind<- mean_wind(upwell_season_BC)
HC_wind<- mean_wind(upwell_season_HC)
CC_wind<- mean_wind(upwell_season_CC)
CalC_wind <- mean_wind(upwell_season_CalC)

BC_metrics <- cbind(BC_cumInt,BC_MeanDur,BC_MeanInt, BC_MeanOns,BC_totalCntFun, BC_speed,BC_wind) %>% 
  select(year,season,cum_intensity,mean_dur,mean_intensity,mean_ons,total_count, mean_speed, dir_wind)

HC_metrics <- cbind(HC_cumInt,HC_MeanDur,HC_MeanInt, HC_MeanOns,HC_totalCntFun,HC_speed,HC_wind) %>% 
  select(year,season,cum_intensity,mean_dur,mean_intensity,mean_ons,total_count, mean_speed, dir_wind)

CC_metrics <- cbind(CC_cumInt,CC_MeanDur,CC_MeanInt, CC_MeanOns,CC_totalCntFun, CC_speed,CC_wind) %>% 
  select(year,season,cum_intensity,mean_dur,mean_intensity,mean_ons,total_count, mean_speed, dir_wind)

CalC_metrics <- cbind(CalC_cumInt,CalC_MeanDur,CalC_MeanInt, CalC_MeanOns,CalC_totalCntFun, CalC_speed, CalC_wind) %>% 
  select(year,season,cum_intensity,mean_dur,mean_intensity,mean_ons,total_count, mean_speed, dir_wind)


save(BC_metrics, file = "data/BC_metrics.RData")
save(HC_metrics, file = "data/HC_metrics.RData")
save(CC_metrics, file = "data/CC_metrics.RData")
save(CalC_metrics, file = "data/CalC_metrics.RData")

