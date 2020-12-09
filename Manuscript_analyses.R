# Statistical analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Loading the datasets produced in Wind_data.R and Upwelling&SST.R script

# 1: Setup environment ------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(heatwaveR)
## devtools::install_github("robwschlegel/coastR")
library(mgcv) # for gam
library(doParallel); registerDoParallel(cores = 4)
source("functions/theme.R")
options(scipen = 999) 

# 2: Loading the data --------------------------------------------------------------------------------------------------------

# Loading the wind data
load("data_official/winds.RData")

# Loading the temperature data4
load("data_official/temp_monthly.RData")

# Loading the upwelling metrics
load("data_official/upwell_south_BC.RData")
load("data_official/upwell_north_BC.RData")
load("data_official/upwell_Canary_current.RData")
load("data_official/upwell_chile.RData")
load("data_official/upwell_peru.RData")
load("data_official/upwell_south_CalC.RData")
load("data_official/upwell_north_CalC.RData")

current_upwelling <- rbind(upwell_south_BC, upwell_north_BC, upwell_Canary_current,
                           upwell_chile, upwell_peru, upwell_south_CalC, upwell_north_CalC)

upwelling_metrics <- current_upwelling %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(mean_intensity = mean(intensity_mean),
            cum_intensity = mean(intensity_cumulative)) 

# Observing changes in upwelling metrics, wind metrics and temperature overtime. 

# 3: Some analyses ------------------------------------------------------------------------------------------------------------
# Duration
anova_func <- function(df){
  sites_aov <- aov(duration_mean ~ current * year * month, data = df)
  return(sites_aov)
}

summary(dur_aov <- anova_func(df = winds))

wind_dur <- winds %>% 
  group_by(current) %>% 
  mutate(year_month = 1:n()) %>% 
  nest() %>% 
  mutate(model_out = purrr::map(data, ~lm(duration_mean ~ year_month, data = .)),
         model_tidy = purrr::map(model_out, broom::tidy)) %>% 
  dplyr::select(-data, -model_out) %>% 
  unnest(cols = "model_tidy") %>% 
  filter(term == "year_month") %>% 
  dplyr::rename(slope = estimate) %>% 
  mutate(p.value = round(p.value, 4))

# Upwelling favourable wind event count
anova_func <- function(df){
  sites_aov <- aov(event_count ~ current * year * month, data = df)
  return(sites_aov)
}

summary(event_aov <- anova_func(df = winds))

wind_event_count <- winds %>% 
  group_by(current) %>% 
  mutate(year_month = 1:n()) %>% 
  nest() %>% 
  mutate(model_out = purrr::map(data, ~lm(event_count ~ year_month, data = .)),
         model_tidy = purrr::map(model_out, broom::tidy)) %>% 
  dplyr::select(-data, -model_out) %>% 
  unnest(cols = "model_tidy") %>% 
  filter(term == "year_month") %>% 
  dplyr::rename(slope = estimate) %>% 
  mutate(p.value = round(p.value, 4))

# Upwelling favourable wind intensity
anova_func <- function(df){
  sites_aov <- aov(intensity_mean ~ current * year * month, data = df)
  return(sites_aov)
}

summary(intensity_aov <- anova_func(df = winds))

wind_intensity <- winds %>% 
  group_by(current) %>% 
  mutate(year_month = 1:n()) %>% 
  nest() %>% 
  mutate(model_out = purrr::map(data, ~lm(event_count ~ year_month, data = .)),
         model_tidy = purrr::map(model_out, broom::tidy)) %>% 
  dplyr::select(-data, -model_out) %>% 
  unnest(cols = "model_tidy") %>% 
  filter(term == "year_month") %>% 
  dplyr::rename(slope = estimate) %>% 
  mutate(p.value = round(p.value, 4))

# Analysing trends in upwelling metrics

