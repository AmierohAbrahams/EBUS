# Loading libraries
library(tidyverse)
library(lubridate)
library(gamair)
library(openair)


# Loading upwelling metrics
load("~/Documents/EBUS/data_wind/upwell_season_BC.RData")
load("~/Documents/EBUS/data_wind/upwell_season_CC.RData")
load("~/Documents/EBUS/data_wind/upwell_season_CalC.RData")
load("~/Documents/EBUS/data_wind/upwell_season_HC.RData")


# Loading data subset
load("data/BC_metrics.RData")
load("data/HC_metrics.RData")
load("data/CC_metrics.RData")
load("data/CalC_metrics.RData")


# Benguela current
BC_metrics_renamed <- BC_metrics %>% 
  rename(ws = mean_speed,
         wd = dir_wind)
polarPlot(BC_metrics_renamed, pollutant = "total_count", statistic =  "nwr",  kernel = "gaussian")
polarPlot(BC_metrics_renamed, pollutant = "mean_intensity", statistic =  "nwr",  kernel = "gaussian", force.positive = FALSE)

# Humboldt current

HC_metrics_renamed <- HC_metrics %>% 
  rename(ws = mean_speed,
         wd = dir_wind)
polarPlot(HC_metrics_renamed, pollutant = "total_count", statistic =  "nwr",  kernel = "gaussian")
polarPlot(HC_metrics_renamed, pollutant = "mean_intensity", statistic =  "nwr",  kernel = "gaussian", force.positive = FALSE)
# Canary current
CC_metrics_renamed <- CC_metrics %>% 
  rename(ws = mean_speed,
         wd = dir_wind)
polarPlot(CC_metrics_renamed, pollutant = "total_count", statistic =  "nwr",  kernel = "gaussian")
polarPlot(CC_metrics_renamed, pollutant = "mean_intensity", statistic =  "nwr",  kernel = "gaussian", force.positive = FALSE)

# Calafornia current
CalC_metrics_renamed <- CalC_metrics %>% 
  rename(ws = mean_speed,
         wd = dir_wind)
polarPlot(CalC_metrics_renamed, pollutant = "total_count", statistic =  "nwr",  kernel = "gaussian")
polarPlot(CalC_metrics_renamed, pollutant = "mean_intensity", statistic =  "nwr",  kernel = "gaussian", force.positive = FALSE)

# test <- openair::mydata
# polarPlot(mydata, pollutant = "so2", type = "season", main = "polarPlot of so2")


# ANOVA anlayses

options(scipen=999) # Forces R to not use exponential notation

anova_func <- function(df){
  currents_aov <- aov(dir_wind ~ mean_intensity * year, data = df)
  return(currents_aov)
}
anova_BC <- anova_func(df = BC_metrics)
summary(anova_BC)
anova_HC <- anova_func(df = HC_metrics)
summary(anova_HC)
anova_CC <- anova_func(df = CC_metrics)
summary(anova_CC)
anova_CalC <- anova_func(df = CalC_metrics)
summary(anova_CalC)


anova_func_2 <- function(df){
  currents_aov <- aov(mean_intensity ~ mean_dur * year, data = df)
  return(currents_aov)
}
anova_BC_sp <- anova_func_2(df = BC_metrics)
summary(anova_BC_sp)
anova_HC_sp <- anova_func_2(df = HC_metrics)
summary(anova_HC_sp)
anova_CC_sp <- anova_func_2(df = CC_metrics)
summary(anova_CC_sp)
anova_CalC_sp <- anova_func_2(df = CalC_metrics)
summary(anova_CalC_sp)

anova_func <- function(df){
  currents_aov <- aov(mean_dur ~ total_count * year, data = df)
  return(currents_aov)
}
anova_BC_dur <- anova_func(df = BC_metrics)
summary(anova_BC_dur)
anova_HC_dur<- anova_func(df = HC_metrics)
summary(anova_HC_dur)
anova_CC_dur <- anova_func(df = CC_metrics)
summary(anova_CC_dur)
anova_CalC_dur <- anova_func(df = CalC_metrics)
summary(anova_CalC_dur)




