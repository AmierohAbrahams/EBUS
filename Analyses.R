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
