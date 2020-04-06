# Loading libraries
library(tidyverse)
library(lubridate)

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