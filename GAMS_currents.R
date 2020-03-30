library('mgcv')
library('brms')
library('ggplot2')
library('schoenberg')
library(lubridate)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())

######## Loading Metrics

load("data/BC_metrics.RData")
load("data/HC_metrics.RData")
load("data/CC_metrics.RData")
load("data/CalC_metrics.RData")

# ####Total count and wind speed
# BC_totalC_wind_spd<- gam(total_count ~ s(mean_speed) + season, data = BC_metrics, method = "REML")
# summary(BC_totalC_wind_spd)
# plot(BC_totalC_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
# plot(BC_totalC_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
# plot(BC_totalC_wind_spd, pages = 1, scheme = 2, unconditional = TRUE)
# 
# 
# ### Mean_intensity and wind direction
# BC_meanInt_wind_dir<- gam(mean_intensity ~ s(mean_wind) + season, data = BC_metrics, method = "REML")
# summary(BC_meanInt_wind_dir)
# plot(BC_meanInt_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
# plot(BC_meanInt_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
# plot(BC_meanInt_wind_dir, pages = 1, scheme = 2, unconditional = TRUE)

## Humboldt current
##### Mean intensity
HC_meanInt_wind_spd<- gam(mean_intensity ~ s(mean_speed) + s(mean_wind), data = HC_metrics, method = "REML")
summary(HC_meanInt_wind_spd)
plot(HC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(HC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(HC_meanInt_wind_spd, pages = 1, scheme = 2, unconditional = TRUE)

####Total count
HC_totalC_wind_dir<- gam(total_count ~ s(mean_speed) + s(mean_wind), data = HC_metrics, method = "REML")
summary(HC_totalC_wind_dir)
plot(HC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(HC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(HC_totalC_wind_dir, pages = 1, scheme = 2, unconditional = TRUE)

## Canary current
##### Mean intensity
CC_meanInt_wind_spd<- gam(mean_intensity ~ s(mean_speed) + s(mean_wind), data = CC_metrics, method = "REML")
summary(CC_meanInt_wind_spd)
plot(CC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(CC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(CC_meanInt_wind_spd, pages = 1, scheme = 2, unconditional = TRUE)


CC_totalC_wind_dir<- gam(total_count ~ s(mean_speed) + s(mean_wind), data = CC_metrics, method = "REML")
summary(HC_totalC_wind_dir)
plot(CC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(CC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(CC_totalC_wind_dir, pages = 1, scheme = 2, unconditional = TRUE)

## California current
##### Mean intensity
CalC_meanInt_wind_spd<- gam(mean_intensity ~ s(mean_speed) + s(mean_wind), data = CalC_metrics, method = "REML")
summary(CalC_meanInt_wind_spd)
plot(CalC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(CalC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(CalC_meanInt_wind_spd, pages = 1, scheme = 2, unconditional = TRUE)

####Total count
CalC_totalC_wind_dir<- gam(total_count ~ s(mean_speed) + s(mean_wind), data = CalC_metrics, method = "REML")
summary(CalC_totalC_wind_dir)
plot(CalC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(CalC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(CalC_totalC_wind_dir, pages = 1, scheme = 2, unconditional = TRUE)


