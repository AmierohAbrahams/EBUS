https://www.fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/
  
## Loading libraries
library('mgcv')
library('brms')
library('ggplot2')
library('schoenberg')
library(lubridate)
library(ggpubr)
theme_set(theme_bw())

# Loading data
load("data/BC_metrics.RData")
load("data/HC_metrics.RData")
load("data/CC_metrics.RData")
load("data/CalC_metrics.RData")


load("data/upwell_season_BC.RData")
load("data/upwell_season_CC.RData")
load("data/upwell_season_CalC.RData")
load("data/upwell_season_HC.RData")

BC <- gam(cum_intensity ~ s(year), data = df, method = "REML")
summary(BC)
plot(BC)

## Cumulative intensity
# Aim: To model the cumulative intensity as a function of year
gam_func <- function(df){
  gam<- brm(bf(cum_intensity ~ s(year)), #brm function: estimates using a full bayesian approach
          data = df, family = gaussian(), cores = 4, seed = 17,
          iter = 4000, warmup = 1000, thin = 100, refresh = 0,
          control = list(adapt_delta = 0.99))
}

BC_cumInt <- gam_func(df = BC_metrics)
CalC_cumInt <- gam_func(df = CalC_metrics)
Hc_cumInt <- gam_func(df = HC_metrics)
CC_cumInt <- gam_func(df = CC_metrics)

summary(BC_cumInt)
summary(HC_cumInt)
summary(CC_cumInt)
summary(CalC_cumInt)

# Plotting (can be found in pictures folder)
BC_cumInt <- marginal_smooths(BC_cumInt)
BC_cumInt_plot <- plot(BC_cumInt)
HC_cumInt <- marginal_smooths(HC_cumInt)
HC_cumInt_plot <- plot(HC_cumInt)
CC_cumInt <- marginal_smooths(CC_cumInt)
CC_cumInt_plot <- plot(CC_cumInt)
CalC_cumInt <- marginal_smooths(CalC_cumInt)
CalC_cumInt_plot <- plot(CalC_cumInt)


## Mean duration
gam_func <- function(df){
  gam<- brm(bf(mean_dur~ s(year)),
            data = df, family = gaussian(), cores = 4, seed = 17,
            iter = 4000, warmup = 1000, thin = 100, refresh = 0,
            control = list(adapt_delta = 0.99))
}

BC_meanDur<- gam_func(df = BC_metrics)
HC_meanDur <- gam_func(df = HC_metrics)
CC_meanDur <- gam_func(df = CC_metrics)
CalC_meanDur<- gam_func(df = CalC_metrics)

summary(BC_meanDur)
summary(HC_meanDur)
summary(CC_meanDur)
summary(CalC_meanDur)

# Plotting (can be found in pictures folder)
BC_meanDur <- marginal_smooths(BC_meanDur)
BC_meanDur_plot <- plot(BC_meanDur)
HC_meanDur <- marginal_smooths(HC_meanDur)
HC_meanDur_plot <- plot(HC_meanDur)
CC_meanDur <- marginal_smooths(CC_meanDur)
CC_meanDur_plot <- plot(CC_meanDur)
CalC_meanDur <- marginal_smooths(CalC_meanDur)
CalC_meanDur_plot <- plot(CalC_meanDur)

## Mean_intensity

gam_func <- function(df){
  gam<- brm(bf(mean_intensity~ s(year)),
            data = df, family = gaussian(), cores = 4, seed = 17,
            iter = 4000, warmup = 1000, thin = 100, refresh = 0,
            control = list(adapt_delta = 0.99))
}

BC_meanInt<- gam_func(df = BC_metrics)
HC_meanInt <- gam_func(df = HC_metrics)
CC_meanInt <- gam_func(df = CC_metrics)
CalC_meanInt<- gam_func(df = CalC_metrics)

summary(BC_meanInt)
summary(HC_meanInt)
summary(CC_meanInt)
summary(CalC_meanInt)

# Plotting (can be found in pictures folder)
BC_meanInt <- marginal_smooths(BC_meanInt)
BC_meanInt_plot <- plot(BC_meanInt)
HC_meanInt <- marginal_smooths(HC_meanInt)
HC_meanInt_plot <- plot(HC_meanInt)
CC_meanInt <- marginal_smooths(CC_meanInt)
CC_meanInt_plot <- plot(CC_meanInt)
CalC_meanInt <- marginal_smooths(CalC_meanInt)
CalC_meanInt_plot <- plot(CalC_meanInt)

## Total count

gam_func <- function(df){
  gam<- brm(bf(total_count~ s(year)),
            data = df, family = gaussian(), cores = 4, seed = 17,
            iter = 4000, warmup = 1000, thin = 100, refresh = 0,
            control = list(adapt_delta = 0.99))
}

BC_totalC<- gam_func(df = BC_metrics)
HC_totalC <- gam_func(df = HC_metrics)
CC_totalC <- gam_func(df = CC_metrics)
CalC_totalC<- gam_func(df = CalC_metrics)

summary(BC_totalC)
summary(HC_totalC)
summary(CC_totalC)
summary(CalC_totalC)

# Plotting (can be found in pictures folder)
BC_totalC <- marginal_smooths(BC_totalC)
BC_totalC_plot <- plot(BC_totalC)
HC_totalC <- marginal_smooths(HC_totalC)
HC_totalC_plot <- plot(HC_totalC)
CC_totalC <- marginal_smooths(CC_totalC)
CC_totalC_plot <- plot(CC_totalC)
CalC_totalC <- marginal_smooths(CalC_totalC)
CalC_totalC_plot <- plot(CalC_totalC)
########################################################333
library(gamair)
options(scipen=999)
# Fit the model
# Loading data

load("data/BC_metrics.RData")
load("data/HC_metrics.RData")
load("data/CC_metrics.RData")
load("data/CalC_metrics.RData")

cumInt_func <- function(df){
  cumInt <- gam(cum_intensity~ s(year),data = df, method = "REML")
}

BC_cumInt <- cumInt_func(df = BC_metrics)
HC_cumInt <- cumInt_func(df = HC_metrics)
CC_cumInt <- cumInt_func(df = CC_metrics)
CalC_cumInt <- cumInt_func(df = CalC_metrics)

summary(BC_cumInt)
summary(HC_cumInt)
summary(CC_cumInt)
summary(CalC_cumInt)

  meanDur_func <- function(df){
  meanDur <- gam(mean_dur~ s(year),data = df, method = "REML")
}

BC_meanDur <- meanDur_func(df = BC_metrics)
HC_meanDur <- meanDur_func(df = HC_metrics)
CC_meanDur <- meanDur_func(df = CC_metrics)
CalC_meanDur <- meanDur_func(df = CalC_metrics)

summary(BC_meanDur)
summary(HC_meanDur)
summary(CC_meanDur)
summary(CalC_meanDur)

intensity_func <- function(df){
  intensity <- gam(mean_intensity~ s(year),data = df, method = "REML")
}

BC_intensity <- intensity_func(df = BC_metrics)
HC_intensity <- intensity_func(df = HC_metrics)
CC_intensity <- intensity_func(df = CC_metrics)
CalC_intensity <- intensity_func(df = CalC_metrics)

summary(BC_intensity)
summary(HC_intensity)
summary(CC_intensity)
summary(CalC_intensity)


totalC_func <- function(df){
  totalC <- gam(total_count~ s(year),data = df, method = "REML")
}

BC_totalC <- totalC_func(df = BC_metrics)
HC_totalC <- totalC_func(df = HC_metrics)
CC_totalC <- totalC_func(df = CC_metrics)
CalC_totalC <- totalC_func(df = CalC_metrics)

summary(BC_totalC)
summary(HC_totalC)
summary(CC_totalC)
summary(CalC_totalC)


plot(BC_totalC)

AIC(BC_totalC)
logLik.gam(BC_totalC)
plot(BC_totalC)
plot(BC_totalC, pages=1, scale=F, shade=T)

plot(BC_totalC, residuals = TRUE)

#######################################################################################################################################################
### LOading libraries

library('mgcv')
library('brms')
library('ggplot2')
library('schoenberg')
library(lubridate)
library(tidyverse)
library(ggpubr)
library(openair)
theme_set(theme_bw())

######## Loading Metrics

load("data/BC_metrics.RData")
load("data/HC_metrics.RData")
load("data/CC_metrics.RData")
load("data/CalC_metrics.RData")


# ggplot(data = BC_metrics, aes(x = year, y =total_count)) +
#   geom_point()+
#   geom_smooth(method = "lm") +
#   geom_jitter(data = BC_metrics, aes(x = year, y = mean_speed),
#               shape = 5, width = 0.05) +
#   geom_smooth(method = "lm")

BC_metrics <- BC_metrics %>% 
  mutate(current = "BC")
HC_metrics <- HC_metrics %>% 
  mutate(current = "HC")
CC_metrics <- CC_metrics %>% 
  mutate(current = "CC")
CalC_metrics <- CalC_metrics %>% 
  mutate(current = "CalC")

combined_metrics <- rbind(BC_metrics,HC_metrics,CC_metrics,CalC_metrics)

### Plotting
ggplot(combined_metrics, aes(x = year, y = total_count, colour = current)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')+
  facet_wrap(~season)

ggplot(combined_metrics, aes(x = year, y = mean_intensity, colour = current)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top')+
  facet_wrap(~season)

# Benguela Current

wind_renamed_func <- function(df){
  wind_renamed <- df %>% 
    mutate(dir_wind = ifelse(mean_wind < 0, mean_wind+360, mean_wind)) %>%
    dplyr::rename(mean_speed = mean_speed) %>%
    dplyr::rename(dir_wind = dir_wind) %>% 
    filter(mean_speed > 0)
}

BC_metrics <- wind_renamed_func(df = BC_metrics)

# https://davidcarslaw.github.io/openair/reference/polarPlot.html
BC_metrics_renamed <- BC_metrics %>% 
  rename(ws = mean_speed,
         wd = dir_wind)
polarPlot(BC_metrics_renamed, pollutant = "total_count", statistic =  "nwr",  kernel = "gaussian")
polarPlot(BC_metrics_renamed, pollutant = "mean_intensity", statistic =  "nwr",  kernel = "gaussian", force.positive = FALSE)

### Mean_intensity and wind speed
BC_meanInt_wind_spd<- gam(mean_intensity ~ s(mean_speed) + s(dir_wind), data = BC_metrics, method = "REML")
summary(BC_meanInt_wind_spd)
plot(BC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(BC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(BC_meanInt_wind_spd, pages = 1, scheme = 2, unconditional = TRUE)

# fit the model
# predict.gam() is used to generate predictions and standard errors
BC_metrics<- BC_metrics %>%
  ungroup
BC_pred <- as.data.frame(predict(BC_meanInt_wind_spd, se.fit = TRUE, unconditional = TRUE))
BC_pred <- transform(BC_pred,
                     upper = fit + (2 * se.fit),
                     lower = fit - (2 * se.fit)) %>% 
  ungroup()
tester <- cbind(BC_pred,BC_metrics) 


ggplot(tester,  aes(x = year, y = mean_intensity)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  facet_wrap(~season) +
theme_bw()

####Total count and wind direction
BC_totalC_wind_dir<- gam(total_count ~ s(mean_speed) + s(dir_wind), data = BC_metrics, method = "REML")
summary(BC_totalC_wind_dir)
plot(BC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(BC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(BC_totalC_wind_dir, pages = 1, scheme = 2, unconditional = TRUE)


# fit the model
# predict.gam() is used to generate predictions and standard errors
BC_metrics<- BC_metrics %>%
  ungroup
BC_pred <- as.data.frame(predict(BC_totalC_wind_dir, se.fit = TRUE, unconditional = TRUE))
BC_pred <- transform(BC_pred,
                     upper = fit + (2 * se.fit),
                     lower = fit - (2 * se.fit)) %>% 
  ungroup()
tester <- cbind(BC_pred,BC_metrics) 


ggplot(tester,  aes(x = year, y = total_count)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  facet_wrap(~season)+
theme_bw()
