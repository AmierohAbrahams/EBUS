https://www.fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/
  
## Loading libraries
library('mgcv')
library('brms')
library('ggplot2')
library('schoenberg')
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
Hc_meanInt <- gam_func(df = HC_metrics)
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
Hc_totalC <- gam_func(df = HC_metrics)
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




