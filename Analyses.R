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

######################################################################################################################################################33

upwell_season_BC <- upwell_season_BC %>% 
  mutate(current = "BC")

upwell_season_HC <- upwell_season_HC %>% 
  mutate(current = "HC")

upwell_season_CC <- upwell_season_CC %>% 
  mutate(current = "CC")

upwell_season_CalC <- upwell_season_CalC %>% 
  mutate(current = "CalC")

mertrics_combined <- rbind(upwell_season_BC,upwell_season_HC, upwell_season_CC, upwell_season_CalC)


lm_coeff <- function(df){
  res <- lm(formula = val ~ date_peak, data = df)
  res_coeff <- as.numeric(res$coefficient[2])
}

# Calculate all of the linear models
lm_metrics <- mertrics_combined %>% 
  ungroup() %>% 
  dplyr::select(-c(speed:index_end), -c(date:season)) %>% 
  pivot_longer(cols = c(duration, intensity_mean:rate_decline), 
               names_to = "var", values_to = "val") %>% 
  group_by(current, var) %>% 
  nest() %>% 
  mutate(slope = purrr::map(data, lm_coeff)) %>% 
  dplyr::select(-data) %>% 
  unnest(cols = slope) %>% 
  # convert from daily to decadal values
  mutate(slope = round((slope*365.25*10), 2)) %>% 
  ungroup()

# Cast the results wide for easier use with ANOVA
lm_metrics_wide <- pivot_wider(lm_metrics, 
                               id_cols = current, 
                               names_from = var, values_from = slope)


# Here are ANOVAs on the linear model results
# NB: This is not advisable as these linear models are based on only 4 years of data.
# That is not enough to draw any conclusions about rates of change over time
# I suppose the arguments could be made that one is not measuring lon term change, 
# but rather than the data products should show reasonable agreement for events detected
# within any gicen year.
summary(aov(duration ~ site + product + distance, data = lm_metrics_wide))
summary(aov(intensity_mean ~ site + product + distance, data = lm_metrics_wide))
summary(aov(intensity_max ~ site + product + distance, data = lm_metrics_wide))
summary(aov(intensity_cumulative ~ site + product + distance, data = lm_metrics_wide))
