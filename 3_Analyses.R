# 7_Analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Observe how wind patterns change over time
# 3: ANOVA analyses
# 4: Linear models
# 5: GAMS

# climate change as a result of global warining resulted in changes in wind patterns and
# ultimately lead to changes in the duration and intensity of upwelling events overtime.

# 1: Setup environment ----------------------------------------------------
library(gridExtra)
library(geosphere)
library(tidyverse)
library(lubridate)
library(heatwaveR)
## devtools::install_github("robwschlegel/coastR")
library(coastR)
library(mgcv) # for gam
library(FNN)
source("functions/theme.R")
options(scipen=999) 

# 2: Wind pattern observation ----------------------------------------------------
# Analyses done to compare how the wind blown in a SE direction during summer months varied over a 30 year period

load("data_complete/CalC_complete.RData") # This datasets used here were created in script "1_Temp_wind_data.R and 4_EBUS_Temp_wind_data.R"
load("data_complete/CC_complete.RData")
load("data_complete/BC_complete.RData")
load("data_complete/HC_complete.RData")

BC_final<- BC_complete %>% 
  mutate(current = "BC") %>% 
  rename(u = u_10,
         v = v_10)
HC_final <- HC_complete %>% 
  mutate(current = "HC")
CC_final <- CC_complete %>% 
  mutate(current = "CC")
CalC_final <- CalC_complete %>% 
  mutate(current = "CalC")

rm(BC_complete); gc()
rm(HC_complete); gc()
rm(CC_complete); gc()
rm(CalC_complete); gc()

combine_currents <- rbind(BC_final,HC_final,CC_final,CalC_final)

rm(BC_final); gc()
rm(HC_final); gc()
rm(CC_final); gc()
rm(CalC_final); gc()

SE_renamed <-combined_currents %>% # Chnaged the names with the data 
  filter(dir >= 180, dir <= 270)
# Then create diifferent temporal results
SE_annual <- SE_renamed %>% 
  group_by(current, year) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
SE_summer <- SE_renamed %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
SE_monthly <- SE_renamed %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            mean_dir = mean(dir, na.rm = T),
            mean_temp = mean(temp, na.rm = T))
# Plots
## Annual count of SE wind
ggplot(data = SE_annual, aes(x = year, y = count)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~current)
## Annual count of SE wind in Summer
### The trends between annual and summer SE wind counts are remarkably similar
ggplot(data = SE_summer, aes(x = year, y = count)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~current)
## Summer month count of SE winds
ggplot(data = SE_monthly, aes(x = year, y = count)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current)

# 3: ANOVA analyses ----------------------------------------------------
# ANOVA anlyses coparing is the number of signals detected each year and each season varied over time

load("data_complete/BC_UI_metrics.RData")
load("data_complete/HC_UI_metrics.RData")
load("data_complete/CC_UI_metrics.RData")
load("data_complete/CalC_UI_metrics.RData")

# Seasons for the southern hemisphere
seasons_S_func <- function(df){
  df_seasons <- df %>% 
    mutate(month = month(date_start, abbr = T, label = T),
           year = year(date_start)) %>% 
    mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Summer", 
                              month %in% c("Mar", "Apr", "May") ~ "Autumn",
                              month %in% c("Jun", "Jul", "Aug") ~ "Winter",
                              month %in% c("Sep", "Oct", "Nov") ~ "Spring"))
}

BC_UI_metrics <- seasons_S_func(df = BC_UI_metrics)
HC_UI_metrics <- seasons_S_func(df = HC_UI_metrics)

# Seasons for the Northern Hemisphere
seasons_N_func <- function(df){
  df_seasons <- df %>% 
    mutate(month = month(date_start, abbr = T, label = T),
           year = year(date_start)) %>% 
    mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Winter", 
                              month %in% c("Mar", "Apr", "May") ~ "Spring",
                              month %in% c("Jun", "Jul", "Aug") ~ "Summer",
                              month %in% c("Sep", "Oct", "Nov") ~ "Autumn"))
}

CC_UI_metrics <- seasons_N_func(df = CC_UI_metrics)
CalC_UI_metrics <- seasons_N_func(df = CalC_UI_metrics)

BC_UI_metrics <- BC_UI_metrics %>% 
  mutate(current = "BC")
HC_UI_metrics <- HC_UI_metrics %>% 
  mutate(current = "HC")
CC_UI_metrics <- CC_UI_metrics %>% 
  mutate(current = "CC")
CalC_UI_metrics <- CalC_UI_metrics %>% 
  mutate(current = "CalC")


combined_products <- rbind(BC_UI_metrics,HC_UI_metrics,CC_UI_metrics,CalC_UI_metrics)
# save(combined_products, file = "data_complete/combined_products.RData")

load("data_complete/combined_products.RData")

total_signals <- combined_products %>%
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year) %>% 
  summarise(y = n()) %>% 
  rename(count = y)

anova_func <- function(df){
  sites_aov <- aov(count ~ current *year * season, data = df)
  return(sites_aov)
}

summary(count_aov<- anova_func(df = total_signals))

# 3: Linear models ----------------------------------------------------
# ANOVA Analyses testing if there is a significant difference in the duration/mean intensity etc, between currents and seasons over a 30 year period

load("data_complete/combined_products.RData")

# Calculate all of the linear models
lm_coeff <- function(df){
  res <- lm(formula = val ~ date_peak, data = df)
  res_coeff <- as.numeric(res$coefficient[2])
}

lm_metrics <- combined_products %>% 
  ungroup() %>% 
  dplyr::select(-c(lon:index_end), -c(month)) %>% 
  pivot_longer(cols = c(duration, intensity_mean:rate_decline), 
               names_to = "var", values_to = "val") %>% 
  group_by(current, season, year, var) %>% 
  nest() %>% 
  mutate(slope = purrr::map(data, lm_coeff)) %>% 
  dplyr::select(-data) %>% 
  unnest(cols = slope) %>% 
  # convert from daily to decadal values
  mutate(slope = round((slope*365.25*10), 2)) %>% 
  ungroup()

lm_metrics_wide <- pivot_wider(lm_metrics, 
                               id_cols = current:season, 
                               names_from = var, values_from = slope)

summary(aov(duration ~ current + season + year, data = lm_metrics_wide))
summary(aov(intensity_mean ~ current + season, data = lm_metrics_wide))
summary(aov(intensity_max ~ current  + season, data = lm_metrics_wide))
summary(aov(intensity_cumulative ~ current + season, data = lm_metrics_wide))

# 4: GAMS ----------------------------------------------------
# GAMs used for findin a non-linear phenomonon but needs to be accounted for when making inferece about other variables.
# Duration? - Observe if there was non linear change in the duration over a 30 year period?
# Cummulative intensity? - Observe if there was non linear change in the cummulative intensity over a 30 year period?

# make a tidy data frame
tidy_combined <- gather(combined_products, key = variable, value = measurements, c(duration, intensity_mean:rate_decline))
dur <- tidy_combined  %>%
  filter(variable == "duration")

dur_gam <- gam(measurements ~ s(current, k = 6), data = dur, method = "REML")

pred <- dur %>%
  select(current, year)
dur_pred <- cbind(dur, as.data.frame(predict(dur_gam, pred, se.fit = TRUE, unconditional = TRUE)))

dur_pred <- transform(dyr_pred,
                      upper = fit + (2 * se.fit),
                      lower = fit - (2 * se.fit))

ggplot(tss_pred, aes(x = year, y = measurements, col = current, group = current)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = current), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  labs(x = "Year", y = "Duration (Days") +
  theme_bw()





















