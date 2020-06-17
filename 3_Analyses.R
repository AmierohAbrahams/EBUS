# 7_Analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Observe how wind patterns change over time
# 3: ANOVA analyses
# 4: Linear models
# 5: GAMS
# 6: Trends

# climate change as a result of global warining resulted in changes in wind patterns and
# ultimately lead to changes in the duration and intensity of upwelling events overtime.
# Changing upwelling region boundries for each current

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
library(broom)
library(circular)
source("functions/theme.R")
options(scipen=999) 

# 2: Wind pattern observation ----------------------------------------------------
# Analyses done to compare how the wind blown in a SE direction during summer months varied over a 30 year period

# The datasets used here were created in script "2_upwelling_identification.R"
load("data/CC_coastal.RData") 
load("data/CalC_coastal.RData")
load("data/BC_coastal.RData")
load("data/HC_coastal.RData")

BC <- BC_coastal%>% 
  mutate(current = "BC") 
HC <- HC_coastal %>% 
  mutate(current = "HC")
CC <- CC_coastal %>% 
  mutate(current = "CC")
CalC <- CalC_coastal %>% 
  mutate(current = "CalC")

current_winds <- rbind(BC,HC,CC,CalC)
rm(BC,BC_coastal,CalC,CalC_coastal,CC_coastal,CC, HC_coastal,HC);gc()
# Then create different temporal results
# First filter out only the SE data

SE_renamed <- current_winds %>% 
  filter(wind_dir_from >= 180, wind_dir_from <= 270) %>% 
  unique()

rm(current_winds);gc()

# Then create diifferent temporal results
SE_summer <- SE_renamed %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season) %>% 
  summarise(count = n(),
            circ_dir = mean.circular(circular(wind_dir_from, units = "degrees")),
            circ_wspd = mean.circular(circular(wind_spd, units = "degrees")),
            mean_temp = mean(temp, na.rm = T))

SE_monthly <- SE_renamed %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            circ_dir = mean.circular(circular(wind_dir_from, units = "degrees")),
            circ_wspd = mean.circular(circular(wind_spd, units = "degrees")),
            mean_temp = mean(temp, na.rm = T))

# Determining the number of pixels within each current
BC_pixels <- SE_renamed %>% 
  filter(current == "BC") %>% 
  dplyr::select(lon, lat) %>% 
  unique()

CC_pixels <- SE_renamed %>% 
  filter(current == "CC") %>% 
  dplyr::select(lon, lat) %>% 
  unique()

HC_pixels <- SE_renamed %>% 
  filter(current == "HC") %>% 
  dplyr::select(lon, lat) %>% 
  unique()

CalC_pixels <- SE_renamed %>% 
  filter(current == "CalC") %>% 
  dplyr::select(lon, lat) %>% 
  unique()


# New facet label names
supp.labs <- c("Benguela current", "California current", "Canary current", "Humboldt current")
names(supp.labs) <- c("BC", "CalC","CC", "HC")

# ggplot(data = SE_monthly, aes(x = year, y = count)) +
#   geom_line(aes(colour = month)) +
#   geom_smooth(aes(colour = month), method = "lm") +
#   facet_wrap(~current,  labeller = labeller(current = supp.labs)) +
#   labs(x = "Year", y = "Wind") +
#   theme(strip.text = element_text(face="bold", size=12))

# Monthly mean temperature
ggplot(data = SE_monthly, aes(x = year, y = mean_temp)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current,  labeller = labeller(current = supp.labs)) +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(strip.text = element_text(face="bold", size=12)) +
  theme_Publication()

# No warming changes in SST over a 30 year periodwhen comparing summer seasons.

# For each current and not each pixel
CC_wind <- SE_monthly %>% 
  filter(current == "CC") %>% 
  mutate(signal = count / 106)

BC_wind <- SE_monthly %>% 
  filter(current == "BC") %>% 
  mutate(signal = count / 73)

CalC_wind <- SE_monthly %>% 
  filter(current == "CalC") %>%
  mutate(signal = count / 185)

HC_wind <- SE_monthly %>% 
  filter(current == "HC") %>% 
  mutate(signal = count / 193)

complete_wind <- rbind(CC_wind,BC_wind,CalC_wind,HC_wind)

# Plots
## Annual count of SE wind in Summer
## Summer month count of SE winds

# Number of signals
ggplot(data = complete_wind, aes(x = year, y = signal)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
 facet_wrap(~current,  labeller = labeller(current = supp.labs)) +
  labs(x = "Year", y = "Count") +
  theme(strip.text = element_text(face="bold", size=12)) +
  theme_Publication()


slope_calc <- function(df){
  df %>% 
    mutate(row_num = 1:n()) %>% 
    do(mod1 = lm(count ~ row_num, data = .),
       mod2 = lm(mean_temp ~ row_num, data = .),
       mod3 = lm(mean_temp ~ count, data = .),
       mod4 = cor(.$mean_temp, .$count, method = "pearson", use = "complete.obs")[1]) %>% 
    mutate(wind_slope = summary(mod1)$coeff[2],
           temp_slope = summary(mod2)$coeff[2],
           temp_wind_slope = summary(mod3)$coeff[2],
           temp_wind_r = mod4[1],
           temp_wind_r2 = glance(mod3)$adj.r.squared) %>%
    select(-mod1, -mod2, -mod3, -mod4) %>% 
    mutate_if(is.numeric, round, 2)
}
# glance(lm(mean_temp ~ count, data = SE_annual))
# Summer stats
complete_wind%>% 
  group_by(current, season) %>% 
  slope_calc()

# Monthly summer stats
complete_wind%>% 
  group_by(current, season, month) %>% 
  slope_calc()

# 3: ANOVA analyses ----------------------------------------------------
# ANOVA analyses comparing is the number of gc(signals detected each year and each season varied over time

load("data/BC_UI_metrics.RData")
load("data/HC_UI_metrics.RData")
load("data/CC_UI_metrics.RData")
load("data/CalC_UI_metrics.RData")

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
#save(combined_products, file = "data/combined_products.RData")

load("data/combined_products.RData")

# Total signals at each pixel
total_signals <- BC_UI_metrics %>%
  mutate(year = year(date_start)) %>% 
  group_by(season,year) %>% 
  group_by(lat, lon) %>% 
  summarise(y = n()) %>% 
  rename(count = y) %>% 
  data.frame() 

CC_signals <- CC_UI_metrics %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 106)
  

BC_signals <- BC_UI_metrics %>% 
  mutate(year = year(date_start)) %>% 
group_by( season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 73)

CalC_signals <- CalC_UI_metrics %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 185)


HC_signals <- HC_UI_metrics %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 193)
    
complete_signal <- rbind(CC_signals,BC_signals,CalC_signals,HC_signals)
#save(complete_signal, file = "data_complete/complete_signal.RData")
load("data_complete/complete_signal.RData")

summer_signal <- complete_signal %>% 
  filter(season == "Summer") %>% 
  group_by(year, current, month) 

ggplot(data = summer_signal, aes(x = year, y = signal)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
 facet_wrap(~current,  labeller = labeller(current = supp.labs)) +
  labs(x = "Year", y = "Number of upwelling signals") +
  theme(strip.text = element_text(face="bold", size=12))+
  theme_Publication()

# Anova analyses to test whether or not a significant difference exist in the amount of 
# signals detected by each of the currents for each year and season

anova_func <- function(df){
  sites_aov <- aov(signal ~ current * year, data = df)
  return(sites_aov)
}

summary(count_aov <- anova_func(df = complete_signal))

# 3: Linear models ----------------------------------------------------
# ANOVA Analyses testing if there is a significant difference in the duration/mean intensity etc,
# between currents and seasons over a 30 year period

load("data_complete/combined_products.RData")
# load("data_complete/complete_signal.RData")

# combined_products <- combined_products %>% 
#   drop_na()
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

dur_gam <- gam(measurements ~ s(current), data = dur, method = "REML")

pred <- dur %>%
  select(current, year)

dur_pred <- cbind(dur, as.data.frame(predict(dur_gam, pred, se.fit = TRUE, unconditional = TRUE)))

dur_pred <- transform(dur_pred,
                      upper = fit + (2 * se.fit),
                      lower = fit - (2 * se.fit))

ggplot(tss_pred, aes(x = year, y = measurements, col = current, group = current)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = current), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  labs(x = "Year", y = "Duration (Days") +
  theme_bw()

# Plot upwelling using geom_smooth

ggplot(data = combined_products, aes(x = date_start, y = duration, colour = current)) +
  #geom_point() +
  geom_smooth()



