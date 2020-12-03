# 3_Analyses
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Create new bounding boxes for the correct upwelling regions as per Varela et al., 2018. I used theold EBUS regions and split them according 
# 3: Observe how temperature change overtime
# 4:  Creating the new EBUS regions
# 5: Analysing the upwelling data - Specifically the number of signals detected and cumulative intensity

# 1: Setup environment --------------------------------------------------------------------------------------------------------------------------------------------
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
library(grid)
source("functions/theme.R")
options(scipen=999) 
supp.labs <- c("Benguela current South", "Benguela current North", "Chile", "Peru", "California current South", "California current North", "Canary current")

#2: Correcting the bounding boxes to fit the paper proposed by Varela et al., 2018 and many others----------------------------------------------------------------
# # The datasets used here were created in script "5_SLP.R"
# load("data/CC_coastal_SLP.RData") 
# load("data/CalC_coastal_SLP.RData")
# load("data/BC_coastal_SLP.RData")
# load("data/HC_coastal_SLP.RData")
# 
# # Filtering out to specific regions (as defined by Varela) within each current:
# # Benguela current: North and South Benguela
# south_BC <- BC_coastal_SLP %>% 
#   filter(lat < -24) %>% 
#   mutate(current = "BC_south")
# 
# north_BC <- BC_coastal_SLP %>% 
#   #dplyr::filter(lat < -17) %>% 
#   filter(lat >= -22, lat <= -17) %>% 
#   mutate(current = "BC_north")
# 
# # Canary Current
# Canary_current <- CC_coastal_SLP %>% 
#   filter(lat > 21) %>% 
#   mutate(current = "CC")
# 
# # Humbold current: Chile and Peru 
# chile <- HC_coastal_SLP %>% 
#   filter(lat >= -41 , lat <= -17) %>% 
#   mutate(current = "HC_chile")
# 
# peru <- HC_coastal_SLP %>% 
#   filter(lat >= -18, lat <= -13) %>% 
#   mutate(current = "HC_peru")
# 
# # California current: North and South California
# south_CalC <- CalC_coastal_SLP %>% 
#   filter(lat >= 34, lat <= 37) %>% 
#   mutate(current = "CalC_south")
# 
# north_CalC <- CalC_coastal_SLP %>% 
#   filter(lat >= 38, lat <= 43) %>% 
#   mutate(current = "CalC_north")
# 
# current_winds <- rbind(south_BC, north_BC, Canary_current,chile,peru, south_CalC,north_CalC)
# rm(south_BC, north_BC, Canary_current,chile,peru, south_CalC,north_CalC);gc()
# # save(current_winds, file = "data_official/current_winds.RData")

# 3:Observe how temperature change overtime ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Then create different temporal results
load("data_official/current_winds.RData")

SE_winds <- current_winds %>% 
  filter(wind_dir_from >= 180, wind_dir_from <= 270) %>% 
  unique()
rm(current_winds);gc()
#save(SE_renamed, file = "data/SE_renamed.RData")

# Then create diifferent temporal results
SE_monthly <- SE_winds %>% 
  filter(season == "Summer") %>% 
  group_by(current, year, season, month) %>% 
  summarise(count = n(),
            circ_dir = mean.circular(circular(wind_dir_from, units = "degrees")),
            circ_wspd = mean.circular(circular(wind_spd, units = "degrees")),
            mean_temp = mean(temp, na.rm = T),
            mean_SLP = mean(slp, na.rm = T))

SE_monthly$month <- as.factor(SE_monthly$month)
SE_monthly$month <- factor(SE_monthly$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
SE_monthly$current = factor(SE_monthly$current, levels=c('BC_south','BC_north','HC_chile', 'HC_peru','CalC_south','CalC_north','CC'))
# Monthly mean temperature
ggplot(data = SE_monthly, aes(x = year, y = mean_temp)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "SST (°C)")+
  theme_bw() +
  labs(colour = "Month") +
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=14, family = "Palatino"),
    axis.title = element_text(size = 18, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 14, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 15, hjust = 0),
    legend.title = element_text(size = 12, family = "Palatino"),
    legend.text = element_text(size = 10, family = "Palatino"),
    legend.key = element_rect(size = 0.8, colour = NA),
    legend.background = element_blank())

# # 4: Creating the new regions---------------------------------------------------------------------------------------------------------------------------------------------------------
The data below were enerated previously using the large EBUS areas (Before Varela et al., 2018 split), this next steps simply takes the large areas and split it into the new regions 

# Upwelling metrics 
# load("data/BC_UI_metrics_SLP.RData")
# load("data/HC_UI_metrics_SLP.RData")
# load("data/CC_UI_metrics_SLP.RData")
# load("data/CalC_UI_metrics_SLP.RData")
# 
# seasons_S_func <- function(df){
#   df_seasons <- df %>%
#     mutate(month = month(date_start, abbr = T, label = T),
#            year = year(date_start)) %>%
#     mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Summer",
#                               month %in% c("Mar", "Apr", "May") ~ "Autumn",
#                               month %in% c("Jun", "Jul", "Aug") ~ "Winter",
#                               month %in% c("Sep", "Oct", "Nov") ~ "Spring"))
# }
# 
# BC_UI_metrics <- seasons_S_func(df = BC_UI_metrics_SLP)
# HC_UI_metrics <- seasons_S_func(df = HC_UI_metrics_SLP)
# 
# # Seasons for the Northern Hemisphere
# seasons_N_func <- function(df){
#   df_seasons <- df %>%
#     mutate(month = month(date_start, abbr = T, label = T),
#            year = year(date_start)) %>%
#     mutate(season = case_when(month %in% c("Dec", "Jan", "Feb") ~ "Winter",
#                               month %in% c("Mar", "Apr", "May") ~ "Spring",
#                               month %in% c("Jun", "Jul", "Aug") ~ "Summer",
#                               month %in% c("Sep", "Oct", "Nov") ~ "Autumn"))
# }
# 
# CC_UI_metrics <- seasons_N_func(df = CC_UI_metrics_SLP)
# CalC_UI_metrics <- seasons_N_func(df = CalC_UI_metrics_SLP)
# 
# # Filtering out to specific regions (as defined by Varela) within each current:
# # Benguela current: North and South Benguela
# upwell_south_BC <- BC_UI_metrics %>%
#   filter(lat < -24) %>%
#   mutate(current = "BC_south")
# 
# upwell_north_BC <- BC_UI_metrics %>%
#   #dplyr::filter(lat < -17) %>%
#   filter(lat >= -22, lat <= -17) %>%
#   mutate(current = "BC_north")
# 
# # Canary Current
# upwell_Canary_current <- CC_UI_metrics %>%
#   filter(lat > 21) %>%
#   mutate(current = "CC")
# 
# # Humbold current: Chile and Peru
# upwell_chile <- HC_UI_metrics %>%
#   filter(lat >= -41 , lat <= -17) %>%
#   mutate(current = "HC_chile")
# 
# upwell_peru <- HC_UI_metrics %>%
#   filter(lat >= -18, lat <= -13) %>%
#   mutate(current = "HC_peru")
# 
# # California current: North and South California
# upwell_south_CalC <- CalC_UI_metrics %>%
#   filter(lat >= 34, lat <= 37) %>%
#   mutate(current = "CalC_south")
# 
# upwell_north_CalC <- CalC_UI_metrics %>%
#   filter(lat >= 38, lat <= 43) %>%
#   mutate(current = "CalC_north")

# save(upwell_south_BC, file = "data_official/upwell_south_BC.RData")
# save(upwell_north_BC, file = "data_official/upwell_north_BC.RData")
# save(upwell_Canary_current, file = "data_official/upwell_Canary_current.RData")
# save(upwell_chile, file = "data_official/upwell_chile.RData")
# save(upwell_peru, file = "data_official/upwell_peru.RData")
# save(upwell_south_CalC, file = "data_official/upwell_south_CalC.RData")
# save(upwell_north_CalC, file = "data_official/upwell_north_CalC.RData")

# 5: Analysing the upwelling data - Specifically the number of signals detected and mean intensity -----------------------------------------------------------------------------
# Loading the data
load("data_official/upwell_south_BC.RData")
load("data_official/upwell_north_BC.RData")
load("data_official/upwell_Canary_current.RData")
load("data_official/upwell_chile.RData")
load("data_official/upwell_peru.RData")
load("data_official/upwell_south_CalC.RData")
load("data_official/upwell_north_CalC.RData")

current_upwelling <- rbind(upwell_south_BC, upwell_north_BC, upwell_Canary_current,upwell_chile,upwell_peru, upwell_south_CalC,upwell_north_CalC)
# # rm(south_BC, north_BC, Canary_current,chile,peru, south_CalC,north_CalC);gc()
# save(current_upwelling, file = "data_official/current_upwelling.RData")

 # Here I average the number of signals for each region by dividing the number by the pixels
BC_S_signals <- upwell_south_BC %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 45) # Here we are dividing by the number of pixels within this region

BC_N_signals <- upwell_north_BC %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 16)

CC_signals <- upwell_Canary_current %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 82)

CalC_S_signals <- upwell_south_CalC %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 24)

CalC_N_signals <- upwell_north_CalC %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 20)

Peru_signals <- upwell_peru %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 25)

Chile_signals <- upwell_chile %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(y = n()) %>% 
  mutate(signal = y / 99)

complete_signal <- rbind(BC_S_signals,BC_N_signals,CC_signals,CalC_N_signals, CalC_S_signals, Peru_signals, Chile_signals)

summer_signal <- complete_signal %>% 
  filter(season == "Summer") #%>% 
# group_by(year, current, month)

summer_signal$month <- as.factor(summer_signal$month)
summer_signal$month <- factor(summer_signal$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
summer_signal$current = factor(summer_signal$current, levels=c('BC_south','BC_north','HC_chile', 'HC_peru','CalC_south','CalC_north','CC'))

# Plotting the number of signals per region
plot_1 <- ggplot(data = summer_signal, aes(x = year, y = signal, colour = Month)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current) + #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "Upwelling events
(count)")+
  # geom_smooth(aes(colour = month), method = "lm", se=FALSE, formula = my.formula) +
  # stat_poly_eq(formula = my.formula,
  #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
  #              parse = TRUE) +
  theme_set(theme_grey()) +
  theme_grey() +
  theme(panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
        panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
        #panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
        # panel.grid.major = element_line(size = 0.2, linetype = 2),
        # panel.grid.minor = element_line(colour = NA),
        strip.text = element_text(size=8, family = "Palatino"),
        axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(size = 8, colour = "black", family = "Palatino"),
        plot.title = element_text(size = 18, hjust = 0),
        legend.title = element_text(size = 10, family = "Palatino"),
        legend.text = element_text(size = 9, family = "Palatino"),
        legend.key.size = unit(0.2, "cm"))

# Determining if there is changes in mean intensity of signals (I will repeat this for cummulative intensty)
mean_intensity_BC_S <- upwell_south_BC %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(mean_intensity = mean(intensity_mean))

mean_intensity_BC_N <- upwell_north_BC %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(mean_intensity = mean(intensity_mean))

mean_intensity_CC <- upwell_Canary_current %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(mean_intensity = mean(intensity_mean))

mean_intensity_chile <- upwell_chile %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(mean_intensity = mean(intensity_mean)) 

mean_intensity_peru <- upwell_peru %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(mean_intensity = mean(intensity_mean)) 

mean_intensity_CalC_S <- upwell_south_CalC %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(mean_intensity = mean(intensity_mean)) 

mean_intensity_CalC_N <- upwell_north_CalC %>% 
  mutate(year = year(date_start)) %>% 
  group_by(current, season,year, month) %>% 
  summarise(mean_intensity = mean(intensity_mean)) 

mean_int <- rbind(mean_intensity_BC_S,mean_intensity_BC_N, mean_intensity_CC, mean_intensity_chile,mean_intensity_peru, mean_intensity_CalC_S, mean_intensity_CalC_N)
mean_int <- mean_int %>% 
  filter(season == "Summer")

mean_int$month <- as.factor(mean_int$month)
mean_int$month <- factor(mean_int$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
mean_int$current = factor(mean_int$current, levels=c('BC_south','BC_north','HC_chile', 'HC_peru','CalC_south','CalC_north','CC'))

plot_2 <- ggplot(data = mean_int, aes(x = year, y = mean_intensity, colour = Month)) +
  geom_line(aes(colour = month)) +
  geom_smooth(aes(colour = month), method = "lm") +
  facet_wrap(~current) +  #,  labeller = labeller(current = supp.labs), ncol = 4) +
  labs(x = "", y = "Mean intensity of 
signals")+
  theme_set(theme_grey()) +
  theme_grey() +
  theme(#panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    # panel.grid.major = element_line(size = 0.2, linetype = 2),
    # panel.grid.minor = element_line(colour = NA),
    strip.text = element_text(size=8, family = "Palatino"),
    axis.title = element_text(size = 9, face = "bold", family = "Palatino"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_line("grey70", linetype = "dashed", size = 0.2),
    panel.grid.minor = element_line("grey70", linetype = "dashed", size = 0.2),
    axis.text = element_text(size = 8, colour = "black", family = "Palatino"),
    plot.title = element_text(size = 18, hjust = 0),
    legend.title = element_text(size = 10, family = "Palatino"),
    legend.text = element_text(size = 9, family = "Palatino"),
    #legend.key = element_rect(size = 0.8, colour = NA),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_blank())