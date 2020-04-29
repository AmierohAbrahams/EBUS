# Loading Libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
source("functions/theme.R")

# Analyses done to compare wind blown in a SE direction during summer months

load("data_complete/CalC_complete.RData") # This datasets used here were created in script "1_Temp_wind_data.R"
load("data_complete/CC_complete.RData")
load("data_complete/BC_complete.RData")
load("data_complete/HC_complete.RData")

HC_complete <- HC_complete %>%
  mutate(lon = lon - 360)
CC_complete <- CC_complete %>%
  mutate(lon = lon - 360)
CalC_complete <- CalC_complete %>%
  mutate(lon = lon - 360)

BC_complete <- BC_complete %>% 
  rename(speed = spd) %>% 
  mutate(site = "BC")
HC_final <- HC_final %>% 
  mutate(current = "HC")
CC_final <- BC_final %>% 
  mutate(current = "CC")
CalC_final <- CalC_final %>% 
  mutate(current = "CalC")

combine_currents <- rbind(BC_final,HC_final,CC_final,CalC_final)

wind_renamed_func <- function(df){    # This bit of code is done to get rid of the negative values
    wind_renamed <- df %>% 
      mutate(wind_dir = ifelse(wind_dir < 0, wind_dir+360, wind_dir)) %>%
      dplyr::rename(spd = speed) %>%
      dplyr::rename(dir = wind_dir) %>% 
      filter(spd > 0)
  }

complete_wind <- wind_renamed_func(complete_wind)


SE_renamed <-complete_wind %>% # Changed the names with the data 
  filter(dir >= 180, dir <= 270)
# Then create different temporal results
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

#############################################################################################################################################

