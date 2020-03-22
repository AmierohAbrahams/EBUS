library(tidyverse) 
library(heatwaveR) 
  library(FNN) 
  library(tidync) 
library(SDMTools)
library(lubridate)

load("data_complete/CC_complete.RData")
load("data_complete/CalC_complete.RData")
load("data_complete/HC_complete.RData")
load("data_complete/BC_complete.RData")


HC_complete <- HC_complete %>% 
  mutate(lon = lon - 360)

CC_complete <- CC_complete %>% 
  mutate(lon = lon - 360)

CalC_complete <- CalC_complete %>% 
  mutate(lon = lon - 360)

# Plotting the region and it shows the entire CC
# ggplot(CC_complete, aes(x = lon, y = lat)) +
#   geom_raster(data = CC_complete, aes(fill = temp))

final_dataset <- function(df){
final <- df %>%
  group_by(date) %>% 
  summarise(mean_temp = mean(temp),
            mean_speed = mean(speed), # change spd to speed
            mean_wind = mean(wind_dir),
            mean_lat = mean(lat),
            mean_lon = mean(lon))
}

CC_final <- final_dataset(df = CC_complete)
HC_final <- final_dataset(df = HC_complete)
CalC_final <- final_dataset(df = CalC_complete)
BC_final <- final_dataset(df = BC_complete)

save(CC_final, file = "data_complete/CC_final.RData")
save(HC_final, file = "data_complete/HC_final.RData")
save(CalC_final, file = "data_complete/CalC_final.RData")
save(BC_final, file = "data_complete/BC_final.RData")




