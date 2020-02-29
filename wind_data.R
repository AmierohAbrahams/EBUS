# Python script to download wind data

# 10m u-component of wind	
# m s-1	
# Eastward component of the 10m wind. It is the horizontal speed of air 
# moving towards the east, at a height of ten metres above the surface of 
#the Earth, in metres per second. Care should be taken when comparing this
# variable with observations, because wind observations vary on small space and time
#scales and are affected by the local terrain, vegetation and buildings that are represented only on 
#average in the ECMWF Integrated Forecasting System. This variable can be combined with the V component of 10m
#wind to give the speed and direction of the horizontal 10m wind.



# 10m v-component of wind	
# m s-1	
# Northward component of the 10m wind. It is the horizontal speed of air moving towards the north, at a height of ten metres above 
#the surface of the Earth, in metres per second. Care should be taken when comparing this variable with observations, because wind observations
#vary on small space and time scales and are affected by the local terrain, vegetation and buildings that are represented only on average in the ECMWF 
#Integrated Forecasting System. This variable can be combined with the U component of 10m wind to give the speed and direction of the horizontal 10m wind.

#################################################################################################################################################################3


# Converting U and V wind variables to wind speed and direction

# load("data/BC_wind.RData")
# load("data/BC_vwind.RData")
# 
# BC_vwind <- BC_vwind %>% 
#   select(v_10)
# BC_wind <- cbind(BC_vwind,BC_wind)
# BC_wind_fin <- BC_wind %>% 
#   select(lon,lat,date,u_10,v_10)

#save(BC_wind_fin , file = "data/BC_wind_fin.RData")

load("data/BC_wind_fin.RData")
# Wind speed
BC_wind_fin$u_squared ='^'(BC_wind_fin$u_10,2)
BC_wind_fin$v_squared ='^'(BC_wind_fin$v_10,2)
BC_wind_fin <- BC_wind_fin %>% 
  mutate(speed = sqrt(u_squared + v_squared))
# Wind direction 

BC_wind_fin <- BC_wind_fin %>% 
  mutate(wind_dir_trig_to = atan2(u_10/speed, v_10/speed),
         wind_dir = wind_dir_trig_to * 180/pi)

BC_wind_complete <- BC_wind_fin %>% 
  select(lon,lat,date,u_10,v_10,speed,wind_dir)

# save(BC_wind_complete , file = "data/BC_wind_complete.RData")





