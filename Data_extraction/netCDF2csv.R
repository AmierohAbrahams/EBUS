# netCDF2csv
# Loading libraries
  
library(ncdf4) # library for processing netCDFs
library(ncdf4)
library(data.table)
library(tidyverse)
library(reshape2)
# library(plyr)
library(lubridate)
library(stringr)
library(doMC); doMC::registerDoMC(cores = 4)
library(heatwaveR)


# bbox were determined using the parameters in the following paper
# Reduced Nearshore Warming Associated With Eastern Boundary Upwelling Systems 
# Rui Seabra 1 , Rubén Varela 2 , António M. Santos 1,3 , Moncho Gómez-Gesteira 2 ,
# Claudia Meneghesso 1,3 , David S. Wethey 4 and Fernando P. Lima 1 *

# Under Pressure: Climate Change, Upwelling, and Eastern Boundary  Upwelling Ecosystems
# Marisol García-Reyes 1 *, William J. Sydeman 1 , David S. Schoeman 2 ,
# Ryan R. Rykaczewski 3 , Bryan A. Black 4 , Albertus J. Smit 5 and Steven J. Bograd 6

bbox <- data.frame(BC = c(-35, -25, 15, 20), # Benguela Current
                   CC = c(25, 35, 340, 355), # Canary Current
                   CalC = c(35, 45, 225, 240), # California Current
                   HC = c(-45.5, -7.5, 275, 290), # Humboldt Current
                   row.names = c("latmin", "latmax", "lonmin", "lonmax"))

# Setting path
nc.dir <- "/home/amieroh/Documents/Data/Datasets/AVHRR/OISST"
csv.dir <- "/home/amieroh/Documents/Data/Datasets/AVHRR/spatial" # Also stored on my harddrive

read_nc <- function(ncFile, location = location, csv.dir = csv.dir) {
  coords <- bbox[,location]
  nc <- nc_open(ncFile)
  pathLen <- nchar(nc.dir) + 1 
  name.stem <-
    substr(ncFile, pathLen + 1, pathLen + 13)
  date.stamp <- substr(ncFile, pathLen + 15, pathLen + 22)
  LatIdx <- which(nc$dim$lat$vals > coords[1] & nc$dim$lat$vals < coords[2])
  LonIdx <- which(nc$dim$lon$vals > coords[3] & nc$dim$lon$vals < coords[4])
  sst <- ncvar_get(nc,
                   varid = "sst",
                   start = c(LonIdx[1], LatIdx[1], 1, 1),
                   count = c(length(LonIdx), length(LatIdx), 1, 1)) %>%
    round(4) 
  dimnames(sst) <- list(lon = nc$dim$lon$vals[LonIdx],
                        lat = nc$dim$lat$vals[LatIdx])
  nc_close(nc)
  sst <-
    as.data.table(melt(sst, value.name = "temp"), row.names = NULL) %>%
    mutate(t = ymd(date.stamp)) %>%
    na.omit()
  fwrite(sst,
         file = paste(csv.dir, "/", location, "-", name.stem, ".", strtDate, "-", endDate, ".csv", sep = ""),
         append = TRUE, col.names = FALSE)
  rm(sst)
}

# the list of files
ncList <- list.files(path = nc.dir, pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
strtDate <- str_sub(ncList[1], start = 15, end = 22)
endDate <- str_sub(ncList[length(ncList)], start = 15, end = 22)

## apply the function
# system.time(llply(ncList, read_nc, location = "BC", csv.dir = csv.dir, .parallel = TRUE))
# system.time(llply(ncList, read_nc, location = "CC", csv.dir = csv.dir, .parallel = TRUE))
# system.time(llply(ncList, read_nc, location = "CalC", csv.dir = csv.dir, .parallel = TRUE))
# system.time(llply(ncList, read_nc, location = "HC", csv.dir = csv.dir, .parallel = TRUE))

BC_temp <- BC-avhrr-only-v2.Document-Document.csv
CC_temp <- CC-avhrr-only-v2.Document-Document.csv
CalC_temp <- CalC-avhrr-only-v2.Document-Document.csv
HC_temp <- HC-avhrr-only-v2.Document-Document.csv

################  ERA 5    ##################3
# https://cds.climate.copernicus.eu/api-how-to#use-the-cds-api-client-for-data-access

# Extracting ERA 5 wind u and v variables
# N/W/S/E coordinate format when downloading
# -26.00/15.00/-36.00/22.00

# Loading libraries
library(stringr)
library(tidyverse)
library(reshape2)
library(ncdf4) # library for processing netCDFs
# library(plyr)
library(lubridate)
library(data.table)
library(doMC); doMC::registerDoMC(cores = 7)

# ERA.dir <- "/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate"
# ERA.csv.dir <- "/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate/Extracted"

####### u-wind 2008-2019
# #          1         2         3         4
# # 12345678901234567890123456789012345678901
# # wu_daily08-19.nc



ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/BC/wind_daily81-99_165.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 13)
u_10 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
u_10_df <- as_tibble(melt(u_10, value.name = "u_10"))
u_10_df$time <- as.POSIXct(u_10_df$time * 60 * 60, origin = "1900-01-01")
tidy <- u_10_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,u_10)
u_10_df_BC <- new %>%
  unite(year, month, day, col = "date", sep = "-")

# save(u_10_df_BC , file = "data/u_10_df_BC.RData")


ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/BC/wind_165.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 14)
u_10.2 <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10.2) <- list(lon = nc$dim$lon$vals,
                         lat = nc$dim$lat$vals,
                         time = nc$dim$time$vals)
nc_close(nc)
u_10.2_df <- as_tibble(melt(u_10.2, value.name = "u_10"))
u_10.2_df$time <- as.POSIXct(u_10.2_df$time * 60 * 60, origin = "1900-01-01")
tidy <- u_10.2_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,u_10)
u_10.2df_BC <- new %>%
  unite(year, month, day, col = "date", sep = "-")

# BC_wind <- rbind(u_10_df_BC,u_10.2df_BC)

load("data/BC_wind.RData")
BC_wind <-rbind(BC_wind,u_10.2df_BC)

# save(BC_wind , file = "data/BC_wind.RData")
# THis file too big- Get from 2008-2012
ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate/wu_daily08-19.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 14)
u_10.3_df <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u_10.3_df) <- list(lon = nc$dim$lon$vals,
                            lat = nc$dim$lat$vals,
                            time = nc$dim$time$vals)
nc_close(nc)
u_10.3_df <- as_tibble(melt(u_10.3_df, value.name = "u_10"))
u_10.3_df$time <- as.POSIXct(u_10.3_df$time * 60 * 60, origin = "1900-01-01")
tidy <- u_10.3_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,u_10)
u_10.2df <- new %>%
  unite(year, month, day, col = "date", sep = "-")



# save(u_10.2df_BC , file = "data/u_10.2df_BC.RData")
# BC_wind <- rbind(u_10_df_BC,u_10.2df_BC)
# save(BC_wind , file = "data/BC_wind.RData")
# 165 - u 
# 166 - v
################################################################################################################################

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/u_v_seperate/wv_daily08-19.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 14)
v_10.3_df <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10.3_df) <- list(lon = nc$dim$lon$vals,
                            lat = nc$dim$lat$vals,
                            time = nc$dim$time$vals)
nc_close(nc)
v_10.3_df <- as_tibble(melt(v_10.3_df, value.name = "u_10"))
v_10.3_df$time <- as.POSIXct(v_10.3_df$time * 60 * 60, origin = "1900-01-01")
tidy <- v_10.3_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,u_10)
u_10.2df <- new %>%
  unite(year, month, day, col = "date", sep = "-")

ERA_5_wind <- rbind(u_10_df, u_10.2_df, u_10.3_df)
####################################################################################################################################################
####################################################################################################################################################
# 166 - V

ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/wind_daily81-99_166.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 13)
v_10 <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
v_10_df <- as_tibble(melt(v_10, value.name = "v_10"))
v_10_df$time <- as.POSIXct(v_10_df$time * 60 * 60, origin = "1900-01-01")
tidy <- v_10_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,v_10)
v_10_df_BC <- new %>%
  unite(year, month, day, col = "date", sep = "-")

# save(v_10_df_BC , file = "data/v_10_df_BC.RData")


ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/BC/wind_166.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 14)
v_10.2 <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v_10.2) <- list(lon = nc$dim$lon$vals,
                         lat = nc$dim$lat$vals,
                         time = nc$dim$time$vals)
nc_close(nc)
v_10.2_df <- as_tibble(melt(v_10.2, value.name = "v_10"))
v_10.2_df$time <- as.POSIXct(v_10.2_df$time * 60 * 60, origin = "1900-01-01")
tidy <- v_10.2_df %>%
  separate(col = time, into = c("year", "month","day1"), sep = "-")
t2 <- tidy %>%
  separate(col = day1, into = c("day", "hour"), sep = " ") %>% 
  select(day)
new <- cbind(t2, tidy)
new <- new %>% 
  select(lon,lat,year,month ,day,v_10)
v_10.2df_BC <- new %>%
  unite(year, month, day, col = "date", sep = "-")


# save(v_10.2df_BC , file = "data/v_10.2df_BC.RData")
# BC_vwind <- rbind(v_10_df_BC,v_10.2df_BC)
# save(BC_vwind , file = "data/BC_vwind.RData")
#BC_vwind <- rbind(BC_vwind,v_10.2df_BC)

###################