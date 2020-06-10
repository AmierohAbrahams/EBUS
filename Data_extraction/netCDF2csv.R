# netCDF2csv
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Extract the OISST data
# 3: Extract the ERA5 data

# 1: Setup environment -------------------------------------------------------------------------------------
library(ncdf4) # library for processing netCDFs
library(ncdf4)
library(data.table)
library(tidyverse)
library(reshape2)
library(lubridate)
library(stringr)
library(doMC); doMC::registerDoMC(cores = 4)
library(heatwaveR)
library(plyr)
# bbox were determined using the parameters in the following paper
# Reduced Nearshore Warming Associated With Eastern Boundary Upwelling Systems 
# Rui Seabra 1 , Rubén Varela 2 , António M. Santos 1,3 , Moncho Gómez-Gesteira 2 ,
# Claudia Meneghesso 1,3 , David S. Wethey 4 and Fernando P. Lima 1 *

# Under Pressure: Climate Change, Upwelling, and Eastern Boundary  Upwelling Ecosystems
# Marisol García-Reyes 1 *, William J. Sydeman 1 , David S. Schoeman 2 ,
# Ryan R. Rykaczewski 3 , Bryan A. Black 4 , Albertus J. Smit 5 and Steven J. Bograd 6

# 2: Extract the OISST data ---------------------------------------------------------------------------------
  bbox <- data.frame(BC = c(-35, -15, 10, 20), # Benguela Current
                   CC = c(15, 45, 340, 350), # Canary Current
                   CalC = c(25, 45, 230, 250), # California Current
                   HC = c(-45.5, -7.5, 280, 290), # Humboldt Current
                   row.names = c("latmin", "latmax", "lonmin", "lonmax"))


nc.dir <- "/home/amieroh/Documents/Data/Datasets/AVHRR/OISST"
csv.dir <- "/media/amieroh/Amieroh/spatial"


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
dplyr::
## apply the function
system.time(llply(ncList, read_nc, location = "BC", csv.dir = csv.dir, .parallel = TRUE))
system.time(llply(ncList, read_nc, location = "CC", csv.dir = csv.dir, .parallel = TRUE))
system.time(llply(ncList, read_nc, location = "CalC", csv.dir = csv.dir, .parallel = TRUE))
system.time(llply(ncList, read_nc, location = "HC", csv.dir = csv.dir, .parallel = TRUE))

BC_temp <- BC-avhrr-only-v2.Document-Document.csv
CC_temp <- CC-avhrr-only-v2.Document-Document.csv
CalC_temp <- CalC-avhrr-only-v2.Document-Document.csv
HC_temp <- HC-avhrr-only-v2.Document-Document.csv


# 2: Extract the ERA5 data ---------------------------------------------------------------------------------
# https://cds.climate.copernicus.eu/api-how-to#use-the-cds-api-client-for-data-access
# Extracting ERA 5 wind u and v variables
# N/W/S/E coordinate format when downloading
# -26.00/15.00/-36.00/22.00

# This extraction is for the Benguela Current the same is repeated for the Canary, California and Humboldt current

# New downloading ERA5 u and v variables
# Extracting the data

# To obtain ERA 5 wind data one needs to download u and v wind variables
# These variables are collected every hour for every day
# Here I extract u and v wind variables and then convert them to daily data in order to match the OISST daily temperature data
# ERA 5 does not have daily wind variables only hourly

library(ncdf4)
library(ncdump)
library(tidync) # tidync is unfortunately not loading on the computer with more RAM for some reason
library(tidyverse)
library(reshape2)
library(lubridate)
library(stringr)
library(circular)
library(doParallel); doParallel::registerDoParallel(cores = 8) 
# RWS: I find doParallel to be more stable than doMC after recent updates to the tidyverse


# Check netCDF info -------------------------------------------------------

ncdump::NetCDF('~/R/forOthers/Amieroh/data.nc')


# Load with ncdf4 ---------------------------------------------------------

# Works but the time variables is not accurate
# ncFile <- '/home/amieroh/Downloads/data.nc'
ncFile <- '/home/amieroh/Downloads/data1.nc'
nc <- nc_open(ncFile)
u <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u) <- list(lon = nc$dim$lon$vals,
                    lat = nc$dim$lat$vals,
                    time = nc$dim$time$vals)
# nc_close(nc)
u <- as_tibble(melt(u, value.name = "u_10"))
u$time <- as.Date(as.POSIXct(u$time * 60 * 60, origin = "1900-01-01"))
BC_u <- u %>%
  dplyr::rename(t = time) %>% 
  group_by(lon, lat, t) %>% 
  summarise(u_10 = mean(u_10, na.rm = T))

# Test coords
u_lon <- nc$dim$lon$vals
u_lat <- nc$dim$lat$vals
nc_close(nc)

# Test time creation
u_test <- u %>% 
  filter(lon == u$lon[1], lat == u$lat[1]) %>% 
  mutate(time = as.Date(as.POSIXct(time * 60 * 60, origin = "1900-01-01")))



  # group_by(lon, lat, t) %>%
  # summarise(u10 = mean(u10, na.rm = T), # To get daily values from hourly
  #           v10 = mean(v10,na.rm = T)) 

# This is the latest data up to 2020 saved as BC/CC/CalC/HC in the complete_data folder
# Latest OISST data for rthe corrrect region is on Hardrive and in data/OISST file from 2018-2020

CC_temp <- read_csv("~/Documents/CC-avhrr-only-v2.Document-Document.csv", col_names = c("lon", "lat", "temp", "date"))
CalC_temp <- read_csv("~/Documents/CalC-avhrr-only-v2.Document-Document.csv", col_names = c("lon", "lat", "temp", "date"))
HC_temp <- read_csv("~/Documents/HC-avhrr-only-v2.Document-Document.csv", col_names = c("lon", "lat", "temp", "date"))
BC_temp <- read_csv("~/Documents/BC-avhrr-only-v2.Document-Document.csv", col_names = c("lon", "lat", "temp", "date"))
OISST_HC <- OISST_HC %>% 
  rename(date = t)
HC <- rbind(HC_temp,OISST_HC)
save(HC, file = "data_complete/HC.RData") 
rm(HC); gc()
rm(HC_temp); gc()
rm(OISST_HC);gc()
# The AJ way ---------------------------------------------------------------------------------------------------------------------------------
nc <- nc_open("/home/amieroh/Downloads/data1.nc")
u10 <- ncvar_get(nc, varid = "u10")
v10 <- ncvar_get(nc, varid = "v10")
dimnames(v10) <- list(lon = nc$dim$longitude$vals,
                      lat = nc$dim$latitude$vals,
                      time = nc$dim$time$vals)

t_origin <- ncatt_get(nc, "time", "units")$value
t_origin

as.ymd <- function(x) {
  as.Date(as.POSIXct(x * 3600, origin = "1900-01-01 00:00:00.0"),
          "GMT",
          "%Y-%m-%d")
}

u10_df <- as_tibble(reshape2::melt(u10, value.name = "u10"), row.names = NULL) %>%
  mutate(time = as.ymd(time)) %>%
  na.omit()

v10_df <- as_tibble(reshape2::melt(v10, value.name = "v10"), row.names = NULL) %>%
  mutate(time = as.ymd(time)) %>%
  na.omit()

v10_df <- v10_df %>% 
  dplyr::select(-lat,-lon,-time)
wind1982 <- cbind(u10_df,v10_df)4

###################################################################################################
# Load with tidync --------------------------------------------------------
rm(BC_match);gc()
rm(BC_match1982);gc()
rm(BC_wind_fin);gc()
rm(wind_daily);gc()

wind_daily <- tidync("/home/amieroh/Downloads/BC_wind/data39.nc") %>%
  hyper_tibble() %>% 
  dplyr::select(longitude, latitude, time, v10, u10) %>% 
  rename(lon = longitude,
         lat = latitude) %>% 
  mutate(t = as.Date(as.POSIXct(time * 60 * 60, origin = "1900-01-01"))) %>% 
  dplyr::select(-time)

BC_wind_fin <- wind_daily %>% 
  mutate( lat = lat - 0.125,
          lon = lon + 0.125) %>% 
  rename(date = t)

match_func <- function(temp_df, wind_df){
  match <- wind_df  %>%
    left_join(temp_df, by = c("lon",  "lat", "date")) %>%
    drop_na()
  return(match)
}


BC_match1982 <- match_func(temp_df = BC_long, wind_df = BC_wind_fin) 

combined <- rbind(combined,BC_match1982)
save(combined, file = "data_complete/combined.RData")

