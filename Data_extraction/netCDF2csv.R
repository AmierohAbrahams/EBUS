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


# bbox were determined using the parameters in the following paper
# Reduced Nearshore Warming Associated With Eastern Boundary Upwelling Systems 
# Rui Seabra 1 , Rubén Varela 2 , António M. Santos 1,3 , Moncho Gómez-Gesteira 2 ,
# Claudia Meneghesso 1,3 , David S. Wethey 4 and Fernando P. Lima 1 *

# Under Pressure: Climate Change, Upwelling, and Eastern Boundary  Upwelling Ecosystems
# Marisol García-Reyes 1 *, William J. Sydeman 1 , David S. Schoeman 2 ,
# Ryan R. Rykaczewski 3 , Bryan A. Black 4 , Albertus J. Smit 5 and Steven J. Bograd 6


# 2: Extract the OISST data ---------------------------------------------------------------------------------

bbox <- data.frame(BC = c(-35, -25, 15, 20), # Benguela Current
                   CC = c(15, 45, 340, 355), # Canary Current
                   CalC = c(25, 45, 225, 240), # California Current
                   HC = c(-45.5, -7.5, 275, 290), # Humboldt Current
                   row.names = c("latmin", "latmax", "lonmin", "lonmax"))


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


# 2: Extract the ERA5 data ---------------------------------------------------------------------------------
# https://cds.climate.copernicus.eu/api-how-to#use-the-cds-api-client-for-data-access
# Extracting ERA 5 wind u and v variables
# N/W/S/E coordinate format when downloading
# -26.00/15.00/-36.00/22.00

# This extraction is for the Benguela Current the same is repeated for the Canary, California and Humboldt current

# EXtracting the u variable - 
ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/BC/wind_165.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 13)
u <- ncvar_get(nc, varid = "u10") %>%
  round(4)
dimnames(u) <- list(lon = nc$dim$lon$vals,
                       lat = nc$dim$lat$vals,
                       time = nc$dim$time$vals)
nc_close(nc)
u <- as_tibble(melt(u, value.name = "u_10"))
u$time <- as.POSIXct(u$time * 60 * 60, origin = "1900-01-01")
BC_u <- u %>% 
  select(lon,lat,u, time) %>% 
  rename(date = time)
# save(BC_u , file = "data/BC_u.RData")

# EXtracting the v variable - 
ncFile <- '/home/amieroh/Documents/Data/Datasets/ERA5/BC/wind_166.nc'
nc <- nc_open(ncFile)
fNameStem <- substr(basename(ncFile), 1, 14)
v <- ncvar_get(nc, varid = "v10") %>%
  round(4)
dimnames(v) <- list(lon = nc$dim$lon$vals,
                         lat = nc$dim$lat$vals,
                         time = nc$dim$time$vals)
nc_close(nc)
v <- as_tibble(melt(v, value.name = "v_10"))
v$time <- as.POSIXct(v$time * 60 * 60, origin = "1900-01-01")

BC_v <- v %>% 
  select(lon,lat,v, time) %>% 
  rename(date = time)
# save(BC_v , file = "data/BC_v.RData")

# BC_fin <- rbind(BC_u,BC_v)
# save(BC_fin , file = "data/BC_fin.RData")
# save(HC_fin , file = "data/HC_fin.RData")
# save(CC_fin , file = "data/CC_fin.RData")
# save(CalC_fin , file = "data/CalC_fin.RData")

