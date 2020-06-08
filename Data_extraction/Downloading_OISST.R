# NB: The packages only need to be installed from GitHub once
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidync")
install.packages("doParallel")
install.packages("plyr")

# Load the packages once they have been downloaded and installed
# The packages we will use
library(dplyr) # A staple for most modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(doParallel) # For parallel processing

# First we tell R where the data are on the interwebs
OISST_base_url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"

OISST_dates <- data.frame(t = seq(as.Date("2018-07-01"), as.Date("2020-05-31"), by = "day"))

# To finish up this step we add some text to those dates so they match the OISST file names
OISST_files <- OISST_dates %>%
  mutate(t_day = gsub("-", "", t),
         t_month = substr(t_day, 1, 6),
         t_year = year(t),
         file_name = paste0(OISST_base_url, t_month, "/", "oisst-avhrr-v02r01.", t_day ,".nc"))

OISST_url_daily_dl <- function(target_URL){
  dir.create("~/data/OISST", showWarnings = F)
  file_name <- paste0("~/data/OISST/",sapply(strsplit(target_URL, split = "/"), "[[", 10))
  if(!file.exists(file_name)) download.file(url = target_URL, method = "libcurl", destfile = file_name)
}

doParallel::registerDoParallel(cores = 3)

# And with that we are clear for take off
system.time(plyr::l_ply(OISST_files$file_name, .fun = OISST_url_daily_dl, .parallel = T)) # ~15 seconds

OISST_load <- function(file_name, lon1, lon2, lat1, lat2){
  OISST_dat <- tidync(file_name) %>%
    hyper_filter(lon = between(lon, lon1, lon2),
                 lat = between(lat, lat1, lat2)) %>%
    hyper_tibble() %>%
    select(lon, lat, time, sst) %>%
    dplyr::rename(t = time, temp = sst) %>%
    mutate(t = as.Date(t, origin = "1978-01-01"))
  return(OISST_dat)
}


# Locate the files that will be loaded
OISST_files <- dir("~/data/OISST", full.names = T)

# Load the data in parallel
OISST_BC <- plyr::ldply(.data = OISST_files, .fun = OISST_load, .parallel = T,
                        lon1 = -35, lon2 = -15, lat1 = 10, lat2 = 20)


OISST_HC <- plyr::ldply(.data = OISST_files, .fun = OISST_load, .parallel = T,
                        lon1 = -45.5, lon2 = -7.5, lat1 = 280, lat2 = 290)


OISST_CC <- plyr::ldply(.data = OISST_files, .fun = OISST_load, .parallel = T,
                        lon1 = 15, lon2 = 45, lat1 = 340, lat2 = 350)


OISST_CalC <- plyr::ldply(.data = OISST_files, .fun = OISST_load, .parallel = T,
                          lon1 = 25, lon2 = 45, lat1 = 280, lat2 = 290)


