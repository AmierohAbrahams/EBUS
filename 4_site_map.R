# 4_site_map
# The purpose of this script is to...
# The steps taken are:
# 1: Setup environment
# 2: Plotting the EBUS
# 3: Using plotdap to plot EBUS
# 4: Plotting Robs suggesion

# 1: Setup environment ----------------------------------------------------------------------------------------------------
library("ggplot2")
library(dplyr)
library("raster")
library("sf")
library("ggspatial")
library("rnaturalearth")
library("rnaturalearthdata")
library("PBSmapping")
library(viridis)
library(plotdap)
# Packages used in this vignette
library(tidyverse) # Base suite of functions
library(heatwaveR) # For detecting MHWs
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))
library(FNN) # For fastest nearest neighbour searches
library(tidync) # For a more tidy approach to managing NetCDF data
library(SDMTools) # For finding points within polygons
library(lubridate)


# 2: Plotting EBUS ----------------------------------------------------------------------------------------------------------
# Default CRS
# +proj=longlat +datum=WGS84 +no_defs
# Load the Global Self-consistent, Hierarchical, High-resolution Geography Database
# Use the full resolution version
gshhsDir <- "/home/amieroh/Documents/Data/Datasets/gshhg-bin-2.3.7"
b <- c(5, 10, 15, 20, 25,30)
colors <- c('#EFF573', '#EFF573', '#EFF573', '#3DA394', '#3A828C', '#456075')


bbox <- data.frame(BC = c(-35, -25, 15, 20), # Benguela Current
                   CC = c(25, 35, 340, 355), # Canary Current
                   CalC = c(35, 45, 225, 240), # California Current
                   HC = c(-17.5, -7.5, 275, 290), # Humboldt Current
                   row.names = c("latmin", "latmax", "lonmin", "lonmax"))

# Make a coastline for the world in sf format
coastline <- importGSHHS(paste0(gshhsDir, "/gshhs_l.b"),
                         xlim = c(0, 360), ylim = c(-90, 90), maxLevel = 1, useWest = FALSE)
polygon <- coastline %>%
  group_by(PID) %>%
  st_as_sf(coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# a simple theme for the map
theme_opts <- list(theme(panel.border = element_rect(colour = "black", size = 0.4, fill = NA),
                         axis.text = element_text(size = 16),
                         axis.line = element_line(colour = "black", size = 0.2),
                         panel.background = element_rect(colour = "black", fill = NA),
                         panel.grid.minor = element_blank(),
                         # panel.grid.major = element_blank(),
                         panel.grid.major = element_line(colour = "black", linetype = "dashed", size = 0.2),
                         axis.ticks = element_line(colour = "black")))


BC <- importGSHHS(paste0(gshhsDir, "/gshhs_f.b"),
                  xlim = c(15.00, 20.00), ylim = c(-35.00, -25.00), maxLevel = 1, useWest = FALSE)
(BC_map <- ggplot() +
    geom_raster(data = BC_complete, aes(x = lon, y = lat, fill = temp)) +
    geom_polygon(data = BC, aes(x = X, y = Y, group = PID), col = "black", fill = "grey60", size = 0.2) +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°E", sep = "", accuracy = .01),
                       breaks = c(39.00, 41.00, 43.00)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°S", sep = "", accuracy = .01)) +
    labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = colors,breaks = b)+
    theme_opts +
    theme(axis.text = element_text(size = 12)))

HC <- importGSHHS(paste0(gshhsDir, "/gshhs_f.b"),
                  xlim = c(275.00, 290.00), ylim = c(-17.5, -7.5), maxLevel = 1, useWest = FALSE)
(HC_map <- ggplot() +
    geom_raster(data = HC_complete, aes(x = lon, y = lat, fill = temp)) +
    geom_polygon(data = HC, aes(x = X, y = Y, group = PID), col = "black", fill = "grey60", size = 0.2) +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°E", sep = "", accuracy = .01),
                       breaks = c(39.00, 41.00, 43.00)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°S", sep = "", accuracy = .01)) +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_fill_gradientn(colors = colors,breaks = b)+
    theme_opts +
    theme(axis.text = element_text(size = 12)))


CC <- importGSHHS(paste0(gshhsDir, "/gshhs_f.b"),
                  xlim = c(340.00, 355.00), ylim = c(25.00, 35.00), maxLevel = 1, useWest = TRUE)
(CC_map <- ggplot() +
    geom_polygon(data = CC, aes(x = X, y = Y, group = PID), col = "black", fill = "grey60", size = 0.2) +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°E", sep = "", accuracy = .01)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°S", sep = "", accuracy = .01),
                       breaks = seq(from = -6.40, to = -5.80, by = 0.20)) +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_fill_gradientn(colors = colors,breaks = b)+
    theme_opts +
    theme(axis.text = element_text(size = 12)))



CalC <- importGSHHS(paste0(gshhsDir, "/gshhs_f.b"),
                    xlim = c(225.00, 240.00), ylim = c(35.00, 45.00), maxLevel = 1, useWest = FALSE)
(CalC_map <- ggplot() +
    geom_raster(data = CalC_complete, aes(x = lon, y = lat, fill = temp)) +
    geom_polygon(data = CalC, aes(x = X, y = Y, group = PID), col = "black", fill = "grey60", size = 0.2) +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°E", sep = "", accuracy = .01)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°S", sep = "", accuracy = .01),
                       breaks = seq(from = -6.40, to = -5.80, by = 0.20)) +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_fill_gradientn(colors = colors,breaks = b)+
    theme_opts +
    theme(axis.text = element_text(size = 12)))
  
# 3: Using plotdap to plot EBUS ----------------------------------------------------------------------------------------------------------

library(plotdap)
plotdap()
plotdap("base")
sstInfo <- rerddap::info('erdVHsstaWS3day')
# get latest 3-day composite sst
viirsSST <- rerddap::griddap(sstInfo, 
                             latitude = c(41., 31.), 
                             longitude = c(-128., -115), 
                             time = c('last','last'), 
                             fields = 'sst')
xpos <- c(135.25, 240.25)
ypos <- c(20.25, 60.25)
zpos <- c(70.02, 70.02)
remove <- c("UK:Great Britain", "France", "Spain", "Algeria", "Mali", "
            Burkina Faso", "Ghana", "Togo")
#subset world2Hires with those countries removed
w <- map("world2Hires", plot = FALSE, fill = TRUE, ylim = ypos, xlim = xpos)
w <- map("world2Hires", regions = w$names[!(w$names %in% remove)], 
         plot = FALSE, fill = TRUE, ylim = ypos, xlim = xpos)
# plot result
plotdap(mapData = w)
# write plot to disk using the Cairo package
library(sf)
#> Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0
library(mapdata)
plotdap(mapTitle = "Grid over Land") %>%
  add_griddap(
    viirsSST, 
    ~sst, 
    fill = "thermal"
  )

library(cshapes)

world <- cshp(date=as.Date("2008-1-1"))
world.points <- fortify(world, region='COWCODE')

# Matching world points with the cordinates in OISST data

OISST_dat_remove <- OISST_dat %>% 
  mutate(lon = lon - 360)

ggplot(world.points, aes(long,lat,group=group)) + 
  geom_polygon() +
  ggplot(OISST_dat, aes(x = lon, y = lat))+
  geom_raster(aes(fill = temp))  

# 4: Plotting Robs suggesion----------------------------------------------------------------------------------------------------------------------------------

load("data/OISST_global.RData") # Created in Data_extraction folder/Downloading_OISST.R
OISST_global <- OISST_global %>%
  mutate(lon = ifelse(lon > 180, lon - 360, lon))

map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon))

# ggplot(map_base, aes(lon, lat, group = group)) + 
#   geom_polygon()

ggplot(OISST_global, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = temp), show.legend = TRUE) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  coord_cartesian(expand = F) +
  #geom_point(data = site_squares, aes(x = lon, y = lat), colour = "red", shape = 0, alpha = 0.8, size = 3) +
  coord_fixed(ratio = 1, xlim = c(-170,180), ylim = c(90, -80),
              expand = TRUE) +
  scale_x_continuous(expand = c(0,0),
                     labels = scales::unit_format(unit = "°W", sep = "")) +
  scale_y_continuous(expand = c(0, 0),
                     labels = scales::unit_format(unit = "°S", sep = "")) +
  scale_fill_gradientn("SST (°C)", values = scales::rescale(c(-1, 7,19,26)),
  colors = c("lightcyan1", "orchid1", "skyblue", "blue3")) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "right")+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(colour = "black", size = 20),
        axis.title = element_text(colour = "black", size = 20),
        axis.ticks = element_line(colour = "black"))
