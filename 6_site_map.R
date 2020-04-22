library("ggplot2")
library(dplyr)
library("raster")
library("sf")
library("ggspatial")
library("rnaturalearth")
library("rnaturalearthdata")
library("PBSmapping")
library(viridis)


# Packages used in this vignette
library(tidyverse) # Base suite of functions
library(heatwaveR) # For detecting MHWs
# cat(paste0("heatwaveR version = ", packageDescription("heatwaveR")$Version))
library(FNN) # For fastest nearest neighbour searches
library(tidync) # For a more tidy approach to managing NetCDF data
library(SDMTools) # For finding points within polygons
library(lubridate)

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
  






