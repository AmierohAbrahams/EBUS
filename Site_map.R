# Plotting '


library("ggplot2")
library(dplyr)
library("raster")
library("sf")
library("ggspatial")
library("rnaturalearth")
library("rnaturalearthdata")
# Default CRS
# +proj=longlat +datum=WGS84 +no_defs

# make a data frame with the site data
locations <- data.frame(site = c("Long Reef", "Malus Island", "Mapopwe Creek"),
                        lon = c(125.73798096, 116.96664647, 39.52190208),
                        lat = c(-13.89397919, -20.29871945, -6.12515592))

# Load the Global Self-consistent, Hierarchical, High-resolution Geography Database
# Use the full resolution version
gshhsDir <- "/Users/ajsmit/spatial/gshhg-bin-2.3.7"

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

# Make a wrapper function for the combined map of the Indian Ocean region
make_maps <- function(crs = crs) {
  
  world_ne <- st_transform(ne_countries(scale = "small", returnclass = "sf"), crs = crs)
  # names(world_ne)
  # world_ne$iso_a2
  # world_ne$continent
  
  # Some continents
  # africa_ne <- st_union(world_ne[world_ne$continent == "Africa", ])
  # asia_ne <- st_union(world_ne[world_ne$continent == "Asia", ])
  # oceania_ne <- st_union(world_ne[world_ne$continent == "Oceania", ])
  
  # Tanzania and Australia
  tanzania_ne <- world_ne[world_ne$iso_a2 == "TZ", ]
  australia_ne <- world_ne[world_ne$iso_a2 == "AU", ]
  
  # Put it together
  map <- ggplot(data = polygon) +
    geom_sf(col = "black", fill = "grey80", size = 0.2) +
    geom_sf(data = tanzania_ne, fill = "grey60", size = 0.2, col = "black") +
    # geom_sf(data = australia_ne, fill = "grey60", size = 0.2, col = "black") +
    # geom_point(data = locations, aes(x = lon, y = lat), col = "black", shape = 18, size = 4) +
    coord_sf(xlim = c(28, 130), ylim = c(-40, 15), expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_opts
  return(map)
}

# Now make the map of the Indian Ocean Region
(indian_ocean <- make_maps(crs = "+proj=merc"))

ggsave("indian_ocean.png", scale = 1, width = 170, height = 110, units = "mm", dpi = 300)

require(rgeos); require(maptools) # maptools must be loaded after rgeos
library(PBSmapping)

chwaka <- importGSHHS(paste0(gshhsDir, "/gshhs_f.b"),
                      xlim = c(39.350, 39.60), ylim = c(-6.250, -6.007), maxLevel = 1, useWest = FALSE)
(chwaka_map <- ggplot() +
    geom_polygon(data = chwaka, aes(x = X, y = Y, group = PID), col = "black", fill = "grey60", size = 0.2) +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°E", sep = "", accuracy = .01),
                       breaks = c(39.40, 39.45, 39.50)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°S", sep = "", accuracy = .01),
                       breaks = seq(from = -6.20, to = -6.05, by = 0.05)) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_opts) +
  theme(axis.text = element_text(size = 12))
ggsave("chwaka.png", scale = 1, width = 170, height = 110, units = "mm", dpi = 300)

nw_aus <- importGSHHS(paste0(gshhsDir, "/gshhs_f.b"),
                      xlim = c(107.75, 134.00), ylim = c(-25.00, -10.00), maxLevel = 1, useWest = FALSE)
(nw_aus_map <- ggplot() +
    geom_polygon(data = nw_aus, aes(x = X, y = Y, group = PID), col = "black", fill = "grey60", size = 0.2) +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°E", sep = "", accuracy = .01)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°S", sep = "", accuracy = .01)) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_opts +
    theme(axis.text = element_text(size = 12)))
ggsave("australia.png", scale = 1, width = 170, height = 110, units = "mm", dpi = 300)

tanzania <- importGSHHS(paste0(gshhsDir, "/gshhs_f.b"),
                        xlim = c(37.00, 44.60), ylim = c(-12.30, -2.70), maxLevel = 1, useWest = FALSE)
(tanzania_map <- ggplot() +
    geom_polygon(data = tanzania, aes(x = X, y = Y, group = PID), col = "black", fill = "grey60", size = 0.2) +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°E", sep = "", accuracy = .01),
                       breaks = c(39.00, 41.00, 43.00)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°S", sep = "", accuracy = .01)) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_opts +
    theme(axis.text = element_text(size = 12)))
ggsave("tanzania.png", scale = 1, width = 170, height = 110, units = "mm", dpi = 300)

zanzibar <- importGSHHS(paste0(gshhsDir, "/gshhs_f.b"),
                        xlim = c(38.65, 39.75), ylim = c(-6.60, -5.60), maxLevel = 1, useWest = FALSE)
(zanzibar_map <- ggplot() +
    geom_polygon(data = zanzibar, aes(x = X, y = Y, group = PID), col = "black", fill = "grey60", size = 0.2) +
    coord_fixed(ratio = 1, expand = FALSE) +
    scale_x_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°E", sep = "", accuracy = .01)) +
    scale_y_continuous(expand = c(0, 0), labels = scales::unit_format(unit = "°S", sep = "", accuracy = .01),
                       breaks = seq(from = -6.40, to = -5.80, by = 0.20)) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_opts +
    theme(axis.text = element_text(size = 12)))
ggsave("zanzibar.png", scale = 1, width = 170, height = 110, units = "mm", dpi = 300)


