# 06_global_miniaturization.R
# Fig. 8: Global phytoplankton miniaturization map with regional occurrence and mechanistic drivers
library(ggplot2); library(sf); library(rnaturalearth); library(rnaturalearthdata)
library(readxl); library(dplyr)

# ---- World basemap ----
world <- ne_countries(scale = "medium", returnclass = "sf")

# ---- Literature data ----
lit <- read_excel("literature_locations.xlsx", sheet = "literature_summary")
coords <- read_excel("literature_locations.xlsx", sheet = "station_coordinates")

# Parse coordinates
coords <- coords %>% filter(!is.na(Longitude) & !is.na(Latitude) & !is.na(Station))

# ---- Cities temperature backdrop (optional) ----
cities <- tryCatch(read.csv("global_cities_temperature.csv"), error = function(e) NULL)

# ---- Main world map ----
p_world <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  theme_minimal() + theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())

# ---- Regional subplots ----
# Mediterranean zoom
p_med <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  coord_sf(xlim = c(-5, 37), ylim = c(30, 46), expand = FALSE) +
  theme_void()

# East China Sea / Bohai Bay zoom
p_bohai <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  coord_sf(xlim = c(105, 130), ylim = c(20, 42), expand = FALSE) +
  theme_void()

# Add study region markers
p_bohai <- p_bohai +
  geom_point(data = coords, aes(x = Longitude, y = Latitude), color = "#D7191C", size = 2, alpha = 0.7) +
  annotate("text", x = 118, y = 39, label = "Bohai Bay", size = 4, fontface = "bold", color = "#D7191C", family = "serif")

# ---- Save outputs ----
ggsave("Fig8_world_map.png", p_world, width = 12, height = 6, dpi = 300)
ggsave("Fig8_mediterranean_inset.png", p_med, width = 5, height = 4, dpi = 300)
ggsave("Fig8_bohai_inset.png", p_bohai, width = 5, height = 4, dpi = 300)

cat("Done: Fig 8 map panels saved\n")
