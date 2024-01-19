library(tidyverse)
library(sf)
library(patchwork)
library(ggthemes)
library(rnaturalearth)
library(ggspatial)
library(ggrepel)


#Mapa todas las islas --------------------------

spdf_mx <- st_transform(st_as_sf(ne_countries(scale = 'large', country = 'mexico')), crs = 4326)

# Sitios de buceo

Sitios_de_buceo <- read.csv("C:/R/Revillagigedo/Sitios de buceo.csv") %>%
  filter(Island %in% c("Socorro", "San Benedicto", "Clarion", "Roca Partida"))

sitios <- Sitios_de_buceo %>%
  group_by(Island, Site, Longitude, Latitude) %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude))

# Transformar en un objeto espacial

sitios_sf <- st_as_sf(sitios, coords = c("Longitude", "Latitude"), crs = 4326)

# Gráfico
ggplot() +
  geom_sf(data = spdf_mx, col = NA, fill = "gray30") +
  geom_sf(data = sitios_sf, size = 1, shape = 21, col = "#242424", fill = "#B2DFEE") +
  xlab("Longitud") + ylab("Latitud") +
  coord_sf(xlim = c(-115, -110), ylim = c(18, 20)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 7, angle = 90),
    axis.text.y = element_text(size = 7, angle = 0, hjust = 0.5),
    axis.title = element_text(),
    axis.ticks = element_line(),
    legend.position = "right",
    legend.text = element_text(size = 5),
    strip.text.x = element_text(face = "italic"),
    strip.background = element_rect(fill = NA, color = NA)
  )


#Mapa de Socorro --------------------

# Filtrar solo los sitios de la isla "Socorro"

sitios_socorro <- Sitios_de_buceo %>%
  filter(Island == "Socorro") %>%
  group_by(Island, Site, Longitude, Latitude) %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude))

# Transformar en un objeto espacial

sitios_socorro_sf <- st_as_sf(sitios_socorro, coords = c("Longitude", "Latitude"), crs = 4326)

# Gráfico

ggplot() +
  geom_sf(data = spdf_mx, col = NA, fill = "gray90") +
  geom_sf(data = sitios_socorro_sf, size = 1.9, shape = 21, col = "#242424", fill = "#B2DFEE") +
  xlab("Longitud") + ylab("Latitud") +
  coord_sf(xlim = c(-111.1, -110.85), ylim = c(18.7, 18.9)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 7, angle = 90),
    axis.text.y = element_text(size = 7, angle = 0, hjust = 0.5),
    axis.title = element_text(),
    axis.ticks = element_line(),
    legend.position = "right",
    legend.text = element_text(size = 5),
    strip.text.x = element_text(face = "italic"),
    strip.background = element_rect(fill = NA, color = NA)
  )
