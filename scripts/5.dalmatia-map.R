# plot dalmatia
# started 4/07/2025
# packages
library(dplyr)
library(sqldf)
library(arrow)
library(ggplot2)
library(sf)
library(ggrepel)
library("gridExtra")
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")

# download rnaturalearthhires for basemap
world <- ne_countries(scale = "large", returnclass = "sf")

# adding road data from DARMC https://hub.arcgis.com/datasets/55a54a1350e14ca0b355d95633da3851_0
roman_roads <- st_read(
  "shape_files/Roman_roads.shp")

# adding road and province data from DARMC https://hub.arcgis.com/datasets/55a54a1350e14ca0b355d95633da3851_0
roman_roads <- st_read(
  "shape_files/Roman_roads.shp")

roman_provincess <- st_read(
  "shape_files/roman_empire_ad_69_provinces.shp")

# adding settlements from Pleiades https://pleiades.stoa.org/downloads
roman_settlements <- st_read(
  "data/mapping/Roman_settlements_pleiades.gpkg")

# create df for layer of key sites
dense_sites <- data.frame(findspot_ancient_clean=c("Salona",
                                                   "Narona",
                                                   "Iader",
                                                   "Burnum",
                                                   "Asseria",
                                                   "Raetinium",
                                                   "Rider",
                                                   "Doclea",
                                                   "M. Malvesiatium",
                                                   "Tilurium",
                                                   "M. S[---]",
                                                   "Aequum",
                                                   "Epidaurum",
                                                   "Scodra",
                                                   "Domavia"),
                          Longitude=c(16.4743,
                                      17.625,
                                      15.223778,
                                      15.9936,
                                      15.6844,
                                      15.9292,
                                      16.0486,
                                      19.2615,
                                      19.5333,
                                      16.689,
                                      19.3204,
                                      16.6547,
                                      18.218868,
                                      19.520268,
                                      19.36267),
                          Latitude=c(43.5384,
                                     43.0801,
                                     44.115501,
                                     44.0317,
                                     44.0103,
                                     44.7885,
                                     43.7034,
                                     42.469,
                                     43.9667,
                                     43.6139,
                                     43.3424,
                                     43.7423,
                                     42.58101,
                                     42.0672575,
                                     44.143981))

print(dense_sites)

(dense_sites_ll <- st_as_sf(dense_sites,
                            coords = c("Longitude",
                                       "Latitude"),
                            remove = FALSE,
                            crs = 4326, agr = "constant"))


key_mil_sites <- data.frame(findspot_ancient_clean=c("Tilurium",
                                                     "Salona",
                                                     "Burnum",
                                                     "Bigeste"),
                            Longitude=c(16.7216523938,
                                        16.483426,
                                        16.025622,
                                        17.52710),
                            Latitude=c(43.609647549,
                                       43.539561,
                                       44.018914,
                                       43.18180))

print(key_mil_sites)

(key_sites_mil_ll <- st_as_sf(key_mil_sites,
                              coords = c("Longitude",
                                         "Latitude"),
                              remove = FALSE,
                              crs = 4326, agr = "constant"))

# create function for omitting nulls and organising a DF for mapping
dataframe_ll <- function(dataframe) {
  library(dplyr)
  library(sf)
  dataframe_place <- na.omit(dataframe %>%
                               select(findspot_ancient_clean,
                                      Longitude,
                                      Latitude) %>%
                               group_by(findspot_ancient_clean) %>%
                               count(findspot_ancient_clean,
                                     Longitude,
                                     Latitude) %>%
                               arrange(desc(n)))
  (dataframe_ll <- st_as_sf(dataframe_place, coords = c("Longitude", "Latitude"),
                            remove = FALSE,
                            crs = 4326, agr = "constant"))
  return(dataframe_ll)
}

# plot on map
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_provincess, color = "red", size = 3) + 
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha= 1, size = 0.8) +
  geom_sf(data = dense_sites_ll, colour = "#000000", size = 2) +
  geom_label_repel(data = dense_sites,
                   fill = "white",
                   aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean)) +
  labs(size = "Density",
       caption = paste("Roads = DARMC (CC BY-NC-SA 4.0).\n",
                       "Settlements = Pleiades (CC-BY).\n",
                       "Provincial boundaries = AWMC (CC-BY-NC 4.0)."),
       title = "Dalmatia",
       subtitle = "Colonia and epigraphically dense sites") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/introduction/dalmatia.jpeg")