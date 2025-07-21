# plot data on scatter maps
# started 17/09/2024
# edited 19/11/2024

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
library("ggspatial")


# download rnaturalearthhires for basemap
world <- ne_countries(scale = "large", returnclass = "sf")

# adding road data from DARMC https://hub.arcgis.com/datasets/55a54a1350e14ca0b355d95633da3851_0
roman_roads <- st_read(
  "shape_files/Roman_roads.shp")

# adding settlements from Pleiades https://pleiades.stoa.org/downloads
roman_settlements <- st_read(
  "data/mapping/Roman_settlements_pleiades.gpkg")

# create df for layer of key sites
## one for lots of sites, one for military, one for forts and capital
key_sites <- data.frame(findspot_ancient_clean=c("Tilurium",
                                                  "Salona",
                                                   "Burnum"),
                            Longitude=c(16.7216523938,
                                        16.483426,
                                        16.025622),
                            Latitude=c(43.609647549,
                                       43.539561,
                                       44.018914))

print(key_sites)

(key_sites_ll <- st_as_sf(key_sites,
                              coords = c("Longitude",
                                         "Latitude"),
                              remove = FALSE,
                              crs = 4326, agr = "constant"))

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
                                                 "Aequum"),
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
                                    16.6547),
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
                                   43.7423))

print(dense_sites)

(dense_sites_ll <- st_as_sf(dense_sites,
                                  coords = c("Longitude",
                                             "Latitude"),
                                  remove = FALSE,
                                  crs = 4326, agr = "constant"))

dense_sites_dated <- data.frame(findspot_ancient_clean = 
                                  c("Tilurium",
                                    "Salona",
                                    "Burnum",
                                    "Iader",
                                    "Narona",
                                    "Rider",
                                    "Asseria",
                                    "Aenona",
                                    "Bigeste"),
                                Longitude=c(16.7216523938,
                                            16.483426,
                                            16.025622,
                                            15.223778,
                                            17.625,
                                            16.0486,
                                            15.6844,
                                            15.1842,
                                            17.5271),
                                Latitude=c(43.609647549,
                                           43.539561,
                                           44.018914,
                                           44.115501,
                                           43.0801,
                                           43.7034,
                                           44.0103,
                                           44.2435,
                                           43.1818))

print(dense_sites_dated)

(dense_sites_dated_ll <- st_as_sf(dense_sites_dated,
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
# save csv of plotted places each
## undated/dated corpora with and without place filtration
LIRE_Dal_corpus <-
  read.csv("output_tables/corpus/undated/LIRE_corpus.csv")
           
LIRE_Dal_corpus_ll <- dataframe_ll(LIRE_Dal_corpus)

LIRE_Dal_corpus_n <- count(LIRE_Dal_corpus)

plot1 <- 
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                              y = Latitude,
                              label = findspot_ancient_clean), 
                  nudge_x = c(-0.25, -1.5,-1.5, 0), 
                  nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags"),
                       title = "Distribution of military funerary and sacral inscriptions",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot1)

ggsave("output_images/geographical_distribution/01.LIRE_corpus_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_place <- st_drop_geometry(LIRE_Dal_corpus_ll)
write.csv(LIRE_Dal_corpus_place,
          file = "output_tables/corpus/places/LIRE_corpus_places_places.csv")

LIRE_Dal_corpus_place_filtering <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_place_filter.csv")

LIRE_Dal_corpus_place_filtering_ll <- dataframe_ll(LIRE_Dal_corpus_place_filtering)

LIRE_Dal_corpus_place_filtering_n <- count(LIRE_Dal_corpus_place_filtering)

plot2 <- 
  ggplot() +  
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_place_filtering_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                              y = Latitude,
                              label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_place_filtering_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words, tags, and places."),
                      title = "Distribution of military funerary and sacral inscriptions",
                      subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot2)

ggsave("output_images/geographical_distribution/02.LIRE_corpus_place_filter_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_place_filtering_place <- st_drop_geometry(LIRE_Dal_corpus_place_filtering_ll)
write.csv(LIRE_Dal_corpus_place_filtering_place,
          file = "output_tables/corpus/places/LIRE_corpus_place_filter_places_places.csv")          

## all inscriptions types
LIRE_Dal_all_corpus <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_all_types.csv")

LIRE_Dal_all_corpus_ll <- dataframe_ll(LIRE_Dal_all_corpus)

LIRE_Dal_all_corpus_n <- count(LIRE_Dal_all_corpus)

plot3 <- 
  ggplot() +  
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_all_corpus_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                          y = Latitude,
                          label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_all_corpus_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words, tags, and places."),
       title = "Distribution of military inscriptions",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot3)

ggsave("output_images/geographical_distribution/03.LIRE_all_types_corpus_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_all_corpus_place <- st_drop_geometry(LIRE_Dal_all_corpus_ll)
write.csv(LIRE_Dal_all_corpus_place,
          file = "output_tables/corpus/places/LIRE_corpus_all_types_dated_places.csv")

## stela
LIRE_Dal_corpus_stela <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_stela.csv")

LIRE_Dal_corpus_stela_ll <- dataframe_ll(LIRE_Dal_corpus_stela)

LIRE_Dal_corpus_stela_n <- count(LIRE_Dal_corpus_stela)

plot4 <- 
  ggplot() +  
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_stela_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                          y = Latitude,
                          label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_stela_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
       title = "Distribution of military stelae",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot4)

ggsave("output_images/geographical_distribution/04.LIRE_corpus_stela_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_stela_place <- st_drop_geometry(LIRE_Dal_corpus_stela_ll)
write.csv(LIRE_Dal_corpus_stela_place,
          file = "output_tables/corpus/places/LIRE_corpus_stela_places.csv")

## epitaphs
LIRE_Dal_corpus_epitaph <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_epitaph.csv")

LIRE_Dal_corpus_epitaph_ll <- dataframe_ll(LIRE_Dal_corpus_epitaph)

LLIRE_Dal_corpus_epitaph_n <- count(LIRE_Dal_corpus_epitaph)

plot5 <- 
  ggplot() +  
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_epitaph_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                          y = Latitude,
                          label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LLIRE_Dal_corpus_epitaph_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
       title = "Distribution of military epitaphs",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot5)

ggsave("output_images/geographical_distribution/05.LIRE_corpus_epitaph_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_epitaph_place <- st_drop_geometry(LIRE_Dal_corpus_epitaph_ll)
write.csv(LIRE_Dal_corpus_epitaph_place,
          file = "output_tables/corpus/places/LIRE_corpus_epitaphs_places.csv")

# votives
LIRE_Dal_corpus_votive <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_votive.csv")

LIRE_Dal_corpus_votive_ll <- dataframe_ll(LIRE_Dal_corpus_votive)

LLIRE_Dal_corpus_votive_n <- count(LIRE_Dal_corpus_votive)

plot6 <- 
  ggplot() +  
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_votive_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                          y = Latitude,
                          label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LLIRE_Dal_corpus_votive_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
       title = "Distribution of military votives",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot6)

ggsave("output_images/geographical_distribution/06.LIRE_corpus_votive_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_votive_place <- st_drop_geometry(LIRE_Dal_corpus_epitaph_ll)
write.csv(LIRE_Dal_corpus_votive_place,
          file = "output_tables/corpus/places/LIRE_corpus_votive_places.csv")

# altars
LIRE_Dal_corpus_altar <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_altar.csv")

LIRE_Dal_corpus_altar_ll <- dataframe_ll(LIRE_Dal_corpus_altar)

LLIRE_Dal_corpus_altar_n <- count(LIRE_Dal_corpus_altar)

plot7 <- 
  ggplot() +  
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_altar_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                          y = Latitude,
                          label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LLIRE_Dal_corpus_altar_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
       title = "Distribution of military altars",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot7)

ggsave("output_images/geographical_distribution/07.LIRE_corpus_altar_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_altar_place <- st_drop_geometry(LIRE_Dal_corpus_epitaph_ll)
write.csv(LIRE_Dal_corpus_altar_place,
          file = "output_tables/corpus/places/LIRE_corpus_altar_places.csv")

## create one for just relevant period for corpus and 
# stela/epitaph (~200 CE cut off)
LIRE_corpus_dated <-
  read.csv("output_tables/corpus/dated/LIRE_corpus_dated.csv")

LIRE_corpus_dated_ll <- dataframe_ll(LIRE_corpus_dated)

LIRE_corpus_dated_n <- count(LIRE_corpus_dated)

plot8 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_corpus_dated_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                              y = Latitude,
                              label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_corpus_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags.\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
                       title = "Distribution of military funerary and sacral inscriptions",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot8)

ggsave("output_images/geographical_distribution/08.LIRE_corpus_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_corpus_dated_place <- st_drop_geometry(LIRE_corpus_dated_ll)
write.csv(LIRE_corpus_dated_place,
          file = "output_tables/corpus/places/LIRE_corpus_dated_places.csv")

## all inscriptions types dated
LIRE_Dal_all_corpus_dated <-
  read.csv("output_tables/corpus/dated/LIRE_corpus_all_types_dated.csv")

LIRE_Dal_all_corpus_dated_ll <- dataframe_ll(LIRE_Dal_all_corpus_dated)

LIRE_Dal_all_corpus_dated_n <- count(LIRE_Dal_all_corpus_dated)

plot9 <- 
  ggplot() +  
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_all_corpus_dated_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                          fill = "white",
                          aes(x = Longitude,
                          y = Latitude,
                          label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_all_corpus_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags.\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
       title = "Distribution of military inscriptions",
       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()
  
plot(plot9)

ggsave("output_images/geographical_distribution/09.LIRE_all_types_corpus_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_all_corpus_dated_place <- st_drop_geometry(LIRE_Dal_all_corpus_dated_ll)
write.csv(LIRE_Dal_all_corpus_dated_place,
          file = "output_tables/corpus/places/LIRE_corpus_all_types_dated_places.csv")

# stela
LIRE_corpus_stela_dated <-
  read.csv("output_tables/corpus/dated/LIRE_corpus_stela_dated.csv")

LIRE_corpus_stela_dated_ll <- dataframe_ll(LIRE_corpus_stela_dated)

LIRE_corpus_stela_dated_n <- count(LIRE_corpus_stela_dated)

plot10 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_corpus_stela_dated_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-0.25, -1.5,-1.5, 0), 
                  nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_corpus_stela_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags.\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
                       title = "Distribution of military stelae",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot10)

ggsave("output_images/geographical_distribution/10.LIRE_corpus_stela_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_corpus_stela_place <- st_drop_geometry(LIRE_corpus_stela_dated_ll)
write.csv(LIRE_corpus_stela_place,
          file = "output_tables/corpus/places/LIRE_corpus_stela_dated_places.csv")

# epitaphs
LIRE_corpus_epitaph_dated <-
  read.csv("output_tables/corpus/dated/LIRE_corpus_epitaph_dated.csv")

LIRE_corpus_epitaph_dated_ll <- dataframe_ll(LIRE_corpus_epitaph_dated)

LIRE_corpus_epitaph_dated_n <- count(LIRE_corpus_epitaph_dated)

plot11 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_corpus_epitaph_dated_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-0.25, -1.5,-1.5, 0), 
                  nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_corpus_epitaph_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags.\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
                       title = "Distribution of military epitaphs",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot11)

ggsave("output_images/geographical_distribution/11.LIRE_corpus_epitaph_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_corpus_epitaph_dated_place <- st_drop_geometry(LIRE_corpus_epitaph_dated_ll)
write.csv(LIRE_corpus_epitaph_dated_place,
          file = "output_tables/corpus/places/LIRE_corpus_epitaph_dated_places.csv")

# Now compare to all Dalmatia
LIRE_Dal <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia.csv")

LIRE_Dal_ll <- dataframe_ll(LIRE_Dal)

LIRE_Dal_n <- count(LIRE_Dal)

plot12 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral inscriptions",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot12)

ggsave("output_images/geographical_distribution/12.LIRE_Dalmatia_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_place <- st_drop_geometry(LIRE_Dal_ll)
write.csv(LIRE_Dal_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_places.csv")

#all inscription types
LIRE_Dal_all <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_all_types.csv")

LIRE_Dal_all_ll <- dataframe_ll(LIRE_Dal_all)

LIRE_Dal_all_n <- count(LIRE_Dal_all)

plot13 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_all_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_all_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of all inscriptions",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot13)

ggsave("output_images/geographical_distribution/13.LIRE_Dalmatia_all_types_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_all_place <- st_drop_geometry(LIRE_Dal_all_ll)
write.csv(LIRE_Dal_all_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_all_types_places.csv")

# stela
LIRE_Dal_stela <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_stela.csv")

LIRE_Dal_stela_ll <- dataframe_ll(LIRE_Dal_stela)

LIRE_Dal_stela_n <- count(LIRE_Dal_stela)

plot14 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_stela_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_stela_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of all stelae",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot14)

ggsave("output_images/geographical_distribution/14.LIRE_Dalmatia_stela_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_stela_place <- st_drop_geometry(LIRE_Dal_stela_ll)
write.csv(LIRE_Dal_stela_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_stela_places.csv")

# epitaphs
LIRE_Dal_epitaph <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_epitaph.csv")

LIRE_Dal_epitaph_ll <- dataframe_ll(LIRE_Dal_epitaph)

LIRE_Dal_epitaph_n <- count(LIRE_Dal_epitaph)

plot15 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_epitaph_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of all epitaphs",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot15)

ggsave("output_images/geographical_distribution/15.LIRE_Dalmatia_epitaph_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_epitaph_place <- st_drop_geometry(LIRE_Dal_epitaph_ll)
write.csv(LIRE_Dal_epitaph_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_epitaph_places.csv")

# votives
LIRE_Dal_votive <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_votive.csv")

LIRE_Dal_votive_ll <- dataframe_ll(LIRE_Dal_votive)

LIRE_Dal_votive_n <- count(LIRE_Dal_votive)

plot16 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_votive_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_votive_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of all votives",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot16)

ggsave("output_images/geographical_distribution/16.LIRE_Dalmatia_votive_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_votive_place <- st_drop_geometry(LIRE_Dal_votive_ll)
write.csv(LIRE_Dal_votive_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_votive_places.csv")

# altars
LIRE_Dal_altar <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_altar.csv")

LIRE_Dal_altar_ll <- dataframe_ll(LIRE_Dal_altar)

LIRE_Dal_altar_n <- count(LIRE_Dal_altar)

plot17 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_altar_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_altar_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of all altars",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot17)

ggsave("output_images/geographical_distribution/17.LIRE_Dalmatia_altar_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_altar_place <- st_drop_geometry(LIRE_Dal_altar_ll)
write.csv(LIRE_Dal_altar_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_altar_places.csv")

# dated
# sacral and funerary
LIRE_Dal_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_dated.csv")

LIRE_Dal_dated_ll <- dataframe_ll(LIRE_Dal_dated)

LIRE_Dal_dated_n <- count(LIRE_Dal_dated)

plot17 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_dated_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_dated_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = dense_sites_dated_ll,
                   fill = "white",
                   aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean),
                   nudge_x = c( 0,  -1, -1.75,-1,    0, -0.5,0, -1,-0.75), 
                   nudge_y = c(-1,-0.5, -0.25, 0,-0.75,-0.25,1,0.5,-0.75))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
                       title = "Distribution of funerary and sacral inscriptions",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot17)

ggsave("output_images/geographical_distribution/17.LIRE_Dalmatia_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_dated_place <- st_drop_geometry(LIRE_Dal_dated_ll)
write.csv(LIRE_Dal_dated_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_dated_places.csv")

#all inscriptions
LIRE_Dal_all_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_all_types_dated.csv")

LIRE_Dal_all_dated_ll <- dataframe_ll(LIRE_Dal_all_dated)

LIRE_Dal_all_dated_n <- count(LIRE_Dal_all_dated)

plot18 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_all_dated_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_dated_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = dense_sites_dated_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,  -1, -1.75,-1,    0, -0.5,0, -1,-0.75), 
                  nudge_y = c(-1,-0.5, -0.25, 0,-0.75,-0.25,1,0.5,-0.75))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_all_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
                       title = "Distribution of all inscriptions",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot18)

ggsave("output_images/geographical_distribution/18.LIRE_Dalmatia_all_types_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_all_dated_place <- st_drop_geometry(LIRE_Dal_all_dated_ll)
write.csv(LIRE_Dal_all_dated_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_all_types_dated_places.csv")

# epitaphs
LIRE_Dal_epitaph_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_epitaph_dated.csv")

LIRE_Dal_epitaph_dated_ll <- dataframe_ll(LIRE_Dal_epitaph_dated)

LIRE_Dal_epitaph_dated_n <- count(LIRE_Dal_epitaph_dated)

plot19 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_epitaph_dated_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_dated_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = dense_sites_dated_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,  -1, -1.75,-1,    0, -0.5,0, -1,-0.75), 
                  nudge_y = c(-1,-0.5, -0.25, 0,-0.75,-0.25,1,0.5,-0.75))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
                       title = "Distribution of epitaphs",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot19)

ggsave("output_images/geographical_distribution/19.LIRE_Dalmatia_epitaph_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_epitaph_dated_place <- st_drop_geometry(LIRE_Dal_epitaph_dated_ll)
write.csv(LIRE_Dal_epitaph_dated_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_epitaph_dated_places.csv")

# stela
LIRE_Dal_stela_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_stela_dated.csv")

LIRE_Dal_stela_dated_ll <- dataframe_ll(LIRE_Dal_stela_dated)

LIRE_Dal_stela_dated_n <- count(LIRE_Dal_stela_dated)

plot20 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_stela_dated_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_dated_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = dense_sites_dated_ll,
                  fill = "white",
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c( 0,  -1, -1.75,-1,    0, -0.5,0, -1,-0.75), 
                  nudge_y = c(-1,-0.5, -0.25, 0,-0.75,-0.25,1,0.5,-0.75))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_stela_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
                       title = "Distribution of stelae",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot20)

ggsave("output_images/geographical_distribution/20.LIRE_Dalmatia_stela_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_stela_dated_place <- st_drop_geometry(LIRE_Dal_stela_dated_ll)
write.csv(LIRE_Dal_stela_dated_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_stela_dated_places.csv")

# now for mapping without Salona
LIRE_Dal_no_salona <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_no_salona.csv")

LIRE_Dal_no_salona_ll <- dataframe_ll(LIRE_Dal_no_salona)

LIRE_Dal_no_salona_n <- count(LIRE_Dal_no_salona)

plot21 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_no_salona_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = dense_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                  y = Latitude,
                  label = findspot_ancient_clean),
                  nudge_x = c(-1.25,    0,-0.75,-1.25,-1,   0,  -1,1,0.25,-0.75,1,1), 
                  nudge_y = c(-0.75,-0.75, 0.25, -0.5, 0,0.75,-0.5,0,1.25,   -1,0,1))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral inscriptions",
                       subtitle = "Dalmatia (Outside Salona)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot21)

ggsave("output_images/geographical_distribution/21.LIRE_Dalmatia_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# all inscriptions
LIRE_Dal_all_no_salona <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_all_types_no_salona.csv")

LIRE_Dal_all_no_salona_ll <- dataframe_ll(LIRE_Dal_all_no_salona)

LIRE_Dal_all_no_salona_n <- count(LIRE_Dal_all_no_salona)

plot22 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_all_no_salona_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_ll, colour = "#000000", size = 1) +
  geom_label_repel(data = dense_sites_ll,
                   fill = "white",
                   aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean),
                  nudge_x = c(-1.25,    0,-0.75,-1.25,-1,   0,  -1,1,0.25,-0.75,1,1), 
                  nudge_y = c(-0.75,-0.75, 0.25, -0.5, 0,0.75,-0.5,0,1.25,   -1,0,1))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_all_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of all inscriptions",
                       subtitle = "Dalmatia (Outside Salona)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot22)

ggsave("output_images/geographical_distribution/22.LIRE_Dalmatia_all_types_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# epitaphs
LIRE_Dal_epitaph_no_salona <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_epitaph_no_salona.csv")

LIRE_Dal_epitaph_no_salona_ll <- dataframe_ll(LIRE_Dal_epitaph_no_salona)

LIRE_Dal_epitaph_no_salona_n <- count(LIRE_Dal_epitaph_no_salona)

plot23 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_epitaph_no_salona_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_ll, colour = "#000000", size = 1) +
  geom_label_repel(data = dense_sites_ll,
                   fill = "white",
                   aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean),
                  nudge_x = c(-1.25,    0,-0.75,-1.25,-1,   0,  -1,1,0.25,-0.75,1,1), 
                  nudge_y = c(-0.75,-0.75, 0.25, -0.5, 0,0.75,-0.5,0,1.25,   -1,0,1))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of epitaphs",
                       subtitle = "Dalmatia (Outside Salona)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot23)

ggsave("output_images/geographical_distribution/23.LIRE_Dalmatia_epitaph_types_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# stela
LIRE_Dal_stela_no_salona <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_stela_no_salona.csv")

LIRE_Dal_stela_no_salona_ll <- dataframe_ll(LIRE_Dal_stela_no_salona)

LIRE_Dal_stela_no_salona_n <- count(LIRE_Dal_stela_no_salona)

plot23 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_stela_no_salona_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_ll, colour = "#000000", size = 1) +
  geom_label_repel(data = dense_sites_ll,
                   fill = "white",
                   aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean),
                  nudge_x = c(-1.25,    0,-0.75,-1.25,-1,   0,  -1,1,0.25,-0.75,1,1), 
                  nudge_y = c(-0.75,-0.75, 0.25, -0.5, 0,0.75,-0.5,0,1.25,   -1,0,1))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_stela_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of stela",
                       subtitle = "Dalmatia (Outside Salona)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot23)

ggsave("output_images/geographical_distribution/23.LIRE_Dalmatia_stela_types_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# military inscriptions
# sacral and funerary
LIRE_Dal_corpus_no_salona <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_no_salona.csv")

LIRE_Dal_corpus_no_salona_ll <- dataframe_ll(LIRE_Dal_corpus_no_salona)

LIRE_Dal_corpus_no_salona_n <- count(LIRE_Dal_corpus_no_salona)

plot24 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_no_salona_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                   fill = "white",
                 aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean), 
                 nudge_x = c(-0.25, -1.5,-1.5, 0), 
                 nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                       title = "Distribution of military funerary and sacral inscriptions",
                       subtitle = "Dalmatia (Outside Salona)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot24)

ggsave("output_images/geographical_distribution/24.LIRE_corpus_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

#all
LIRE_Dal_all_corpus_no_salona <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_all_types_no_salona.csv")

LIRE_Dal_all_corpus_no_salona_ll <- dataframe_ll(LIRE_Dal_all_corpus_no_salona)

LIRE_Dal_all_corpus_no_salona_n <- count(LIRE_Dal_all_corpus_no_salona)

plot25 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_all_corpus_no_salona_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                   fill = "white",
                 aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean), 
                 nudge_x = c(-0.25, -1.5,-1.5, 0), 
                 nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_all_corpus_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                       title = "Distribution of military inscriptions",
                       subtitle = "Dalmatia (Outside Salona)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot25)

ggsave("output_images/geographical_distribution/25.LIRE_corpus_all_types_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# non-military/all
LIRE_Dal_dated_no_salona <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_dated_no_salona.csv")

LIRE_Dal_dated_no_salona_ll <- dataframe_ll(LIRE_Dal_dated_no_salona)

LIRE_Dal_dated_no_salona_n <- count(LIRE_Dal_dated_no_salona)

plot26 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_dated_no_salona_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = dense_sites_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = dense_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                  y = Latitude,
                  label = findspot_ancient_clean),
                  nudge_x = c(-1.25,    0,-0.75,-1.25,-1,   0,  -1,1,0.25,-0.75,1,1), 
                  nudge_y = c(-0.75,-0.75, 0.25, -0.5, 0,0.75,-0.5,0,1.25,   -1,0,1))+
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_dated_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
       title = "Distribution of funerary and sacral inscriptions",
       subtitle = "Dalmatia (Outside Salona): Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot26)

ggsave("output_images/geographical_distribution/26.LIRE_Dalmatia_dated_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

#military sacral funerary
LIRE_Dal_corpus_dated_no_salona <-
  read.csv("output_tables/corpus/dated/LIRE_corpus_dated_no_salona.csv")

LIRE_Dal_corpus_dated_no_salona_ll <- dataframe_ll(LIRE_Dal_corpus_dated_no_salona)

LIRE_Dal_corpus_dated_no_salona_n <- count(LIRE_Dal_corpus_dated_no_salona)

plot27 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_dated_no_salona_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_mil_ll,
                   fill = "white",
                   aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean), 
                   nudge_x = c(-0.25, -1.5,-1.5, 0), 
                   nudge_y = c(-1   , -0.5,   0,-1)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_dated_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags.\n",
                       "Date ranges starting between 35 BCE/192 CE and ending 1/200 CE"),
                       title = "Distribution of military funerary and sacral inscriptions",
                       subtitle = "Dalmatia (Outside Salona): Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot27)

ggsave("output_images/geographical_distribution/27.LIRE_corpus_dated_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

#now for EDH, for comparison
dataframe_EDH_ll <- function(dataframe) {
  library(dplyr)
  library(stringr)
  dataframe_EDH_place <- na.omit(dataframe %>%
                               select(fo_antik,
                                       koordinaten1) %>%
                                group_by(fo_antik) %>%
                                count(fo_antik,
                                      koordinaten1) %>%
                                arrange(desc(n)))
  dataframe_EDH_place[c('latitude', 'longitude')] <- str_split_fixed(dataframe_EDH_place$koordinaten1, ',', 2)
  (dataframe_EDH_ll <- st_as_sf(dataframe_EDH_place, coords = c("longitude", "latitude"),
                            remove = FALSE,
                            crs = 4326, agr = "constant"))
  return(dataframe_EDH_ll)
}

# plot on map
EDH_Dalmatia <-
  read.csv("output_tables/corpus/EDH/EDH_Dalmatia.csv")

EDH_Dalmatia_ll <- dataframe_EDH_ll(EDH_Dalmatia)

EDH_Dalmatia_n <- count(EDH_Dalmatia)

plot28 <- 
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = EDH_Dalmatia_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                  y = Latitude,
                  label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density",
       caption = paste("n = ",
                       EDH_Dalmatia_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
       title = "Distribution of all inscriptions",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot28)

ggsave("output_images/geographical_distribution/28.EDH_Dalmatia_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

EDH_Dalmatia_place <- st_drop_geometry(EDH_Dalmatia_ll)
write.csv(EDH_Dalmatia_place,
          file = "output_tables/corpus/EDH/places/EDH_Dalmatia_places.csv")

EDH_Dalmatia_votive_epitaph <-
  read.csv("output_tables/corpus/EDH/EDH_Dalmatia_votive_epitaph.csv")

EDH_Dalmatia_votive_epitaph_ll <- dataframe_EDH_ll(EDH_Dalmatia_votive_epitaph)

EDH_Dalmatia_votive_epitaph_n <- count(EDH_Dalmatia_votive_epitaph)

plot29 <- 
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = EDH_Dalmatia_votive_epitaph_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                  y = Latitude,
                  label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density", 
       caption = paste("n = ",
                       EDH_Dalmatia_votive_epitaph_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
       title = "Distribution of funerary and sacral inscriptions",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot29)

ggsave("output_images/geographical_distribution/29.EDH_Dalmatia_funerary_sacral.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

EDH_Dalmatia_epitaph <-
  read.csv("output_tables/corpus/EDH/EDH_Dalmatia_epitaph.csv")

EDH_Dalmatia_epitaph_ll <- dataframe_EDH_ll(EDH_Dalmatia_epitaph)

EDH_Dalmatia_epitaph_n <- count(EDH_Dalmatia_epitaph)

plot30 <-
  ggplot() +
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#a1a1a1", alpha=0.6, size = 0.8) +
  geom_sf(data = EDH_Dalmatia_epitaph_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_label_repel(data = key_sites_ll,
                  fill = "white",
                  aes(x = Longitude,
                  y = Latitude,
                  label = findspot_ancient_clean),
                  nudge_x = c( 0,     -1,  -1.5), 
                  nudge_y = c(-1.25,-0.5, -0.25))+
  labs(size = "Density",
       caption = paste("n = ",
                       EDH_Dalmatia_epitaph_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
       title = "Distribution of funerary inscriptions",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot30)

ggsave("output_images/geographical_distribution/30.EDH_Dalmatia_epitaph.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

## combine 13 and 28 for comparison
doubletrouble <- grid.arrange(plot13, plot28, ncol = 2)

ggsave("output_images/geographical_distribution/31.LIRE_EDH_comparison.jpeg",
       doubletrouble, width = 240, height = 120, unit = "mm", dpi = 600)

## combine 13, 22

doubletroubler <- grid.arrange(plot13, plot22, ncol = 2)

ggsave("output_images/geographical_distribution/32.Dalmatia_all_types_Salona_comparison.jpeg",
       doubletroubler, width = 240, height = 120, unit = "mm", dpi = 600)

## combine 12 and 21 for comparison
doubletroublest <- grid.arrange(plot12, plot21, ncol = 2)

ggsave("output_images/geographical_distribution/33.Dalmatia_Salona_comparison.jpeg",
       doubletroublest, width = 240, height = 120, unit = "mm", dpi = 600)

## combine 1 and 8 for comparison
doubletroublestest <- grid.arrange(plot1, plot8, ncol = 2)

ggsave("output_images/geographical_distribution/34.corpus_dated_undated_comparison.jpeg",
       doubletroublestest, width = 240, height = 120, unit = "mm", dpi = 600)

## combine 3 and 9 for comparison
doubletroublestest <- grid.arrange(plot3, plot9, ncol = 2)

ggsave("output_images/geographical_distribution/35.corpus_dated_undated_all_types_comparison.jpeg",
       doubletroublestest, width = 240, height = 120, unit = "mm", dpi = 600)

## combine 4 and 10 for comparison
doubletroublestest <- grid.arrange(plot4, plot10, ncol = 2)

ggsave("output_images/geographical_distribution/36.corpus_dated_undated_stela_comparison.jpeg",
       doubletroublestest, width = 240, height = 120, unit = "mm", dpi = 600)

## combine 5 and 11 for comparison
doubletroublestest <- grid.arrange(plot5, plot11, ncol = 2)

ggsave("output_images/geographical_distribution/37.corpus_dated_undated_epitaph_comparison.jpeg",
       doubletroublestest, width = 240, height = 120, unit = "mm", dpi = 600)