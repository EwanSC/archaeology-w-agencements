# Plotting dates, LIRE
# Made 16/9/24
# Edited: 25/11/24
## using https://cran.r-project.org/web/packages/datplot/datplot.pdf
## and https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html 
library(dplyr)
library(datplot)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(RColorBrewer)
library("gridExtra")

LIRE_Dal_corpus <-
  read.csv("output_tables/corpus/undated/LIRE_corpus.csv")

LIRE_Dal <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia.csv")
LIRE_Dalmatia <-
  read.csv("data/LIRE/LIRE_Dalmatia.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_corpus_epitaph <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_epitaph.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_epitaph <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_epitaph.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_corpus_stela <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_stela.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_stela <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_stela.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_votive <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_votive.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_tabula <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_tabula.csv", na = c("","NA","NULL",NULL))
LIRE_Dal_corpus_tabula <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_tabula.csv", na = c("","NA","NULL",NULL))

count(LIRE_Dal_corpus)
count(LIRE_Dal)
count(LIRE_Dalmatia)
count(LIRE_Dal_corpus_epitaph)
count(LIRE_Dal_epitaph)
count(LIRE_Dal_corpus_stela)
count(LIRE_Dal_stela)
count(LIRE_Dal_votive)
count(LIRE_Dal_tabula)
count(LIRE_Dal_corpus_tabula)

# distinguish between the two types by adding variable column
LIRE_Dal_corpus$variable <- "Military"
LIRE_Dal$variable <- "All"
LIRE_Dalmatia$variable <- "All"
LIRE_Dal_corpus_epitaph$variable <- "Military"
LIRE_Dal_epitaph$variable <- "All"
LIRE_Dal_corpus_stela$variable <- "Military"
LIRE_Dal_stela$variable <- "All"
LIRE_Dal_votive$variable <- "All"
LIRE_Dal_tabula$variable <- "All"
LIRE_Dal_corpus_tabula$variable <- "Military"

# make function to clean to only required data for aorist analysis
# this includes modifying column classes and removing nulls
prepare_for_density <- function(dataframe) {
  library(dplyr)
  library(tidyverse)
  prepare_for_density_dates <- na.omit(dataframe %>%
               select(`LIST.ID`,variable,not_before,not_after))
  prepare_for_density_altered <- type.convert(prepare_for_density_dates, as.is = TRUE)    
  prepare_for_density <- na.omit(prepare_for_density_altered)
  return(prepare_for_density)
}

# use function
LIRE_Dal_corpus_na <- prepare_for_density(LIRE_Dal_corpus)
LIRE_Dal_na <- prepare_for_density(LIRE_Dal)
LIRE_Dalmatia_na <- prepare_for_density(LIRE_Dalmatia)
LIRE_Dal_corpus_epitaph_na <- prepare_for_density(LIRE_Dal_corpus_epitaph)
LIRE_Dal_epitaph_na <- prepare_for_density(LIRE_Dal_epitaph)
LIRE_Dal_corpus_stela_na <- prepare_for_density(LIRE_Dal_corpus_stela)
LIRE_Dal_stela_na <- prepare_for_density(LIRE_Dal_stela)
LIRE_Dal_votive_na <- prepare_for_density(LIRE_Dal_votive)
LIRE_Dal_tabula_na <- prepare_for_density(LIRE_Dal_tabula)
LIRE_Dal_corpus_tabula_na <- prepare_for_density(LIRE_Dal_corpus_tabula)

LIRE_Dal_corpus_count <- count(LIRE_Dal_corpus_na)
LIRE_Dal_count <- count(LIRE_Dal_na)
LIRE_Dalmatia_count <- count(LIRE_Dalmatia_na)
LIRE_Dal_corpus_epitaph_count <- count(LIRE_Dal_corpus_epitaph)
LIRE_Dal_epitaph_count <- count(LIRE_Dal_epitaph)
LIRE_Dal_corpus_stela_count <- count(LIRE_Dal_corpus_stela)
LIRE_Dal_stela_count <- count(LIRE_Dal_stela)
LIRE_Dal_votive_count <- count(LIRE_Dal_votive)
LIRE_Dal_tabula_count <- count(LIRE_Dal_tabula)
LIRE_Dal_corpus_tabula_count <- count(LIRE_Dal_corpus_tabula)

# check distribution of monuments using clean
## Following https://static.cambridge.org/content/id/urn:cambridge.org:id:article:S2326376821000085/resource/name/S2326376821000085sup001.pdf
LIRE_Dal_corpus %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#FF8247", colour="#9F522E") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Military, sacral and funerary",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags.\n",
                       "Method = Steinmann & Weissova 2021.")) +
   scale_x_continuous(breaks = seq(0, 600, by = 50))

ggsave("output_images/chronological_distribution/01.LIRE_corpus_inscription_ranges.jpeg",
       dpi = 600)

LIRE_Dal_corpus_epitaph %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#FF8247", colour="#9F522E") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Military epitaphs",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags.\n",
                       "Method = Steinmann & Weissova 2021."))  +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

ggsave("output_images/chronological_distribution/02.LIRE_corpus_epitaph_inscription_ranges.jpeg",
       dpi = 600)

LIRE_Dal %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#3468d6", colour="#234183") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: Sacral and funerary",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

ggsave("output_images/chronological_distribution/03.LIRE_Dalmatia_inscription_ranges.jpeg",
       dpi = 600)

LIRE_Dalmatia %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#3468d6", colour="#234183") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: all inscriptions",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

ggsave("output_images/chronological_distribution/04.LIRE_Dalmatia_all_types_inscription_ranges.jpeg",
       dpi = 600)

LIRE_Dal_epitaph %>%
  mutate(timespan = not_after-not_before) %>%
  ggplot(aes(x = timespan)) +
  geom_histogram(binwidth = 10, fill = "#3468d6", colour="#234183") +
  labs(title = "Inscription date range",
       subtitle = "Dalmatia: epitaphs",
       x = "Length of timespan (10 year bins)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Method = Steinmann & Weissova 2021.")) +
  scale_x_continuous(breaks = seq(0, 600, by = 50))

ggsave("output_images/chronological_distribution/05.LIRE_Dalmatia_epitaph_inscription_ranges.jpeg",
       dpi = 600)

# now make histograms using the 'mean'
LIRE_Dal_corpus$DAT_mean <- (LIRE_Dal_corpus$not_after + LIRE_Dal_corpus$not_before) / 2

ggplot(LIRE_Dal_corpus, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#FF8247", colour="#9F522E") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

ggsave("output_images/chronological_distribution/06.LIRE_corpus_mean_histogram.jpeg",
       dpi = 600)

LIRE_Dal_corpus_epitaph$DAT_mean <- (LIRE_Dal_corpus_epitaph$not_after + LIRE_Dal_corpus_epitaph$not_before) / 2

ggplot(LIRE_Dal_corpus_epitaph, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#FF8247", colour="#9F522E") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military epitaphs",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

ggsave("output_images/chronological_distribution/07.LIRE_corpus_epitaph_mean_histogram.jpeg",
       dpi = 600)

LIRE_Dal$DAT_mean <- (LIRE_Dal$not_after + LIRE_Dal$not_before) / 2

ggplot(LIRE_Dal, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#3468d6", colour="#234183") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all funerary and sacral inscriptions",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

ggsave("output_images/chronological_distribution/08.LIRE_Dalmatia_mean_histogramn.jpeg",
       dpi = 600)

LIRE_Dalmatia$DAT_mean <- (LIRE_Dalmatia$not_after + LIRE_Dalmatia$not_before) / 2

ggplot(LIRE_Dalmatia, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#3468d6", colour="#234183") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all inscriptions",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

ggsave("output_images/chronological_distribution/09.LIRE_Dalmatia_all_types_mean_histogramn.jpeg",
       dpi = 600)

LIRE_Dal_epitaph$DAT_mean <- (LIRE_Dal_epitaph$not_after + LIRE_Dal_epitaph$not_before) / 2

ggplot(LIRE_Dal_epitaph, aes(x = DAT_mean)) +
  geom_histogram(binwidth = 20, position = "dodge", fill = "#3468d6", colour="#234183") +
  labs(x = "Date (BCE/CE)", y = "Number of inscriptions",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of epitaphs",
       subtitle = paste("Using mean",
                        "with a binwidth of 20")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 20))

ggsave("output_images/chronological_distribution/10.LIRE_Dalmatia_epitaph_mean_histogramn.jpeg",
       dpi = 600)

# now for weighted density using datplot and 'clean' dfs
## Work with https://cran.r-project.org/web/packages/datplot/vignettes/how-to.html
LIRE_Dal_corpus_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_na,
                                                     stepsize = 15),
                                            var = "all")

ggplot(data = LIRE_Dal_corpus_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247")+
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                           "with stepsize of ",
                           attributes(LIRE_Dal_corpus_scaled)$stepsize,
                           sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/11.LIRE_corpus_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_corpus_epitaph_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_epitaph_na,
                                                         stepsize = 15),
                                                var = "all")

ggplot(data = LIRE_Dal_corpus_epitaph_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military epitaphs",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_epitaph_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/12.LIRE_corpus_epitaph_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_corpus_stela_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_stela_na,
                                                         stepsize = 15),
                                                var = "all")

ggplot(data = LIRE_Dal_corpus_stela_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_stela_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military stelae",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_stela_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/13.LIRE_corpus_stela_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_corpus_tabula_scaled <- scaleweight(datsteps(LIRE_Dal_corpus_tabula_na,
                                                         stepsize = 15),
                                                var = "all")

ggplot(data = LIRE_Dal_corpus_tabula_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#FF8247") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_tabula_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military tabulae",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_corpus_tabula_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


ggsave("output_images/chronological_distribution/14.LIRE_corpus_tabula_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_scaled <- scaleweight(datsteps(LIRE_Dal_na,
                                              stepsize = 15),
                                     var = "all")

ggplot(data = LIRE_Dal_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/15.LIRE_Dalmatia_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dalmatia_scaled <- scaleweight(datsteps(LIRE_Dalmatia_na,
                                             stepsize = 15),
                                    var = "all")

ggplot(data = LIRE_Dalmatia_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/16.LIRE_Dalmatia_all_types_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_epitaph_scaled <- scaleweight(datsteps(LIRE_Dal_epitaph_na,
                                             stepsize = 15),
                                    var = "all")

ggplot(data = LIRE_Dal_epitaph_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of epitaphs",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_epitaph_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/17.LIRE_Dalmatia_epitaph_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_Dal_stela_scaled <- scaleweight(datsteps(LIRE_Dal_stela_na,
                                             stepsize = 15),
                                    var = "all")

ggplot(data = LIRE_Dal_stela_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_stela_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of stelae",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_stela_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/18.LIRE_Dalmatia_stela_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

#tabula
LIRE_Dal_tabula_scaled <- scaleweight(datsteps(LIRE_Dal_tabula_na,
                                              stepsize = 15),
                                     var = "all")

ggplot(data = LIRE_Dal_tabula_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_tabula_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all tabulae",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_tabula_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/19.LIRE_Dalmatia_tabula_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

# just votives
LIRE_Dal_votive_scaled <- scaleweight(datsteps(LIRE_Dal_votive_na,
                                              stepsize = 15),
                                     var = "all")

ggplot(data = LIRE_Dal_votive_scaled, aes(x = DAT_step, weight = weight)) +
  geom_density(alpha = 0.8, fill = "#3468d6") +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_votive_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of all votive inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_votive_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/20.LIRE_Dalmatia_votive_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)
# now for with a histogram
##get histogram 
LIRE_Dal_corpus_scaled_histogramscale <- get.histogramscale(LIRE_Dal_corpus_scaled)
LIRE_Dal_scaled_histogram <- get.histogramscale(LIRE_Dal_scaled)
LIRE_Dalmatia_scaled_histogram <- get.histogramscale(LIRE_Dalmatia_scaled)

## plot with histogram
ggplot(LIRE_Dal_corpus_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_corpus_scaled)$stepsize,
                 position = "dodge", colour = "#000000", fill = "#9F522E") +
  stat_density(alpha = 0.8, position = "dodge", colour = "#000000", fill = "#FF8247",
               aes(y = (after_stat(density) * LIRE_Dal_corpus_scaled_histogramscale),
                   weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of military funerary and sacral inscriptions",
     subtitle = paste("Using the weighted output of datsteps() ",
                      "with stepsize of ",
                      attributes(LIRE_Dal_corpus_scaled)$stepsize,
                      sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/21.LIRE_corpus_histogram_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

ggplot(LIRE_Dal_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dal_scaled)$stepsize,
                 position = "dodge", colour = "#000000", fill = "#234183") +
  stat_density(alpha = 0.8, position = "dodge", colour = "#000000", fill = "#3468d6",
               aes(y = (after_stat(density) * LIRE_Dal_scaled_histogram), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dal_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/22.LIRE_Dalmatia_histogram_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

ggplot(LIRE_Dalmatia_scaled, aes(x = DAT_step)) +
  geom_histogram(alpha = 0.8, binwidth = attributes(LIRE_Dalmatia_scaled)$stepsize,
                 position = "dodge", colour = "#000000", fill = "#234183") +
  stat_density(alpha = 0.8, position = "dodge", colour = "#000000", fill = "#3468d6",
               aes(y = (after_stat(density) * LIRE_Dalmatia_scaled_histogram), weight = weight)) +
  labs(x = "Date (BCE/CE)", y = "Density",
       caption = paste("n = ",
                       LIRE_Dalmatia_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Chronological distribution of funerary and sacral inscriptions",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/23.LIRE_Dalmatia_all_types_histogram_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

#now to compare
LIRE_both <- rbind(LIRE_Dal_na, LIRE_Dal_corpus_na)

LIRE_both_scaled <- scaleweight(datsteps(LIRE_both, stepsize = 15),
                                var = "all")

ggplot(data = LIRE_both_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#3468d6"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_count$n,
                       ". All n = ",
                       LIRE_Dal_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of funerary and sacral inscriptions (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_both_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/24.LIRE_Dalmatia_comparison_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_both_epitaph <- rbind(LIRE_Dal_epitaph_na, LIRE_Dal_corpus_epitaph_na)

LIRE_both_epitaph_scaled <- scaleweight(datsteps(LIRE_both_epitaph, stepsize = 15),
                                          var = "all")

ggplot(data = LIRE_both_epitaph_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#3468d6"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       ". All n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of epitaphs (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_both_epitaph_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/25.LIRE_Dalmatia_epitaph_comparison_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_both_stela <- rbind(LIRE_Dal_stela_na, LIRE_Dal_corpus_stela_na)

LIRE_both_stela_scaled <- scaleweight(datsteps(LIRE_both_stela, stepsize = 15),
                                          var = "all")

ggplot(data = LIRE_both_stela_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#3468d6"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_stela_count$n,
                       ". All n = ",
                       LIRE_Dal_stela_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of stelae (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_both_stela_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/26.LIRE_Dalmatia_stela_comparison_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

LIRE_both_tabula <- rbind(LIRE_Dal_tabula_na, LIRE_Dal_corpus_tabula_na)

LIRE_both_tabula_scaled <- scaleweight(datsteps(LIRE_both_tabula, stepsize = 15),
                                          var = "all")

ggplot(data = LIRE_both_tabula_scaled, aes(x = DAT_step, weight = weight,
                                          group=variable, fill=variable)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Military" = "#FF8247", "All" = "#3468d6"),
                    name = "Category") +
  labs(x = "Date (BCE/CE)", y = "Density",
       variable = "Category",
       caption = paste("Military n = ",
                       LIRE_Dal_corpus_tabula_count$n,
                       ". All n = ",
                       LIRE_Dal_tabula_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Filtered by key words and tags."),
       title = "Chronological distribution of tabulae (Density)",
       subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_both_tabula_scaled)$stepsize,
                        sep = "")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("output_images/chronological_distribution/27.LIRE_Dalmatia_tabula_comparison_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

## now for differences based on type
LIRE_Dalmatia_types <-
  read.csv("data/LIRE/LIRE_Dalmatia.csv", na = c("","NA","NULL",NULL))

LIRE_Dalmatia_types <- LIRE_Dalmatia_types %>%
rename(variable = type_of_inscription_auto)

LIRE_Dalmatia_votives <- LIRE_Dalmatia_types %>%
    filter(variable %in% c("votive inscription"))

LIRE_Dalmatia_votives_na <- prepare_for_density(LIRE_Dalmatia_votives)

LIRE_Dalmatia_votives_count <- count(LIRE_Dalmatia_votives_na)

LIRE_Dalmatia_votives_scaled <- scaleweight(datsteps(LIRE_Dalmatia_votives_na,
                                                stepsize = 15),
                                       var = "all")

LIRE_Dalmatia_epitaphs <- LIRE_Dalmatia_types %>%
    filter(variable %in% c("epitaph"))

LIRE_Dalmatia_epitaphs_na <- prepare_for_density(LIRE_Dalmatia_epitaphs)

LIRE_Dalmatia_epitaphs_count <- count(LIRE_Dalmatia_epitaphs_na)

LIRE_Dalmatia_epitaphs_scaled <- scaleweight(datsteps(LIRE_Dalmatia_epitaphs_na,
                                                stepsize = 15),
                                       var = "all")
plot1 <-
  ggplot(data = LIRE_Dalmatia_votives_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("votive inscription" = "#DCDCDC"),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_Dalmatia_votives_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of votive inscripitions",
         subtitle = paste("Using the weighted output of datsteps() ",
                          "with stepsize of ",
                          attributes(LIRE_Dalmatia_votives_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot1)

plot2 <-
  ggplot(data = LIRE_Dalmatia_epitaphs_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c( "epitaph" = '#EE0000'),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_Dalmatia_epitaphs_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of epitaphs",
         subtitle = paste("Using the weighted output of datsteps() ",
                          "with stepsize of ",
                          attributes(LIRE_Dalmatia_epitaphs_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot2)

epitaphs_votives <- grid.arrange(plot1, plot2, ncol = 1, nrow = 2)

ggsave("output_images/chronological_distribution/28.LIRE_Dalmatia_types_altar_epitaph.jpeg",
       epitaphs_votives, width = 211, height = 240, unit = "mm", dpi = 600)

#military
LIRE_corpus_types <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_all_types.csv")

LIRE_corpus_types <- LIRE_corpus_types %>%
rename(variable = type_of_inscription_auto)

LIRE_corpus_votives <- LIRE_corpus_types %>%
    filter(variable %in% c("votive inscription"))

LIRE_corpus_votives_na <- prepare_for_density(LIRE_corpus_votives)

LIRE_corpus_votives_count <- count(LIRE_corpus_votives_na)

LIRE_corpus_votives_scaled <- scaleweight(datsteps(LIRE_corpus_votives_na,
                                                stepsize = 15),
                                       var = "all")

LIRE_corpus_epitaphs <- LIRE_corpus_types %>%
    filter(variable %in% c("epitaph"))

LIRE_corpus_epitaphs_na <- prepare_for_density(LIRE_corpus_epitaphs)

LIRE_corpus_epitaphs_count <- count(LIRE_corpus_epitaphs_na)

LIRE_corpus_epitaphs_scaled <- scaleweight(datsteps(LIRE_corpus_epitaphs_na,
                                                stepsize = 15),
                                       var = "all")

plot3 <-
  ggplot(data = LIRE_corpus_votives_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("votive inscription" = "#050505"),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_corpus_votives_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of military votive inscriptions",
         subtitle = paste("Using the weighted output of datsteps() ",
                          "with stepsize of ",
                          attributes(LIRE_corpus_votives_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot3)

plot4 <-
  ggplot(data = LIRE_corpus_epitaphs_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("epitaph" = '#FF8247'),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_corpus_epitaphs_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of military epitaphs",
         subtitle = paste("Using the weighted output of datsteps() ",
                          "with stepsize of ",
                          attributes(LIRE_corpus_epitaphs_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot4)

corpus_epitaphs_votives <- grid.arrange(plot3, plot4, ncol = 1, nrow = 2)

ggsave("output_images/chronological_distribution/29.LIRE_corpus_types_altar_epitaph.jpeg",
       corpus_epitaphs_votives, width = 211, height = 240, unit = "mm", dpi = 600)

# monument types
LIRE_Dalmatia_monu_types <-
  read.csv("data/LIRE/LIRE_Dalmatia.csv", na = c("","NA","NULL",NULL))

LIRE_Dalmatia_monu_types <- LIRE_Dalmatia_monu_types %>%
  rename(variable = type_of_monument_clean)
  
LIRE_Dalmatia_altar <- LIRE_Dalmatia_monu_types %>%
  filter(variable %in% c("altar"))

LIRE_Dalmatia_altar_na <- prepare_for_density(LIRE_Dalmatia_altar)

LIRE_Dalmatia_altar_count <- count(LIRE_Dalmatia_altar)

LIRE_Dalmatia_altar_scaled <- scaleweight(datsteps(LIRE_Dalmatia_altar_na,
                                                                stepsize = 15),
                                                       var = "all")

LIRE_Dalmatia_stelae <- LIRE_Dalmatia_monu_types %>%
  filter(variable %in% c("stele"))

LIRE_Dalmatia_stelae_na <- prepare_for_density(LIRE_Dalmatia_stelae)

LIRE_Dalmatia_stelae_count <- count(LIRE_Dalmatia_stelae)

LIRE_Dalmatia_stelae_scaled <- scaleweight(datsteps(LIRE_Dalmatia_stelae_na,
                                                                stepsize = 15),
                                                       var = "all")

LIRE_Dalmatia_sarcophagi <- LIRE_Dalmatia_monu_types %>%
  filter(variable %in% c("sarcophagus"))

LIRE_Dalmatia_sarcophagi_na <- prepare_for_density(LIRE_Dalmatia_sarcophagi)

LIRE_Dalmatia_sarcophagi_count <- count(LIRE_Dalmatia_sarcophagi)

LIRE_Dalmatia_sarcophagi_scaled <- scaleweight(datsteps(LIRE_Dalmatia_sarcophagi_na,
                                                                stepsize = 15),
                                                       var = "all")

LIRE_Dalmatia_tabulae <- LIRE_Dalmatia_monu_types %>%
  filter(variable %in% c("tabula"))

LIRE_Dalmatia_tabulae_na <- prepare_for_density(LIRE_Dalmatia_tabulae)

LIRE_Dalmatia_tabulae_count <- count(LIRE_Dalmatia_tabulae)

LIRE_Dalmatia_tabulae_scaled <- scaleweight(datsteps(LIRE_Dalmatia_tabulae_na,
                                                                stepsize = 15),
                                                       var = "all")

plot5 <-                                                       
  ggplot(data = LIRE_Dalmatia_altar_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("altar" = "#54FF9F"),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_Dalmatia_altar_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of altars",
         subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_altar_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot5)

plot6 <-                                                       
  ggplot(data = LIRE_Dalmatia_stelae_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("stele" = '#FFE4B5'),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_Dalmatia_stelae_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of stelae",
         subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_stelae_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot6)

plot7 <-                                                       
  ggplot(data = LIRE_Dalmatia_sarcophagi_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("sarcophagus" = '#EE0000'),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_Dalmatia_sarcophagi_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of sarcophagi",
         subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_sarcophagi_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot7)

plot8 <-                                                       
  ggplot(data = LIRE_Dalmatia_tabulae_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("tabula" ='#DCDCDC'),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_Dalmatia_tabulae_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of tabulae",
         subtitle = paste("Using the weighted output of datsteps() ",
                        "with stepsize of ",
                        attributes(LIRE_Dalmatia_tabulae_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot8)

monuments <- grid.arrange(plot5, plot6, plot7, plot8, ncol = 2, nrow = 2)

ggsave("output_images/chronological_distribution/30.LIRE_Dalmatia_types_altar_stela_sarcophagus_tabula.jpeg",
       monuments, width = 420, height = 240, unit = "mm", dpi = 600)

#military
LIRE_corpus_monu_types <-
  read.csv("output_tables/corpus/undated/LIRE_corpus_all_types.csv", na = c("","NA","NULL",NULL))

LIRE_corpus_monu_types <- LIRE_corpus_monu_types[!(LIRE_corpus_monu_types$LIST.ID == 475169),]

LIRE_corpus_monu_types <- LIRE_corpus_monu_types %>%
  rename(variable = type_of_monument_clean)

LIRE_corpus_altar <- LIRE_corpus_monu_types %>%
  filter(variable %in% c("altar"))

LIRE_corpus_altar_na <- prepare_for_density(LIRE_corpus_altar)

LIRE_corpus_altar_count <- count(LIRE_corpus_altar)

LIRE_corpus_altar_scaled <- scaleweight(datsteps(LIRE_corpus_altar_na,
                                                                stepsize = 15),
                                                       var = "all")

LIRE_corpus_stelae <- LIRE_corpus_monu_types %>%
  filter(variable %in% c("stele"))

LIRE_corpus_stelae_na <- prepare_for_density(LIRE_corpus_stelae)

LIRE_corpus_stelae_count <- count(LIRE_corpus_stelae)

LIRE_corpus_stelae_scaled <- scaleweight(datsteps(LIRE_corpus_stelae_na,
                                                                stepsize = 15),
                                                       var = "all")

LIRE_corpus_tabulae <- LIRE_corpus_monu_types %>%
  filter(variable %in% c("tabula"))

LIRE_corpus_tabulae_na <- prepare_for_density(LIRE_corpus_tabulae)

LIRE_corpus_tabulae_count <- count(LIRE_corpus_tabulae)

LIRE_corpus_tabulae_scaled <- scaleweight(datsteps(LIRE_corpus_tabulae_na,
                                                                stepsize = 15),
                                                       var = "all")

plot9 <-
  ggplot(data = LIRE_corpus_altar_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("altar" = "#DCDCDC"),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_corpus_altar_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of military altars",
         subtitle = paste("Using the weighted output of datsteps() ",
                          "with stepsize of ",
                          attributes(LIRE_corpus_altar_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot9)

plot10 <-
  ggplot(data = LIRE_corpus_stelae_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("stele" = "#FF8247"),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_corpus_stelae_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of military stelae",
         subtitle = paste("Using the weighted output of datsteps() ",
                          "with stepsize of ",
                          attributes(LIRE_corpus_stelae_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot10)

plot11 <-
  ggplot(data = LIRE_corpus_tabulae_scaled,
         aes(x = DAT_step, fill = variable, weight = weight)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("tabula" = '#000000'),
                      name = "Category") +
      labs(x = "Date (BCE/CE)", y = "Relative type density",
         caption = paste("n = ",
                         LIRE_corpus_tabulae_count$n,
                         sep = "",
                         ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
         title = "Chronological distribution of military tabulae",
         subtitle = paste("Using the weighted output of datsteps() ",
                          "with stepsize of ",
                          attributes(LIRE_corpus_tabulae_scaled)$stepsize,
                          sep = "")) +
    scale_x_continuous(
      limits = c(-50, 350),
      breaks = seq(-50, 350, by = 25)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())
plot(plot11)

corpus_monuments <- grid.arrange(plot9, plot10, plot11, ncol = 2, nrow = 2)

ggsave("output_images/chronological_distribution/31.LIRE_corpus_types_altar_stela_tabula.jpeg",
       corpus_monuments, width = 420, height = 240, unit = "mm", dpi = 600)

# beta: trying to make cumulative graph
LIRE_Dal_epitaph_cumulative <- LIRE_Dal_epitaph %>%
  arrange(DAT_mean) %>%
  mutate(cumulative_count = row_number())

LIRE_Dal_epitaph_corpus_cumulative <- LIRE_Dal_corpus_epitaph %>%
  arrange(DAT_mean) %>%
  mutate(cumulative_count = row_number())

ggplot(LIRE_Dal_epitaph_corpus_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#FF8247", linewidth = 1) +
  labs(x = "Date (BCE/CE)", y = "Cumulative count",
       caption = paste("n = ",
                       LIRE_Dal_corpus_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Cumulative count of military epitaphs",
       subtitle = paste("Dalmatia: using mean")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25))

ggsave("output_images/chronological_distribution/32.LIRE_corpus_epitaph_cumulative_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)

ggplot(LIRE_Dal_epitaph_cumulative, aes(x = DAT_mean, y = cumulative_count)) +
  geom_line(color = "#3468d6", linewidth = 1) +
  labs(x = "Date (BCE/CE)", y = "Cumulative count",
       caption = paste("n = ",
                       LIRE_Dal_epitaph_count$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0)."),
       title = "Cumulative count of epitaphs",
       subtitle = paste("Dalmatia: using mean")) +
  scale_x_continuous(
    limits = c(-50, 350),
    breaks = seq(-50, 350, by = 25))

ggsave("output_images/chronological_distribution/33.LIRE_epitaph_cumulative_plot.jpeg",
       width = 180, height = 100, unit = "mm", dpi = 600)
