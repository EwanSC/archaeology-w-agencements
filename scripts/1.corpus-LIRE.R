# getting corpus from cleaned LIRE data
# started 5/9/2024
# edited 18/11/2024
library(dplyr)
library(sqldf)
library(arrow)

# clean and read LIRE data
# and make NA values consistent
# data as of 10/10/2024 (via /scripts/appendix-0.LIRE-data)
LIRE_Dalmatia <-
  read.csv("data/LIRE/LIRE_Dalmatia.csv")

# function to remove unwanted monument/inscription types
clean_monument_types <- function(dataframe) {
  library(dplyr)
  clean_monument_types <- dataframe %>%
    filter(!type_of_monument_clean %in% c("architectural member",
                                          "mile-/leaguestone",
                                          "tile",
                                          "instrumentum domesticum",
                                          "table",
                                          "instrumentum militare")
           & !type_of_inscription_auto %in% c("building/dedicatory inscription",
                                               "honorific inscription",
                                               "boundary inscription",
                                               "mile-/leaguestone",
                                               "military diploma",
                                               "building/dedicatory inscription",
                                               "list",
                                               "seat inscription",
                                               "public legal inscription",
                                               "prayer",
                                               "seat inscription",
                                               "owner/artist inscription",
                                               "identification inscription",
                                               "assignation inscription",
                                               "acclamation",
                                               "calendar"))  
  return(clean_monument_types)
}

# function to get epitaphs 
epitaphs <- function(dataframe) {
  library(dplyr)
  epitaphs <- dataframe %>%
    filter(type_of_inscription_auto %in% c("epitaph"))
  return(epitaphs)
}

# function to get stelae 
stelas <- function(dataframe) {
  library(dplyr)
  stelas <- dataframe %>%
    filter(type_of_monument_clean %in% c("stele"))
  return(stelas)
}

# function to get tabula 
tabulas <- function(dataframe) {
  library(dplyr)
  tabulas <- dataframe %>%
    filter(type_of_monument_clean %in% c("tabula"))
  return(tabulas)
}

# function to get votive inscriptions 
votives <- function(dataframe) {
  library(dplyr)
  votives <- dataframe %>%
    filter(type_of_inscription_auto %in% c("votive inscription"))
  return(votives)
}

# function to get altars 
altars <- function(dataframe) {
  library(dplyr)
  altars <- dataframe %>%
    filter(type_of_monument_clean %in% c("altar"))
  return(altars)
}

#function to put within date parameter (Claudians - Antonines)
within_date_range <- function(dataframe) {
  library(dplyr)
  within_date_range <- dataframe %>%
    filter(not_before %in% (-35:192), not_after %in% (1:200)) %>%
    arrange(not_after, not_before)
  return(within_date_range)
}

# run them on the data to make dated/undated base and cleaned base data
LIRE_Dal_all <- LIRE_Dalmatia
LIRE_Dal_all$count<- 1
write.csv(LIRE_Dal_all,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_all_types.csv")

LIRE_Dal_all_dated <- within_date_range(LIRE_Dalmatia)
LIRE_Dal_all_dated$count<- 1
write.csv(LIRE_Dal_all_dated,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_all_types_dated.csv")

LIRE_Dal <- clean_monument_types(LIRE_Dalmatia)
LIRE_Dal$count<- 1
write.csv(LIRE_Dal,
          file = "output_tables/corpus/undated/LIRE_Dalmatia.csv")

LIRE_Dal_dated <- within_date_range(LIRE_Dal)
LIRE_Dal_dated$count<- 1
write.csv(LIRE_Dal_dated,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_dated.csv")

LIRE_Dal_stela <- stelas(LIRE_Dalmatia)
LIRE_Dal_stela$count<- 1
write.csv(LIRE_Dal_stela,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_stela.csv")

LIRE_Dal_stela_dated <- within_date_range(LIRE_Dal_stela)
LIRE_Dal_stela_dated$count<- 1
write.csv(LIRE_Dal_stela_dated,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_stela_dated.csv")

LIRE_Dal_tabula <- tabulas(LIRE_Dalmatia)
LIRE_Dal_tabula$count<- 1
write.csv(LIRE_Dal_tabula,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_tabula.csv")

LIRE_Dal_tabula_dated <- within_date_range(LIRE_Dal_tabula)
LIRE_Dal_tabula_dated$count<- 1
write.csv(LIRE_Dal_tabula_dated,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_tabula_dated.csv")

LIRE_Dal_epitaph <- epitaphs(LIRE_Dalmatia)
LIRE_Dal_epitaph$count<- 1
write.csv(LIRE_Dal_epitaph,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_epitaph.csv")

LIRE_Dal_epitaph_dated <- within_date_range(LIRE_Dal_epitaph)
LIRE_Dal_epitaph_dated$count<- 1
write.csv(LIRE_Dal_epitaph_dated,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_epitaph_dated.csv")

LIRE_Dal_votive <- votives(LIRE_Dalmatia)
LIRE_Dal_votive$count<- 1
write.csv(LIRE_Dal_votive,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_votive.csv")

LIRE_Dal_votive_dated <- within_date_range(LIRE_Dal_votive)
LIRE_Dal_votive_dated$count<- 1
write.csv(LIRE_Dal_votive_dated,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_votive_dated.csv")

LIRE_Dal_altar <- altars(LIRE_Dalmatia)
LIRE_Dal_altar$count<- 1
write.csv(LIRE_Dal_altar,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_altar.csv")

LIRE_Dal_altar_dated <- within_date_range(LIRE_Dal_altar)
LIRE_Dal_altar_dated$count<- 1
write.csv(LIRE_Dal_altar_dated,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_altar_dated.csv")

# make function for filtering military monuments
# (some based on place, others just key words and tags)

##with just key words
load_military_terms <- function(dataframe) {
  library(sqldf)
  library(dplyr)
  load_military_terms <- sqldf("Select * from dataframe
                  WHERE clean_text_interpretive_word 
                    LIKE '%legio%'
                  OR clean_text_interpretive_word 
                    LIKE '%cohor%'
                  OR clean_text_interpretive_word 
                    LIKE '%ala%'
                  OR clean_text_interpretive_word 
                    LIKE '%alae%'
                  OR clean_text_interpretive_word 
                    LIKE '%milit%'
                  OR clean_text_interpretive_word 
                    LIKE '%eques%'
                  OR clean_text_interpretive_word 
                    LIKE '%equit%'
                  OR clean_text_interpretive_word 
                    LIKE '%duplicari%'
                  OR clean_text_interpretive_word 
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%centuri%'
                  or clean_text_interpretive_word
                    LIKE '%immun%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiar%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%corniculari%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%praefectus castrorum%'
                  or clean_text_interpretive_word
                    LIKE '%optio %'
                  or clean_text_interpretive_word
                    LIKE '%option%'
                  or clean_text_interpretive_word
                    LIKE '%stipendi%'
                  or status_notation
                    LIKE '%milites%'
                    ")
load_military_terms <- load_military_terms %>%
  filter(!LIST.ID %in% c("468295",
                    "468613",
                    "468717",
                    "468832",
                    "469155",
                    "469186",
                    "469226",
                    "469341",
                    "469464",
                    "469506",
                    "469916",
                    "470136",
                    "470434",
                    "470543",
                    "470704",
                    "470864",
                    "471007",
                    "471036",
                    "471038",
                    "471230",
                    "471402",
                    "471414",
                    "471496",
                    "471645",
                    "471678",
                    "471682",
                    "471759",
                    "471777",
                    "471800",
                    "471809",
                    "471996",
                    "472452",
                    "472453",
                    "472586",
                    "472928",
                    "472984",
                    "473108",
                    "473129",
                    "473194",
                    "473217",
                    "473422",
                    "473564",
                    "473959",
                    "474003",
                    "474473",
                    "474478",
                    "474519",
                    "474682",
                    "474858",
                    "474992",
                    "475226",
                    "475259",
                    "475320",
                    "475377",
                    "475378",
                    "500263",
                    "106845",
                    "468356",
                    "474444"))
return(load_military_terms)
}

## with places and key words
load_military_terms_and_sites <- function(dataframe) {
  library(sqldf)
  library(dplyr)
  load_military_terms_and_sites <- sqldf("Select * from dataframe
                  WHERE clean_text_interpretive_word 
                    LIKE '%legio%'
                  OR clean_text_interpretive_word 
                    LIKE '%cohor%'
                  OR clean_text_interpretive_word 
                    LIKE '%ala%'
                  OR clean_text_interpretive_word 
                    LIKE '%alae%'
                  OR clean_text_interpretive_word 
                    LIKE '%milit%'
                  OR clean_text_interpretive_word 
                    LIKE '%eques%'
                  OR clean_text_interpretive_word 
                    LIKE '%equit%'
                  OR clean_text_interpretive_word 
                    LIKE '%duplicari%'
                  OR clean_text_interpretive_word 
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%centuri%'
                  or clean_text_interpretive_word
                    LIKE '%immun%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiar%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%corniculari%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%praefectus castrorum%'
                  or clean_text_interpretive_word
                    LIKE '%optio %'
                  or clean_text_interpretive_word
                    LIKE '%option%'
                  or clean_text_interpretive_word
                    LIKE '%stipendi%'
                  or status_notation
                    LIKE '%milites%'
                  OR findspot_ancient_clean = 'Tilurium'
                  OR findspot_ancient_clean = 'Burnum'
                  OR findspot_ancient_clean = 'Andetrium'
                  OR findspot_ancient_clean = 'Bigeste'
                  OR findspot_modern_clean = 'Ljubuški'
                  ")
load_military_terms_and_sites <- load_military_terms_and_sites %>%
  filter(!LIST.ID %in% c("468295",
                    "468613",
                    "468717",
                    "468832",
                    "469155",
                    "469186",
                    "469226",
                    "469341",
                    "469464",
                    "469506",
                    "469916",
                    "470136",
                    "470434",
                    "470543",
                    "470704",
                    "470864",
                    "471007",
                    "471036",
                    "471038",
                    "471230",
                    "471402",
                    "471414",
                    "471496",
                    "471645",
                    "471678",
                    "471682",
                    "471759",
                    "471777",
                    "471800",
                    "471809",
                    "471996",
                    "472452",
                    "472453",
                    "472586",
                    "472928",
                    "472984",
                    "473108",
                    "473129",
                    "473194",
                    "473217",
                    "473422",
                    "473564",
                    "473959",
                    "474003",
                    "474473",
                    "474478",
                    "474519",
                    "474682",
                    "474858",
                    "474992",
                    "475226",
                    "475259",
                    "475320",
                    "475377",
                    "475378",
                    "500263",
                    "106845",
                    "468356",
                    "474444"))
return(load_military_terms_and_sites)
}

#now run functions to make military corpus (undated, dated, key words and place, no places)
##add value for count
##Save data
LIRE_Dal_corpus <- load_military_terms(LIRE_Dal)
LIRE_Dal_corpus$count<- 1
write.csv(LIRE_Dal_corpus,
          file = "output_tables/corpus/undated/LIRE_corpus.csv")

LIRE_Dal_corpus_place_filtering <- load_military_terms_and_sites(LIRE_Dal)
LIRE_Dal_corpus_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_place_filtering,
          file = "output_tables/corpus/undated/LIRE_corpus_place_filter.csv")

LIRE_Dal_all_corpus <- load_military_terms(LIRE_Dal_all)
LIRE_Dal_all_corpus$count<- 1
write.csv(LIRE_Dal_all_corpus,
          file = "output_tables/corpus/undated/LIRE_corpus_all_types.csv")

LIRE_Dal_all_corpus_place_filtering <- load_military_terms_and_sites(LIRE_Dal_all)
LIRE_Dal_all_corpus_place_filtering$count<- 1
write.csv(LIRE_Dal_all_corpus_place_filtering,
          file = "output_tables/corpus/undated/LIRE_corpus_all_types_place_filter.csv")

LIRE_Dal_corpus_stela <- load_military_terms(LIRE_Dal_stela)
LIRE_Dal_corpus_stela$count<- 1
write.csv(LIRE_Dal_corpus_stela,
          file = "output_tables/corpus/undated/LIRE_corpus_stela.csv")

LIRE_Dal_corpus_stela_place_filtering <- load_military_terms_and_sites(LIRE_Dal_stela)
LIRE_Dal_corpus_stela_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_stela_place_filtering,
          file = "output_tables/corpus/undated/LIRE_corpus_stela_place_filter.csv")

LIRE_Dal_corpus_tabula <- load_military_terms(LIRE_Dal_tabula)
LIRE_Dal_corpus_tabula$count<- 1
write.csv(LIRE_Dal_corpus_tabula,
          file = "output_tables/corpus/undated/LIRE_corpus_tabula.csv")

LIRE_Dal_corpus_tabula_place_filtering <- load_military_terms_and_sites(LIRE_Dal_tabula)
LIRE_Dal_corpus_tabula_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_tabula_place_filtering,
          file = "output_tables/corpus/undated/LIRE_corpus_tabula_place_filter.csv")

LIRE_Dal_corpus_epitaph <- load_military_terms(LIRE_Dal_epitaph)
LIRE_Dal_corpus_epitaph$count<- 1
write.csv(LIRE_Dal_corpus_epitaph,
          file = "output_tables/corpus/undated/LIRE_corpus_epitaph.csv")

LIRE_Dal_corpus_epitaph_place_filtering <- load_military_terms_and_sites(LIRE_Dal_epitaph)
LIRE_Dal_corpus_epitaph_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_epitaph_place_filtering,
          file = "output_tables/corpus/undated/LIRE_corpus_epitaph_place_filter.csv")

LIRE_Dal_corpus_votive <- load_military_terms(LIRE_Dal_votive)
LIRE_Dal_corpus_votive$count<- 1
write.csv(LIRE_Dal_corpus_votive,
          file = "output_tables/corpus/undated/LIRE_corpus_votive.csv")

LIRE_Dal_corpus_votive_place_filtering <- load_military_terms_and_sites(LIRE_Dal_votive)
LIRE_Dal_corpus_votive_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_votive_place_filtering,
          file = "output_tables/corpus/undated/LIRE_corpus_votive_place_filter.csv")

LIRE_Dal_corpus_altar <- load_military_terms(LIRE_Dal_altar)
LIRE_Dal_corpus_altar$count<- 1
write.csv(LIRE_Dal_corpus_altar,
          file = "output_tables/corpus/undated/LIRE_corpus_altar.csv")

LIRE_Dal_corpus_altar_place_filtering <- load_military_terms_and_sites(LIRE_Dal_altar)
LIRE_Dal_corpus_altar_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_altar_place_filtering,
          file = "output_tables/corpus/undated/LIRE_corpus_altar_place_filter.csv")

##now dated
LIRE_Dal_corpus_dated <- load_military_terms(LIRE_Dal_dated)
LIRE_Dal_corpus_dated$count<- 1
write.csv(LIRE_Dal_corpus_dated,
          file = "output_tables/corpus/dated/LIRE_corpus_dated.csv")

LIRE_Dal_corpus_dated_place_filtering <- load_military_terms_and_sites(LIRE_Dal_dated)
LIRE_Dal_corpus_dated_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_dated_place_filtering,
          file = "output_tables/corpus/dated/LIRE_corpus_dated_place_filter.csv")

LIRE_Dal_all_corpus_dated <- load_military_terms(LIRE_Dal_all_dated)
LIRE_Dal_all_corpus_dated$count<- 1
write.csv(LIRE_Dal_all_corpus_dated,
          file = "output_tables/corpus/dated/LIRE_corpus_all_types_dated.csv")

LIRE_Dal_all_corpus_dated_place_filtering <- load_military_terms_and_sites(LIRE_Dal_all_dated)
LIRE_Dal_all_corpus_dated_place_filtering$count<- 1
write.csv(LIRE_Dal_all_corpus_dated_place_filtering,
          file = "output_tables/corpus/dated/LIRE_corpus_all_types_dated_place_filter.csv")

LIRE_Dal_corpus_stela_dated <- load_military_terms(LIRE_Dal_stela_dated)
LIRE_Dal_corpus_stela_dated$count<- 1
write.csv(LIRE_Dal_corpus_stela_dated,
          file = "output_tables/corpus/dated/LIRE_corpus_stela_dated.csv")

LIRE_Dal_corpus_stela_dated_place_filtering <- load_military_terms_and_sites(LIRE_Dal_stela_dated)
LIRE_Dal_corpus_stela_dated_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_stela_dated_place_filtering,
          file = "output_tables/corpus/dated/LIRE_corpus_stela_dated_place_filter.csv")

LIRE_Dal_corpus_tabula_dated <- load_military_terms(LIRE_Dal_tabula_dated)
LIRE_Dal_corpus_tabula_dated$count<- 1
write.csv(LIRE_Dal_corpus_tabula_dated,
          file = "output_tables/corpus/dated/LIRE_corpus_tabula_dated.csv")

LIRE_Dal_corpus_tabula_dated_place_filtering <- load_military_terms_and_sites(LIRE_Dal_tabula_dated)
LIRE_Dal_corpus_tabula_dated_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_tabula_dated_place_filtering,
          file = "output_tables/corpus/dated/LIRE_corpus_tabula_dated_place_filter.csv")

LIRE_Dal_corpus_epitaph_dated <- load_military_terms(LIRE_Dal_epitaph_dated)
LIRE_Dal_corpus_epitaph_dated$count<- 1
write.csv(LIRE_Dal_corpus_epitaph_dated,
          file = "output_tables/corpus/dated/LIRE_corpus_epitaph_dated.csv")

LIRE_Dal_corpus_epitaph_dated_place_filtering <- load_military_terms_and_sites(LIRE_Dal_epitaph_dated)
LIRE_Dal_corpus_epitaph_dated_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_epitaph_dated_place_filtering,
          file = "output_tables/corpus/dated/LIRE_corpus_epitaph_dated_place_filter.csv")

LIRE_Dal_corpus_votive_dated <- load_military_terms(LIRE_Dal_votive_dated)
LIRE_Dal_corpus_votive_dated$count<- 1
write.csv(LIRE_Dal_corpus_votive_dated,
          file = "output_tables/corpus/dated/LIRE_corpus_votive_dated.csv")

LIRE_Dal_corpus_votive_dated_place_filtering <- load_military_terms_and_sites(LIRE_Dal_votive_dated)
LIRE_Dal_corpus_votive_dated_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_votive_dated_place_filtering,
          file = "output_tables/corpus/dated/LIRE_corpus_votive_dated_place_filter.csv")

LIRE_Dal_corpus_altar_dated <- load_military_terms(LIRE_Dal_altar_dated)
LIRE_Dal_corpus_altar_dated$count<- 1
write.csv(LIRE_Dal_corpus_altar_dated,
          file = "output_tables/corpus/dated/LIRE_corpus_altar_dated.csv")

LIRE_Dal_corpus_altar_dated_place_filtering <- load_military_terms_and_sites(LIRE_Dal_altar_dated)
LIRE_Dal_corpus_altar_dated_place_filtering$count<- 1
write.csv(LIRE_Dal_corpus_altar_dated_place_filtering,
          file = "output_tables/corpus/dated/LIRE_corpus_altar_dated_place_filter.csv")

##filter clean dalmatia and military without Salona
##add value for count
##Save data

LIRE_Dal_no_salona <- LIRE_Dal %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_no_salona <- LIRE_Dal_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_no_salona$count<- 1
write.csv(LIRE_Dal_no_salona,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_no_salona.csv")

LIRE_Dal_all_no_salona <- LIRE_Dal_all %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_all_no_salona <- LIRE_Dal_all_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_all_no_salona$count<- 1
write.csv(LIRE_Dal_all_no_salona,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_all_types_no_salona.csv")

LIRE_Dal_dated_no_salona <- LIRE_Dal_dated %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_dated_no_salona <- LIRE_Dal_dated_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_dated_no_salona$count<- 1
write.csv(LIRE_Dal_dated_no_salona,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_dated_no_salona.csv")

LIRE_Dal_all_dated_no_salona <- LIRE_Dal_all_dated %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_all_dated_no_salona <- LIRE_Dal_all_dated_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_all_dated_no_salona$count<- 1
write.csv(LIRE_Dal_all_dated_no_salona,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_all_types_dated_no_salona.csv")

LIRE_Dal_stela_no_salona <- LIRE_Dal_stela %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_stela_no_salona <- LIRE_Dal_stela_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_stela_no_salona$count<- 1
write.csv(LIRE_Dal_stela_no_salona,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_stela_no_salona.csv")

LIRE_Dal_stela_dated_no_salona <- LIRE_Dal_stela_dated %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_stela_dated_no_salona <- LIRE_Dal_stela_dated_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_stela_dated_no_salona$count<- 1
write.csv(LIRE_Dal_stela_dated_no_salona,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_stela_dated_no_salona.csv")

LIRE_Dal_epitaph_no_salona <- LIRE_Dal_epitaph %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_epitaph_no_salona <- LIRE_Dal_epitaph_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_epitaph_no_salona$count<- 1
write.csv(LIRE_Dal_epitaph_no_salona,
          file = "output_tables/corpus/undated/LIRE_Dalmatia_epitaph_no_salona.csv")

LIRE_Dal_epitaph_dated_no_salona <- LIRE_Dal_epitaph_dated %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_epitaph_dated_no_salona <- LIRE_Dal_epitaph_dated_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_epitaph_dated_no_salona$count<- 1
write.csv(LIRE_Dal_epitaph_dated_no_salona,
          file = "output_tables/corpus/dated/LIRE_Dalmatia_epitaph_dated_no_salona.csv")

LIRE_Dal_all_corpus_no_salona <- LIRE_Dal_all_corpus %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_all_corpus_no_salona <- LIRE_Dal_all_corpus_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_all_corpus_no_salona$count<- 1
write.csv(LIRE_Dal_all_corpus_no_salona,
          file = "output_tables/corpus/undated/LIRE_corpus_all_types_no_salona.csv")

LIRE_Dal_corpus_no_salona <- LIRE_Dal_corpus %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_corpus_no_salona <- LIRE_Dal_corpus_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_corpus_no_salona$count<- 1
write.csv(LIRE_Dal_corpus_no_salona,
          file = "output_tables/corpus/undated/LIRE_corpus_no_salona.csv")

LIRE_Dal_all_corpus_dated_no_salona <- LIRE_Dal_all_corpus_dated %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_all_corpus_dated_no_salona <- LIRE_Dal_all_corpus_dated_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_all_corpus_dated_no_salona$count<- 1
write.csv(LIRE_Dal_all_corpus_dated_no_salona,
          file = "output_tables/corpus/dated/LIRE_corpus_all_types_dated_no_salona.csv")

LIRE_Dal_corpus_dated_no_salona <- LIRE_Dal_corpus_dated %>%
  filter(!findspot_ancient_clean %in% c("Salonae"))
LIRE_Dal_corpus_dated_no_salona <- LIRE_Dal_corpus_dated_no_salona %>%
  filter(!place %in% c("Solin / Salona"))
LIRE_Dal_corpus_dated_no_salona$count<- 1
write.csv(LIRE_Dal_corpus_dated_no_salona,
          file = "output_tables/corpus/dated/LIRE_corpus_dated_no_salona.csv")

# now count monument types
## save data
LIRE_Dal_types <- LIRE_Dal %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_types,
          file = "output_tables/corpus/undated/types/LIRE_Dalmatia_types.csv")

LIRE_Dal_monu_types <- LIRE_Dal %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_monu_types,
          file = "output_tables/corpus/undated/types/LIRE_Dalmatia_monu_types.csv")

LIRE_Dal_inscr_types <- LIRE_Dal %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_inscr_types,
          file = "output_tables/corpus/undated/types/LIRE_Dalmatia_inscr_types.csv")

LIRE_Dal_all_types <- LIRE_Dal_all %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_all_types,
          file = "output_tables/corpus/undated/types/LIRE_Dalmatia_all_types_types.csv")

LIRE_Dal_all_monu_types <- LIRE_Dal_all %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_all_monu_types,
          file = "output_tables/corpus/undated/types/LIRE_Dalmatia_all_types_monu_types.csv")

LIRE_Dal_all_inscr_types <- LIRE_Dal_all %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_all_inscr_types,
          file = "output_tables/corpus/undated/types/LIRE_Dalmatia_all_types_inscr_types.csv")

LIRE_Dal_dated_types <- LIRE_Dal_dated %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_dated_types,
          file = "output_tables/corpus/dated/types/LIRE_Dalmatia_dated_types.csv")

LIRE_Dal_dated_monu_types <- LIRE_Dal_dated %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_dated_monu_types,
          file = "output_tables/corpus/dated/types/LIRE_Dalmatia_dated_monu_types.csv")

LIRE_Dal_dated_inscr_types <- LIRE_Dal_dated %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_dated_inscr_types,
          file = "output_tables/corpus/dated/types/LIRE_Dalmatia_dated_inscr_types.csv")

LIRE_Dal_all_dated_types <- LIRE_Dal_all_dated %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_all_dated_types,
          file = "output_tables/corpus/dated/types/LIRE_Dalmatia_all_types_dated_types.csv")

LIRE_Dal_all_dated_monu_types <- LIRE_Dal_all_dated %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_all_dated_monu_types,
          file = "output_tables/corpus/dated/types/LIRE_Dalmatia_all_types_dated_monu_types.csv")

LIRE_Dal_all_dated_inscr_types <- LIRE_Dal_all_dated %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_all_dated_inscr_types,
          file = "output_tables/corpus/dated/types/LIRE_Dalmatia_all_types_dated_inscr_types.csv")

LIRE_Dal_corpus_types <- LIRE_Dal_corpus %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_corpus_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_types.csv")

LIRE_Dal_corpus_monu_types <- LIRE_Dal_corpus %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_corpus_monu_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_monu_types.csv")

LIRE_Dal_corpus_inscr_types <- LIRE_Dal_corpus %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_corpus_inscr_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_inscr_types.csv")

LIRE_Dal_all_corpus_types <- LIRE_Dal_all_corpus %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_all_corpus_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_all_types_types.csv")

LIRE_Dal_all_corpus_monu_types <- LIRE_Dal_all_corpus %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_all_corpus_monu_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_all_types_monu_types.csv")

LIRE_Dal_all_corpus_inscr_types <- LIRE_Dal_all_corpus %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_all_corpus_inscr_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_all_types_inscr_types.csv")

LIRE_Dal_corpus_place_filtering_types <- LIRE_Dal_corpus_place_filtering %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_corpus_place_filtering_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_place_filter_types.csv")

LIRE_Dal_corpus_place_filtering_monu_types <- LIRE_Dal_corpus_place_filtering %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_corpus_place_filtering_monu_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_place_filter_monu_types.csv")

LIRE_Dal_corpus_place_filtering_inscr_types <- LIRE_Dal_corpus_place_filtering %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_corpus_place_filtering_inscr_types,
          file = "output_tables/corpus/undated/types/LIRE_corpus_place_filter_inscr_types.csv")

LIRE_Dal_corpus_dated_types <- LIRE_Dal_corpus_dated %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_corpus_dated_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_dated_types.csv")

LIRE_Dal_corpus_dated_monu_types <- LIRE_Dal_corpus_dated %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_corpus_dated_monu_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_dated_monu_types.csv")

LIRE_Dal_corpus_dated_inscr_types <- LIRE_Dal_corpus_dated %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_corpus_dated_inscr_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_dated_inscr_types.csv")

LIRE_Dal_corpus_dated_place_filtering_types <- LIRE_Dal_corpus_dated_place_filtering %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_corpus_dated_place_filtering_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_dated_place_filter_types.csv")

LIRE_Dal_corpus_dated_place_filtering_monu_types <- LIRE_Dal_corpus_dated_place_filtering %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_corpus_dated_place_filtering_monu_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_dated_place_filter_monu_types.csv")

LIRE_Dal_corpus_dated_place_filtering_inscr_types <- LIRE_Dal_corpus_dated_place_filtering %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_corpus_dated_place_filtering_inscr_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_dated_place_filter_inscr_types.csv")

LIRE_Dal_all_corpus_dated_types <- LIRE_Dal_all_corpus_dated %>% 
  count(type_of_monument_clean, type_of_inscription_auto)
write.csv(LIRE_Dal_all_corpus_dated_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_all _types_dated_types.csv")

LIRE_Dal_all_corpus_dated_monu_types <- LIRE_Dal_all_corpus_dated %>% 
  count(type_of_monument_clean)
write.csv(LIRE_Dal_all_corpus_dated_monu_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_all _types_dated_monu_types.csv")

LIRE_Dal_all_corpus_dated_inscr_types <- LIRE_Dal_all_corpus_dated %>% 
  count(type_of_inscription_auto)
write.csv(LIRE_Dal_all_corpus_dated_inscr_types,
          file = "output_tables/corpus/dated/types/LIRE_corpus_all _types_dated_inscr_types.csv")