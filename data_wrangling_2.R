#Access of csv's
#IUCN Red List Dataset:
#https://www.dropbox.com/s/t48dd7m0o088w69/LPI_pops_restricted_20160421.csv?dl=0
#National Parks Dataset:
#https://www.kaggle.com/gsdeepakkumar/biodiversity-an-eda/data?select=species.csvgbif.org/dataset/67c54f85-7910-4cbf-8de4-6f0b136a0e34
#Wildlife Trade Dataset:
#https://www.kaggle.com/cites/cites-wildlife-trade-database
#Accessed March 2021

#City Biodiversity Dataset:
#http://www.urbanhabitats.org/v01n01/speciesdiversity_full.html
#Conservation Funding Dataset:
#https://datadryad.org/stash/dataset/doi:10.5061/dryad.p69t1
#Accessed April 2021

#Load Libraries
library(tidyverse)
library(robotstxt)
library(rvest)
library(knitr)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)

#Read in csv's
cities <- read_csv("city_totals.csv")
conservation <- read_csv("conservation_funding.csv")
data <- read.csv("./trade.csv")
threatened_species <- read.csv("threatened.csv")
red_list_cats <- read_csv("iucn_categories.csv")

###### CITY BIODIVERSITY ######
city_names <- c("Boston", "Chicago", "Detroit", "District of Columbia", 
                "Minneapolis", "New York", "Philadelphia", "Saint Louis")
cities_final <- cities %>%
  pivot_longer(city_names, names_to = "City", values_to = "Species") %>%
  pivot_wider(names_from = "Type", values_from = "Species") %>%
  pivot_longer(c("Native", "Introduced"), names_to = "Origin", values_to = "Species")
write_csv(x = cities_final, file = "./cities_final.csv")

###### CONSERVATION FUNDING ######
cons_selected <- conservation %>%
  clean_names() %>%
  select(country, total_aid_funding, total_domestic_funding, 
         trust_funds_and_debt_swaps, other) %>%
  mutate(other = case_when(is.na(other) == TRUE ~ 0, TRUE ~ other)) %>%
  filter(is.na(total_domestic_funding) == FALSE) %>%
  mutate(total = total_aid_funding + total_domestic_funding
         + trust_funds_and_debt_swaps + other) %>%
  rename(region = country) %>%
  mutate(region = case_when(region == "United States" ~ "USA", 
                            TRUE ~ as.character(region))) %>%
  mutate(region = case_when(region == "Myanmar (Burma)" ~ "Myanmar", 
                            TRUE ~ as.character(region))) %>%
  mutate(region = case_when(region == "Tanzania, United Republic of" ~ "Tanzania", 
                            TRUE ~ as.character(region))) %>%
  mutate(region = case_when(region == "United Kingdom" ~ "UK", 
                            TRUE ~ as.character(region))) %>%
  mutate(region = case_when(region == "Congo" ~ "Republic of Congo", 
                            TRUE ~ as.character(region))) %>%
  mutate(log_funding = log(total)) %>%
  mutate(sqrt_funding = sqrt(total))
world_map <- map_data(map = "world"
                      , region = ".")
funding_map <- cons_selected %>%
  right_join(world_map, by = "region")
write_csv(x = funding_map, file = "./conservation_final.csv")

####### Count of Species exported by each country #########
exp_country <- data %>%
               select(exporter_country, exporter) %>%
               group_by(exporter, exporter_country) %>%
               summarize(exported_qty = n(), .groups = 'drop') %>%
               filter(!(is.na(exporter_country) | exporter_country == "")) %>%
               filter(str_detect(exporter_country, "[a-z]")) 

######## Species traded between countries ############
trade_coun <- data %>%
              select(importer_country, exporter_country) %>%
              group_by(importer_country, exporter_country) %>%
              summarize(trade_occurrence = n(), .groups = 'drop') %>%
              filter(!(is.na(importer_country) | importer_country == "") &
                     !(is.na(exporter_country) | exporter_country == "") ) %>%
              filter(trade_occurrence >= 200) %>%
              arrange(desc(trade_occurrence))

##### THREATENED SPECIES AND COUNTRY #####
threatened_country <- threatened_species %>%
  select(Class, Red_list_category, Country) %>%
  filter(!(Red_list_category %in% c("DD", "NE"))) %>%
  group_by(Red_list_category, Country) %>%
  summarise(num_species = n())

# renaming countries to join with world map
setdiff(threatened_country$Country, world_map$region)

threatened_country_clean <- threatened_country %>%
  mutate(Country = as.character(Country)
         , Country = case_when(Country == "Bonaire, Saint Eustatius And Saba" ~ "Bonaire"
                               , Country == "Tanzania, United Republic Of" ~ "Tanzania"
                               , Country == "Viet Nam" ~ "Vietnam"
                               , Country == "Congo, The Democratic Republic Of The" ~ "Democratic Republic of the Congo"
                               , Country == "Gibraltar" ~ "Spain"
                               , Country == "Iran, Islamic Republic Of" ~ "Iran"
                               , Country == "Taiwan, Province Of China" ~ "Taiwan"
                               , Country == "Bolivia, Plurinational State Of" ~ "Bolivia"
                               , Country == "Falkland Islands (Malvinas)" ~ "Falkland Islands"
                               , Country == "Macedonia, The Former Yugoslav Republic Of" ~ "Macedonia"
                               , Country == "Cura̤ao" ~ "Curacao"
                               , Country == "Antigua And Barbuda" ~ "Antigua"
                               , Country == "Antigua And Barbuda" ~ "Barbuda"
                               , Country == "Korea, Republic Of" ~ "North Korea"
                               , Country == "Korea, Republic Of" ~ "South Korea"
                               , Country == "Trinidad and Tobago" ~ "Tobago"
                               , Country == "Trinidad and Tobago" ~ "Trinidad"
                               , Country == "Saint Helena, Ascension And Tristan Da Cunha" ~ "Saint Helena"
                               , Country == "United Kingdom" ~ "UK"
                               , Country == "Venezuela, Bolivarian Republic Of" ~ "Venezuela"
                               , Country == "Virgin Islands, U.S." ~ "Virgin Islands"
                               , Country == "C̫te D'Ivoire" ~ "Ivory Coast"
                               , Country == "Hong Kong" ~ "China"
                               , Country == "Lao People's Democratic Republic" ~ "Laos"
                               , Country == "Syrian Arab Republic" ~ "Syria"
                               , Country == "Heard Island And McDonald Islands" ~ "Heard Island"
                               , Country == "Saint Kitts And Nevis" ~ "Saint Kitts"
                               , Country == "Virgin Islands, British" ~ "Virgin Islands"
                               , Country == "United States" ~ "USA"
                               , Country == "Russian Federation" ~ "Russia"
                               , TRUE ~ Country))

# changing red list category name to join
threatened_country_clean$Red_list_category <- gsub("LR.*", "LC", 
                                                   threatened_country_clean$Red_list_category)

# joining the names of red list categories
threatened_iucn <- threatened_country_clean %>%
  left_join(red_list_cats, by = "Red_list_category") %>%
  drop_na()

# join with world map
world_map <- map_data(map = "world"
                      , region = ".")