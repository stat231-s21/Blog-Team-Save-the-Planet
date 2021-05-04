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

###### CITY BIODIVERSITY ######
city_names <- c("Boston", "Chicago", "Detroit", "District of Columbia", 
                "Minneapolis", "New York", "Philadelphia", "Saint Louis")
cities_final <- cities %>%
  pivot_longer(city_names, names_to = "City", values_to = "Species") %>%
  pivot_wider(names_from = "Type", values_from = "Species")

###### CONSERVATION FUNDING ######
cons_selected <- conservation %>%
  clean_names() %>%
  select(country, total_aid_funding, total_domestic_funding, 
         trust_funds_and_debt_swaps, other) %>%
  mutate(other = case_when(is.na(other) == TRUE ~ 0, TRUE ~ other)) %>%
  filter(is.na(total_domestic_funding) == FALSE) %>%
  mutate(total = total_aid_funding + total_domestic_funding
         + trust_funds_and_debt_swaps + other)

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

        
