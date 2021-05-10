library(tidyverse)
library(robotstxt)
library(rvest)
library(knitr)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(maps)
library(viridis)
library(RColorBrewer)

# read in csv
threatened_species <- read.csv("threatened.csv")
red_list_cats <- read_csv("iucn_categories.csv")

##### THREATENED SPECIES AND COUNTRIES #####
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

threatened_world <- threatened_iucn %>%
  full_join(world_map, by = c("Country" = "region")) 

#############################

# define choice values and labels for user inputs #
map_choice_names <- c("Extinct", "Extinct in the Wild", 
                      "Critically Endangered", 
                      "Endangered", "Vulnerable",
                      "Near Threatened", "Least Concern")

##### ui #####
ui <- navbarPage(
  
  title="Global Distribution of Threatened Species",
  
  tabPanel(
    title = "World Map",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "concern_lvl"
                           , label = "Choose the level(s) of concern to plot:"
                           , choices = map_choice_names
                           , selected = c("Endangered", "Vulnerable"))
      ),
      mainPanel(
        plotOutput(outputId = "map"),
      )
    )
  )
)

##### server #####
server <- function(input,output,session){
  # map of species distribution by country and category
  map_data <- reactive({
    data <- filter(threatened_world, Category %in% input$concern_lvl)
  })
  
  output$map <- renderPlot({
    ggplot(data = map_data(), aes(x = long, y = lat, group = group)) +
      geom_polygon(mapping = aes(x = long, y = lat, fill = num_species), 
                   colour = "white") +
      theme_void()  +
      coord_fixed(ratio = 1.3) +
      scale_fill_viridis(option = "magma", direction = -1, na.value = "gray") 
  })
}

##### call to Shinyapp #####
shinyApp(ui = ui, server = server)
  