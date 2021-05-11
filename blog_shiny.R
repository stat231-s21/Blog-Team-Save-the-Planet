##Work on this Shiny App was divided by tab, with 
##Sanjana contributing the Species Trade tab (Tab 2),
##Kim contributing the Threatened Species tab (Tab 1), and
##Evan contributing the Conservation Funding tab (Tab 3)

#Adding Libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(robotstxt)
library(rvest)
library(knitr)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(leaflet) # for interactive maps
library(shinythemes) # for a theme
library(wordcloud)
library(shinyWidgets)
library(bslib)

#Importing Data
conservation <- read_csv("conservation_final.csv")
threatened_iucn <- read.csv("threatenediucn_final.csv")
data <- read.csv("./trade.csv")

#Create Log Options for Conservation Funding
log_ops <- c("Yes", "No")

#Create Threatened Species Map Options
map_choice_names <- c("Extinct", "Extinct in the Wild", 
                      "Critically Endangered", 
                      "Endangered", "Vulnerable",
                      "Near Threatened", "Least Concern")

# Define UI for application
ui <- navbarPage(
  
  title = "Species Biodiversity",
  theme = ("united"),
  
  #TAB 1: Threatened Species
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
        plotOutput(outputId = "map")
      )
    ),
    #Sets background for entire app, but ensures that when the 
    #app opens it opens to this tab and not just a blank screen
    setBackgroundColor(
      color = c("#329932", "#4ca64c"),
      gradient = "linear",
      direction = "bottom"
    )
  ),
  
  #TAB 2: Species Trade
  tabPanel(
    title = "Species Trade",
    theme = ("united"),
    mainPanel(
      h5("Trade of Species between countries"),
      plotOutput(outputId = "net"),
      br(),
      h5("Count of Species exported by the countries\n\n"),
      leafletOutput(outputId = "chloro"),
      h6("Hover and click on a country for details on Taxonomy \n\n"),
      br(),
      br(),
      h5("Taxonomy of Appendix I species exported and Count of Species by Appendix exported"),
      fluidRow(
        splitLayout(cellWidths = c("65%", "35%"),
                    DT::dataTableOutput(outputId = "table"),
                    plotOutput(outputId = "time"))
      )
    )
  ),
  
  #TAB 3: Conservation Funding
    tabPanel(
      title = "Conservation Funding",
      
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "log"
                       , label = "Take the log of total funding?"
                       , choices = log_ops
                       , selected = "No")
        ),
        mainPanel(
          plotOutput("totalPlot")
        )
      )
    )
  )
##### UI END #####

###### Server #######

# Define server logic
server <- function(input, output) {
  
  #TAB 1: Threatened Species Details
  # map of species distribution by country and category
  world_map <- map_data(map = "world"
                        , region = ".")
  map_data <- reactive({
    data <- filter(threatened_iucn, Category %in% input$concern_lvl) %>%
      full_join(world_map, by = c("Country" = "region"))
  })
  
  output$map <- renderPlot({
    ggplot(data = map_data(), aes(x = long, y = lat, group = group)) +
      geom_polygon(mapping = aes(x = long, y = lat, fill = num_species), 
                   colour = "black") +
      theme_void()  +
      coord_fixed(ratio = 1.3) +
      scale_fill_viridis(option = "magma", direction = -1, na.value = "white") +
      labs(title = "Global Species Diversity by IUCN Threat Level"
           , fill = "Number of Species*"
           , caption = "*Regions in white have no data")
  })
  
  #TAB 2:  Trade Species Details
  output$net <- renderPlot ({
    # Render the Network of trade between countries
    trade_coun <- data %>%
      select(importer_country, exporter_country) %>%
      group_by(importer_country, exporter_country) %>%
      summarize(trade_occurrence = n(), .groups = 'drop') %>%
      filter(!(is.na(importer_country) | importer_country == "") &
               !(is.na(exporter_country) | exporter_country == "") ) %>%
      filter(trade_occurrence >= 200) %>%
      arrange(desc(trade_occurrence))
    
    net <- graph_from_data_frame(trade_coun, directed = TRUE)
    net_plot <- ggnetwork(net)
    ggplot(data = net_plot
           , aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(curvature = 0.1, arrow=arrow(type="closed", length=unit(8,"pt"))
                 , aes(color = trade_occurrence)) +
      geom_nodes() +
      geom_nodelabel(aes(label = name)) +
      theme_blank() +
      labs(color = "Total Occurrence") +
      scale_color_continuous(type = "viridis")
  }) # RENDER NETWORK PLOT
  
  output$chloro <- renderLeaflet ({
    # data vector to extract the total species exported by each country
    exp_country <- data %>%
      select(exporter_country, exporter) %>%
      group_by(exporter, exporter_country) %>%
      summarize(count = n(), .groups = 'drop') %>%
      filter(!(is.na(exporter_country) | exporter_country == "")) %>%
      filter(str_detect(exporter_country, "[a-z]")) 
    
    # Read this shape file with the rgdal library. 
    world_spdf <- readOGR( 
      dsn = paste0(getwd(),"") , 
      layer = "TM_WORLD_BORDERS_SIMPL-0.3",
      verbose = FALSE
    ) 
    # join data frame with shape file
    data_chloro <- world_spdf@data %>%
      full_join(exp_country, by = c("ISO2" = "exporter"))
    
    mypalette <- colorBin( palette = "YlOrBr", domain = data_chloro$count
                           , na.color = "transparent", bins = 7)
    mytext <- paste(
      "Country: ", data_chloro$exporter_country,"<br/>", 
      "Count of Species Exported: ", data_chloro$count, "<br/>", 
      "Category: ", "Appendix I", 
      sep = "") %>%
      lapply(htmltools::HTML)
    
    # Basic choropleth with leaflet?
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addLegend( pal = mypalette, values = ~data_chloro$count
                 , opacity=0.9, title = "Count"
                 , position = "bottomleft" ) %>%
      addPolygons(
        fillColor = ~mypalette(data_chloro$count), 
        layerId = ~data_chloro$exporter_country,
        stroke = FALSE, 
        smoothFactor = 0.5,
        fillOpacity = 0.9, 
        color = "white", 
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"))
  }) #Render Chloropeth
  
  observe({
    #click on a country 
    p <- input$chloro_shape_click
    if(is.null(p))
      return()
    
    data_03 <- data %>% # to retrieve the count of species exported by appendix
      filter(exporter_country == p$id) %>%
      na.omit() 
    
    data_03_A <- data_03 %>%
      group_by(app, year) %>%
      summarise(count = n(), .groups = "drop") %>%
      select(app, count, year)
    
    data_03_B <- data_03 %>%
      filter(app == "I") %>%
      group_by(taxon, term) %>%
      select(taxon, term) %>%
      distinct() # eliminate duplicate rows
    
    # vector to render bar plot
    output$time <- renderPlot({
      ggplot(data = data_03_A, aes(x = app, y = count)) +
        geom_bar(stat = "Identity", aes(fill = year)) + 
        labs(title = paste0(" \nby ", p$id)) +
        xlab("Appendix") + 
        ylab("Number of Species")
    }, height = 250, width = 300)
    
    # vector to render table
    output$table <- DT::renderDataTable({
      DT::datatable(data_03_B
                    , selection = 'single'
                    , style = 'bootstrap'
                    , class = 'table-bordered'
                    , caption = tags$caption(
                      style = "caption-side: top; text-align: left; margin: 10px 0; color: black",
                      paste0("by ", p$id)))
      
    }, height = 250, width = 350) # END Render Table
  })
  
  #TAB 2:  Trade Species Details - END
  
  #TAB 3: Conservation Funding
  #Making a map of Conservation Funding by Country
  output$totalPlot <- renderPlot({
    #Giving the user the option to do a log transform
    if(input$log == "Yes"){
      ggplot(conservation, aes(x = long, y = lat, group = group, 
                               fill = log_funding)) +
        geom_polygon(color = "black") +
        theme_void() +
        coord_fixed(ratio = 1.3) +
        labs(title = "Conservation Funding By Country*"
             , subtitle = "as of 2013"
             , fill = ""
             , caption = "*Regions in white have no data") +
        #scale_fill_continuous(low="thistle2", high="darkred",                      guide="colorbar", na.value="white")
        scale_fill_viridis(option = "viridis", direction = -1, na.value="white")
    }
    else {
      ggplot(conservation, aes(x = long, y = lat, group = group, 
                               fill = total)) +
        geom_polygon(color = "black") +
        theme_void() +
        coord_fixed(ratio = 1.3) +
        labs(title = "Log(Conservation Funding) By Country*"
             , subtitle = "as of 2013"
             , fill = ""
             , caption = "*Regions in white have no data") +
        #scale_fill_continuous(low="thistle2", high="darkred",                      guide="colorbar", na.value="white")
        scale_fill_viridis(option = "plasma", direction = -1, na.value="white")
    }
  })
  #TAB 3: Conservation Funding - END
}  

#### SERVER END ####


#### call to shinyApp ####

# Run the application 
shinyApp(ui = ui, server = server)

