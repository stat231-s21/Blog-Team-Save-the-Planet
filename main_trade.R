#Trade of Species
#Name: Sanjana Sunder

# wordcloud for trade by imported and exported countries
# display of data table of exporting countries  
# references:
# https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html

#Adding Libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(leaflet) # for interactive maps
library(bslib)
library(rgdal)
library(sqldf)
# for networks
library(ggnetwork)
library(igraph)

data <- read.csv("./trade.csv")

############
#    ui    #
############
ui <- fluidPage(
  
  title = "Species Biodiversity",
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
    ) # main panel
) # UI

############
# server   #
############
server <- function(input, output){
  
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
} 
  ####################
  # call to shinyApp #
  ####################
  shinyApp(ui = ui, server = server)
  
  