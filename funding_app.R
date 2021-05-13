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
library(viridis)

#Importing Data
conservation <- read_csv("conservation_final.csv")

#Create Transformation Options
log_ops <- c("Yes", "No")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Conservation Funding"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "log"
                   , label = "Take the log of total funding?"
                   , choices = log_ops
                   , selected = "No")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("totalPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$totalPlot <- renderPlot({
    
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
        labs(title = "Conservation Funding By Country*"
             , subtitle = "as of 2013"
             , fill = ""
             , caption = "*Regions in white have no data") +
        #scale_fill_continuous(low="thistle2", high="darkred",                      guide="colorbar", na.value="white")
        scale_fill_viridis(option = "plasma", direction = -1, na.value="white")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
