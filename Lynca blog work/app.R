library(shiny)
library(leaflet)
library(RColorBrewer)
#load packages
library(tidyverse)
library(shiny)
library(dygraphs)
library(xts)
library(shinythemes)

#import data
covid_worldwide_obs <- readRDS("data/covid_worldwide_7_leaflet.Rds")


library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


#restricting the analysis to a choice of continet: 
continent_choices <- unique(covid_worldwide_obs$continent)

my_palette <- colorNumeric ( palette = "YIGnBu", 
                             domain = covid_worldwide_obs$people_vaccinated, 
                             alpha = 0.8)

ui <- fluidPage (
title="Comparative Analysis of Covid-19 Statistics and Wealth Indicators Across Different Countries.",
tabPanel (
  title = "Choose one indicator of a country's wealth.",
  sidebarLayout(
    sidebarPanel(

    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel( "Map Summary", 
        withSpinner(leafletOutput("mymap")), 
        p(), #adds a line break for aesthetic reasons.
        actionButton("recalc", "New points")),
        
        tabPanel ( "PlotSummary", 
                   withSpinner(plotOutput(outputId = "linegraph"))
                   
                   
          
        )
        
      )
    )
  )
)
)




server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet(data = covid_worldwide_obs) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolygons (
        fillColor = ~my_palette(covid_worldwide_obs$people_vaccinated),
        color = "#b2aeae", 
        stroke = FALSE, 
        popup = ~ glue:: glue("Country_name: {str_to_title(name)} <br>")) %>%
      addMarkers ( data = covid_worldwide_obs, lat = ~latitude, lng = ~longitude)
  })
}

shinyApp(ui, server)