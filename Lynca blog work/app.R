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



#gathering some observations: 
poverty_indicator_names <- c("Population Density (persons per km²)", "Extreme Poverty ($per day)", "Diabetes Prevalence")
poverty_indicator_values <- c("population_density", "extreme_poverty", "diabetes_prevalence")

names(poverty_indicator_values) <- poverty_indicator_names

ui <- fluidPage (
title="Comparative Analysis of Covid-19 Statistics and Wealth Indicators Across Different Countries.",
tabPanel (
  title = "Choose one indicator of a country's wealth.",
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "id_continent_choices"
                     , label = "Identify which continents you want to focus your analysis on:"
                     , choices = continent_choices
                     , selected = NULL
                     , multiple = FALSE),
    
    selectInput ( inputId = "poverty_Indicator", 
                  label = "Choose an indicator of poverty", 
                  choices = poverty_indicator_values, 
                  selected = NULL)
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

  
  #selecting the right data to display, and making it reactive to the user's input.
  
server <- function(input, output, session) {

  mydata <- reactive (
    {
      return (filter(covid_worldwide_obs, continent %in% input$id_continent_choices))
    }
  )
  
  points <- eventReactive(input$recalc, {
   # cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    
    my_palette <- colorNumeric(
      palette = "YlGnBu", 
      domain = mydata()[[input$poverty_Indicator]]
    )
    
    leaflet(data = mydata()) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                      options = providerTileOptions(noWrap = TRUE)
      ) %>%
      
     addPolygons (
       fillColor = ~my_palette(mydata()[[input$poverty_Indicator]]),
        color = "#b2aeae", 
        stroke = FALSE, 
        weight = 1, 
        fillOpacity = 0.7, 
        popup = ~ glue:: glue("Country_name: {str_to_title(name)} <br>")) %>%
      
    leaflet::addLegend(pal = my_palette 
                       ,values = ~mydata()[[input$poverty_Indicator]] 
                       ,opacity = 0.7
                       ,position = 'bottomright' 
                       ,title = poverty_indicator_names[poverty_indicator_values == input$poverty_Indicator])
      
      #addMarkers ( data = covid_worldwide_obs, lat = ~latitude, lng = ~longitude)
  })
}

#print(mydata()[[input$poverty_Indicator]])

#server <- function(input, output, session) {
  
 # points <- eventReactive(input$recalc, {
   
 # }, ignoreNULL = FALSE)
  
  #output$mymap <- renderLeaflet({
    
    #covid_worldwide_obs_transformed <- st_transform(covid_worldwide_obs, 4326)
    
   # my_palette <- colorNumeric(
    #  palette = "YlGnBu", 
    #  domain = covid_worldwide_obs_transformed[[input$poverty_Indicator]]
   # )
    
   # leaflet(data = covid_worldwide_obs_transformed) %>%
    #  addProviderTiles(providers$Stamen.TonerLite,
     #                  options = providerTileOptions(noWrap = TRUE)
   #   ) %>%
   #   addPolygons (
    #    fillColor = ~my_palette(covid_worldwide_obs_transformed[[input$poverty_Indicator]]),
    #    color = "#b2aeae", 
    #    stroke = FALSE, 
    #    popup = ~ glue:: glue("Country_name: {str_to_title(name)} <br>") %>%
    # addLegend(pal = my_palette,
    #          values = covid_worldwide_obs_transformed[[input$poverty_Indicator]],
     #         position = "bottomright",
     #         title = poverty_indicator_names[poverty_indicator_values == input$poverty_Indicator]))
      #addMarkers ( data = covid_worldwide_obs, lat = ~latitude, lng = ~longitude)
 # })
#}

shinyApp(ui, server)