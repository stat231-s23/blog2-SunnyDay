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

my_palette <- colorNumeric ( palette = "YIGnBu", 
                             domain = covid_worldwide_obs$people_vaccinated)

ui <- fluidPage(
  leafletOutput("mymap"),
  p(), #adds a line break for aesthetic reasons.
  actionButton("recalc", "New points")
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
        fillColor = ~my_palette(people_vaccinated), 
        stroke = FALSE, 
        popup = ~ glue:: glue("Country_name: {str_to_title(name)} <br>"))
  })
}

shinyApp(ui, server)