library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("St Thomas' Physiology Data Console"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("DatesMerge",
                  "Dates:",
                  min = as.Date("2006-01-01","%Y-%m-%d"),
                  max = as.Date("2016-12-01","%Y-%m-%d"),
                  value=as.Date("2016-12-01"),timeFormat="%Y-%m-%d")
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      tabsetPanel(
        tabPanel("Breath Tests",plotOutput("distPlotLactul")),
      )
    )
  ))
)

source("S:\\Usage.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlotLactul <- renderPlot({
    #Create the data
    DatesMerge<-input$DatesMerge
    
    # draw the histogram with the specified number of bins
    
    ggplot(TotsLactul)+
      geom_bar(aes(DatesMerge,fill=year))+
      labs(title=paste("Num")) +
      xlab("Time") +
      ylab("NumP") +
      theme(axis.text.x=element_text(angle=-90)) +
      theme(legend.position="top")+
      theme(axis.text=element_text(size=6))
    
    
  })
  
  
})
