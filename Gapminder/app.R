library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)


gapminder <- readRDS("gapminder.rds")
source("auxfunctions.R")

ui <- fluidPage(theme = shinytheme("slate"),
    

    titlePanel("Gapminder visualizer"),
    
    fluidRow(
        column(12,
               plotOutput("gapminderPlot", click="plotClick"),
               tableOutput("graphData"),
               verbatimTextOutput("clickData")
        )
    ),
    fluidRow(
        column(6,
               sliderInput("year",
                           "Select year",
                           min = min(gapminder$year),
                           max = max(gapminder$year),
                           value = min(gapminder$year),
                           sep="", # Hide thousands , separator
                           step=1, # Select every year
                           animate = animationOptions(interval = 1250)
               )
        ),
        column(6,
               checkboxGroupInput("continent",
                                  "Select continents",
                                  choices = levels(gapminder$continent),
                                  selected = levels(gapminder$continent))
        )
    )
)


server <- function(input, output) {
    
    activeCountry <- reactiveVal()
    
    historicData <- reactive({
        gapminder %>% 
            filter(year <= input$year) %>% 
            filter(country == activeCountry()) # activeCountry is reactive, its value is accessed using activeCountry()
    })
    
    observeEvent(input$plotClick, 
                 {
                     nearCountry <- nearPoints(plotData(), input$plotClick, maxpoints = 1)
                     activeCountry(as.character(nearCountry$country)) # Extract just the country name and assign it to activeCountry()
                 })
    
    plotData <- reactive({
        gapminder %>% 
            filter(year == input$year) %>% 
            filter(continent %in% input$continent)
    })
    
    output$graphData <- renderTable({
        plotData() %>% 
            getRichestCountry()
    })
    
    output$gapminderPlot <- renderPlot({
        if (length(activeCountry()) == 0) { 
            plotData() %>% 
                produceGapminderPlot() 
        } else {
            plotData() %>% 
                produceGapminderPlot(makeSemiTransparent = TRUE) %>% 
                addHistoricPlot(historicData())
        }
    })
    
    output$clickData <- renderPrint(({
        activeCountry()
    }))
}

shinyApp(ui = ui, server = server)
