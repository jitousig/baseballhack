library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)

df <- readRDS("filtered.RDS")

ui <- fluidPage(titlePanel("2016 Pitch Explorer"),
                sidebarLayout(sidebarPanel( radioButtons("balls", "Balls:",
                                                         c("0" = 0,
                                                           "1" = 1,
                                                           "2" = 2,
                                                           "3" = 3)
                                                         ),
                                            radioButtons("strikes", "Strikes:",
                                                         c("0" = 0,
                                                           "1" = 1,
                                                           "2" = 2)
                                                         ),
                                            checkboxGroupButtons(
                                              "onbase", label = "Select runners on base:", 
                                              choices = c("First" = 1 , "Second" = 2, "Third" = 3), 
                                              justified = TRUE, status = "primary",
                                              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                              ),
                                            switchInput("pitcher_handedness", label = "Pitcher Handedness", onLabel = "Left", offLabel = "Right", value = FALSE),
                                            switchInput("batter_handedness", label = "Batter Handedness", onLabel = "Left", offLabel = "Right", value = FALSE)),
                              mainPanel(plotlyOutput("coolplot")) 
                
                ))



server <- function(input, output) {
  filtered <- reactive({
    df %>% filter(balls == input$balls, strikes == input$strikes)
  })
  
  output$coolplot <- renderPlotly({
    plot_ly(filtered(), type = "histogram", x = ~pitchTypeDescription)
  })
}


shinyApp(ui, server)