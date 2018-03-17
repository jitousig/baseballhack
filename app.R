library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)

df <- readRDS("pitches.rds")

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
                                            switchInput("batter_handedness", label = "Batter Handedness", onLabel = "Left", offLabel = "Right", value = FALSE),
                                            knobInput("run_difference", label = "Fielding team leading by:", min = -15, max = 15, value = 0)
                                            ),
                              mainPanel(plotlyOutput("coolplot"), br(), br(), verbatimTextOutput("base")) 
                
                ))



server <- function(input, output) {
  filtered <- reactive({
    pitches %>% filter(balls == input$balls, strikes == input$strikes, pitcherThrowHand == ifelse(input$pitcher_handedness, "L", "R"), hitterBatHand == ifelse(input$batter_handedness, "L", "R"))
  })
  
  output$coolplot <- renderPlotly({
    plot_ly(filtered(), type = "histogram", x = ~pitchCategory)
  })
  
  output$base <- renderPrint({ input$onbase })
}


shinyApp(ui, server)