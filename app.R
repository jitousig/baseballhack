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
                                                           "3" = 3), inline = TRUE
                                                         ),
                                            radioButtons("strikes", "Strikes:",
                                                         c("0" = 0,
                                                           "1" = 1,
                                                           "2" = 2), inline = TRUE
                                                         ),
                                            radioButtons("outs", "Outs in Inning:",
                                                         c("0" = 0,
                                                           "1" = 1,
                                                           "2" = 2), inline = TRUE
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
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Summary", plotlyOutput("coolplot")),
                                  tabPanel("Fastballs", plotlyOutput("fastballs"))
                                ))))


server <- function(input, output) {
  filtered <- reactive({
    pitches %>% filter(startingBalls == input$balls, startingStrikes == input$strikes, startingOuts == input$outs, pitcherThrowHand == ifelse(input$pitcher_handedness, "L", "R"), hitterBatHand == ifelse(input$batter_handedness, "L", "R"))
  })
  df_fastballs <- reactive({
    pitches %>% filter(pitchCategory == "Fastball", startingBalls == input$balls, startingStrikes == input$strikes, startingOuts == input$outs, pitcherThrowHand == ifelse(input$pitcher_handedness, "L", "R"), hitterBatHand == ifelse(input$batter_handedness, "L", "R"))
  })
  
  output$coolplot <- renderPlotly({
    plot_ly(filtered(), type = "histogram", x = ~pitchCategory)
  })
  
  output$fastballs <- renderPlotly({
    plot_ly(df_fastballs(), type = "histogram", x = ~is_ball)
  })
  
  output$base <- renderPrint({ input$onbase })
}


shinyApp(ui, server)