library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(tidyr)

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
                                  tabPanel("Fastballs", plotlyOutput("fastballs"), verbatimTextOutput("fastballEV"), plotlyOutput("fastballruns")),
                                  tabPanel("Breaking Balls", plotlyOutput("breakingballs"), verbatimTextOutput("breakingEV"), plotlyOutput("breakingruns")),
                                  tabPanel("Changeups", plotlyOutput("changeups"), verbatimTextOutput("changeupEV"), plotlyOutput("changeupruns"))
                                ))))


server <- function(input, output) {
  filtered <- reactive({
    df %>% filter(startingBalls == input$balls, startingStrikes == input$strikes, startingOuts == input$outs, pitcherThrowHand == ifelse(input$pitcher_handedness, "L", "R"), hitterBatHand == ifelse(input$batter_handedness, "L", "R"), runner_on_first == ifelse(1 %in% input$onbase, TRUE, FALSE), runner_on_second == ifelse(2 %in% input$onbase, TRUE, FALSE), runner_on_third == ifelse(3 %in% input$onbase, TRUE, FALSE))
    })
  df_fastballs <- reactive({
    df %>% filter(pitchCategory == "Fastball", startingBalls == input$balls, startingStrikes == input$strikes, startingOuts == input$outs, pitcherThrowHand == ifelse(input$pitcher_handedness, "L", "R"), hitterBatHand == ifelse(input$batter_handedness, "L", "R"), runner_on_first == ifelse(1 %in% input$onbase, TRUE, FALSE), runner_on_second == ifelse(2 %in% input$onbase, TRUE, FALSE), runner_on_third == ifelse(3 %in% input$onbase, TRUE, FALSE))
  })
  df_breakingballs <- reactive({
    df %>% filter(pitchCategory == "Breaking Ball", startingBalls == input$balls, startingStrikes == input$strikes, startingOuts == input$outs, pitcherThrowHand == ifelse(input$pitcher_handedness, "L", "R"), hitterBatHand == ifelse(input$batter_handedness, "L", "R"), runner_on_first == ifelse(1 %in% input$onbase, TRUE, FALSE), runner_on_second == ifelse(2 %in% input$onbase, TRUE, FALSE), runner_on_third == ifelse(3 %in% input$onbase, TRUE, FALSE))
  })
  df_changeup <- reactive({
    df %>% filter(pitchCategory == "Changeup", startingBalls == input$balls, startingStrikes == input$strikes, startingOuts == input$outs, pitcherThrowHand == ifelse(input$pitcher_handedness, "L", "R"), hitterBatHand == ifelse(input$batter_handedness, "L", "R"), runner_on_first == ifelse(1 %in% input$onbase, TRUE, FALSE), runner_on_second == ifelse(2 %in% input$onbase, TRUE, FALSE), runner_on_third == ifelse(3 %in% input$onbase, TRUE, FALSE))
  })
  summary_summary <- reactive({filtered() %>% count(pitchCategory, is_ball) %>% spread(is_ball,n) %>% mutate(s = Ball + Strike) %>% mutate(totalsum = sum(s), prop_ball = Ball/totalsum, prop_strike = Strike/totalsum)})
  
  summary_fastballs <- reactive({df_fastballs() %>% count(is_ball, atBatOutcome) %>% spread(atBatOutcome,n) %>% mutate(s = Out + `Not out`) %>% mutate(totalsum = sum(s), prop_out = Out/totalsum, prop_not_out = `Not out`/totalsum)})

  summary_breakingballs <- reactive({df_breakingballs() %>% count(is_ball, atBatOutcome) %>% spread(atBatOutcome,n) %>% mutate(s = Out + `Not out`) %>% mutate(totalsum = sum(s), prop_out = Out/totalsum, prop_not_out = `Not out`/totalsum)})
  
  summary_changeup <- reactive({df_changeup() %>% count(is_ball, atBatOutcome) %>% spread(atBatOutcome,n) %>% mutate(s = Out + `Not out`) %>% mutate(totalsum = sum(s), prop_out = Out/totalsum, prop_not_out = `Not out`/totalsum)})
  
  df_fastball_joel <- reactive({df_fastballs() %>% count(total_runs_to_come) %>% ungroup %>% mutate(totalrows = sum(n)) %>%
    mutate(Probability = n/totalrows)})
  
  df_breaking_joel <- reactive({df_breakingballs() %>% count(total_runs_to_come) %>% ungroup %>% mutate(totalrows = sum(n)) %>%
      mutate(Probability = n/totalrows)})
  
  df_changeup_joel <- reactive({df_changeup() %>% count(total_runs_to_come) %>% ungroup %>% mutate(totalrows = sum(n)) %>%
      mutate(Probability = n/totalrows)})
  
  expected_runs_fastball  <- reactive({df_fastball_joel() %>% mutate(expectedruns = total_runs_to_come*Probability) %>%
    summarize(mean(expectedruns)) %>% as.numeric()})
  
  expected_runs_breaking  <- reactive({df_breaking_joel() %>% mutate(expectedruns = total_runs_to_come*Probability) %>%
      summarize(mean(expectedruns)) %>% as.numeric()})
  
  expected_runs_changeup  <- reactive({df_changeup_joel() %>% mutate(expectedruns = total_runs_to_come*Probability) %>%
      summarize(mean(expectedruns)) %>% as.numeric()})
  
  output$coolplot <- renderPlotly({
    plot_ly(summary_summary(), type = "bar", x = ~pitchCategory, y = ~prop_ball, name = "Ball") %>% add_trace(y = ~prop_strike, name = "Strike") %>% layout(xaxis = list(title = "Type of Pitch"), yaxis = list(title = "Proportion of Total Pitches", range = c(0,1)), barmode = 'stack')
  })
  
  output$fastballs <- renderPlotly({
    plot_ly(summary_fastballs(), type = "bar", x = ~is_ball, y = ~prop_out, name = "Out") %>% add_trace(y = ~prop_not_out, name = "Not Out") %>% layout(xaxis = list(title = "Outcome of Pitch"), yaxis = list(title = "Proportion of Total Pitches", range = c(0, 1)), barmode = 'stack')
  })
  
  output$breakingballs <- renderPlotly({
    plot_ly(summary_breakingballs(), type = "bar", x = ~is_ball, y = ~prop_out, name = "Out") %>% add_trace(y = ~prop_not_out, name = "Not Out") %>% layout(xaxis = list(title = "Outcome of Pitch"), yaxis = list(title = "Proportion of Total Pitches", range = c(0, 1)), barmode = 'stack')
  })
  
  output$changeups <- renderPlotly({
    plot_ly(summary_changeup(), type = "bar", x = ~is_ball, y = ~prop_out, name = "Out") %>% add_trace(y = ~prop_not_out, name = "Not Out") %>% layout(xaxis = list(title = "Outcome of Pitch"), yaxis = list(title = "Proportion of Total Pitches", range = c(0, 1)), barmode = 'stack')
  })
  
  output$fastballruns <- renderPlotly({plot_ly(
    df_fastball_joel(),
    x= ~total_runs_to_come,
    y = ~Probability,
    name = "Distribution of expected runs to come",
    type = "bar"
  )})
  
  output$breakingruns <- renderPlotly({plot_ly(
    df_breaking_joel(),
    x= ~total_runs_to_come,
    y = ~Probability,
    name = "Distribution of expected runs to come",
    type = "bar"
  )})
  
  output$changeupruns <- renderPlotly({plot_ly(
    df_changeup_joel(),
    x= ~total_runs_to_come,
    y = ~Probability,
    name = "Distribution of expected runs to come",
    type = "bar"
  )})
  
  output$fastballEV <- renderText({ 
    paste(expected_runs_fastball(), " more runs are expected to score in this inning")})
  
  output$breakingEV <- renderText({ 
    paste(expected_runs_breaking(), " more runs are expected to score in this inning")})
  
  output$changeupEV <- renderText({ 
    paste(expected_runs_changeup(), " more runs are expected to score in this inning")})
}


shinyApp(ui, server)