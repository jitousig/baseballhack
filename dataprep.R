library(bigrquery)
library(data.table)
library(dplyr)
library(plotly)
library(shiny)
library(readr)

datapath = "C:/Users/985076660/Downloads/"
data0path = paste0(datapath,"data000000000000")
data1path = paste0(datapath,"data000000000001")

data0 = read_csv(data0path)
data1 = read_csv(data1path)

alldata<-rbind(data0,data1)

filtered <- select(alldata,durationMinutes,             
                   awayTeamName,                 
                   homeTeamName,                 
                   venueOutfieldDistances,        
                   homeFinalRuns,     
                   homeFinalHits,
                   homeFinalErrors,              
                   awayFinalRuns,                           
                   homeFinalRunsForInning,       
                   awayFinalRunsForInning,        
                   inningNumber,                 
                   inningHalf,                                 
                   inningHalfEventSequenceNumber,                  
                   atBatEventType,                                                       
                   outcomeDescription,                    
                   hitterLastName,                
                   hitterFirstName,                              
                   hitterBatHand,                
                   pitcherFirstName,              
                   pitcherLastName,              
                   pitcherThrowHand,                               
                   pitchTypeDescription,          
                   pitchSpeed,                   
                   pitchZone,                     
                   pitcherPitchCount,            
                   hitterPitchCount,                       
                   hitType,
                   startingBalls,                
                   startingStrikes,               
                   startingOuts,                 
                   balls,                         
                   strikes,                      
                   outs,                         
                   rob0_start,                   
                   rob1_start,
                   rob2_start,                   
                   rob3_start,                                          
                   is_wild_pitch,                
                   is_passed_ball,                
                   homeCurrentTotalRuns,         
                   awayCurrentTotalRuns,          
                   awayFielder1,                 
                   lineupPosition,               
                   lineupOrder 
) %>%
  filter(atBatEventType == 'PITCH')
