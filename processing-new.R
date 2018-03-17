library(dplyr)

pitches<-readRDS("filtered1.RDS")

#Drop certain Pitch outcomes
pitches<-pitches[!(pitches$outcomeDescription == 'Balk' | pitches$outcomeDescription == 'Catcher Interference' | pitches$outcomeDescription == 'Intentional Ball'),]

#Drop certain Pitch Types
pitches<-pitches[!(pitches$pitchTypeDescription == 'Intentional Ball' | pitches$pitchTypeDescription == 'Knuckleball'),]

#Adding column 'is_ball' with TRUE or FALSE entries
pitches = mutate(pitches, is_ball = ifelse(outcomeDescription %in% c('Ball','Dirt Ball','Hit by pitch', 'Pitch Out'), "Ball","Strike")

#Adding column 'pitchCategory'
pitches = mutate(pitches, pitchCategory = ifelse(pitchTypeDescription %in% c('Fastball','Cutter','Sinker', 'Splitter'),'Fastball',ifelse(pitchTypeDescription %in% c('Changeup'),'Changeup','Breaking Ball')))

pitches = mutate(pitches, runner_on_first = !is.na(rob1_start))
pitches = mutate(pitches, runner_on_second = !is.na(rob2_start))
pitches = mutate(pitches, runner_on_third = !is.na(rob3_start))

df_outcome <- pitches %>% filter(pitches$is_ab_over == 1)
df_outcome$atBatOutcome <- ifelse(df_outcome$outs - df_outcome$startingOuts == 1, "Out", "Not out")
df_outcome <- df_outcome %>% select(gameId, inningNumber, inningHalf, inningHalfEventSequenceNumber, atBatOutcome)

pitches2 <- left_join(pitches, df_outcome, by = c('gameId','inningNumber','inningHalf','inningHalfEventSequenceNumber'))

saveRDS(pitches2, "pitches.rds")

