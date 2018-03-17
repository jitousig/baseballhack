pitches<-readRDS("filtered1.RDS")

#Drop certain Pitch outcomes
pitches<-pitches[!(pitches$outcomeDescription == 'Balk' | pitches$outcomeDescription == 'Catcher Interference' | pitches$outcomeDescription == 'Intentional Ball'),]

#Drop certain Pitch Types
pitches<-pitches[!(pitches$pitchTypeDescription == 'Intentional Ball' | pitches$pitchTypeDescription == 'Knuckleball'),]

#Adding column 'ball' with TRUE or FALSE entries
pitches = mutate(pitches, is_ball = outcomeDescription %in% c('Ball','Dirt Ball','Hit by pitch', 'Pitch Out'))

#Adding column 'pitchCategory'
pitches = mutate(pitches, pitchCategory = ifelse(pitchTypeDescription %in% c('Fastball','Cutter','Sinker', 'Splitter'),'Fastball',ifelse(pitchTypeDescription %in% c('Changeup'),'Changeup','Breaking Ball')))

saveRDS(pitches, "pitches.rds")
