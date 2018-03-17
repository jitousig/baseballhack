#Drop certain Pitch outcomes
pitches<-readRDS("filtered1.RDS")

pitches<-pitches[!(pitches$outcomeDescription == 'Balk' | pitches$outcomeDescription == 'Catcher Interference' | pitches$outcomeDescription == 'Intentional Ball'),]

#Adding column 'ball' with TRUE or FALSE entries
pitches = mutate(pitches, is_ball = outcomeDescription %in% c('Ball','Dirt Ball','Hit by pitch', 'Pitch Out'))

#pitches %>% select(outcomeDescription, ball) 