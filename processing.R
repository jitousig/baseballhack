#Drop certain Pitch outcomes
filtered2<-pitches[!(pitches$outcomeDescription == 'Balk' | pitches$outcomeDescription == 'Catcher Interference' | pitches$outcomeDescription == 'Intentional Ball'),]

#Adding column 'ball' with TRUE or FALSE entries
filtered2 = mutate(filtered2, ball = outcomeDescription %in% c('Ball','Dirt Ball','Hit by pitch', 'Pitch Out'))

filtered2 %>% select(outcomeDescription, ball) 