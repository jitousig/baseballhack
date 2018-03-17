#Drop certain Pitch outcomes
filtered2<-pitches[!(pitches$outcomeDescription == 'Balk' | pitches$outcomeDescription == 'Catcher Interference' | pitches$outcomeDescription == 'Intentional Ball'),]

#Drop certain Pitches
filtered2<-filtered2[!(filtered2$pitchTypeDescription == 'Intentional Ball' | filtered2$pitchTypeDescription == 'Knuckleball'),]

#Adding column 'ball' with TRUE or FALSE entries
filtered2 = mutate(filtered2, ball = outcomeDescription %in% c('Ball','Dirt Ball','Hit by pitch', 'Pitch Out'))

#Crosscheck below  
#filtered2 %>% select(outcomeDescription, ball) 

#Adding column 'fastball' with TRUE or FALSE entries
filtered2 = mutate(filtered2, pitchCategory = ifelse(pitchTypeDescription %in% c('Fastball','Cutter','Sinker', 'Splitter'),'Fastball',ifelse(pitchTypeDescription %in% c('Changeup'),'Changeup','Breaking Ball')))

#Crosscheck below                                             
#filtered2 %>% select(pitchTypeDescription, pitchCategory) 
