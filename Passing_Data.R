
library(StatsBombR)
library(tidyverse)
 library(SBpitch)


comp <- FreeCompetitions()
head(comp)


comp<- FreeCompetitions() %>% filter(competition_id == 37 , season_name == "2019/2020")
  head(comp)

Matches <- FreeMatches(comp)  

StatsBombData<-StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)

StatsBombData = allclean(StatsBombData)

d1<-StatsBombData%>%
  filter(match_id == 2275096, type.name == "Pass", team.name == "Arsenal WFC")

create_Pitch()+ geom_point(data = d1, aes(x = location.x, y = location.y))+
  geom_segment(data = d1, aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y))


d1<-StatsBombData%>%
  filter(match_id == 2275096, type.name == "Pass", team.name == "Arsenal WFC", player.name == "Leah Williamson")
