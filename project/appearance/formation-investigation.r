### let's try to figure out what the most common formations are in general

splitTeamDF = gbgdf2 %>%
  group_by(season, team, teamgamenumber) %>%
  do(SplitGame(.$player, .$startTime2, .$endTime2)) %>%
  lazy_left_join(gbgdf2,
                  c('season', 'team', 'teamgamenumber', 'player'),
                 'mainpos2')

splitTeamDF = splitTeamDF %>%
  lazy_left_join(resultDF,
                 c('season', 'team', 'teamgamenumber'),
                 'maxEndTime')

sumInPosByMatch = splitTeamDF %>%
  group_by(season, team, teamgamenumber, mainpos2) %>%
  summarise(sumInPos = sum(minDiff)/sum(maxEndTime))
## no, can't figure this out, come back to it later
