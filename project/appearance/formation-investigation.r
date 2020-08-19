### let's try to figure out what the most common formations are in general

source('c:/git/lineup/new-model-startup.r')
source('project/appearance/biased-urn-funct.R')
library(doParallel)

fakeMaxTime = 1000L

# need to sort out seasonNumber. why do we need 1516 in seasonInfoDF?
seasonInfoDF$seasonNumber = match(seasonInfoDF$season, with(seasonInfoDF, sort(season[havegbg])))
resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')

numBlockWithinSeason = 4
resultDF$inBlock = as.integer(with(resultDF, (seasonNumber - 1) * numBlockWithinSeason +
                                     cut(teamgamenumber, br = seq(0.5, 38.5, le = 5), labels = FALSE)))

gbgdf = lazy_left_join(gbgdf,
                       resultDF,
                       c('season', 'team', 'teamgamenumber'),
                       'inBlock')

gbgdf2 = gbgdf %>%
  filter(!grepl('(injury|suspension)', startTime)) %>%
  mutate(startTime2 = ifelse(!startTime %in% c('U', 'UU'), startTime, as.character(fakeMaxTime)),
         endTime2 = ifelse(!startTime %in% c('U', 'UU'), endTime, as.character(fakeMaxTime))) %>%
  mutate(startTime2 = as.integer(startTime2),
         endTime2 = as.integer(endTime2))
# except we've got the problem that players sometimes officially start the match at eg 97 minutes.
# let's ceiling the endTime to be the greater of 94 and the biggest startTime of the match

gbgdf2$mainpos2 = with(gbgdf2, case_when(
  mainpos %in% c('D', 'DMC') ~ 'def',
  mainpos == 'M' ~ 'mid',
  mainpos %in% c('AM', 'FW') ~ 'att',
  TRUE ~ 'other'
))

gbgdf2 = gbgdf2 %>%
  select(season, seasonNumber, teamgamenumber, team, player, startTime2, endTime2, minute, played, available, mainpos2, inBlock)

# now, we will want to run it e.g 4 times a season, on every occasion predicting the next quarter of the season
# i think that's the level we would want it at

# but in fact, predicting isn't trivial, that's a job unto itself
# because you need to provide estimate of how many att/mids etc will play
# so let's not do the downweight just yet, let's try to predict minutes for a team for a block of the season

splitTeamDF = gbgdf2 %>%
  group_by(season, team, teamgamenumber) %>%
  do(SplitGame(.$player, .$startTime2, .$endTime2)) %>%
  lazy_left_join(gbgdf2,
                  c('season', 'team', 'teamgamenumber', 'player'),
                 'mainpos2') %>%
  ungroup()

splitTeamDF = splitTeamDF %>%
  lazy_left_join(resultDF,
                 c('season', 'team', 'teamgamenumber'),
                 'maxEndTime')

sumInPosByMatch = splitTeamDF %>%
  group_by(season, team, teamgamenumber, mainpos2) %>%
  summarise(sumInPos = sum(played * minDiff/maxEndTime))

# is that quite what we want? i'd say not, i think we want the actual formation along with a proportion

vertFormationByMatchSection = splitTeamDF %>%
  group_by(season, team, teamgamenumber, mainpos2, matchSection) %>%
  summarise(sumInPos = sum(played),
            matchProportion = minDiff[1] / maxEndTime[1])

# ok, now we spread that to get the formation
formationByMatchSection = vertFormationByMatchSection %>%
  spread(key = mainpos2, value = sumInPos)

# ok, awesome, now what we want is the frequency of each formation
formationCount = formationByMatchSection %>%
  group_by(att, def, mid, other) %>%
  summarise(sumMatchProportion = sum(matchProportion))

# so by default how about we continue to do what we've done, but if we have the problem of an impossible formation given the available players, we instead predict for the highest weighted common formation?
