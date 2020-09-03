
source('c:/git/lineup/new-model-startup.r')
source('project/appearance/biased urn approach/biased-urn-funct.R')
#library(doParallel)
#registerDoParallel()

fakeMaxTime = 1000L

# need to sort out seasonNumber. why do we need 1516 in seasonInfoDF?
seasonInfoDF$seasonNumber = match(seasonInfoDF$season, with(seasonInfoDF, sort(season[havegbg])))
resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')
resultDF = resultDF %>%
  mutate(alltimetgn = (seasonNumber - 1) * 38 + teamgamenumber)

gbgdf = lazy_left_join(gbgdf,
                       resultDF,
                       c('season', 'team', 'teamgamenumber'),
                       c('alltimetgn'))

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

#gbgdf2 = gbgdf2 %>%
#  select(season, seasonNumber, teamgamenumber, alltimetgn, team, player, startTime2, endTime2, minute, played, available, mainpos2, #inBlock)
gbgdf2 = gbgdf2 %>%
  select(alltimetgn, team, player, startTime2, endTime2, minute, played, available, mainpos2)


historicFormationDF = MakeHistoricFormationDF(gbgdf2)

unTeamMainposTgn = gbgdf2 %>%
  distinct(team, mainpos2, alltimetgn) %>%
  group_by(team, mainpos2) %>%
  arrange(alltimetgn) %>%
  mutate(alltimetgnDelta = alltimetgn - lag(alltimetgn, 1),
         isValid = !is.na(alltimetgnDelta) & alltimetgnDelta == 1) %>%
  filter(mainpos2 %in% c('def', 'mid', 'att') & isValid) %>%
  select(team, mainpos2, alltimetgn) %>%
  ungroup()
unTeamTgn = unTeamMainposTgn %>%
  distinct(team, alltimetgn)

allTeamTgnObservedNumGame = gbgdf2 %>%
  lazy_left_join(resultDF, c('alltimetgn', 'team'), 'maxEndTime') %>%
  group_by(team, alltimetgn, player) %>%
  summarise(observedNumGame = sum( (endTime2 - startTime2) / maxEndTime))
