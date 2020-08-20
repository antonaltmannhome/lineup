## this file should do everything wrt the biased urn stuff

### let's try to figure out what the most common formations are in general

source('c:/git/lineup/new-model-startup.r')
source('project/appearance/biased-urn-funct.R')
library(doParallel)
registerDoParallel()

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

historicFormationDF = MakeHistoricFormationDF(gbgdf2)



unTeamMainposBlock = gbgdf2 %>%
  distinct(team, mainpos2, inBlock) %>%
  group_by(team, mainpos2) %>%
  arrange(inBlock) %>%
  mutate(blockDelta = inBlock - lag(inBlock, 1),
         isValid = !is.na(blockDelta) & blockDelta == 1) %>%
  filter(mainpos2 %in% c('def', 'mid', 'att') & isValid) %>%
  select(team, mainpos2, inBlock) %>%
  ungroup()

allTeamSeasonMainposEstimateDF = foreach (tspi=1:nrow(unTeamMainposBlock),
                                          .combine = rbind,
                                          .packages=c("dplyr")) %dopar% {
                                            with(unTeamMainposBlock[tspi,],
                                                 GetPlayerAppearanceMleByBlock(team, mainpos2, inBlock, 4))
                                          }

unTeamBlock = unTeamMainposBlock %>%
  distinct(team, inBlock)

allTeamBlockFormationDF = foreach (tspi=1:nrow(unTeamBlock),
                                   .combine = rbind,
                                   .packages=c("dplyr")) %dopar% {
                                     with(unTeamBlock[tspi,],
                                          GetFormationProbabilityByTeamBlock(team, inBlock, 4, historicFormationDF))
                                   }

allTeamBlockExpectedNumGame = foreach (tspi=1:nrow(unTeamBlock),
                                       .combine = rbind,
                                       .packages=c("dplyr", "tidyr")) %dopar% {
                                         with(unTeamBlock[tspi,],
                                              GetExpectedNumGameByTeamBlock(team, inBlock, 4,
                                                      allTeamSeasonMainposEstimateDF, allTeamBlockFormationDF))
                                       }
