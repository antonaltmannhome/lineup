### sticking plaster, let's just setup the spreadsheet again, but with a little more info than before
### spec: show previous 5? games. then the the mean played minutes in the ten prior games in another column
### by default show the prvious value by me
### display U/I/S etc if not playing
### for DFs, show CS
### for everyone, highlight goal/assist

## so if maguire scores and gets CS, his entry will be 94 G,CS

source('c:/git/lineup/new-model-startup.r')
playerDF = ffDataJoining:::MatchFFPlayerData(playerDF)

lookBack = 5

seasoninfo = left_join(seasoninfo, gbgdf %>% distinct(season, seasonNumber), 'season')
resultdf = lazy_left_join(resultdf, seasoninfo, 'season', 'seasonNumber')
allCurrentTeam = resultdf %>% filter(season == currentseason) %>% distinct(team)
currentSeasonNumber = with(seasoninfo, seasonNumber[which(season == currentseason)])
allTimeTeamNumGame = resultdf %>%
  semi_join(allCurrentTeam, 'team') %>%
  lazy_left_join(seasoninfo, 'season', 'seasonNumber') %>%
  filter(seasonNumber >= currentSeasonNumber - 1) %>%
  group_by(team) %>%
  arrange(date) %>%
  mutate(allTimeTeamGameNumber = 1:n()) %>%
  ungroup()

totalTeamNumGame = allTimeTeamNumGame %>%
  group_by(team) %>%
  summarise(maxTeamNumGame = max(allTimeTeamGameNumber))

subgbgdf = gbgdf %>%
  lazy_left_join(seasoninfo, 'season', 'seasonNumber') %>%
  filter(seasonNumber >= currentSeasonNumber - 1) %>%
  semi_join(allCurrentTeam, 'team') %>%
  lazy_left_join(allTimeTeamNumGame, c('date', 'team'), 'allTimeTeamGameNumber') %>%
  left_join(totalTeamNumGame, 'team') %>%
  filter(allTimeTeamGameNumber >= maxTeamNumGame - lookBack) %>%
  group_by(team, player) %>%
  mutate(gameBack = maxTeamNumGame - allTimeTeamGameNumber) %>%  
  mutate(appearanceInfo = ifelse(played, paste0('(', startTime, ', ', endTime, ')'), startTime),
          goalInfo = ifelse(goal > 0, paste0('G', goal), ''),
         assistInfo = ifelse(assist > 0, paste0('A', assist), ''),
         csInfo = ifelse(mainpos %in% c('GK', 'D') & teamconceded == 0, 'CS', '')) %>%
  ungroup()

subgbgdf = subgbgdf %>%
  mutate(gameInfo = appearanceInfo) %>%
  mutate_cond(goalInfo != '', gameInfo = paste(gameInfo, goalInfo, sep = ', ')) %>%
  mutate_cond(assistInfo != '', gameInfo = paste(gameInfo, assistInfo, sep = ', ')) %>%
  mutate_cond(csInfo != '', gameInfo = paste(gameInfo, csInfo, sep = ', '))

horizSubGbgDF = subgbgdf %>%
  select(team, player, mainpos, gameBack, gameInfo) %>%
  spread(key = gameBack, value = gameInfo) %>%
  arrange(team, match(mainpos, c('GK', 'D', 'DMC', 'M', 'AM', 'FW')))

## but then we want the mean played/mean available prior to that
## but can't be arsed right now, let's just write that out
