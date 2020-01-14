### sticking plaster, let's just setup the spreadsheet again, but with a little more info than before
### spec: show previous 5? games. then the the mean played minutes in the ten prior games in another column
### by default show the prvious value by me
### display U/I/S etc if not playing
### for DFs, show CS
### for everyone, highlight goal/assist

## so if maguire scores and gets CS, his entry will be 94 G,CS

source('c:/git/lineup/new-model-startup.r')
playerDF = ffModel:::CalculateUpToDatePlayerSmooth(gbgdf)

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

mainposByPlayer = subgbgdf %>%
  group_by(team, player) %>%
  count(mainpos) %>%
  arrange(desc(n)) %>%
  slice(1)

horizSubGbgDF = subgbgdf %>%
  select(team, player, gameBack, gameInfo) %>%
  spread(key = gameBack, value = gameInfo) %>%
  lazy_left_join(mainposByPlayer, c('team', 'player'), 'mainpos') %>%
  arrange(team, match(mainpos, c('GK', 'D', 'DMC', 'M', 'AM', 'FW'))) %>%
  lazy_left_join(playerDF, c('team', 'player'), 'eMin') %>%
  mutate(eMin = round(eMin, 1)) %>%
  mutate(manualEMin = round(eMin)) %>%
  select(team, player, mainpos, manualEMin, eMin, everything())

## but then we want the mean played/mean available prior to that
## but can't be arsed right now, let's just write that out
# also need to do the admin bit, reading in previous file, writing to latest update dir.
# but will copy that acrosss from existing code. let's just get the actual correct minutes for now
# why is hause for aston villa NA?

write.csv(file = paste0(DATAPATH, 'active-player.csv'), horizSubGbgDF, row.names = FALSE)
