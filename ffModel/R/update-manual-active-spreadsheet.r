UpdateManualActiveSpreadsheet = function(gbgdf, playerDF, seasonInfoDF, resultDF) {
  
  lookBack = 5
  
  seasonInfoDF = left_join(seasonInfoDF, gbgdf %>% distinct(season, seasonNumber), 'season')
  resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')
  allCurrentTeam = resultDF %>% filter(season == currentseason) %>% distinct(team)
  currentSeasonNumber = with(seasonInfoDF, seasonNumber[which(season == currentseason)])
  allTimeTeamNumGame = resultDF %>%
    semi_join(allCurrentTeam, 'team') %>%
    lazy_left_join(seasonInfoDF, 'season', 'seasonNumber') %>%
    filter(seasonNumber >= currentSeasonNumber - 1) %>%
    group_by(team) %>%
    arrange(date) %>%
    mutate(allTimeTeamGameNumber = 1:n()) %>%
    ungroup()
  
  totalTeamNumGame = allTimeTeamNumGame %>%
    group_by(team) %>%
    summarise(maxTeamNumGame = max(allTimeTeamGameNumber))
  
  subgbgdf = gbgdf %>%
    lazy_left_join(seasonInfoDF, 'season', 'seasonNumber') %>%
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
    semi_join(playerDF %>% filter(!hasleft), c('team', 'player')) %>%
    select(team, player, gameBack, gameInfo) %>%
    mutate(gameBack = paste0('b', gameBack)) %>%
    spread(key = gameBack, value = gameInfo) %>%
    lazy_left_join(mainposByPlayer, c('team', 'player'), 'mainpos') %>%
    lazy_left_join(playerDF, c('team', 'player'), 'eMin') %>%
    arrange(team, match(mainpos, c('GK', 'D', 'DMC', 'M', 'AM', 'FW')), desc(eMin)) %>%
    mutate(eMin = round(eMin, 1)) %>%
    # mutate(manualEMin = round(eMin)) %>% only do that when initialising the thing
    mutate(manualEMin = NA_real_) %>%
    select(team, player, mainpos, manualEMin, eMin, everything())
  
  ## but then we want the mean played/mean available prior to that
  totalGameForTeamByPlayer = gbgdf %>%
    group_by(team, player) %>%
    summarise(totalGameForTeamByPlayer = max(gameForTeamNumber))
  gbgdf = left_join(gbgdf, totalGameForTeamByPlayer, c('team', 'player'))
  gbgdf = indicate_overlapping_combination(gbgdf,
                                           playerDF %>% filter(!hasleft),
                                           c('team', 'player'),
                                           'isWithCurrentTeam')
  gbgdf$isInPriorSample = with(gbgdf, gameForTeamNumber > totalGameForTeamByPlayer - 10 - lookBack &
                                 gameForTeamNumber <= totalGameForTeamByPlayer - lookBack &
                                 isWithCurrentTeam)
  
  priorInfo = gbgdf %>%
    filter(isInPriorSample) %>%
    group_by(player, team) %>%
    summarise(priorMeanPlayed = round(94 * sum(minute/94)/sum(available), 1),
              priorSumAvailable = sum(available))
  
  horizSubGbgDF = left_join(horizSubGbgDF,
                            priorInfo,
                            c('team', 'player'))
  
  ## but can't be arsed right now, let's just write that out
  # also need to do the admin bit, reading in previous file, writing to latest update dir.
  # but will copy that acrosss from existing code. let's just get the actual correct minutes for now
  # why is hause for aston villa NA?
  
  ## let's store by season and gameweek number. think it's quite rare you'd want to update not at the end of a gameweek
  
  currentGameweek = with(resultDF, max(gameweek[season == currentseason]))
  activeFile = paste0(DATAPATH, 'active_player/active-player-', currentseason, '-', currentGameweek, '.csv')
  previousActiveFile = paste0(DATAPATH, 'active_player/active-player-', currentseason, '-', currentGameweek - 1, '.csv')
  # let's insert the previous week's manual values
  
  prevHorizSubGbgDF = read_csv(previousActiveFile,
                               col_types = list(
                                 team = col_character(),
                                 player = col_character(),
                                 mainpos = col_character(),
                                 manualEMin = col_double(),
                                 eMin = col_double(),
                                 b0 = col_character(),
                                 b1 = col_character(),
                                 b2 = col_character(),
                                 b3 = col_character(),
                                 b4 = col_character(),
                                 b5 = col_character()
                               ))
  horizSubGbgDF = join_on_overlap(horizSubGbgDF,
                                  prevHorizSubGbgDF %>%
                                    select(team, player, manualEMin),
                                  c('team', 'player')) %>%
                    select(-isOverlapping)
  
  # final step, indicate which players we don't have to bother checking
  # ie if we have 85 mins for an outfield player and they played 94 mins, obviously we should still say 85 mins
  # unfortunately we've created a bastard of a column to actually use, so join previous manualEMin to laste game data separately
  previousManualEMinPlusLatestGameDF = left_join(prevHorizSubGbgDF %>%
                                                   select(team, player, mainpos, manualEMin),
                                                 gbgdf %>%
                                                   filter(season == currentseason &
                                                            totalGameForTeamByPlayer == gameForTeamNumber) %>%
                                                   select(player, team, minute),
                                                 c('player', 'team')) %>%
                                        mutate(ignore = case_when(
                                          mainpos == 'GK' & near(manualEMin, 90) & near(minute, 94) ~ '*',
                                          mainpos == 'GK' & manualEMin < 50.1 & minute < 50 ~ '*',
                                          mainpos != 'GK' & near(manualEMin, 85) & minute > 85 ~ '*',
                                          mainpos != 'GK' & manualEMin < 50.1 & minute < 50 ~ '*',
                                          TRUE ~ ''))
  
  # awesome, now join that column in
  horizSubGbgDF = lazy_left_join(horizSubGbgDF,
                                 previousManualEMinPlusLatestGameDF,
                                 c('team', 'player'),
                                 'ignore') %>%
                  select(team, player, mainpos, ignore, everything())

  # also, players who've been injured for the entire set of games, they can be ignored
  allInjured = horizSubGbgDF %>%
    select(team, player, matches('^b[0-5]+$')) %>%
    gather(lookback, appData, - c(team, player)) %>% group_by(team, player) %>%
    summarise(allInjured = all(!is.na(appData) & appData == 'injury'))
  horizSubGbgDF$ignore[with(horizSubGbgDF, paste(team, player)) %in%
                         with(allInjured, paste(team, player)[allInjured])] = '*'
  # actually do we just want to extend this to anyone who was injured in the most recent game...?
  
  write_csv(x= horizSubGbgDF, path = activeFile)
  message('Have created file ', activeFile)
}

ReadManualEMinFile = function(playerDF, resultDF) {
  currentGameweek = with(resultDF, max(gameweek[season == currentseason]))
  activeFile = paste0(DATAPATH, 'active_player/active-player-', currentseason, '-', currentGameweek, '.csv')

  activePlayerDF = read_csv(activeFile,
                               col_types = list(
                                 team = col_character(),
                                 player = col_character(),
                                 mainpos = col_character(),
                                 manualEMin = col_double(),
                                 eMin = col_double(),
                                 b0 = col_character(),
                                 b1 = col_character(),
                                 b2 = col_character(),
                                 b3 = col_character(),
                                 b4 = col_character(),
                                 b5 = col_character()
                               ))
  
  playerDF = lazy_left_join(playerDF, activePlayerDF, c('team', 'player'), 'manualEMin')
  
  playerDF = within(playerDF, rm(eMin))
  playerDF = playerDF %>%
    rename(eMin = manualEMin)
  
  return(playerDF)
}
