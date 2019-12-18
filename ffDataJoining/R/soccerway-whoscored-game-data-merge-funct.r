MergeSoccerwayWhoscoredGameData = function(appearanceDF, gbgdf) {

  playerMatchDF = ffDataJoining:::LoadAllSoccerwayWhoscoredPlayerMap(appearanceDF) %>%
                rename(whoscoredplayer = whoscoredPlayer) %>%
                distinct(season, team, playerid, whoscoredplayer) %>%
                ungroup()

  ffDataJoining:::CheckDoubleNamePlayer(playerMatchDF)

  gbgdf = lazy_left_join(gbgdf, playerMatchDF, c('season', 'team', 'whoscoredplayer'), 'playerid')

  playerStatColumn = c('goal', 'assist', 'shotoob', 'shot6yd', 'shotib', 'openplay', 'counter',
                       'setpiece', 'penaltytaken', 'offt', 'bar', 'ont', 'block', 'penaltyscored', 'longkp', 'shortkp')
  columnToTransfer = c('whoscoredplayer', 'mainpos', playerStatColumn)
  appearanceDF = lazy_left_join(appearanceDF %>% rename(soccerwayPlayer = player),
                                gbgdf,
                                c('season', 'team', 'teamgamenumber', 'playerid'),
                                columnToTransfer) %>%
                      rename(player = whoscoredplayer)

  # there are always going to be players who aren't in gbgdf who are in appearanceDF, because appearanceDF helpfully tells us who was sidelined/on the bench etc
  appearanceDF = FillInPlayedButNoStats(appearanceDF, gbgdf, playerMatchDF, playerStatColumn)
  
  # a more common problem is eg a player is in appearanceDF at the start of the season via the bench. they won't appear in gbgdf yet so won't be in playermatch, but we can get rid of the NA player names and NA for match stat totals quite easily:
  appearanceDF = appearanceDF %>%
    mutate(missingPlayerId = is.na(player) & startTime %in% c('injury', 'suspension', 'U', 'UU'))
  appearanceDF = subset_join(appearanceDF,
                            playerMatchDF %>%
                              distinct(playerid, whoscoredplayer) %>%
                              rename(player = whoscoredplayer),
                              'playerid',
                              missingPlayerId)
  for (si in 1:length(playerStatColumn)) {
    appearanceDF = appearanceDF %>%
                  mutate_cond(missingPlayerId, !!playerStatColumn[si] := 0)
  }
  appearanceDF = remove_column(appearanceDF, 'missingPlayerId')

  fileOut = paste(DATAPATH, 'player-result.csv')
  write.csv(file = fileOut, appearanceDF, row.names = FALSE)

  return(appearanceDF)
}


FillInPlayedButNoStats = function(appearanceDF, gbgdf, playerMatchDF, playerStatColumn) {
  # EDIT this is now a problem. what can happen is a player comes off the bench late in a game, doesn't do enough to get any whoscored stats, so doesn't appear in gbgdf.
  # in this case we need to insert them into gbgdf with 0s for all stats
  playedButNoStats = anti_join(appearanceDF %>%
                                 filter(!startTime %in% c('U', 'UU', 'injury', 'suspension')),
                               gbgdf,
                               c('date', 'playerid')) %>% # NB but their 'player' value is NA, v annoying, replace
    lazy_left_join(playerMatchDF %>%
                     rename(player = whoscoredplayer),
                   'playerid',
                   'player')
  
  appearanceDF = indicate_overlapping_combination(appearanceDF, playedButNoStats, c('date', 'playerid'), 'playedButNoStats')
  appearanceDF[which(appearanceDF$playedButNoStats),playerStatColumn] = 0

  # but then you need to put something in for the position, just go with most commonly listed one in the relevant season
  # and we've nto got the whscored name yet
  mostCommonPositionDF = gbgdf %>%
    group_by(season, team, playerid, whoscoredplayer, mainpos) %>%
    count() %>%
    ungroup() %>%
    group_by(season, team, playerid) %>%
    filter(n == max(n)) %>%
    remove_column('n') %>%
    rename(player = whoscoredplayer)
  
  appearanceDF = subset_join(appearanceDF, mostCommonPositionDF, c('season', 'team', 'playerid'), playedButNoStats)

  # there will be the odd occasion when a player is still NA because they've not susbsequently played a game urghh, just sa y they're a midfielder
  # pretty sure it won't matter, this gets overwritten each time
  appearanceDF = appearanceDF %>%
    mutate_cond(playedButNoStats & is.na(mainpos),
                player = soccerwayPlayer,
                mainpos = 'M')
  
  appearanceDF = remove_column(appearanceDF, 'playedButNoStats')
  
  return(appearanceDF)
}
