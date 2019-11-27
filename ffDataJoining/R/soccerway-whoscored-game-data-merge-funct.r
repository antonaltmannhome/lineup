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
  # but there are a handful of cases where there might be a data error on whoscored, or some other as yet undiagnosed situation, where data is missing from gbgdf. there's very little right now and it's not important, but worht keeping an eye on it:
  ffDataJoining:::CheckNonTrivialMissing(appearanceDF)

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
