MatchPriceDataToCurrentTeam = function() {
  
  resultDF = ffDataLoading:::GetResultDF()
  fixtDF = ffDataLoading:::GetFixtDF(resultDF)
  playerDF = ffDataLoading:::ReadCurrentSeasonPlayerDF() %>%
    rename(player = whoscoredPlayer)
  playerDF = ffDataJoining:::MatchFFPlayerData(playerDF)
  
  currentteam = read_csv(paste(DATAPATH, 'currentteam.csv', sep = ''), col_types = list(
    team = col_character(),
    player = col_character(),
    ffposition = col_character()
  ))
  
  nextGameWeek = min(fixtDF$gameweek)
  currentteam = lazy_left_join(currentteam,
                               fixtDF %>% filter(gameweek == nextGameWeek),
                               'team',
                               'oppteam') %>%
    lazy_left_join(playerDF, c('team', 'player'), 'ffPrice')
  
  playerPriceDF = read_csv(paste0(DATAPATH, 'unmatched-player-price.csv'), col_types = list(
    currentPrice = col_double(),
    sellingPrice = col_double(),
    purchasePrice = col_double(),
    form = col_double(),
    totalPoint = col_double(),
    nextFixtureAbbn = col_character(),
    nextFixture = col_character(),
    ffPosition = col_character()
  ))
  
  currentteam[,c('sellingPrice', 'currentPrice', 'purchasePrice')] = NA_real_
  for (j in 1:nrow(currentteam)) {
    candidatePriceIndex = which(playerPriceDF$currentPrice == currentteam$ffPrice[j] &
                                  playerPriceDF$ffPosition == currentteam$ffposition[j] &
                                  playerPriceDF$nextFixture == currentteam$oppteam[j])
    if (length(candidatePriceIndex) == 1) {
      currentteam[j, c('sellingPrice', 'currentPrice', 'purchasePrice')] =
        playerPriceDF[candidatePriceIndex, c('sellingPrice', 'currentPrice', 'purchasePrice')]
    }
    if (length(candidatePriceIndex) != 1) {
      stop()
    }
  }
  
  write_csv(file = paste0(DATAPATH, 'current-team-plus-value.csv'),
            currentteam)
}
