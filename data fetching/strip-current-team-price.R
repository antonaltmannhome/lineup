
NavigateAndSaveTransferPage = function() {
  
  if (file.exists('c:/temp/temp-ff-team-value.dat')) {
    file.remove('c:/temp/temp-ff-team-value.dat')
  }
  
  initfile()
  ### firstly navigate to main page of team
  mypage=paste('https://fantasy.premierleague.com/transfers')
  gotowebadd(mypage,browserchoice='Firefox',thiscomputer = thiscomputer)
  
  insertabort()
  runscript()
  
  message('Now click on the list view tab, press enter when done')
  dum = askcond(FALSE, TRUE)
  
  initfile()
  insertselectwindow('Firefox')
  selectalltonotepad('c:/temp/temp-ff-team-value.dat', deselectpoint=c(100, 500))
  
  insertabort()
  runscript()
}

GetCleanCurrentTeamPriceDF = function() {
  # and here is the relevant file that has info about prices
  b = scan('c:/temp/ff-transfer-list-view.txt', sep = '\n', what = '', quiet = TRUE)
  
  # find bit that refers to the prices
  startIndex = grep(' \tGoalkeepers\tCP\tSP\tPP\tF\tTP\tFix', b)
  priceLine = b[startIndex: (startIndex + 18)]
  playerPriceLine = priceLine[c(2:3, 5:9, 11:15, 17:19)]
  
  PlayerPriceLineToDF = function(x) {
    strsplit(x, split = '\t')[[1]]
  }
  
  playerPriceMatrix = t(sapply(playerPriceLine, PlayerPriceLineToDF))
  rownames(playerPriceMatrix) = NULL
  colnames(playerPriceMatrix)  = c('currentPrice', 'sellingPrice', 'purchasePrice', 'form', 'totalPoint', 'nextFixtureAbbn')
  
  playerPriceDF = as_tibble(playerPriceMatrix)
  playerPriceDF$currentPrice = as.numeric(playerPriceDF$currentPrice)
  playerPriceDF$sellingPrice = as.numeric(playerPriceDF$sellingPrice)
  playerPriceDF$purchasePrice = as.numeric(playerPriceDF$purchasePrice)
  # ok, but now we have to figure out who is who
  # to do that, we start with current team
  
  teamAbbn = read_csv(paste0(DATAPATH, 'team_abbn.csv'), col_types = list(
    source = col_character(),
    wrongname = col_character(),
    correctname = col_character()
  )) %>%
    filter(source == 'fftransfer')
  
  playerPriceDF$nextFixture = teamAbbn$correctname[
    match(tolower(gsub(' .+$', '', playerPriceDF$nextFixtureAbbn)),
          teamAbbn$wrongname)]
  if (any(is.na(playerPriceDF$nextFixture))) {
    stop('You need to update d:/team_abbn.dat to add the new team three letter abbs\n')
  }
  playerPriceDF$ffPosition = rep(c('g', 'd', 'm', 'f'), c(2, 5, 5, 3))
  
  write_csv(playerPriceDF, paste0(DATAPATH, 'current-team-value.csv'))
}

MatchPriceToCurrentTeam = function() {
  currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))
  ## this should be a data joining function i think. wait til we've got some new data to work with
}
