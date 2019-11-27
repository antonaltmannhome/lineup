InitialiseCurrentSeasonPlayerId = function(appearanceDF) {
  # you run this after the first set of fixtures of the year (although if you leave it later than that it also works fine)

  currentSeasonPlayerDF = appearanceDF %>%
    filter(season == currentseason) %>%
    distinct(team, soccerwayPlayer, playerid, player)

  mostRecentAppearanceByPlayer = appearanceDF %>%
    filter(season == currentseason) %>%
    group_by(playerid) %>%
    arrange(desc(date)) %>%
    slice(1)
  currentSeasonPlayerDF = lazy_left_join(currentSeasonPlayerDF, mostRecentAppearanceByPlayer, 'playerid', 'team')

  currentSeasonPlayerDF$ffuseswholename = FALSE
  currentSeasonPlayerDF$hasleft = FALSE
  currentSeasonPlayerDF$adjustedwhoscoredPlayer = 'none'
  currentSeasonPlayerDF$ffPrice = NA

  currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')

  write.csv(file = currentSeasonPlayerIdFile, currentSeasonPlayerDF, row.names = FALSE)
}

UpdateCurrentSeasonPlayerId = function(appearanceDF) {
  # firstly load in our existing player ids

  currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')

  currentSeasonPlayerId = readr::read_csv(currentSeasonPlayerIdFile, col_types = list(
    season = readr::col_integer(),
    playerid = readr::col_integer(),
    soccerwayPlayer = readr::col_character(),
    whoscoredPlayer = readr::col_character(),
    team = readr::col_character(),
    ffuseswholename = readr::col_logical(),
    hasleft = readr::col_logical(),
    adjustedwhoscoredPlayer = readr::col_character(),
    ffPrice = readr::col_double()
  ))

  # but that could be out of date, let's see if anyone new has turned up
  upToDateCurrentPlayerDF = appearanceDF %>%
                              filter(season == currentseason) %>%
                              distinct(season, playerid, soccerwayPlayer, player, team)
  newCurrentPlayerDF = anti_join(upToDateCurrentPlayerDF,
                                    currentSeasonPlayerId,
                                    'playerid') %>%
                        rename(soccerwayPlayer = soccerwayPlayer,
                                whoscoredPlayer = player) %>%
                        mutate(ffuseswholename = FALSE,
                                hasleft = FALSE,
                                adjustedwhoscoredPlayer = 'none')

  updatedCurrentSeasonPlayerId = bind_rows(currentSeasonPlayerId,
                                            newCurrentPlayerDF)

  # but let's put it back in team/player order of course
  updatedCurrentSeasonPlayerId = updatedCurrentSeasonPlayerId %>%
                                  arrange(team, whoscoredPlayer)

  write.csv(file = currentSeasonPlayerIdFile, updatedCurrentSeasonPlayerId, row.names = FALSE)
}

ReadFFPlayerPriceDF = function() {
  ffplayerpricedf = readr::read_csv(paste0(DATAPATH, 'ff-price.csv'),
                              col_types = list(player = readr::col_character(),
                                               team = readr::col_character(),
                                               ffposition = readr::col_character(),
                                               price = readr::col_double())) %>%
                    rename(ffplayer = player)
  ffplayerpricedf$ffteamplayer = with(ffplayerpricedf, paste(team, ffplayer))

  return(ffplayerpricedf)
}

MatchFFPlayerData = function() {

  ffplayerpricedf = ffDataJoining:::ReadFFPlayerPriceDF()

  currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')

  # there are ones we don't worry about, ie the ones who hven't played a game this season
  # but the ones we should correct are these:

  satis = FALSE
  while(!satis) {

    currentSeasonPlayerDF = ffDataLoading:::ReadCurrentSeasonPlayerDF()

    currentSeasonPlayerDF = currentSeasonPlayerDF %>%
      mutate(surname = case_when(
        adjustedwhoscoredPlayer == 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',whoscoredPlayer),
        adjustedwhoscoredPlayer == 'none' & ffuseswholename ~ whoscoredPlayer,
        adjustedwhoscoredPlayer != 'none' ~ adjustedwhoscoredPlayer))
    currentSeasonPlayerDF$teamsurname = with(currentSeasonPlayerDF, paste(team, surname))

    currentSeasonPlayerDF$ffindex = match(currentSeasonPlayerDF$teamsurname, ffplayerpricedf$ffteamplayer)
    unmatchedButCouldMatchIndex =
      with(currentSeasonPlayerDF, which(!is.na(whoscoredPlayer) & is.na(ffindex) & !hasleft))
    if (length(unmatchedButCouldMatchIndex) == 0) {
      satis = TRUE
    }

    if (length(unmatchedButCouldMatchIndex) > 0) {
      print(currentSeasonPlayerDF[unmatchedButCouldMatchIndex, c('team', 'whoscoredPlayer', 'ffuseswholename', 'hasleft', 'adjustedwhoscoredPlayer', 'teamsurname')])
      message('Try to fix up these player names in \'', currentSeasonPlayerIdFile, '\'')
      message('Type \'h\' if you are happy with how it has been matched')
      message('Type \'r\' if you want to edit the spreadsheet and see if you get more matches')

      userEntry = askcond(FALSE, FALSE)
      if (userEntry == 'h') {
        satis = TRUE
      }
      if (userEntry == 'r') {
        message('Ok make your changes then hit ENTER')
        dum = askcond(FALSE, TRUE)
      }
    }
  }

  currentSeasonPlayerDF$ffPrice = ffplayerpricedf$price[currentSeasonPlayerDF$ffindex]
  currentSeasonPlayerDF$ffPosition = ffplayerpricedf$ffposition[currentSeasonPlayerDF$ffindex]
  currentSeasonPlayerDF = remove_column(currentSeasonPlayerDF, c('surname', 'teamsurname', 'ffindex'))

  message('I am about to write the latest version of the player id file to disk, please close it if you have it open!')
  message('Hit ENTER when it is closed')
  dum = askcond(FALSE, TRUE)

  write.csv(file = currentSeasonPlayerIdFile, currentSeasonPlayerDF, row.names = FALSE)
}
