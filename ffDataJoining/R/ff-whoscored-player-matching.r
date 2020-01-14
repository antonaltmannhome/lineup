InitialiseCurrentSeasonPlayerId = function(playerResultDF) {
  # you run this after the first set of fixtures of the year (although if you leave it later than that it also works fine)

  currentSeasonPlayerDF = playerResultDF %>%
    filter(season == currentseason) %>%
    distinct(team, soccerwayPlayer, playerid, player) %>%
    rename(whoscoredPlayer = player) %>%
    mutate(season = currentseason)

  mostRecentAppearanceByPlayer = playerResultDF %>%
    filter(season == currentseason) %>%
    group_by(playerid) %>%
    arrange(desc(date)) %>%
    slice(1)
  currentSeasonPlayerDF = lazy_left_join(currentSeasonPlayerDF, mostRecentAppearanceByPlayer, 'playerid', 'team')

  currentSeasonPlayerDF$ffuseswholename = FALSE
  currentSeasonPlayerDF$hasleft = FALSE
  currentSeasonPlayerDF$adjustedwhoscoredPlayer = 'none'

  currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')

  write.csv(file = currentSeasonPlayerIdFile, currentSeasonPlayerDF, row.names = FALSE)
}

UpdateCurrentSeasonPlayerId = function(playerResultDF) {
  # firstly load in our existing player ids

  currentSeasonPlayerDF = playerResultDF %>%
    filter(season == currentseason) %>%
    mutate(played = grepl('^[0-9]+$', startTime)) %>%
    group_by(team, soccerwayPlayer, playerid, player) %>%
    summarise(numGamePlayed = sum(played)) %>%
    ungroup() %>%
    rename(whoscoredPlayer = player) %>%
    mutate(season = currentseason,
           ffuseswholename = FALSE,
           hasleft = FALSE,
           adjustedwhoscoredPlayer = 'none')

  currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')

  previousCurrentSeasonPlayerDF = readr::read_csv(currentSeasonPlayerIdFile, col_types = list(
    soccerwayPlayer = readr::col_character(),
    playerid = readr::col_integer(),
    whoscoredPlayer = readr::col_character(),
    team = readr::col_character(),
    ffuseswholename = readr::col_logical(),
    hasleft = readr::col_logical(),
    adjustedwhoscoredPlayer = readr::col_character(),
    season = readr::col_integer()
  ))
  
  # all we want to retain is the manually entered info from that, so let's port it across
  currentSeasonPlayerDF = join_on_overlap(currentSeasonPlayerDF,
                                          previousCurrentSeasonPlayerDF %>%
                                            select(playerid, team, ffuseswholename, hasleft, adjustedwhoscoredPlayer),
                                          c('playerid', 'team'))
  

  # but let's have it in team/player order of course
  currentSeasonPlayerDF = currentSeasonPlayerDF %>%
                            arrange(team, whoscoredPlayer)

  write.csv(file = currentSeasonPlayerIdFile, currentSeasonPlayerDF, row.names = FALSE)
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

MatchFFPlayerData = function(playerDF, interactive = FALSE) {

  # there are ones we don't worry about, ie the ones who hven't played a game this season
  # but the ones we should correct are these:
  ffplayerpricedf = ffDataJoining:::ReadFFPlayerPriceDF()
  
  satis = FALSE
  while(!satis) {

    currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')
    
    currentSeasonPlayerDF = readr::read_csv(currentSeasonPlayerIdFile, col_types = list(
      soccerwayPlayer = readr::col_character(),
      playerid = readr::col_integer(),
      whoscoredPlayer = readr::col_character(),
      team = readr::col_character(),
      ffuseswholename = readr::col_logical(),
      hasleft = readr::col_logical(),
      adjustedwhoscoredPlayer = readr::col_character(),
      season = readr::col_integer()
    ))
    
    playerDF = playerDF %>%
      filter(season == currentseason) %>%
      lazy_left_join(currentSeasonPlayerDF, c('team', 'playerid'), c('ffuseswholename', 'adjustedwhoscoredPlayer', 'hasleft')) %>%
      mutate(surname = case_when(
        adjustedwhoscoredPlayer == 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',player),
        adjustedwhoscoredPlayer == 'none' & ffuseswholename ~ player,
        adjustedwhoscoredPlayer != 'none' ~ adjustedwhoscoredPlayer))
    playerDF$teamsurname = with(playerDF, paste(team, surname))

    playerDF$ffindex = match(playerDF$teamsurname, ffplayerpricedf$ffteamplayer)
    unmatchedButCouldMatchIndex =
      with(playerDF, which(!is.na(player) & is.na(ffindex) & !hasleft))
    if (length(unmatchedButCouldMatchIndex) == 0) {
      satis = TRUE
    }

    if (length(unmatchedButCouldMatchIndex) > 0) {
      print(playerDF[unmatchedButCouldMatchIndex, c('team', 'player', 'ffuseswholename', 'hasleft', 'adjustedwhoscoredPlayer', 'teamsurname')])
      message('These players are logged as being in the squad this year by soccerway but have not been matched up on fantasy football site.')
      if (!interactive) {
        message('Add them to \'', currentSeasonPlayerIdFile, '\', if this is a concern')
        satis = TRUE
      }
      if (interactive) {
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
  }

  playerDF$ffPrice = ffplayerpricedf$price[playerDF$ffindex]
  playerDF$ffPosition = ffplayerpricedf$ffposition[playerDF$ffindex]
  playerDF = remove_column(playerDF,
                           c('surname', 'teamsurname', 'ffindex',
                             'ffuseswholename', 'adjustedwhoscoredPlayer'))
  
  return(playerDF)
}
