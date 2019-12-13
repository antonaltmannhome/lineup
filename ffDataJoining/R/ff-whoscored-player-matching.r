InitialiseCurrentSeasonPlayerId = function(playerResultDF) {
  # you run this after the first set of fixtures of the year (although if you leave it later than that it also works fine)

  currentSeasonPlayerDF = playerResultDF %>%
    filter(season == currentseason) %>%
    distinct(team, soccerwayPlayer, playerid, player) %>%
    rename(whoscoredPlayer = player) %>%
    mutate(season = currentSeason)

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

  currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')
  stop('need to check ffDataJoining/ff-whoscored-player-matching/UpdateCurrentSeasonPlayerId code works since changing it')
  
  currentSeasonPlayerDF = readr::read_csv(currentSeasonPlayerIdFile, col_types = list(
    season = readr::col_integer(),
    playerid = readr::col_integer(),
    soccerwayPlayer = readr::col_character(),
    whoscoredPlayer = readr::col_character(),
    team = readr::col_character(),
    ffuseswholename = readr::col_logical(),
    hasleft = readr::col_logical(),
    adjustedwhoscoredPlayer = readr::col_character(),
  ))

  # but that could be out of date, let's see if anyone new has turned up
  upToDateCurrentPlayerDF = playerResultDF %>%
                              filter(season == currentseason) %>%
                              distinct(season, playerid, soccerwayPlayer, player, team)
  newCurrentPlayerDF = anti_join(upToDateCurrentPlayerDF,
                                    currentSeasonPlayerDF,
                                    'playerid') %>%
                        rename(soccerwayPlayer = soccerwayPlayer,
                                whoscoredPlayer = player) %>%
                        mutate(ffuseswholename = FALSE,
                                hasleft = FALSE,
                                adjustedwhoscoredPlayer = 'none')

  updatedCurrentSeasonPlayerDF = bind_rows(currentSeasonPlayerDF,
                                            newCurrentPlayerDF)

  # but let's put it back in team/player order of course
  updatedCurrentSeasonPlayerDF = updatedCurrentSeasonPlayerDF %>%
                                  arrange(team, whoscoredPlayer)

  write.csv(file = currentSeasonPlayerIdFile, updatedCurrentSeasonPlayerDF, row.names = FALSE)
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

  ffplayerpricedf = ffDataJoining:::ReadFFPlayerPriceDF()

  # there are ones we don't worry about, ie the ones who hven't played a game this season
  # but the ones we should correct are these:

  satis = FALSE
  while(!satis) {

    playerDF = playerDF %>%
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
      currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')
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
                           c('surname', 'teamsurname', 'ffindex', 'soccerwayPlayer',
                             'ffuseswholename', 'adjustedwhoscoredPlayer'))
  
  return(playerDF)
}
