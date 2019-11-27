# yet another suite of functions, doesn't matter, we'll tidy them all up later once we know they work

CreateCurrentPlayerDFFromGame = function() {
  # will need to rewrite this once season is underway, need to update when a new player appears, but also need to join to what has already been done manually
  dum = list.files(paste0(DATAPATH, 'soccerway_saved/team_season_player_match/'))
  currentteamfile = dum[grep(currentseason, dum)]
  currentplayerlist = vector('list', length(currentteamfile))
  for (ti in 1:length(currentteamfile)) {
    currentplayerlist[[ti]] = read.csv(paste0(DATAPATH, 'soccerway_saved/team_season_player_match/', currentteamfile[ti]), as.is = TRUE)
  }
  currentplayerdf = bind_rows(currentplayerlist)
  
  # then we want the most recent team they have appeared for
  
  appearanceDF = getappearancedf()
  
  mostRecentAppearanceByPlayer = appearanceDF %>%
    filter(season == currentseason) %>%
    group_by(playerid) %>%
    arrange(desc(date)) %>%
    slice(1)
  currentplayerdf = lazy_left_join(currentplayerdf, mostRecentAppearanceByPlayer, 'playerid', 'team')
  
  currentplayerdf$ffuseswholename = FALSE
  currentplayerdf$hasleft = FALSE
  currentplayerdf$adjustedwhoscoredPlayer = 'none'
  
  write.csv(file = currentSeasonPlayerIdFile, currentplayerdf, row.names = FALSE)
}

UpdateCurrentSeasonPlayerDF = function(currentseason, appearanceDF) {
  currentTeamPlayer = appearanceDF %>%
    filter(season == currentseason) %>%
    distinct(team, soccerwayPlayer, player, playerid)
  
}

LoadCurrentPlayerId = function(ffplayerpricedf) {
  
  currentSeasonPlayerId = readr::read_csv(currentSeasonPlayerIdFile, col_types = list(
    season = readr::col_integer(),
    playerid = readr::col_integer(),
    soccerwayPlayer = readr::col_character(),
    whoscoredPlayer = readr::col_character(),
    team = readr::col_character(),
    ffuseswholename = readr::col_logical(),
    hasleft = readr::col_logical(),
    adjustedwhoscoredPlayer = readr::col_character()
  ))
  
  currentSeasonPlayerId = currentSeasonPlayerId %>%
    mutate(surname = case_when(
      adjustedwhoscoredPlayer == 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',whoscoredPlayer),
      #adjustedwhoscoredname != 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',adjustedwhoscoredname),
      #adjustedwhoscoredname != 'none' & ffuseswholename ~ adjustedwhoscoredname,
      adjustedwhoscoredPlayer == 'none' & ffuseswholename ~ whoscoredPlayer,
      adjustedwhoscoredPlayer != 'none' ~ adjustedwhoscoredPlayer))
  currentSeasonPlayerId$teamsurname = with(currentSeasonPlayerId, paste(team, surname))
  
  currentSeasonPlayerId$ffindex = match(currentSeasonPlayerId$teamsurname, ffplayerpricedf$ffteamplayer)
  
  return(currentSeasonPlayerId)
}
