  seasoninfo = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''))
  StoreSeason(1617)
  StoreSeason(1718)
  StoreSeason(1819)
  StoreSeason(1920)

playerMatchDF = LoadAllPlayerMatchFile() %>%
                rename(whoscoredplayer = whoscoredPlayer) %>%
                distinct()

doubleNamePlayer = playerMatchDF %>% distinct(playerid, whoscoredplayer) %>% count(playerid) %>% filter(n > 1)
doubleNameInfo = semi_join(playerMatchDF, doubleNamePlayer, 'playerid')
if (nrow(doubleNameInfo) > 0) {
  print(doubleNameInfo)
  message('aaarghh, we do not want players having two names within playerMatchDF or gfgdf')
}

gbgdf = LoadGbgDF()
gbgdf = lazy_left_join(gbgdf, playerMatchDF, c('season', 'whoscoredplayer'), 'playerid')
gbgdf %>% filter(is.na(playerid)) %>% distinct(season, team, whoscoredplayer)

# ok so both these problem seem to have the same cause. at this time of night i don't know why one causes the double name problem and one causes the missing player id problem though. solution is the same in any case: add a line to fixplayername, re-run the apprpriate StoreSeason, fix the soccerway_saved/team_season_player_match/... file if necessary (actually, that might be why you get different problems in each case), then re run the checks above. I've still not sorted the IGNORE issue yet, that was when whoscored had a data entry player problem for a complete non-entity of a player. i assume that hardly ever happens and would normally get corrected quickly
