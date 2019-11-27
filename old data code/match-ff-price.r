### start by loading in the list of all players active this season
# source('c:/research/lineup/ff_startup.r')

source('c:/research/lineup/appearance model/appearance-model-funct.r')
source('c:/research/lineup/data code/ffprice-matching-function.r')

currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')

# need to re-run this when new players appear not coded it up appropriately yet though
# CreateCurrentPlayerDFFromGame()

ffplayerpricedf = readr::read_csv(paste0(DATAPATH, 'ff-price.csv'),
                            col_types = list(player = readr::col_character(),
                                             team = readr::col_character(),
                                             ffposition = readr::col_character(),
                                             price = readr::col_double())) %>%
                  rename(ffplayer = player)
ffplayerpricedf$ffteamplayer = with(ffplayerpricedf, paste(team, ffplayer))


satis = FALSE
while(!satis) {
  currentPlayerId = LoadCurrentPlayerId(ffplayerpricedf)
  
  unmatchedPlayerId = currentPlayerId %>%
                      filter(is.na(ffindex))
  
  if (nrow(unmatchedPlayerId) == 0) {
    message('Congratulations! All players are matched up to FF website')
    satis = TRUE
  }
  
  if (nrow(unmatchedPlayerId) > 0) {
    print(unmatchedPlayerId %>% select(team, whoscoredPlayer, ffuseswholename, hasleft, adjustedwhoscoredPlayer))
    message('The players above have not been matched. If you want to edit the \'current-season-player-id.csv\' to match up more, do so then press ENTER')
    message('If you are happy with how it has matched up, press \'h\'')
    userEntry  = askcond(FALSE, TRUE)
    if (!is.null(userEntry) && userEntry == 'h') {
      satis = TRUE
    }
  }
}
 
ffplayerpricedf$playerid = currentPlayerId$playerid[match(1:nrow(ffplayerpricedf), currentPlayerId$ffindex)]

write.csv(file = paste0(DATAPATH, 'ff-price.csv'), ffplayerpricedf, row.names = FALSE)

