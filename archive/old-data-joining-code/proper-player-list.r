### let's try to get proper system in place for players. let's have a file which lists players, and date range for when they were at each club
### this file should have the players' ids. then a separate file has one row for each palyer, along with their current ff name, then soccerway number

source('c:/research/lineup/ff_startup.r')
source('model_startup.r')

# this would be very annoying if we did it in the actual anlysis, but it's useful to do it while we're trying to sort out player ids:

gbgdf = gbgdf %>% rename(whoscoredplayer = player)

playerteamseasondatedf = gbgdf %>%
                    group_by(season, whoscoredplayer, team) %>%
                    summarise(joindate = min(date),
                              leavedate = max(date)) %>%
                    arrange(joindate) %>%
  mutate(nextjoindate = lead(joindate, 1),
         overlap = !is.na(nextjoindate) & nextjoindate < leavedate) %>%
  ungroup()

## ah players can be on loan on course
### or even worse, gbgdf puts in a 0 minutes line for them even after they've left, we don't want that
### but that's just because whoscored keeps their data in there. it's our job to sift out those situations

### so, let's look out for
# (1) players having only 0 minutes sequences while having a sequence at another club
# (2) don't think we can distinguish between a player leaving mid season for a non-prem club, and a player being at a club but never being picked. but it doesn't matter for the player model and riku we have systems in place to help with that

# so it's only 1 that we need to be clever with, so let's have a go at that

gbgdf$playerhasleft = FALSE
possiblemidseasonleaverdf = playerteamseasondatedf %>%
                            filter(overlap) %>%
                            mutate(hasleft = FALSE)
for (oi in 1:nrow(possiblemidseasonleaverdf)) {
  overlapinfo = possiblemidseasonleaverdf %>% slice(oi)
  ### so the question is, the games of 'team' where date is greater than nextjoindate, are they all 0 minutes?
  currentteamminuteafternextjoindate = semi_join(gbgdf, overlapinfo, c('season', 'whoscoredplayer', 'team')) %>%
                                        filter(date >= overlapinfo$nextjoindate) %>%
                                        select(season, teamgamenumber, team, minute, whoscoredplayer)
  playerhasostensiblyleft = all(currentteamminuteafternextjoindate$minute == 0)
  if (playerhasostensiblyleft) {
    currentteamminuteafternextjoindate$playerhasleft = TRUE
    possiblemidseasonleaverdf$hasleft[oi] = TRUE
    gbgdf = join_on_overlap(gbgdf,
                               currentteamminuteafternextjoindate,
                                        c('season', 'teamgamenumber', 'team', 'whoscoredplayer'))
  }
  if (!playerhasostensiblyleft) {
    message('eek, we have an exmaple of a player who seems to be at two clubs at once')
    print(overlapinfo)
    stop()
  }
}

# gbgdf %>% filter(season == 1617 & grepl('chambers', player)) %>% select(season, date, team, teamgamenumber, playerhasleft)

# i'm confused, the lines seem to be blurred between making a current set of players and fixing the old data, need to find a way of separating these processes efficiently
gbgdf = gbgdf %>%
        filter(!playerhasleft)

# ok that helps us lots
# now we should be able to make a table, one row for each player/season/team...

playerteamseasondf = gbgdf %>%
                        group_by(season, whoscoredplayer, team) %>%
                        summarise(joindate = min(date),
                                  leavedate = max(date)) %>%
                        arrange(joindate)
# this doesn't tell one thing though which is whether the leavedate is the most recent game by the team
# although we really can't determine that from the data. i think we'll give the user a column to indicate that
# except when a player has actually left for another club midseason and we know about it, as above. no harm in entering this information if we've got it

currentplayerdf = playerteamseasondf %>%
                  filter(season == currentseason)
  
if (FALSE) {
  # think you only do this first time you ever write the df to disk
  currentplayerdf$hasleft = FALSE
  currentplayerdf = join_on_overlap(currentplayerdf,
                                    possiblemidseasonleaverdf %>%
                                      select(season, team, whoscoredplayer, hasleft),
                                    c('season', 'team', 'whoscoredplayer')) %>%
                    select(-isOverlapping)

  currentplayerdf = currentplayerdf %>%
                    mutate(ffuseswholename = FALSE,
                           adjustedwhoscoredname = 'none')

  # write the bastard to disk, although we need to deal with scanning in previous manually entered data
  write.csv(file = paste0(DATAPATH, 'current-player.csv'), currentplayerdf, row.names = FALSE)
}
  
## the more common situation, where you are scanning in existing information, retaining the manually adjusted stuff but updating the rest
if (TRUE) {
  storedplayerdf = readr::read_csv(paste0(DATAPATH, 'current-player.csv'),
                                    col_types = list(
                                      season = readr::col_integer(),
                                      whoscoredplayer = readr::col_character(),
                                      team = readr::col_character(),
                                      joindate = readr::col_double(),
                                      leavedate = readr::col_double(),
                                      ffuseswholename = readr::col_logical(),
                                      adjustedwhoscoredname = readr::col_character(),
                                      hasleft = readr::col_logical()
                                    ))
  
  # then we want to join the updated currentplayerdf to the manually entered bits of the stored one
  currentplayerdf = lazy_left_join(currentplayerdf,
                                  storedplayerdf,
                                  c('season', 'team', 'whoscoredplayer'),
                                  c('ffuseswholename', 'adjustedwhoscoredname', 'hasleft'))
}

# of course, we're missing the unused players for the current season. but that's ok, we can add them when we join up to the ff price db. and later, we'll join up to riku

ffplayerpricedf = readr::read_csv(paste0(DATAPATH, 'ff-price.csv'),
                            col_types = list(player = readr::col_character(),
                                             team = readr::col_character(),
                                             ffposition = readr::col_character(),
                                             price = readr::col_double()))
ffplayerpricedf$teamsurname=with(ffplayerpricedf,paste(team,player))

currentplayerdf = playerteamseasondf %>%
                  filter(season == currentseason)
currentplayerdf = currentplayerdf %>%
                      mutate(surname = case_when(
                        adjustedwhoscoredname == 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',whoscoredname),
                        #adjustedwhoscoredname != 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',adjustedwhoscoredname),
                        #adjustedwhoscoredname != 'none' & ffuseswholename ~ adjustedwhoscoredname,
                        adjustedwhoscoredname == 'none' & ffuseswholename ~ whoscoredname,
                        adjustedwhoscoredname != 'none' ~ adjustedwhoscoredname))
playerdf$teamsurname=with(playerdf,paste(probablelatestteam,surname))
