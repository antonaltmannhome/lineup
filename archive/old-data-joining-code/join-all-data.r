## so we've done th horrible job of matchup up allplayers, now it's time to join everything up.
## we want soccerway data and whoscord data in the same data frame, and ff prices joined into a player summary data frame too.
## the output should be stored as an RDS, model startup should not be doing any data munging

source('c:/research/lineup/ff_startup.r')
source('data code/make_gbgdf.r')
source('c:/research/lineup/appearance model/appearance-model-funct.r')
source('c:/research/lineup/data code/sort-player-name-funct.r')

appearanceDF = getappearancedf()
gbgdf = LoadGbgDF()

unTeamSeason = LogAlreadyMatched(appearanceDF)

playerMatchDF = LoadAllPlayerMatchFile() %>%
                rename(whoscoredplayer = whoscoredPlayer) %>%
                distinct(season, team, playerid, whoscoredplayer) %>%
                ungroup()

CheckDoubleNamePlayer(playerMatchDF)

gbgdf = lazy_left_join(gbgdf, playerMatchDF, c('season', 'team', 'whoscoredplayer'), 'playerid')

playerStatColumn = c('goal', 'assist', 'shotoob', 'shot6yd', 'shotib', 'openplay', 'counter',
                     'setpiece', 'penaltytaken', 'offt', 'bar', 'ont', 'block', 'penaltyscored', 'longkp', 'shortkp')
columnToTransfer = c('whoscoredplayer', playerStatColumn)
appearanceDF = lazy_left_join(appearanceDF %>% rename(soccerwayPlayer = player),
                              gbgdf,
                              c('season', 'team', 'teamgamenumber', 'playerid'),
                              columnToTransfer) %>%
                    rename(player = whoscoredplayer)

# there are always going to be players who aren't in gbgdf who are in appearanceDF, because appearanceDF helpfully tells us who was sidelined/on the bench etc
# but there are a handful of cases where there might be a data error on whoscored, or some other as yet undiagnosed situation, where data is missing from gbgdf. there's very little right now and it's not important, but worht keeping an eye on it:
CheckNonTrivialMissing(appearanceDF)

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

if (FALSE) {
  checkIt = appearanceDF %>%
            group_by(season, player) %>%
            summarise(sumGoal = sum(goal, na.rm = TRUE),
                      sumAssist = sum(assist, na.rm = TRUE)) %>%
            ungroup()
  checkGoal = checkIt %>% group_by(season) %>% arrange(desc(sumGoal)) %>% slice(1:5)
  checkAssist = checkIt %>% group_by(season) %>% arrange(desc(sumAssist)) %>% slice(1:5)
}

# how irritating, assists are counted much less generously on whoscored than fantasy football. we'll probably be less likely to pick attacking players as a result

# we also want a summary of all the current squads along with the player prices

currentTeamPlayer = appearanceDF %>%
  filter(season == currentseason) %>%
  distinct(team, soccerwayPlayer, player, playerid)
# there are still a few NAs for players, that's when they have been on the bench at the start of the current season but not played. don't think we care about those.

# now we want to write the latest set of players to disk, but don't of course overwrite 

ffplayerpricedf = readr::read_csv(paste0(DATAPATH, 'ff-price.csv'),
                                  col_types = list(player = readr::col_character(),
                                                   team = readr::col_character(),
                                                   ffposition = readr::col_character(),
                                                   price = readr::col_double()))
ffplayerpricedf$teamsurname=with(ffplayerpricedf,paste(team,player))
