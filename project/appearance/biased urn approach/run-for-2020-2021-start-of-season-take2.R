## expect this will evolve into the actual released file

source('new-model-startup.r')
source('project/appearance/biased urn approach/biased-urn-game-by-game-set-up-data.r')

# oh for fucks sake why does digne still have two positions
# because he changed in between seasons, which is still allowed by the code
# i suppose it means that when joining to the current squad, we should join not only by player but by mainpos2 too
# aargh, whoscored positions change quite frequently unfortunately
# how about, take whatever their most recent position is, override past data with that, then optimise?

currentSeasonTeam = fixtDF %>%
  distinct(team) %>%
  arrange(team) %>%
  left_join(resultDF %>%
              filter(seasonNumber >= currentseasonnumber - 1) %>%
              group_by(team) %>%
              summarise(numRecentSeasonGame = n()),
            'team')

mostRecentPositionByPlayer = gbgdf %>%
  semi_join(currentSeasonTeam, 'team') %>%
  group_by(team) %>%
  filter(alltimetgn == max(alltimetgn)) %>%
  select(player, mainpos2) %>%
  ungroup()

# now propogate that throughout the past data
gbgdf2 = gbgdf2 %>%
  join_on_overlap(mostRecentPositionByPlayer,
                  c('team', 'player'))

priorStrength = 10
timeDownWeightCoef = 0.1

myList = vector('list', 3 * nrow(currentSeasonTeam))
for (ti in which(currentSeasonTeam$numRecentSeasonGame >= 2)) {
  maxAllTimeTgn = with(resultDF, max(alltimetgn[team == currentSeasonTeam$team[ti]]))
  for (posi in 1:3) {
    myMainPos = c('def', 'mid', 'att')[posi]
    myList[[3 * (ti - 1) + posi]] =
      GetPlayerAppearanceMle(currentSeasonTeam$team[ti],
                             myMainPos,
                             maxAllTimeTgn + 1,
                             38, priorStrength, timeDownWeightCoef)
  }
  message('Have processed latest estimates for ', currentSeasonTeam$team[ti])
}

playerAppearanceMleDF = bind_rows(myList)

# ok. so we store those and then need to convert to actual expected minutes, then we're where we were last season

write_csv(path = paste0(DATAPATH, 'active_player/player-mle-', currentseason, '-1.csv'),
          x = playerAppearanceMleDF)

# ok, next step, turn these into expected minutes, come on
# ah, but formation needs to be a little different, it needs to have a notion of who's expected to be available in the next game
# which is a little tricky at the start of the season, think we need to have the data from the ff website

# or
playerAppearanceMleDF = read_csv(paste0(DATAPATH, 'active_player/player-mle-', nextSeason, '-1.csv'), col_types = list(
  team = col_character(),
  alltimetgn = col_integer(),
  mainpos2 = col_character(),
  player = col_character(),
  appearanceMle = col_double(),
  appearanceOdds = col_double()))

ffPriceDF = read_csv(paste0(DATAPATH, 'ff-price.csv'), col_types = list(
  player = col_character(),
  team = col_character(),
  ffposition = col_character(),
  price = col_double()
))

# right, we have to get that talking to the formation code somehow
# this is a right pain! the position classifications on ff are completely different to what they are in whoscored data. This is surely not worth doing just for the first gameweek

# would be nice to indicate which players are long term injured too

myList = list('vector', nrow(currentSeasonTeam))
for (ti in which(currentSeasonTeam$numRecentSeasonGame >= 2)) {
  maxAllTimeTgn = with(resultDF, max(alltimetgn[team == currentSeasonTeam$team[ti]]))
  myList[[ti]] = GetFormationProbabilityByTeam(currentSeasonTeam$team[ti],
                                               maxAllTimeTgn, 38,
                                               historicFormationDF)
}
# for the recently promoted teams, just go with the historic ones to start with
for (ti in which(currentSeasonTeam$numRecentSeasonGame == 1)) {
  numAvailablePlayer = gbgdf %>%
    filter(team == currentSeasonTeam$team[ti]) %>%
    count(mainpos2)
  numAvailablePlayerByPosition = with(numAvailablePlayer,
                                      tibble(numAvailableDef = n[mainpos2 == 'def'],
                                             numAvailableMid = n[mainpos2 == 'mid'],
                                             numAvailableAtt = n[mainpos2 == 'att']))
  myList[[ti]] = bind_cols(historicFormationDF,
                           numAvailablePlayerByPosition) %>%
                  mutate(team = currentSeasonTeam$team[ti]) %>%
    filter(att <= numAvailableAtt &
             mid <= numAvailableMid &
             def <= numAvailableDef) %>%
    arrange(desc(sumMatchProportion)) %>%
    slice(1:3) %>%
    mutate(formationWgt = sumMatchProportion / sum(sumMatchProportion)) %>%
    select(-c(sumMatchProportion, other))
}                 
    
myTeamTgnFormationDF = bind_rows(myList)
# ok this is a real arse, you can't really get formation probabilities that include the most recent game
# will sort that next week i guess. in the mean time, pretend what we have includes the most recent game

myList = list('vector', nrow(currentSeasonTeam))
# actually, instead of predicting just hte next game, maybe THIS should still predict the next block of games
# to get rid of the resting issue
for (tbi in 1:nrow(currentSeasonTeam)) {
  myList[[tbi]] =  GetNextGamePropByTeam(myTeam = currentSeasonTeam$team[tbi],
                                          myLatestTeamEstimateDF = playerAppearanceMleDF %>%
                                                    filter(team == currentSeasonTeam$team[tbi]),
                                                  myLatestTeamTgnFormationDF = myTeamTgnFormationDF %>%
                                                    filter(team == currentSeasonTeam$team[tbi]),
                                         myPlayerDF = playerDF %>%
                                            filter(team == currentSeasonTeam$team[tbi]))
}
myTeamExpectedPropGameDF = bind_rows(myList)

# ok, let's just write that to disk, and say anyone with >0.7 is a starter, then choose from those players

write_csv(path = paste0(DATAPATH, 'active_player/biased_urn_output/playing-prob.csv'),
          x = myTeamExpectedPropGameDF)
