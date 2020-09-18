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
  arrange(team)

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
for (ti in 1:nrow(currentSeasonTeam)) {
  maxAllTimeTgn = with(resultDF, max(alltimetgn[team == currentSeasonTeam$team[j]]))
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

write_csv(path = paste0(DATAPATH, 'active_player/player-mle-', nextSeason, '-1.csv'),
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

myList = list('vector', nrow(survivingTeamDF))
for (ti in 1:nrow(survivingTeamDF)) {
  myList[[ti]] = with(survivingTeamDF[ti,],
                       GetFormationProbabilityByTeam(team, finalAllTimeTgn + 1, 38, historicFormationDF))
}
myTeamTgnFormationDF = bind_rows(myList)

myList = list('vector', nrow(myTeamTgn))
# actually, instead of predicting just hte next game, maybe THIS should still predict the next block of games
# to get rid of the resting issue
for (tbi in 1:nrow(myTeamTgn)) {
  myList[[tbi]] =  with(myTeamTgn[tbi,],
                        GetExpectedPropGameByTeam(team, alltimetgn,
                                                  myTeamEstimateDF, myTeamTgnFormationDF))
}
myTeamExpectedPropGameDF = bind_rows(myList)
