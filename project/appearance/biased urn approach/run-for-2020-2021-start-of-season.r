## let's just get hold of player appearance rankings for the start of the 2020/2021 season
## this is intended to be run before the season. separate code to run once season's underway

source('new-model-startup.r')
source('project/appearance/biased urn approach/biased-urn-game-by-game-set-up-data.r')

nextSeason = 2021 # ugh, hard coded number

GetAllPastMle = function(myTeam, timeDownWeightCoef, priorStrength) {
  
  myTeamMainposTgn = unTeamMainposTgn %>%
    filter(team == myTeam)
  myTeamTgn = unTeamTgn %>%
    filter(team == myTeam)
  
  myList = vector('list', nrow(myTeamMainposTgn))
  for(tbpi in 1:nrow(myTeamMainposTgn)) {
    myList[[tbpi]] = with(myTeamMainposTgn[tbpi,],
                          GetPlayerAppearanceMle(team, mainpos2, alltimetgn, 38,
                                                 priorStrength, timeDownWeightCoef))
    if ( (tbpi %% 10) == 0) {
      message('Have calculated ', tbpi, ' out of ', nrow(myTeamMainposTgn), ' team/block/position blocks so far')
    }
  }
  myTeamEstimateDF = bind_rows(myList)
  
  fileOut = paste0(DATAPATH, 'active_player/biased_urn_output/playermle_', myTeam, '_timedownweight_', timeDownWeightCoef, '_priorstrength_', priorStrength, '.csv')
  write_csv(x= myTeamEstimateDF, path = fileOut)
}

# ah but what do we do about the new teams
# don't know. let's sort the existing teams first

previousSeasonPoint = resultDF %>%
  filter(seasonNumber == max(seasonNumber)) %>%
  group_by(team) %>%
  summarise(point = 3 * sum(scored > conceded) + sum(scored == conceded) + 0.001 * sum(scored - conceded),
            finalAllTimeTgn = max(alltimetgn)) %>%
  arrange(desc(point)) %>%
  ungroup()

survivingTeamDF = previousSeasonPoint %>%
  slice(1:(n() - 3))

timeDownWeightCoef = 0.1
priorStrength = 10

myList = vector('list', 3 * nrow(survivingTeamDF))
for (ti in 1:nrow(survivingTeamDF)) {
  for (posi in 1:3) {
    myMainPos = c('def', 'mid', 'att')[posi]
    myList[[3 * (ti - 1) + posi]] = GetPlayerAppearanceMle(survivingTeamDF$team[ti], myMainPos, survivingTeamDF$finalAllTimeTgn[ti] + 1, 38, priorStrength, timeDownWeightCoef)
  }
  message('Have processed latest estimates for ', survivingTeamDF$team[ti])
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
