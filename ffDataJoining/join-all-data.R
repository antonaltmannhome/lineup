### so we've got all the raw data, just got to join it up. we need the following:
### file that runs through the standard procedure for each week
source('c:/git/lineup/ffDataJoining/data-joining-startup.r')

if (FALSE) {
  ffDataJoiningPath = paste0(USERPATH, 'ffDataJoining')
  # usethis::create_package(ffDataJoiningPath)
  #devtools::install(ffDataJoiningPath)
  devtools::load_all(ffDataJoiningPath)
}

# results, along with past odds (EDIT no this is not part of this)
# fixtures, along with any available future odds (see above)
# player/results, which is the merge of soccerway and whoscored
# current players, which includs the fantast football prices as well

## let's focus on player/results first. that requires matching up of soccerway and whoscored data
## the first thing we need is to actualyl combine all the week by week cumulative whoscored data into a single file
## this function does that

ffDataJoining:::CombineWhoscoredGameByGameDataForSeason(currentseason)

gbgdf = ffDataJoining:::LoadGbgWithoutAppearanceDF()

## but that doesn't have any player ids attached to it. we want to join that to appearancedf in any case
appearanceDF = ffDataJoining:::LoadSoccerwayData()

## so we firstly update all the files on disk giving the details of the player maps:
ffDataJoining:::UpdateSoccerwayWhoscoredPlayerMap(appearanceDF)

### and having done that, we now load in all of the player map data:
### NOTE 07/12/2019: problem with astonvilla / ezri konsa ngoyo 
playerResultDF = ffDataJoining:::MergeSoccerwayWhoscoredGameData(appearanceDF, gbgdf)
# note that writes the appearanceDF file to disk, it's what we will use for analysis

### that will be part of a big list that gets made into an RDS at the end of this. awesome df let's face it

### next step, loading/updating the ff player <-> playerid map

if (FALSE) {
  # is it the start of the season? then run this to set up the player maps
  ffDataJoining:::InitialiseCurrentSeasonPlayerId(playerResultDF)
}

ffDataJoining:::UpdateCurrentSeasonPlayerId(playerResultDF)
ffDataJoining:::MatchFFPlayerData(interactive = TRUE)

### next, odds
### actually, don't do this, because there's a fair chance you'll want to redo it more recently than the data stripping
# dum = ffDataJoining:::AlignOddsWithResultsAndFixtures()
