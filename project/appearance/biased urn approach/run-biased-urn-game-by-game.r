## this file should do everything wrt the biased urn stuff

### let's try to figure out what the most common formations are in general
source('project/appearance/biased urn approach/biased-urn-game-by-game-set-up-data.r')
# ok it will take 45 hours to run everybody ever
# so i say we run 1 team at a time and build up the picture gradually
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
  

tdwVec = c(0.033, 0.1, 0.33)
strVec = c(3.33, 10, 33)
tdwStrComboDF = expand.grid(timeDownWeightCoef = tdwVec, priorStrength = strVec)

for (j in 1:nrow(tdwStrComboDF)) {
  with(tdwStrComboDF[j,],
       GetAllPastMle('chelsea', timeDownWeightCoef = timeDownWeightCoef, priorStrength = priorStrength))
}

  myList = list('vector', nrow(myTeamTgn))
  for (tbi in 1:nrow(myTeamTgn)) {
    myList[[tbi]] = with(myTeamTgn[tbi,],
                         GetFormationProbabilityByTeam(team, alltimetgn, 38, historicFormationDF))
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
  
  myOutput = lst(myTeamEstimateDF,
                 myTeamTgnFormationDF,
                 myTeamExpectedPropGameDF)
  
  fileOut = paste0(DATAPATH, 'active_player/biased_urn_output/output_', myTeam, '_timedownweight_', timeDownWeightCoef, '_priorstrength_', priorStrength, '.rds')
  saveRDS(object = myOutput, file = fileOut)
  
  return(myOutput)
}

MeasureOutput = function(myTeam, timeDownWeightCoef, priorStrength) {
  fileIn = paste0(DATAPATH, 'active_player/biased_urn_output/output_', myTeam, '_timedownweight_', timeDownWeightCoef, '_priorstrength_', priorStrength, '.rds')
  myOutput = readRDS(fileIn)
  # now see how well we have predicted what happened
  myTeamExpectedPropGameDF = myOutput$myTeamExpectedPropGameDF
  
  subGbgdf2 = gbgdf2 %>%
    filter(team == myTeam) %>%
    lazy_left_join(myTeamExpectedPropGameDF,
                   c('team', 'alltimetgn', 'player'),
                   'expectedPropGame')
  

  calibplot(subGbgdf2$expectedPropGame * 94,
            subGbgdf2$minute,
            xlab = 'expected minutes',
            ylab = 'observed minutes')
  # prior looks too weak: when we're predicting 10 we need to rein it in, likewise when we predict 0
  # it's not bad though
  # but i think it's rubbish to start the blocks at the start of each season, that's where all the worst predictions are
  # not so bad now that I've done that, here are the new sqdiffs, they look good to me

  return(with(subGbgdf2, mean( (minute - expectedPropGame)^2, na.rm = TRUE)))
}

# tdw of 0.1 seems to be about right, hint that maybe we should increase the prior strength

# let's give that a bash
# 10 seems right.
# So, we've written tdw = 0.1, str = 10 to 'active_player' folder
# next is the fun bit, seeing how we need to adjust with fixture pileup / injury recovery / scoring / keeping clean sheet

# ok, so i've corrected the unkonw players have 0 expected minutes thing, the calibration plot is obviously better bu tmy sqdiff is worse, what the heck. let's look into that

timeDownWeightCoef = 0.1
priorStrength = 10
fileIn = paste0(DATAPATH, 'active_player/biased_urn_output/output_timedownweight_', timeDownWeightCoef, '_priorstrength_', priorStrength, '.rds')
myOutput = readRDS(fileIn)
# now see how well we have predicted what happened
newAllTeamBlockExpectedNumGame = myOutput$allTeamBlockExpectedNumGame

fileIn = paste0(DATAPATH, 'active_player/biased_urn_output/old crap might as well wipe/old_output_timedownweight_', timeDownWeightCoef, '_priorstrength_', priorStrength, '.rds')
myOutput = readRDS(fileIn)
# now see how well we have predicted what happened
oldAllTeamBlockExpectedNumGame = myOutput$allTeamBlockExpectedNumGame

blockObservedExpected = allTeamBlockObservedNumGame %>%
  left_join(newAllTeamBlockExpectedNumGame %>%
            rename(newExpectedNumGame = expectedNumGame),
            c('team', 'inBlock', 'player')) %>%
  left_join(oldAllTeamBlockExpectedNumGame %>%
              rename(oldExpectedNumGame = expectedNumGame),
            c('team', 'inBlock', 'player'))

par(mfrow = c(2, 1), mai = c(1, 1, .1, .1))
with(blockObservedExpected[sax,], calibplot(oldExpectedNumGame, observedNumGame))
with(blockObservedExpected[sax,], calibplot(newExpectedNumGame, observedNumGame))

# there is a lot of data in the lower end with the new version, maybe we could reoptimise the prior to help that
# although somehow we would rather our measure focussed on the players who appear more regularly, they are the ones we actually care about
# but somehow we need to define those points independently first
# but is that really true? it's surely bad to overlook a player who might well play

# and then run our snazzy thing that prevents over-predicting the dead certs
# which we seem to have corrected actually with this latest change to be fair
# don't like this new thing that much though. would rather run it more frequently and see if it corrects itself quickly
# at the moment, a new player can force the estimates of the other players down too much i think
# although the calibration plot appears to disagree
# still think it's odd that we run in blocks and allow this to become such a problem though
# let's do another version that runs game by game
