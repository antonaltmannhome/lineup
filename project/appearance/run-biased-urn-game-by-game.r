## this file should do everything wrt the biased urn stuff

### let's try to figure out what the most common formations are in general

source('c:/git/lineup/new-model-startup.r')
source('project/appearance/biased-urn-funct.R')
#library(doParallel)
#registerDoParallel()

fakeMaxTime = 1000L

# need to sort out seasonNumber. why do we need 1516 in seasonInfoDF?
seasonInfoDF$seasonNumber = match(seasonInfoDF$season, with(seasonInfoDF, sort(season[havegbg])))
resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')
resultDF = resultDF %>%
  mutate(alltimetgn = (seasonNumber - 1) * 38 + teamgamenumber)

uniqueSeasonTeamGameNumber = resultDF %>%
  distinct(seasonNumber, teamgamenumber) %>%
  arrange(seasonNumber, teamgamenumber)
uniqueSeasonTeamGameNumber$inBlock = NA
for (si in 1:length(unique(uniqueSeasonTeamGameNumber$seasonNumber))) {
  # first four games are in previous season's block, or no block if it's the first season
  currentSeasonIndex = with(uniqueSeasonTeamGameNumber, which(seasonNumber == si))
  initialBlock = cut(uniqueSeasonTeamGameNumber$teamgamenumber[currentSeasonIndex],
                     br = c(-0.5, seq(4.5, 34.5, 6), 38.5),
                     labels = FALSE) - 1
  adjustedBlock = initialBlock + (si - 1) * 6
  uniqueSeasonTeamGameNumber$inBlock[currentSeasonIndex] = adjustedBlock
}
resultDF = resultDF %>%
  left_join(uniqueSeasonTeamGameNumber,
            c('seasonNumber', 'teamgamenumber'))

gbgdf = lazy_left_join(gbgdf,
                       resultDF,
                       c('season', 'team', 'teamgamenumber'),
                       c('alltimetgn', 'inBlock'))

gbgdf2 = gbgdf %>%
  filter(!grepl('(injury|suspension)', startTime)) %>%
  mutate(startTime2 = ifelse(!startTime %in% c('U', 'UU'), startTime, as.character(fakeMaxTime)),
         endTime2 = ifelse(!startTime %in% c('U', 'UU'), endTime, as.character(fakeMaxTime))) %>%
  mutate(startTime2 = as.integer(startTime2),
         endTime2 = as.integer(endTime2))
# except we've got the problem that players sometimes officially start the match at eg 97 minutes.
# let's ceiling the endTime to be the greater of 94 and the biggest startTime of the match

gbgdf2$mainpos2 = with(gbgdf2, case_when(
  mainpos %in% c('D', 'DMC') ~ 'def',
  mainpos == 'M' ~ 'mid',
  mainpos %in% c('AM', 'FW') ~ 'att',
  TRUE ~ 'other'
))

#gbgdf2 = gbgdf2 %>%
#  select(season, seasonNumber, teamgamenumber, alltimetgn, team, player, startTime2, endTime2, minute, played, available, mainpos2, #inBlock)
gbgdf2 = gbgdf2 %>%
  select(alltimetgn, team, player, startTime2, endTime2, minute, played, available, mainpos2, inBlock)
  

historicFormationDF = MakeHistoricFormationDF(gbgdf2)

unTeamMainposTgn = gbgdf2 %>%
  distinct(team, mainpos2, alltimetgn) %>%
  group_by(team, mainpos2) %>%
  arrange(alltimetgn) %>%
  mutate(alltimetgnDelta = alltimetgn - lag(alltimetgn, 1),
         isValid = !is.na(alltimetgnDelta) & alltimetgnDelta == 1) %>%
  filter(mainpos2 %in% c('def', 'mid', 'att') & isValid) %>%
  select(team, mainpos2, alltimetgn) %>%
  ungroup()
unTeamTgn = unTeamMainposTgn %>%
  distinct(team, alltimetgn)

unTeamMainposBlock = gbgdf2 %>%
  distinct(team, mainpos2, inBlock) %>%
  group_by(team, mainpos2) %>%
  arrange(inBlock) %>%
  mutate(blockDelta = inBlock - lag(inBlock, 1),
         isValid = !is.na(blockDelta) & blockDelta == 1) %>%
  filter(mainpos2 %in% c('def', 'mid', 'att') & isValid) %>%
  select(team, mainpos2, inBlock) %>%
  ungroup()
unTeamBlock = unTeamMainposBlock %>%
  distinct(team, inBlock)


allTeamBlockObservedNumGame = gbgdf2 %>%
  lazy_left_join(resultDF, c('alltimetgn', 'team'), 'maxEndTime') %>%
  group_by(team, inBlock, player) %>%
  summarise(observedNumGame = sum( (endTime2 - startTime2) / maxEndTime))

allTeamTgnObservedNumGame = gbgdf2 %>%
  lazy_left_join(resultDF, c('alltimetgn', 'team'), 'maxEndTime') %>%
  group_by(team, alltimetgn, player) %>%
  summarise(observedNumGame = sum( (endTime2 - startTime2) / maxEndTime))

# ok it will take 45 hours to run everybody ever
# so i say we run 1 team at a time and build up the picture gradually
RunEntireLoop = function(myTeam, timeDownWeightCoef, priorStrength) {
  
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
                          GetExpectedNumGameByTeamBlock(team, alltimetgn, 8,
                                                        myTeamEstimateDF, myTeamTgnFormationDF))
  }
  myTeamExpectedNumGameDF = bind_rows(myList)
  
  myOutput = lst(myTeamEstimateDF,
                 myTeamTgnFormationDF,
                 myTeamExpectedPropPlay)
  
  fileOut = paste0(DATAPATH, 'active_player/biased_urn_output/output_', myTeam, '_timedownweight_', timeDownWeightCoef, '_priorstrength_', priorStrength, '.rds')
  saveRDS(object = myOutput, file = fileOut)
  
  return(myOutput)
}

if (FALSE) {
date()
outputT0.1P0.1 = RunEntireLoop(0.1, 0.1)
date()
saveRDS(object = outputT0.1P0.1, file = 'c:/temp/outputT0.1P0.1.rds')

tdwVec = c(0.033, 0.1, 0.33)
strVec = c(0.033, 0.1, 0.33)
tdwStrDF = expand.grid(tdw = tdwVec, str = strVec)
for (j in 1:nrow(tdwStrDF)) {
  fileOut = paste0('c:/temp/outputT', tdwStrDF$tdw[j], 'P', tdwStrDF$str[j], '.rds')
  if (!file.exists(fileOut)) {
    myOutput = RunEntireLoop(tdwStrDF$tdw[j], tdwStrDF$str[j])
    saveRDS(object = myOutput, file = fileOut)
  }
}

tdwVec = c(0.1)
strVec = c(1, 3.33, 10)
tdwStrDF = expand.grid(tdw = tdwVec, str = strVec)
for (j in 1:nrow(tdwStrDF)) {
  fileOut = paste0('c:/temp/outputT', tdwStrDF$tdw[j], 'P', tdwStrDF$str[j], '.rds')
  if (!file.exists(fileOut)) {
    myOutput = RunEntireLoop(tdwStrDF$tdw[j], tdwStrDF$str[j])
    saveRDS(object = myOutput, file = fileOut)
  }
}

}

MeasureOutput = function(timeDownWeightCoef, priorStrength) {
  fileIn = paste0(DATAPATH, 'active_player/biased_urn_output/output_timedownweight_', timeDownWeightCoef, '_priorstrength_', priorStrength, '.rds')
  myOutput = readRDS(fileIn)
  # now see how well we have predicted what happened
  allTeamBlockExpectedNumGame = myOutput$allTeamBlockExpectedNumGame
  
  blockObservedExpected = left_join(allTeamBlockExpectedNumGame,
                                    allTeamBlockObservedNumGame,
                                    c('team', 'inBlock', 'player'))
  
  calibplot(blockObservedExpected$expectedNumGame,
            blockObservedExpected$observedNumGame,
            xlab = 'expected number',
            ylab = 'observed number',
            xlim = c(0, 10), ylim = c(0, 10))
  # prior looks too weak: when we're predicting 10 we need to rein it in, likewise when we predict 0
  # it's not bad though
  # but i think it's rubbish to start the blocks at the start of each season, that's where all the worst predictions are
  # not so bad now that I've done that, here are the new sqdiffs, they look good to me
  print(blockObservedExpected %>%
          group_by(inBlock) %>%
          summarise(mean( (observedNumGame - expectedNumGame)^2)))
  
  return(with(blockObservedExpected, mean( (observedNumGame - expectedNumGame)^2)))
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
