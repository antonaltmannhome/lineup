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
                       c('inBlock', 'alltimetgn'))

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
  select(alltimetgn, team, player, startTime2, endTime2, minute, played, available, mainpos2,
         inBlock)
  

historicFormationDF = MakeHistoricFormationDF(gbgdf2)

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

RunEntireLoop = function(timeDownWeightCoef, priorStrength) {
  
  myList = vector('list', nrow(unTeamMainposBlock))
  for(tbpi in 1:nrow(unTeamMainposBlock)) {
    myList[[tbpi]] = with(unTeamMainposBlock[tbpi,],
                          GetPlayerAppearanceMleByBlock(team, mainpos2, inBlock, 4,
                                                        priorStrength, timeDownWeightCoef))
    if ( (tbpi %% 10) == 0) {
      message('Have calculated ', tbpi, ' out of ', nrow(unTeamMainposBlock), ' team/block/position blocks so far')
    }
  }
  allTeamSeasonMainposEstimateDF = bind_rows(myList)
  
  myList = list('vector', nrow(unTeamBlock))
  for (tbi in 1:nrow(unTeamBlock)) {
    myList[[tbi]] = with(unTeamBlock[tbi,],
                         GetFormationProbabilityByTeamBlock(team, inBlock, 4, historicFormationDF))
  }
  allTeamBlockFormationDF = bind_rows(myList)
  
  myList = list('vector', nrow(unTeamBlock))
  for (tbi in 1:nrow(unTeamBlock)) {
    myList[[tbi]] =  with(unTeamBlock[tbi,],
                          GetExpectedNumGameByTeamBlock(team, inBlock, 4,
                                                        allTeamSeasonMainposEstimateDF, allTeamBlockFormationDF))
  }
  allTeamBlockExpectedNumGame = bind_rows(myList)
  
  return(lst(allTeamSeasonMainposEstimateDF,
             allTeamBlockFormationDF,
             allTeamBlockExpectedNumGame))
  
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

# now compare to what actually happened, which should be easy i think
allTeamBlockObservedNumGame = gbgdf2 %>%
  lazy_left_join(resultDF, c('alltimetgn', 'team'), 'maxEndTime') %>%
  group_by(team, inBlock, player) %>%
  summarise(observedNumGame = sum( (endTime2 - startTime2) / maxEndTime))

MeasureOutput = function(tdw, str) {
  myOutput = readRDS(paste0('c:/temp/outputT', tdw, 'P', str, '.rds'))
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
