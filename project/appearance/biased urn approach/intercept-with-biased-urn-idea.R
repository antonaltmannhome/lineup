# what about this idea of having an intercept to stop over-predicting the prob of ever presents playing? that might be a better way than a prior of fixing that problem

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

tdw = 0.1
str = 10

myOutput = readRDS(paste0('c:/temp/outputT', tdw, 'P', str, '.rds'))
# now see how well we have predicted what happened
allTeamSeasonMainposEstimateDF = myOutput$allTeamSeasonMainposEstimateDF
allTeamBlockExpectedNumGame = myOutput$allTeamBlockExpectedNumGame
allTeamBlockFormationDF = myOutput$allTeamBlockFormationDF


GetExpectedNumGameByTeamBlock = function(myTeam, myBlock, blockWindowSize, 
                                         allTeamSeasonMainposEstimateDF, allTeamBlockFormationDF) {
  # so for each match, we need to calculate the prob of each available player playing for each plausible formation
  matchFormationDF = allTeamBlockFormationDF %>%
    filter(team == myTeam & inBlock == myBlock)
  # loop for now of course
  myList = vector('list', 3)
  for (i in 1:3) {
    myMainpos = c('def', 'mid', 'att')[i]
    myPlayerMle = allTeamSeasonMainposEstimateDF %>%
      filter(team == myTeam & inBlock == myBlock & mainpos2 == myMainpos)
    
    subGbgDF = gbgdf2 %>%
      filter(team == myTeam & inBlock == myBlock & available & mainpos2 == myMainpos) %>%
      lazy_left_join(myPlayerMle, 'player', 'appearanceOdds') %>%
      replace_na(list(appearanceOdds = 0.0001))
    
    subMatchFormationDF = matchFormationDF %>%
      select(alltimetgn, formationIndex, myMainpos, formationWgt) %>%
      rename(numPlayerToSelect = myMainpos)
    
    repSubGbgDF = full_join(subGbgDF,
                            subMatchFormationDF,
                            'alltimetgn')
    
    repSubGbgDF = repSubGbgDF %>%
      group_by(alltimetgn, formationIndex) %>%
      mutate(probPlay = BiasedUrn::meanMWNCHypergeo(rep(1, n()), numPlayerToSelect[1], appearanceOdds)) %>%
      ungroup()
    
    myList[[i]] = repSubGbgDF %>%
      group_by(player) %>%
      summarise(expectedNumGame = sum(probPlay * formationWgt)) %>%
      mutate(team = myTeam, mainpos2 = myMainpos, inBlock = myBlock)
  }
  
  # ok so that all works, i believe we need to loop by team/block
  expectedNumGameDF = bind_rows(myList)
  
  return(expectedNumGameDF)
}

# think i've cracked it although the ansswers it's producing are wrong, let's investigate

.MapIndexToTF = function(y, numAvailable) {
  (1:numAvailable) %in% y
}
.ProduceComboForNumSelected = function(x, numAvailable) {
  t(apply(t(combn(numAvailable, x)), 1, .MapIndexToTF, numAvailable = numAvailable))
}

FiddleAround = function(weight, numSelected, restProb) {
  # can only rest as many players as you can afford to so
  numAvailable = length(weight)
  splitPossibleCombo = purrr::map(numSelected:numAvailable,
                                  .ProduceComboForNumSelected, numAvailable = numAvailable)
  possibleCombo = do.call(rbind, splitPossibleCombo)
  
  # cool, then we've got to assign the probs and multiply it all out, will do that next
  playProbComboMat = ifelse(possibleCombo, 1 - restProb, restProb)
  dum = apply(playProbComboMat, 1, prod)
  combiProb = dum / sum(dum)
  
  playProbForCombo = t(apply(possibleCombo, 1, function(x) {
    BiasedUrn::meanMWNCHypergeo(rep(1, numAvailable), numSelected, ifelse(x, weight, 0))
    }))
  
  playProbForComboWeightedByProb = playProbForCombo * combiProb
  
  playProb = colSums(playProbForComboWeightedByProb)
  
  return(playProb)
}

# let's try to actually implement that then
CheckExample = function(myTeam, myBlock, myMainpos, restProb) {
  matchFormationDF = allTeamBlockFormationDF %>%
    filter(team == myTeam & inBlock == myBlock)
  myPlayerMle = allTeamSeasonMainposEstimateDF %>%
    filter(team == myTeam & inBlock == myBlock & mainpos2 == myMainpos)
  
  subGbgDF = gbgdf2 %>%
    filter(team == myTeam & inBlock == myBlock & available & mainpos2 == myMainpos) %>%
    lazy_left_join(myPlayerMle, 'player', 'appearanceOdds') %>%
    replace_na(list(appearanceOdds = 0.0001))
  
  subMatchFormationDF = matchFormationDF %>%
    select(alltimetgn, formationIndex, myMainpos, formationWgt) %>%
    rename(numPlayerToSelect = myMainpos)
  
  repSubGbgDF = full_join(subGbgDF,
                          subMatchFormationDF,
                          'alltimetgn')
  
  repSubGbgDF = repSubGbgDF %>%
    group_by(alltimetgn, formationIndex) %>%
    #mutate(probPlay = BiasedUrn::meanMWNCHypergeo(rep(1, n()), numPlayerToSelect[1], appearanceOdds)) %>%
    mutate(probPlay = FiddleAround(appearanceOdds, numPlayerToSelect[1], restProb)) %>%
    ungroup()
  
  playerProbByGame = repSubGbgDF %>%
    group_by(alltimetgn, player) %>%
    summarise(expectedNumGame = sum(probPlay * formationWgt)) %>%
    mutate(team = myTeam, mainpos2 = myMainpos, inBlock = myBlock)
  
  return(playerProbByGame)
}

# ok, well i think i've only gone and cracked it
# still not sure about that replace_na with 0.00001 thing though, isn't it better to put the prior value in instead?
