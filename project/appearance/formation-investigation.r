### let's try to figure out what the most common formations are in general

source('c:/git/lineup/new-model-startup.r')
source('project/appearance/biased-urn-funct.R')
library(doParallel)
registerDoParallel()

fakeMaxTime = 1000L

# need to sort out seasonNumber. why do we need 1516 in seasonInfoDF?
seasonInfoDF$seasonNumber = match(seasonInfoDF$season, with(seasonInfoDF, sort(season[havegbg])))
resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')

numBlockWithinSeason = 4
resultDF$inBlock = as.integer(with(resultDF, (seasonNumber - 1) * numBlockWithinSeason +
                                     cut(teamgamenumber, br = seq(0.5, 38.5, le = 5), labels = FALSE)))

gbgdf = lazy_left_join(gbgdf,
                       resultDF,
                       c('season', 'team', 'teamgamenumber'),
                       'inBlock')

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

gbgdf2 = gbgdf2 %>%
  select(season, seasonNumber, teamgamenumber, team, player, startTime2, endTime2, minute, played, available, mainpos2, inBlock)

# now, we will want to run it e.g 4 times a season, on every occasion predicting the next quarter of the season
# i think that's the level we would want it at

# but in fact, predicting isn't trivial, that's a job unto itself
# because you need to provide estimate of how many att/mids etc will play
# so let's not do the downweight just yet, let's try to predict minutes for a team for a block of the season

splitTeamDF = gbgdf2 %>%
  group_by(season, team, teamgamenumber) %>%
  do(SplitGame(.$player, .$startTime2, .$endTime2)) %>%
  lazy_left_join(gbgdf2,
                  c('season', 'team', 'teamgamenumber', 'player'),
                 'mainpos2') %>%
  ungroup()

splitTeamDF = splitTeamDF %>%
  lazy_left_join(resultDF,
                 c('season', 'team', 'teamgamenumber'),
                 'maxEndTime')

sumInPosByMatch = splitTeamDF %>%
  group_by(season, team, teamgamenumber, mainpos2) %>%
  summarise(sumInPos = sum(played * minDiff/maxEndTime))

# is that quite what we want? i'd say not, i think we want the actual formation along with a proportion

vertFormationByMatchSection = splitTeamDF %>%
  group_by(season, team, teamgamenumber, mainpos2, matchSection) %>%
  summarise(sumInPos = sum(played),
            matchProportion = minDiff[1] / maxEndTime[1])

# ok, now we spread that to get the formation
formationByMatchSection = vertFormationByMatchSection %>%
  spread(key = mainpos2, value = sumInPos)

# ok, awesome, now what we want is the frequency of each formation
historicFormationCount = formationByMatchSection %>%
  group_by(att, def, mid, other) %>%
  summarise(sumMatchProportion = sum(matchProportion))

# so by default how about we continue to do what we've done, but if we have the problem of an impossible formation given the available players, we instead predict for the highest weighted common formation?

# ok, the previous file is a complete mess, let's zero in on the problem


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

GetPastFormationForTeamBlock = function(pastGbgDF) {
  numGameInBlock = nrow(pastGbgDF %>% distinct(season, teamgamenumber))
  pastFormationDF = pastGbgDF %>%
    lazy_left_join(resultDF,
                   c('season', 'team', 'teamgamenumber'),
                   'maxEndTime') %>%
    group_by(mainpos2) %>%
    summarise(numPlayerInPos = sum( (endTime2 - startTime2) / maxEndTime) / numGameInBlock) %>%
    mutate(floorNP = floor(numPlayerInPos),
           ceilingNP = ceiling(numPlayerInPos))
  # get rid of gk and sort the normal way
  pastFormationDF = pastFormationDF %>%
    filter(mainpos2 %in% c('def', 'mid', 'att')) %>%
    arrange(match(mainpos2, c('def', 'mid', 'att')))
  
  return(pastFormationDF)
}

FormationLikFunct = function(theta, pastFormationDF, validFormationMatrix) {
  wgtVec = c(1, exp(theta))
  calcFormation = colSums(validFormationMatrix * wgtVec)/sum(wgtVec)
  sqDiff = sum( (pastFormationDF$numPlayerInPos - calcFormation) ^ 2)
  return(sqDiff)
}

GetFormationWeight = function(pastFormationDF) {
  
  expandedPastFormationDF = with(pastFormationDF,
                                 expand.grid(def = c(floorNP[mainpos2 == 'def'], ceilingNP[mainpos2 == 'def']),
                                             mid = c(floorNP[mainpos2 == 'mid'], ceilingNP[mainpos2 == 'mid']),
                                             att = c(floorNP[mainpos2 == 'att'], ceilingNP[mainpos2 == 'att'])))
  validFormationMatrix = expandedPastFormationDF[which(rowSums(expandedPastFormationDF) == 10),]
  
  # but then we need to weighted combo that sums to the average
  maxInfo = nlm(FormationLikFunct, p = c(0, 0),
                pastFormationDF = pastFormationDF,
                validFormationMatrix = validFormationMatrix,
                stepmax = 3)
  formationWeightDF = tibble(formationIndex = 1:nrow(validFormationMatrix),
                             formationWgt = c(1, exp(maxInfo$est)) / sum(c(1, exp(maxInfo$est))))
  
  formationDF = as_tibble(validFormationMatrix) %>%
    mutate(formationIndex = 1:n()) %>%
    select(formationIndex, everything()) %>%
    left_join(formationWeightDF, 'formationIndex')
  
  return(formationDF)
}

MakeMatchFormationDF = function(myTeam, myBlock, formationDF) {
  
  # now we want a list of matches-formnation combos, but only the ones that are actually possible
  blockNumPlayerAvailableByPos = gbgdf2 %>%
                                    filter(team == myTeam & inBlock == myBlock) %>%
                                    group_by(season, teamgamenumber) %>%
                                    summarise(numAvailableDef = sum(mainpos2 == 'def' & available),
                                              numAvailableMid = sum(mainpos2 == 'mid' & available),
                                              numAvailableAtt = sum(mainpos2 == 'att' & available))
  
  matchFormationDF = merge(resultDF %>%
                             filter(team == myTeam & inBlock == myBlock) %>%
                             select(season, teamgamenumber),
                           formationDF)
  
  matchFormationDF = matchFormationDF %>%
    left_join(blockNumPlayerAvailableByPos,
              c('season', 'teamgamenumber')) %>%
    mutate(isPossible = (numAvailableDef >= def &
                           numAvailableMid >= mid &
                           numAvailableAtt >= att))
  # the impossible ones need to have formationWgt set to 0 and the remaining ones summed up accordingly
  sumPossible = matchFormationDF %>%
    group_by(season, teamgamenumber) %>%
    summarise(sumPossible = sum(formationWgt[isPossible]))
  # if the sumPossible is too small, then fill up with most likely possible historic ones
  
  sumPossibleCutOff = 0.2
  # this is the fiddly bit, chuck out the bad bits of matchFormationDF, bring in the historic bits in the right place
  matchFormationDF = left_join(matchFormationDF,
                                sumPossible,
                                c('season', 'teamgamenumber')) %>%
                      mutate(toRetainMatch = sumPossible > sumPossibleCutOff) %>%
                      mutate_cond(toRetainMatch, formationWgt = formationWgt / sumPossible) %>%
                      filter(toRetainMatch & isPossible) %>%
                      select(-c(isPossible, sumPossible, toRetainMatch))

  # of what we're retaining, need to multiply the probabilities up so that they sum to 1
  
  if (any(sumPossible$sumPossible <= sumPossibleCutOff)) {
    # none of the proposed formations are possible it seems, so bolster with most likley historic formations that are possible
    historicAlternativeFormation = merge(blockNumPlayerAvailableByPos,
                                       historicFormationCount) %>%
      mutate(isPossible = (numAvailableDef >= def &
                           numAvailableMid >= mid &
                           numAvailableAtt >= att)) %>%
      filter(isPossible) %>%
      group_by(season, teamgamenumber) %>%
      arrange(desc(sumMatchProportion)) %>%
      slice(1)
    # NB there should always be at least one formation possible, namely the one that was used for the match that is giving problems
    matchFormationDF = bind_rows(matchFormationDF,
                                 anti_join(historicAlternativeFormation %>%
                                             select(-c(other, sumMatchProportion, isPossible)) %>%
                                             mutate(formationIndex = 1,
                                                    formationWgt = 1),
                                           matchFormationDF,
                                           c('season', 'teamgamenumber')))
  }
  
  return(matchFormationDF)  
}

GetFormationProbabilityByTeamBlock = function(myTeam, myBlock, blockWindowSize) {
  
  # filter out players who officially were here but never really played
  pastGbgDF = gbgdf2 %>%
    filter(team == myTeam & inBlock < myBlock & inBlock >= myBlock - blockWindowSize)
  pastGbgDF = semi_join(pastGbgDF,
                        pastGbgDF %>%
                          group_by(player) %>%
                          summarise(sumMinute = sum(minute)) %>%
                          filter(sumMinute > 10),
                        'player')
  
  pastFormationDF = GetPastFormationForTeamBlock(pastGbgDF)
  formationDF = GetFormationWeight(pastFormationDF)
  #numFormation = nrow(formationDF)
  matchFormationDF = MakeMatchFormationDF(myTeam, myBlock, formationDF)
  matchFormationDF$team = myTeam
  matchFormationDF$inBlock = myBlock
  
  return(matchFormationDF)
}

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
      select(season, teamgamenumber, formationIndex, myMainpos, formationWgt) %>%
      rename(numPlayerToSelect = myMainpos)
    
    repSubGbgDF = full_join(subGbgDF,
                            subMatchFormationDF,
                            c('season', 'teamgamenumber'))
    
    if (FALSE) {
      repSubGbgDF = subGbgDF %>%
        lazy_left_join(myPlayerMle, 'player', 'appearanceOdds') %>%
        replace_na(list(appearanceOdds = 0.0001)) %>%
        slice(rep(1:n(), each = numFormation)) %>%
        mutate(formationIndex = rep(1:numFormation, n()/numFormation)) %>%
        left_join(formationDF %>%
                    gather(mainpos2, numPlayerInPos, -c(formationIndex, formationWgt)),
                  c('formationIndex', 'mainpos2')) %>%
        left_join(subGbgDF %>%
                    group_by(season, teamgamenumber) %>%
                    summarise(sumAvailable = n()),
                  c('season', 'teamgamenumber')) %>%
        mutate(isPossible = sumAvailable >= numPlayerInPos)
      # but are there any impossible formations? eg 5 defender slots but only 4 defenders available?
      # if so, need to set its formationWgt to zero and compensate with the remaining ones
      scaleUpQuant = repSubGbgDF %>%
        group_by(season, teamgamenumber) %>%
        summarise(toScaleup = 1/sum(formationWgt[isPossible]))
    }
    
    repSubGbgDF = repSubGbgDF %>%
      group_by(teamgamenumber, formationIndex) %>%
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

allTeamSeasonMainposEstimateDF = read_csv('c:/temp/temp-player-appearance-mle.csv', col_types = list(
  team = col_character(),
  inBlock = col_integer(),
  mainpos2 = col_character(),
  player = col_character(),
  appearanceMle = col_double(),
  appearanceOdds = col_double()
))

# the error:
GetExpectedNumGameByTeamBlock('everton', 5, 4, allTeamSeasonMainposEstimateDF)
# now, i think we could have anticipated that
# we can start by doing all the formations separately in advance

allTeamBlockFormationDF = foreach (tspi=1:nrow(unTeamBlock),
                                          .combine = rbind,
                                          .packages=c("dplyr")) %dopar% {
                                            with(unTeamBlock[tspi,],
                                                 GetFormationProbabilityByTeamBlock(team, inBlock, 4))
                                          }

date()
allTeamBlockExpectedNumGame = foreach (tspi=1:nrow(unTeamBlock),
                                       .combine = rbind,
                                       .packages=c("dplyr", "tidyr")) %dopar% {
                                         with(unTeamBlock[tspi,],
                                              GetExpectedNumGameByTeamBlock(team, inBlock, 4,
                                                                  allTeamSeasonMainposEstimateDF, allTeamBlockFormationDF))
                                       }
date()

# ok this runs all the way through. now needs a tidy up, then make it possible to compare to what actually happened, then be able to tweak downwieght and prior strength
