SplitGame = function(myPlayer, myStartTime, myEndTime) {
  # for debugging:
  # subGbgdf = gbgdf2 %>% filter(team == 'mancity' & season == 1920 & teamgamenumber == 2 & mainpos %in% c('D', 'DMC')); myStartTime = subGbgdf %>% pull(startTime2); myEndTime = subGbgdf %>% pull(endTime2); myPlayer = subGbgdf %>% pull(player)
  unSubTime = sort(unique(c(myStartTime[myStartTime != fakeMaxTime],
                            myEndTime[!is.na(myEndTime) & myEndTime != fakeMaxTime])))
  if (length(unSubTime) == 0) {
    myRepeatedPlayedDF = data.frame(player = character(),
                                    matchSection = integer(),
                                    minDiff = integer(),
                                    startTime = integer(),
                                    endTime = integer(),
                                    lhTimeCutoff = integer(),
                                    rhTimeCutoff = integer(),
                                    played = integer())
  }
  if (length(unSubTime) > 0) {
    minDiff = diff(unSubTime)
    numInterval = length(unSubTime) - 1
    myRepeatedPlayedDF = data.frame(player = rep(myPlayer, numInterval),
                                    matchSection = rep(1:numInterval, rep(length(myStartTime), numInterval)),
                                    minDiff = rep(minDiff, rep(length(myStartTime), numInterval)),
                                    startTime = rep(myStartTime, numInterval),
                                    endTime = rep(myEndTime, numInterval),
                                    lhTimeCutoff = rep(head(unSubTime, -1), rep(length(myStartTime), numInterval)),
                                    rhTimeCutoff = rep(tail(unSubTime, -1), rep(length(myStartTime), numInterval)))
    myRepeatedPlayedDF$played = with(myRepeatedPlayedDF, as.integer(startTime <= lhTimeCutoff & endTime >= rhTimeCutoff))
  }
  
  return(myRepeatedPlayedDF)
}

# this function makes al permutations - but that Recall thing does funny stuff, so need a wrapper function to actually call it
InternalCalculateAllPermutation = function(x) {
  if (length(x) == 1) {
    allPermutationMatrix = matrix(x, nrow = 1, ncol = 1)
  }
  else {
    allPermutationMatrix = matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      allPermutationMatrix <- rbind(allPermutationMatrix, cbind(x[i], Recall(x[-i])))
    }
  }
  return(allPermutationMatrix)
}

CalculateAllPermutation = function(playedIndicator, playerNumber) {
  x = which(playedIndicator == 1)
  allPermutationByIndex = InternalCalculateAllPermutation(x)
  # but that references just 1:(number of players), we need our actual players referenced
  allPermutationByPlayerNumber = matrix(playerNumber[allPermutationByIndex],
                                        nrow = nrow(allPermutationByIndex),
                                        ncol = ncol(allPermutationByIndex))
  myList = list(allPermutationByPlayerNumber = allPermutationByPlayerNumber)
  return(myList)
}

# so firstly we need the permutations for all matches in advance

AABiasedUrnProb = function(allPermMatrix, activePlayerNumber, weight) {
  bottomLineMatrix = matrix(sum(weight[activePlayerNumber]), ncol = 1, nrow = nrow(allPermMatrix))
  if (ncol(allPermMatrix) > 1) {
    for (j in 2:ncol(allPermMatrix)) {
      bottomLineMatrix = cbind(bottomLineMatrix, bottomLineMatrix[,j-1] - weight[allPermMatrix[,j - 1]])
    }
  }
  topLineMatrix = matrix(weight[allPermMatrix], nrow = nrow(allPermMatrix), ncol = ncol(allPermMatrix))
  prob = sum(apply(topLineMatrix / bottomLineMatrix, 1, prod))
  return(prob)
}

CalculateAllMatchSectionLogLik = function(theta0, allPermutationDF, priorStrength) {
  theta = exp(theta0)
  allPermutationDF = allPermutationDF %>%
    rowwise() %>%
    mutate(byMatchSectionProb = AABiasedUrnProb(allPermutationByPlayerNumber, activePlayerList, theta),
           byMatchSectionLogLik = minDiff * log(byMatchSectionProb))
  dataLogLik = with(allPermutationDF, sum(timeDownWeight * byMatchSectionLogLik))
  
  penaltyLik = priorStrength * sum(theta0^2)
  totalLogLik = dataLogLik - penaltyLik
  if (FALSE) {
    print(theta)
    print(totalLogLik)
  }
  return(-totalLogLik)
}

MakeHistoricFormationDF = function(gbgdf2) {
  
  splitTeamDF = gbgdf2 %>%
    group_by(alltimetgn, team) %>%
    do(SplitGame(.$player, .$startTime2, .$endTime2)) %>%
    lazy_left_join(gbgdf2,
                   c('alltimetgn', 'team', 'player'),
                   'mainpos2') %>%
    ungroup()
  
  splitTeamDF = splitTeamDF %>%
    lazy_left_join(resultDF,
                   c('alltimetgn', 'team'),
                   'maxEndTime')
  
  sumInPosByMatch = splitTeamDF %>%
    group_by(alltimetgn, team, mainpos2) %>%
    summarise(sumInPos = sum(played * minDiff/maxEndTime))
  
  # is that quite what we want? i'd say not, i think we want the actual formation along with a proportion
  
  vertFormationByMatchSection = splitTeamDF %>%
    group_by(alltimetgn, team, mainpos2, matchSection) %>%
    summarise(sumInPos = sum(played),
              matchProportion = minDiff[1] / maxEndTime[1])
  
  # ok, now we spread that to get the formation
  formationByMatchSection = vertFormationByMatchSection %>%
    spread(key = mainpos2, value = sumInPos)
  
  # ok, awesome, now what we want is the frequency of each formation
  historicFormationDF = formationByMatchSection %>%
    group_by(att, def, mid, other) %>%
    summarise(sumMatchProportion = sum(matchProportion)) %>%
    ungroup()
  
  return(historicFormationDF)
}

GetPastResultGbgDF = function(myTeam, myTgn, windowSize, timeDownWeightCoef) {
  
  pastTeamResultDF = resultDF %>%
    filter(team == myTeam & alltimetgn < myTgn & alltimetgn >= myTgn - windowSize)
  
  maxAllTimeTgn = max(pastTeamResultDF$alltimetgn)
  pastTeamResultDF = pastTeamResultDF %>%
    mutate(timeDownWeight = exp(-timeDownWeightCoef * (maxAllTimeTgn - alltimetgn)))
  
  pastTeamGbgDF = gbgdf2 %>%
    semi_join(pastTeamResultDF, c('team', 'alltimetgn'))
  
  pastTeamGbgDF = semi_join(pastTeamGbgDF,
                            pastTeamGbgDF %>%
                              group_by(player) %>%
                              summarise(sumMinute = sum(minute)) %>%
                              filter(sumMinute > 10),
                            'player')
  
  return(lst(pastTeamResultDF, pastTeamGbgDF))
}


GetPlayerAppearanceMle = function(myTeam, myMainpos, myTgn, windowSize, priorStrength, timeDownWeightCoef) {
  
  dum = GetPastResultGbgDF(myTeam, myTgn, windowSize, timeDownWeightCoef)
  pastTeamResultDF = dum$pastTeamResultDF
  pastTeamGbgDF = dum$pastTeamGbgDF
  
  splitTeamDF = pastTeamGbgDF %>%
    filter(mainpos2 == myMainpos) %>%
    group_by(team, alltimetgn) %>%
    do(SplitGame(.$player, .$startTime2, .$endTime2))
  
  if (nrow(splitTeamDF) == 0) {
    myAppearanceMle = data.frame(team = character(),
                                 alltimetgn = integer(),
                                 mainpos2 = character(),
                                 player = character(),
                                 appearanceMle = numeric(),
                                 appearanceOdds = numeric())
  }
  
  if (nrow(splitTeamDF) > 0) {
    
    unMatchSection = splitTeamDF %>%
      group_by(alltimetgn, matchSection, minDiff) %>%
      summarise(numWhoPlayed = sum(played)) %>%
      ungroup() %>%
      mutate(logLik = NA)
    
    # but this leaves us with some match sections where no players play
    # which we don't want, so get rid
    unMatchSection = unMatchSection %>%
      filter(numWhoPlayed > 0)
    splitTeamDF = splitTeamDF %>%
      semi_join(unMatchSection, c('alltimetgn', 'matchSection'))
    
    # who has made the median number of appearances, they should have value zero
    myTotalMinutePlayed = splitTeamDF %>%
      group_by(player) %>%
      summarise(totalMinutePlayed = sum(endTime - startTime)) %>%
      arrange(totalMinutePlayed)
    
    referencePlayer = myTotalMinutePlayed$player[floor(0.5 * nrow(myTotalMinutePlayed)+1)]
    myUnPlayer = c(referencePlayer, setdiff(myTotalMinutePlayed$player, referencePlayer))
    splitTeamDF$playerNumber = match(splitTeamDF$player, myUnPlayer)
    
    allPermutationDF = splitTeamDF %>%
      group_by(alltimetgn, matchSection) %>%
      summarise(allPermutationByPlayerNumber = CalculateAllPermutation(played, playerNumber),
                activePlayerList = list(playerNumber),
                minDiff = minDiff[1]) %>%
      lazy_left_join(pastTeamResultDF, 'alltimetgn', 'timeDownWeight')
    
    theta0 = rep(0, length(myUnPlayer))
    
    maxInfo = nlm(CalculateAllMatchSectionLogLik, p = theta0,
                  allPermutationDF = allPermutationDF,
                  priorStrength = priorStrength,
                  stepmax = 5)
    
    myAppearanceMle = data.frame(team = myTeam, alltimetgn = myTgn, mainpos2 = myMainpos, player = myUnPlayer,
                                 appearanceMle = maxInfo$estimate,
                                 appearanceOdds = exp(maxInfo$estimate) / sum(exp(maxInfo$estimate)))
  }
  
  return(myAppearanceMle)
}

GetPastFormationForTeamBlock = function(pastTeamResultDF, pastTeamGbgDF) {
  numGameInBlock = nrow(pastTeamResultDF)
  pastFormationDF = pastTeamGbgDF %>%
    lazy_left_join(resultDF,
                   c('alltimetgn', 'team'),
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
                                 expand.grid(def = c(floorNP[mainpos2 == 'def'], floorNP[mainpos2 == 'def'] + 1),
                                             mid = c(floorNP[mainpos2 == 'mid'], floorNP[mainpos2 == 'mid'] + 1),
                                             att = c(floorNP[mainpos2 == 'att'], floorNP[mainpos2 == 'att'] + 1)))
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

MakeMatchFormationDF = function(myTeam, myTgn, formationDF, historicFormationDF) {
  
  # now we want a list of matches-formnation combos, but only the ones that are actually possible
  numPlayerAvailableByPos = gbgdf2 %>%
    filter(team == myTeam & alltimetgn == myTgn) %>%
    group_by(alltimetgn) %>%
    summarise(numAvailableDef = sum(mainpos2 == 'def' & available),
              numAvailableMid = sum(mainpos2 == 'mid' & available),
              numAvailableAtt = sum(mainpos2 == 'att' & available))
  
  matchFormationDF = merge(resultDF %>%
                             filter(team == myTeam & alltimetgn == myTgn) %>%
                             select(alltimetgn),
                           formationDF)
  
  matchFormationDF = matchFormationDF %>%
    left_join(numPlayerAvailableByPos,
              'alltimetgn') %>%
    mutate(isPossible = (numAvailableDef >= def &
                           numAvailableMid >= mid &
                           numAvailableAtt >= att))
  # the impossible ones need to have formationWgt set to 0 and the remaining ones summed up accordingly
  sumPossible = matchFormationDF %>%
    group_by(alltimetgn) %>%
    summarise(sumPossible = sum(formationWgt[isPossible]))
  # if the sumPossible is too small, then fill up with most likely possible historic ones
  
  sumPossibleCutOff = 0.2
  # this is the fiddly bit, chuck out the bad bits of matchFormationDF, bring in the historic bits in the right place
  matchFormationDF = left_join(matchFormationDF,
                               sumPossible,
                               'alltimetgn') %>%
    mutate(toRetainMatch = sumPossible > sumPossibleCutOff) %>%
    mutate_cond(toRetainMatch, formationWgt = formationWgt / sumPossible) %>%
    filter(toRetainMatch & isPossible) %>%
    select(-c(isPossible, sumPossible, toRetainMatch))
  
  # of what we're retaining, need to multiply the probabilities up so that they sum to 1
  
  if (any(sumPossible$sumPossible <= sumPossibleCutOff)) {
    # none of the proposed formations are possible it seems, so bolster with most likley historic formations that are possible
    historicAlternativeFormation = merge(numPlayerAvailableByPos,
                                         historicFormationDF) %>%
      mutate(isPossible = (numAvailableDef >= def &
                             numAvailableMid >= mid &
                             numAvailableAtt >= att)) %>%
      filter(isPossible) %>%
      group_by(alltimetgn) %>%
      arrange(desc(sumMatchProportion)) %>%
      slice(1)
    # NB there should always be at least one formation possible, namely the one that was used for the match that is giving problems
    matchFormationDF = bind_rows(matchFormationDF,
                                 anti_join(historicAlternativeFormation %>%
                                             select(-c(other, sumMatchProportion, isPossible)) %>%
                                             mutate(formationIndex = 1,
                                                    formationWgt = 1),
                                           matchFormationDF,
                                           'alltimetgn'))
  }
  
  return(matchFormationDF)  
}

GetFormationProbabilityByTeam = function(myTeam, myTgn, windowSize, historicFormationDF) {
  
  # filter out players who officially were here but never really played
  dum = GetPastResultGbgDF(myTeam, myTgn, windowSize, timeDownWeightCoef = 0)
  pastTeamResultDF = dum$pastTeamResultDF
  pastTeamGbgDF= dum$pastTeamGbgDF
  
  pastFormationDF = GetPastFormationForTeamBlock(pastTeamResultDF, pastTeamGbgDF)
  formationDF = GetFormationWeight(pastFormationDF)
  #numFormation = nrow(formationDF)
  matchFormationDF = MakeMatchFormationDF(myTeam, myTgn, formationDF, historicFormationDF)
  matchFormationDF$team = myTeam
  matchFormationDF$alltimetgn = myTgn
  
  return(matchFormationDF)
}

GetExpectedPropGameByTeam = function(myTeam, myTgn, myTeamEstimateDF, myTeamTgnFormationDF) {
  # so for each match, we need to calculate the prob of each available player playing for each plausible formation
  matchFormationDF = myTeamTgnFormationDF %>%
    filter(alltimetgn == myTgn)
  # loop for now of course
  myList = vector('list', 3)
  for (i in 1:3) {
    myMainpos = c('def', 'mid', 'att')[i]
    myPlayerMle = myTeamEstimateDF %>%
      filter(alltimetgn == myTgn & mainpos2 == myMainpos)
    # but we need to insert any players who didn't feature before, with mle = 0
    subGbgDF = gbgdf2 %>%
      filter(team == myTeam & alltimetgn == myTgn & available & mainpos2 == myMainpos) 
    newPlayer = anti_join(subGbgDF %>%
                            distinct(team, mainpos2, player),
                          myPlayerMle,
                          c('team', 'player'))
    if (nrow(newPlayer) > 0) {
      myPlayerMle = bind_rows(myPlayerMle,
                              newPlayer %>%
                                mutate(appearanceMle = 0))
      # NB you could be a bit more clever there, an expensive signing could have a higher mle than an unknown
      myPlayerMle$appearanceOdds = with(myPlayerMle, exp(appearanceMle)/ sum(exp(appearanceMle)))
    }
    
    subGbgDF = subGbgDF %>%
      lazy_left_join(myPlayerMle, 'player', 'appearanceOdds')
    
    subMatchFormationDF = matchFormationDF %>%
      select(formationIndex, myMainpos, formationWgt) %>%
      rename(numPlayerToSelect = myMainpos)
    
    repSubGbgDF = merge(subGbgDF,
                        subMatchFormationDF)

    repSubGbgDF = repSubGbgDF %>%
      group_by(formationIndex) %>%
      mutate(expectedPropGameGivenFormation = BiasedUrn::meanMWNCHypergeo(rep(1, n()), numPlayerToSelect[1], appearanceOdds)) %>%
      ungroup()
    
    myList[[i]] = repSubGbgDF %>%
      group_by(player) %>%
      summarise(expectedPropGame = sum(expectedPropGameGivenFormation * formationWgt)) %>%
      mutate(team = myTeam, mainpos2 = myMainpos, alltimetgn = myTgn)
  }
  
  # ok so that all works, i believe we need to loop by team/block
  expectedNumGameDF = bind_rows(myList)
  
  return(expectedNumGameDF)
}

GetNextGamePropByTeam = function(myTeam, myLatestTeamEstimateDF, myLatestTeamTgnFormationDF, myPlayerDF) {
  # so for each match, we need to calculate the prob of each available player playing for each plausible formation
  # loop for now of course
  myList = vector('list', 3)
  for (i in 1:3) {
    myMainpos = c('def', 'mid', 'att')[i]
    myPlayerMle = myLatestTeamEstimateDF %>%
      filter(mainpos2 == myMainpos)
    
    mySubPlayerDF = inner_join(myPlayerDF,
                            myPlayerMle %>%
                              select(player, appearanceMle),
                            'player') %>%
      replace_na(list(appearanceMle = 0)) %>%
      mutate(appearanceOdds = exp(appearanceMle) / sum(exp(appearanceMle)))
 
    subMatchFormationDF = myLatestTeamTgnFormationDF %>%
      select(formationIndex, myMainpos, formationWgt) %>%
      rename(numPlayerToSelect = myMainpos)
    
    repPlayerDF = merge(mySubPlayerDF,
                        subMatchFormationDF)
    
    repPlayerDF = repPlayerDF %>%
      group_by(formationIndex) %>%
      mutate(expectedPropGameGivenFormation = BiasedUrn::meanMWNCHypergeo(rep(1, n()), numPlayerToSelect[1], appearanceOdds)) %>%
      ungroup()
    
    myList[[i]] = repPlayerDF %>%
      group_by(player) %>%
      summarise(expectedPropGame = sum(expectedPropGameGivenFormation * formationWgt)) %>%
      mutate(team = myTeam, mainpos2 = myMainpos)
  }
  
  # ok so that all works, i believe we need to loop by team/block
  expectedNumGameDF = bind_rows(myList)
  
  return(expectedNumGameDF)
}
