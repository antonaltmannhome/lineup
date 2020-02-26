### went down a few blind alleys with previous approach, let's start again with another idea

source('c:/git/lineup/new-model-startup.r')

# and also, if we have enough data for a player throughout a season, do we really have to make it all oos? yes, because we want to knwo the value of a goal at predicting goals
gbgdf$nonpengoal = with(gbgdf, goal - penaltyscored)
gbgdf$ontargetbutmiss = with(gbgdf, ont - goal)
gbgdf$offtnotbar = with(gbgdf, offt - bar)

# let's please have camelcase vars
gbgdf$nonPenaltyGoal = with(gbgdf, goal - penaltyscored)
gbgdf$onTargetNotGoal = with(gbgdf, ont - goal)
gbgdf$offTargetNotBar = with(gbgdf, offt - bar)
gbgdf$shotInBox = gbgdf$shotib
gbgdf$shotOutBox = gbgdf$shotoob
gbgdf$shot6Yard = gbgdf$shot6yd

# this shows we've got pretty much every goal correctly accounted for:
with(gbgdf, table(shotOutBox + shotInBox + shot6Yard - (offTargetNotBar + onTargetNotGoal + block + bar+ nonPenaltyGoal + penaltyscored)))

gbgdf = ffModel:::ImputeMissingMatchOdds(gbgdf, resultDF)

seasonTotalDF = gbgdf %>%
  group_by(season, team, player) %>%
  summarise(sumMinute = sum(minute),
            sumNonPenGoal = sum(nonPenaltyGoal),
            sumShotInBox = sum(shotInBox),
            sumShotOutBox = sum(shotOutBox),
            sumShot6Yard = sum(shot6Yard),
            sumOffTargetNotBar = sum(offTargetNotBar),
            sumOnTargetNotGoal = sum(onTargetNotGoal),
            sumBar = sum(bar),
            sumBlock = sum(block),
            sumNonPenaltyGoal = sum(nonPenaltyGoal),
            sumGame = n(),
            sumTeamOddsEScored = sum(teamoddsescored))

oosDF = gbgdf %>%
  left_join(seasonTotalDF, c('season', 'team', 'player')) %>%
  group_by(season, team, player) %>%
  mutate(oosMinute = sumMinute - minute,
         oosNonPenGoal = sumNonPenaltyGoal - nonPenaltyGoal,
         oosShotInBox = sumShotInBox - shotInBox,
         oosShotOutBox = sumShotOutBox - shotOutBox,
         oosShot6Yard = sumShot6Yard - shot6Yard,
         oosOffTargetNotBar = sumOffTargetNotBar - offTargetNotBar,
         oosOnTargetNotGoal = sumOnTargetNotGoal - onTargetNotGoal,
         oosBar = sumBar - bar,
         oosBlock = sumBlock - block,
         oosNonPenaltyGoal = sumNonPenaltyGoal - nonPenaltyGoal,
         oosTeamOddsEScored = sumTeamOddsEScored - teamoddsescored,
         oosGame = sumGame - 1,
         oosMeanEScored = oosTeamOddsEScored / oosGame)

# restrict to players who've played at least 10 games in the season, and get rid of gks
subOosDF = oosDF %>%
  filter(oosMinute > 900 &
           mainpos %in% c('AM', 'D', 'DMC', 'FW', 'M')) %>%
  mutate(gameProp = minute / 94,
         oosSumGameProp = oosMinute / 94,
         ubiPropAdj = gameProp * teamoddsescored / oosMeanEScored)

## so the idea now is to map each quantity onto the goals scale, then allow it to be mixed in


AccuracyLikFunct1 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1])
  overallPriorStrength = exp(theta[2])
  nonPenaltyGoalWeight = exp(theta[3])
  subOosDF$postTopLine = with(subOosDF, overallMeanRate * overallPriorStrength + nonPenaltyGoalWeight * oosNonPenaltyGoal)
  subOosDF$postBottomLine = with(subOosDF, overallPriorStrength + oosSumGameProp * nonPenaltyGoalWeight)
  subOosDF$eGoal = with(subOosDF, gameProp * postTopLine / postBottomLine)

  if (runMode == 'max') {
    subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
    sumLogLik = sum(subOosDF$logLik)
    toReturn = -sumLogLik
  }
  if (runMode == 'fit') {
    toReturn = subOosDF$eGoal
  }
  return(toReturn)
}

maxInfo1 = nlm(AccuracyLikFunct1, p = c(log(0.05), log(10), 0), stepmax = 2)
subOosDF$eGoal1 = AccuracyLikFunct1(maxInfo1$est, runMode = 'fit')

# that's our benchmark. now let's add, i dunno, shots in the box

# but we can quickly do better by using the opposition strength
# so it's opposition strength compared to normal opp strength that we care about

AccuracyLikFunct2 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1])
  overallPriorStrength = exp(theta[2])
  nonPenaltyGoalWeight = exp(theta[3])
  subOosDF$postTopLine = with(subOosDF, overallMeanRate * overallPriorStrength + nonPenaltyGoalWeight * oosNonPenaltyGoal)
  subOosDF$postBottomLine = with(subOosDF, overallPriorStrength + oosSumGameProp * nonPenaltyGoalWeight)
  subOosDF$eGoal = with(subOosDF, ubiPropAdj * postTopLine / postBottomLine)
  
  if (runMode == 'max') {
    subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
    sumLogLik = sum(subOosDF$logLik)
    toReturn = -sumLogLik
  }
  if (runMode == 'fit') {
    toReturn = subOosDF$eGoal
  }
  return(toReturn)
}

maxInfo2 = nlm(AccuracyLikFunct2, p = c(log(0.05), log(10), 0), stepmax = 2)
subOosDF$eGoal2 = AccuracyLikFunct2(maxInfo2$est, runMode = 'fit')


AccuracyLikFunct3 = function(theta) {
  overallMeanRate = exp(theta[1])
  overallPriorStrength = exp(theta[2])
  nonPenaltyGoalWeight = exp(theta[3])
  shotInBoxWeight = exp(theta[4])
  subOosDF$postTopLine = with(subOosDF, overallMeanRate * overallPriorStrength + nonPenaltyGoalWeight * oosNonPenaltyGoal + shotInBoxWeight * oosShotInBox)
  subOosDF$postBottomLine = with(subOosDF, overallPriorStrength + oosSumGameProp)
  subOosDF$eGoal = with(subOosDF, ubiPropAdj * postTopLine / postBottomLine)
  subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
  sumLogLik = sum(subOosDF$logLik)
  
  return(-sumLogLik)
}

maxInfo3 = nlm(AccuracyLikFunct3, p = c(log(0.05), log(10), 0, 0), stepmax = 2)
# that looks sensible to me


AccuracyLikFunct4 = function(theta) {
  overallMeanRate = exp(theta[1])
  overallPriorStrength = exp(theta[2])
  nonPenaltyGoalWeight = exp(theta[3])
  shotInBoxWeight = exp(theta[4])
  shotOutBoxWeight = exp(theta[5])
  subOosDF$postTopLine = with(subOosDF, ubiPropAdj * overallMeanRate * overallPriorStrength + nonPenaltyGoalWeight * oosNonPenaltyGoal + shotInBoxWeight * oosShotInBox + shotOutBoxWeight * oosShotOutBox)
  subOosDF$postBottomLine = with(subOosDF, overallPriorStrength + oosSumGameProp)
  subOosDF$eGoal = with(subOosDF, ubiPropAdj * postTopLine / postBottomLine)
  subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
  sumLogLik = sum(subOosDF$logLik)
  
  return(-sumLogLik)
}

maxInfo4 = nlm(AccuracyLikFunct4, p = c(log(0.05), log(10), rep(0, 3)), stepmax = 2)

# but i still don't like the overallMeanRate getting smaller every time, that shouldn't be happening
# and it should include the ubiPropAdj tooEDIT added but doesn't help

# because we've not done the poisson mean properly, let's do that instead, stop being such a dick


muSigmaToAlphaBeta=function(mu, sigma) {
  alphaparam=mu^2/sigma^2
  betaparam=mu/sigma^2
  return(cbind(alphaparam=alphaparam,betaparam=betaparam))
}

AccuracyLikFunct2b = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1])
  overallPriorSigma = exp(theta[2])
  nonPenaltyGoalWeight = exp(theta[3])
  subOosDF$priorMean = with(subOosDF, overallMeanRate * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha + nonPenaltyGoalWeight * oosNonPenaltyGoal)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp * nonPenaltyGoalWeight)
  subOosDF$eGoal = with(subOosDF, ubiPropAdj * postTopLine / postBottomLine)
  
  if (runMode == 'max') {
    subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
    sumLogLik = sum(subOosDF$logLik)
    toReturn = -sumLogLik
  }
  if (runMode == 'fit') {
    toReturn = subOosDF$eGoal
  }
  return(toReturn)
}

maxInfo2b = nlm(AccuracyLikFunct2b, p = c(log(0.1), log(1), 0), stepmax = 2)
subOosDF$eGoal2b = AccuracyLikFunct2b(maxInfo2b$est, runMode = 'fit')

# well it all look sensible but loglik a little lower than '2'
ViewPlayer = function(myDF) {
  myDF %>%
    mutate(meanOosGoal = oosNonPenaltyGoal / oosSumGameProp,
           preAdjEGoal = postTopLine / postBottomLine) %>%
    select(minute, oosMeanEScored, teamoddsescored, priorMean, alpha, beta,
           meanOosGoal, preAdjEGoal, ubiPropAdj, eGoal)
}

# it all looks ok. let's keep adding sensible things to model and see if model 2 stays ahead

subOosDF$posMap = match(subOosDF$mainpos, c('D', 'DMC', 'M', 'AM', 'FW'))

AccuracyLikFunct2PlusPos = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorStrength = exp(theta[6])
  nonPenaltyGoalWeight = exp(theta[7])
  subOosDF$postTopLine = with(subOosDF, overallMeanRate[posMap] * overallPriorStrength + nonPenaltyGoalWeight * oosNonPenaltyGoal)
  subOosDF$postBottomLine = with(subOosDF, overallPriorStrength + oosSumGameProp * nonPenaltyGoalWeight)
  subOosDF$eGoal = with(subOosDF, ubiPropAdj * postTopLine / postBottomLine)
  
  if (runMode == 'max') {
    subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
    sumLogLik = sum(subOosDF$logLik)
    toReturn = -sumLogLik
  }
  if (runMode == 'fit') {
    toReturn = subOosDF$eGoal
  }
  return(toReturn)
}

maxInfo2PlusPos = nlm(AccuracyLikFunct2PlusPos, p = c(rep(log(0.05), 5), log(10), 0), stepmax = 2)
subOosDF$eGoal2PlusPos = AccuracyLikFunct2PlusPos(maxInfo2PlusPos$est, runMode = 'fit')

AccuracyLikFunct2bPlusPos = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorSigma = exp(theta[6])
  nonPenaltyGoalWeight = exp(theta[7])
  subOosDF$priorMean = with(subOosDF, overallMeanRate[posMap] * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha + nonPenaltyGoalWeight * oosNonPenaltyGoal)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp * nonPenaltyGoalWeight)
  subOosDF$eGoal = with(subOosDF, ubiPropAdj * postTopLine / postBottomLine)
  
  if (runMode == 'max') {
    subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
    sumLogLik = sum(subOosDF$logLik)
    toReturn = -sumLogLik
  }
  if (runMode == 'fit') {
    toReturn = subOosDF$eGoal
  }
  return(toReturn)
}

maxInfo2bPlusPos = nlm(AccuracyLikFunct2bPlusPos, p = c(rep(log(0.1), 5), log(1), 0), stepmax = 2)
subOosDF$eGoal2bPlusPos = AccuracyLikFunct2bPlusPos(maxInfo2bPlusPos$est, runMode = 'fit')

# ha ha it doesn't good. let's start a new file with htis new sensible starting point
