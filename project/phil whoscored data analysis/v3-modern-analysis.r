# v2 got cluttered, let's restart from its nice ending point


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
subOosDF$posMap = match(subOosDF$mainpos, c('D', 'DMC', 'M', 'AM', 'FW'))

muSigmaToAlphaBeta=function(mu, sigma) {
  alphaparam=mu^2/sigma^2
  betaparam=mu/sigma^2
  return(cbind(alphaparam=alphaparam,betaparam=betaparam))
}


AccuracyLikFunct1 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorSigma = exp(theta[6])
  subOosDF$priorMean = with(subOosDF, overallMeanRate[posMap] * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha + oosNonPenaltyGoal)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp)
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

maxInfo1 = nlm(AccuracyLikFunct1, p = c(rep(log(0.1), 5), log(1)), stepmax = 2)
subOosDF$eGoal1 = AccuracyLikFunct1(maxInfo1$est, runMode = 'fit')

# useful tool to see what it's done for a player
ViewPlayer = function(myDF) {
  myDF %>%
    mutate(meanOosGoal = oosNonPenaltyGoal / oosSumGameProp,
           preAdjEGoal = postTopLine / postBottomLine) %>%
    select(season, team, player, mainpos, minute, oosMeanEScored, teamoddsescored, priorMean, alpha, beta,
           meanOosGoal, preAdjEGoal, ubiPropAdj, eGoal)
}
# eg ViewPlayer(subOosDF %>% filter(grepl('vardy', player) & season == 1920))

# ok, prior seems to be very strong. maybe using something other than goals will swing it away from that


AccuracyLikFunct2 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorSigma = exp(theta[6])
  goalWeight = exp(theta[7])
  shotInBoxWeight = exp(theta[8])
  subOosDF$priorMean = with(subOosDF, overallMeanRate[posMap] * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha + goalWeight * oosNonPenaltyGoal + shotInBoxWeight * oosShotInBox)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp)
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

maxInfo2 = nlm(AccuracyLikFunct2, p = c(rep(log(0.1), 5), log(1), rep(0, 2)), stepmax = 2)
subOosDF$eGoal2 = AccuracyLikFunct2(maxInfo2$est, runMode = 'fit')


AccuracyLikFunct3 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorSigma = exp(theta[6])
  goalWeight = exp(theta[7])
  shotInBoxWeight = exp(theta[8])
  shotOutBoxWeight = exp(theta[9])
  shot6YardWeight = exp(theta[10])
  subOosDF$priorMean = with(subOosDF, overallMeanRate[posMap] * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha +
                                goalWeight * oosNonPenaltyGoal +
                                shotInBoxWeight * oosShotInBox +
                                shotOutBoxWeight * oosShotOutBox +
                                shot6YardWeight * oosShot6Yard)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp)
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

maxInfo3 = nlm(AccuracyLikFunct3, p = c(rep(log(0.1), 5), log(1), rep(0, 4)), stepmax = 2, iterlim = 200)
subOosDF$eGoal3 = AccuracyLikFunct3(maxInfo3$est, runMode = 'fit')

# 6yard shots worse than in box shots? is that due to them being corners so often maybe?
# gain from goals to shots in box massive, 111 points, but just 17 with other two stats

# now try shot location instead
# offTargetNotBar + onTargetNotGoal + block + bar+ nonPenaltyGoal

AccuracyLikFunct4 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorSigma = exp(theta[6])
  goalWeight = exp(theta[7])
  shotOnTargetWeight = exp(theta[8])
  subOosDF$priorMean = with(subOosDF, overallMeanRate[posMap] * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha +
                                goalWeight * oosNonPenaltyGoal +
                                shotOnTargetWeight * oosOnTargetNotGoal)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp)
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

maxInfo4 = nlm(AccuracyLikFunct4, p = c(rep(log(0.1), 5), log(1), rep(0, 2)), stepmax = 2, iterlim = 200)
subOosDF$eGoal4 = AccuracyLikFunct4(maxInfo4$est, runMode = 'fit')

AccuracyLikFunct5 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorSigma = exp(theta[6])
  goalWeight = exp(theta[7])
  shotOnTargetWeight = exp(theta[8])
  shotOffTargetWeight = exp(theta[9])
  subOosDF$priorMean = with(subOosDF, overallMeanRate[posMap] * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha +
                                goalWeight * oosNonPenaltyGoal +
                                shotOnTargetWeight * oosOnTargetNotGoal +
                                shotOffTargetWeight * oosOffTargetNotBar)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp)
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

maxInfo5 = nlm(AccuracyLikFunct5, p = c(rep(log(0.1), 5), log(1), rep(0, 3)), stepmax = 2, iterlim = 200)
subOosDF$eGoal5 = AccuracyLikFunct5(maxInfo5$est, runMode = 'fit')

AccuracyLikFunct6 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorSigma = exp(theta[6])
  goalWeight = exp(theta[7])
  shotOnTargetWeight = exp(theta[8])
  shotOffTargetWeight = exp(theta[9])
  shotBarWeight = exp(theta[10])
  shotBlockWeight = exp(theta[11])
  subOosDF$priorMean = with(subOosDF, overallMeanRate[posMap] * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha +
                                goalWeight * oosNonPenaltyGoal +
                                shotOnTargetWeight * oosOnTargetNotGoal +
                                shotOffTargetWeight * oosOffTargetNotBar +
                                shotBarWeight * oosBar +
                                shotBlockWeight * oosBlock)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp)
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

maxInfo6 = nlm(AccuracyLikFunct6, p = c(rep(log(0.1), 5), log(1), rep(0, 5)), stepmax = 2, iterlim = 200)
subOosDF$eGoal6 = AccuracyLikFunct6(maxInfo6$est, runMode = 'fit')

# again, pretty dismal gain, most comes from shots on target

# so what if we mix models 2 and 4?


AccuracyLikFunct7 = function(theta, runMode = 'max') {
  overallMeanRate = exp(theta[1:5])
  overallPriorSigma = exp(theta[6])
  goalWeight = exp(theta[7])
  shotOnTargetWeight = exp(theta[8])
  shotInBoxWeight = exp(theta[9])
  subOosDF$priorMean = with(subOosDF, overallMeanRate[posMap] * oosMeanEScored)
  subOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(subOosDF$priorMean, overallPriorSigma)
  
  subOosDF$postTopLine = with(subOosDF, alpha +
                                goalWeight * oosNonPenaltyGoal +
                                shotOnTargetWeight * oosOnTargetNotGoal +
                                shotInBoxWeight * oosShotInBox)
  subOosDF$postBottomLine = with(subOosDF, beta + oosSumGameProp)
  subOosDF$eGoal = with(subOosDF, ubiPropAdj * postTopLine / postBottomLine)
  
  if (runMode == 'max') {
    subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
    sumLogLik = sum(subOosDF$logLik)
    toReturn = -sumLogLik
  }
  if (runMode == 'fit') {
    toReturn = subOosDF
  }
  return(toReturn)
}

maxInfo7 = nlm(AccuracyLikFunct7, p = c(rep(log(0.1), 5), log(1), rep(0, 3)), stepmax = 2)
subOosDF7 = AccuracyLikFunct7(maxInfo7$est, runMode = 'fit')
 # 8992, with quite a small set of parameters

# but do the expected goal things actually have same mean as goals?
theta = maxInfo7$estimate
goalWeight = exp(theta[7])
shotOnTargetWeight = exp(theta[8])
shotInBoxWeight = exp(theta[9])
subOosDF$expectedGoal = with(subOosDF, (goalWeight * oosNonPenaltyGoal +
                              shotOnTargetWeight * oosOnTargetNotGoal +
                              shotInBoxWeight * oosShotInBox) / oosSumGameProp)
with(subOosDF, calibplot(ubiPropAdj * expectedGoal, nonPenaltyGoal))
### hmm, not bad, why is it a little bit high though

# but, what if we split between the defensive players and attacking players?

AccuracyLikFunct8 = function(theta, runMode = 'max', myOosDF, numPos) {
  overallMeanRate = exp(theta[1:numPos])
  overallPriorSigma = exp(theta[numPos + 1])
  goalWeight = exp(theta[numPos + 2])
  shotOnTargetWeight = exp(theta[numPos + 3])
  shotInBoxWeight = exp(theta[numPos + 4])
  myOosDF$priorMean = with(myOosDF, overallMeanRate[posMap] * oosMeanEScored)
  myOosDF[,c('alpha', 'beta')] = muSigmaToAlphaBeta(myOosDF$priorMean, overallPriorSigma)
  
  myOosDF$postTopLine = with(myOosDF, alpha +
                                goalWeight * oosNonPenaltyGoal +
                                shotOnTargetWeight * oosOnTargetNotGoal +
                                shotInBoxWeight * oosShotInBox)
  myOosDF$postBottomLine = with(myOosDF, beta + oosSumGameProp)
  myOosDF$eGoal = with(myOosDF, ubiPropAdj * postTopLine / postBottomLine)
  
  if (runMode == 'max') {
    myOosDF$logLik = with(myOosDF, log(dpois(nonPenaltyGoal, eGoal)))
    sumLogLik = sum(myOosDF$logLik)
    toReturn = -sumLogLik
  }
  if (runMode == 'fit') {
    toReturn = myOosDF
  }
  return(toReturn)
}

subOosDF$eGoal = NA
posList = list(c('D', 'DMC'), 'M', c('AM', 'FW'))
for (posi in 1:length(posList)) {
  myPos = posList[[posi]]
  myOosDF = subOosDF %>%
    filter(mainpos %in% myPos) %>%
    mutate(posMap = match(mainpos, myPos))
  maxInfo7 = nlm(AccuracyLikFunct8, p = c(rep(log(0.1), length(myPos)), log(1), rep(0, 3)),
                 myOosDF = myOosDF, numPos = length(myPos), stepmax = 2)
  myOosDF = AccuracyLikFunct7(maxInfo7$estimate, myOosDF = myOosDF, numPos = length(myPos), runMode = 'fit')
  subOosDF = join_on_overlap(subOosDF,
                           myOosDF %>%
                             select(season, team, player, teamgamenumber, eGoal),
                           c('season', 'team', 'player', 'teamgamenumber'))
  message('Have done estimates for ', posi)
}

subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
sumLogLik = sum(subOosDF$logLik)
# [1] -8976.124
# not that exciting a gain, really?

# so go back to model 7

# let's make the deserved goals column

theta = maxInfo7$est
goalWeight = exp(theta[7])
shotOnTargetWeight = exp(theta[8])
shotInBoxWeight = exp(theta[9])

gbgdf$deservedGoal = with(gbgdf,
                              goalWeight * nonPenaltyGoal +
                              shotOnTargetWeight * onTargetNotGoal +
                              shotInBoxWeight * shotInBox)

ViewPlayer2 = function(myDF) {
  myDF %>%
    select(season, team, player, minute, goal, shotInBox, onTargetNotGoal, deservedGoal)
}

# total deserved goals is much lower than actual goals, that is terrible
