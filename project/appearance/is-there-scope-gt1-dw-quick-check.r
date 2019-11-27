## let's see if we can do better than just a single time downweight in the probability starts


gbgdf$isStart = with(gbgdf, startTime == '0')
gbgdf$available = with(gbgdf, !startTime %in% c('injury', 'suspension'))

gbgdf = gbgdf %>%
  group_by(team, player) %>%
  arrange(seasonNumber, teamgamenumber) %>%
  mutate(gameForTeamNumber = 1:n()) %>%
  ungroup()

CalculateQuantity = function(theta, quantityChoice, runMode = 'max') {
  
  # theta = c(-0.9439482, -0.1502391, -0.8147686) # is sensible
  gameDownweightCoef = exp(theta[1])
  seasonDownweightCoef = exp(theta[2])
  priorStrength = exp(theta[3])
  
  subgbgdf = ffModel:::PrepareGbgdfForSmoothing(gbgdf, quantityChoice)
  subgbgdf[,c('sumValue', 'sumNumOb', 'expectedValue')] = NA
  
  overallMeanValue = with(subgbgdf, mean(value[isValid]))
  for (gi in unique(subgbgdf$gameForTeamNumber)) {
    # first fill out the first game with the prior
    if (gi == 1) {
      byPlayerInfo = subgbgdf %>%
        group_by(team, player) %>%
        arrange(seasonNumber, teamgamenumber) %>%
        slice(1) %>%
        mutate(expectedValue = overallMeanValue)
    }
    if (gi > 1) {
      currentTeamPlayer = subgbgdf %>%
        filter(gameForTeamNumber == gi)
      currentgbgdf = semi_join(subgbgdf,
                               currentTeamPlayer,
                               c('team', 'player'))
      pastgbgdf = currentgbgdf %>%
        filter(isValid & gameForTeamNumber < gi)
      
      byPlayerInfo = ffModel:::CalculateSmoothFromPlayerGame(pastgbgdf, overallMeanValue,
                                                             gameDownweightCoef, seasonDownweightCoef, priorStrength)
      
      byPlayerInfo = byPlayerInfo %>%
        mutate(gameForTeamNumber = gi)
      # but that won't include e.g players who have never started if we're looking at expected minutes given starting. so should put in prior for that
      missingPlayerInfo = anti_join(currentTeamPlayer,
                                    byPlayerInfo,
                                    c('team', 'player')) %>%
        mutate(expectedValue = overallMeanValue) %>%
        select(team, player, gameForTeamNumber, sumValue, sumNumOb, expectedValue)
      byPlayerInfo = bind_rows(byPlayerInfo, missingPlayerInfo)
    }
    
    subgbgdf = join_on_overlap(subgbgdf,
                               byPlayerInfo %>%
                                 select(team, player, gameForTeamNumber, sumValue, sumNumOb, expectedValue),
                               c('team', 'player', 'gameForTeamNumber'))
    if ( (gi %% 20) == 0) message('Have calculated for team-game number ', gi)
  }
  
  
  # get it back onto original DF
  gbgdf = lazy_left_join(gbgdf,
                         subgbgdf,
                         c('seasonNumber', 'team', 'player', 'teamgamenumber'),
                         c('sumValue', 'sumNumOb', 'expectedValue'))
  
  if (quantityChoice == 'probStart') {
    gbgdf$probStart = gbgdf$expectedValue
  }
  if (quantityChoice == 'probOffBench') {
    gbgdf$probOffBench = gbgdf$expectedValue
  }
  if (quantityChoice == 'minuteStart') {
    gbgdf$eMinStart = gbgdf$expectedValue
  }
  if (quantityChoice == 'minuteBench') {
    gbgdf$eMinBench = gbgdf$expectedValue
  }
  
  gbgdf = within(gbgdf, rm(expectedValue))
  gbgdf$overallMeanValue = overallMeanValue
  
  if (runMode == 'fit') {
    toReturn = gbgdf
  }
  
  return(toReturn)
}


currentOptTheta = c(-1.031, 0.785, -0.658)
currentOptGbgDF = CalculateQuantity(currentOptTheta, quantityChoice = 'probStart', runMode = 'fit')
# is optimum (see project/optimise-theta-for-expected-minute-model)
# corresponds to: dw: 0.26, seasonmult: 2.19, prior 0.518
# BS parameters, downweights really fast then double weights previous season
# no it doesn't do that, 2.19 means last year has exp(-2.12*1) extra downweighting i.e. 0.12, 2 years ago has exp(-2.12 * 2) = 0.014
# feels far too fast

# what we want is if someone like mane doesn't play for a game, then they don't get dragged down as much as someone like lovren. 

slowTheta = c(log(0.1), 0, log(0.5))
slowDwGbgDF = CalculateQuantity(slowTheta, quantityChoice = 'probStart', runMode = 'fit')
fastTheta = c(log(0.5), 0, log(0.5))
fastDwGbgDF = CalculateQuantity(fastTheta, quantityChoice = 'probStart', runMode = 'fit')

CalcLogLik = function(myGbgDF) {
  myGbgDF$logLik = with(myGbgDF, log(dbinom(isStart, 1, probStart)))
  meanProbCorrect = exp(with(myGbgDF, mean(logLik[available])))
  return(meanProbCorrect)
}

# Now, can we mix the fast and the slow and get valid numbers?

TryMixture = function(priorStr, slowWgt, fastWgt) {
  mixedGbgDF = lazy_left_join(
    slowDwGbgDF %>%
      rename(slowSumValue = sumValue, slowSumNumOb = sumNumOb),
    fastDwGbgDF %>%
      rename(fastSumValue = sumValue, fastSumNumOb = sumNumOb),
    c('season', 'team', 'playerid', 'teamgamenumber'),
    c('fastSumValue', 'fastSumNumOb'))

  mixedGbgDF = mixedGbgDF %>%
    mutate( topLine = priorStr * overallMeanValue + slowWgt * slowSumValue + fastWgt * fastSumValue,
            bottomLine = priorStr + slowWgt * slowSumNumOb + fastWgt * fastSumNumOb,
            probStart = topLine / bottomLine)
  # except that doesn't cover the missing players, just sub them in
  mixedGbgDF = mixedGbgDF %>%
    mutate_cond(is.na(probStart) & available, probStart = overallMeanValue)
  CalcLogLik(mixedGbgDF)
}

TryMixture(0.5, 0.2, 0.8)
# better than either individually, awesome, so let's see if nlm can find this
# iot didn't, we need a different approach clearly
