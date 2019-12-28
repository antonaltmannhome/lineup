
source('new-model-startup.r')
gbgdf = ffModel:::CalculateDeservedGoalAssist(gbgdf)
summarydf = ffModel::CalculateDeservedGoalAssist(summarydf)

gbgdf = ffModel:::ImputeMissingMatchOdds(gbgdf, resultDF)

dum = ffModel:::CalculateSeasonDeservedGoalAssist(summarydf, gbgdf)
seasondeservedsummary = dum$seasondeservedsummary
gbgdf = dum$gbgdf


gameForTeamNumberDF = gbgdf %>%
  filter(played) %>%
  group_by(team, player) %>%
  arrange(seasonNumber, teamgamenumber) %>%
  mutate(gameForTeamNumber = 1:n()) %>%
  ungroup()

gbgdf = lazy_left_join(gbgdf,
                  gameForTeamNumberDF,
                  c('team', 'player', 'seasonNumber', 'teamgamenumber'),
                  'gameForTeamNumber')

gbgdf = gbgdf %>%
  mutate(minute2 = ifelse(minute > 0, minute, 1),
        normDeservedGoal = deservedgoal * (94 / minute2) / teamoddsescored,
         normDeservedAssist = deservedassist * (94 / minute2) / teamoddsescored)

PrepareGbgdfForSmoothing = function(gbgdf, quantityChoice, position) {
  
  subgbgdf = gbgdf %>%
    filter(played & mainpos == position)
  if (quantityChoice == 'goal') {
    subgbgdf$smoothValue = subgbgdf$normDeservedGoal
    subgbgdf$predValue = subgbgdf$goal
  }
  if (quantityChoice == 'assist') {
    subgbgdf$smoothValue = subgbgdf$normDeservedAssist
    subgbgdf$predValue = subgbgf$assist
  }
  
  subgbgdf$normExpectedValue = NA
  
  return(subgbgdf)
}

MakeTimeDownweight = function(myAppearanceDF, gameDownweightCoef, seasonDownweightCoef) {
  myAppearanceDF = myAppearanceDF %>%
    left_join(myAppearanceDF %>%
                group_by(seasonNumber, team, player) %>%
                summarise(maxGameForTeamWithinSeason = max(teamgamenumber)),
              c('seasonNumber', 'team', 'player')) %>%
    left_join(myAppearanceDF %>%
                group_by(team, player) %>%
                summarise(maxSeasonNumber = max(seasonNumber)),
              c('team', 'player'))
  
  myAppearanceDF$seasDelta = with(myAppearanceDF, maxSeasonNumber - seasonNumber)
  myAppearanceDF$gameDelta = with(myAppearanceDF, maxGameForTeamWithinSeason - teamgamenumber)
  myAppearanceDF$gameTimeDownweight = with(myAppearanceDF, exp(-gameDownweightCoef * gameDelta))
  myAppearanceDF$seasonTimeDownweight = with(myAppearanceDF, exp( - seasonDownweightCoef * seasDelta))
  myAppearanceDF$timeDownweight = with(myAppearanceDF, gameTimeDownweight * seasonTimeDownweight)
  
  # but we don't need all of the intermediate columns so get rid
  myAppearanceDF = within(myAppearanceDF, rm(maxSeasonNumber, maxGameForTeamWithinSeason, seasDelta, gameDelta, gameTimeDownweight, seasonTimeDownweight))
  
  return(myAppearanceDF)
}

CalculateSmoothFromPlayerGame = function(subgbgdf, overallMeanValue,
                                         gameDownweightCoef, seasonDownweightCoef, priorStrength) {
  subgbgdf = MakeTimeDownweight(subgbgdf, gameDownweightCoef, seasonDownweightCoef)
  
  byPlayerInfo = subgbgdf %>%
    group_by(team, player) %>%
    summarise(sumValue = sum(timeDownweight * smoothValue * minute2 / 94),
              sumNumOb = sum(timeDownweight * minute2 / 94)) %>%
    ungroup()
  
  ### now we can make our probabilities
  byPlayerInfo$topLine = with(byPlayerInfo, priorStrength * overallMeanValue + sumValue)
  byPlayerInfo$bottomLine = with(byPlayerInfo, priorStrength + sumNumOb)
  byPlayerInfo$normExpectedValue = with(byPlayerInfo, topLine / bottomLine)
  
  return(byPlayerInfo)
}

CalculateHistoricSingleQuantity = function(theta, quantityChoice, position, gbgdf) {
  
  # theta = c(-0.9439482, -0.1502391, -0.8147686) # is sensible
  gameDownweightCoef = exp(theta[1])
  seasonDownweightCoef = exp(theta[2])
  priorStrength = exp(theta[3])
  
  subgbgdf = PrepareGbgdfForSmoothing(gbgdf, quantityChoice, position)
  subgbgdf$normExpectedValue = NA
  
  totalNumValue = with(subgbgdf, sum(predValue))
  totalTeamOddsEScored = with(subgbgdf, sum( (teamoddsescored * minute2 / 94)))
  overallMeanValue = totalNumValue / totalTeamOddsEScored
  for (gi in unique(subgbgdf$gameForTeamNumber)) {
    # first fill out the first game with the prior
    if (gi == 1) {
      byPlayerInfo = subgbgdf %>%
        group_by(team, player) %>%
        arrange(seasonNumber, teamgamenumber) %>%
        slice(1) %>%
        mutate(normExpectedValue = overallMeanValue)
    }
    if (gi > 1) {
      ### ah no there's a problem, players' data get eliminate if their mainpos gets reclassified, that's bollox
      currentTeamPlayer = subgbgdf %>%
        filter(gameForTeamNumber == gi)
      currentgbgdf = semi_join(subgbgdf,
                               currentTeamPlayer,
                               c('team', 'player'))
      pastgbgdf = currentgbgdf %>%
        filter(gameForTeamNumber < gi)
      
      byPlayerInfo = CalculateSmoothFromPlayerGame(pastgbgdf, overallMeanValue,
                                                    gameDownweightCoef, seasonDownweightCoef, priorStrength)
      
      byPlayerInfo = byPlayerInfo %>%
        mutate(gameForTeamNumber = gi)
      # but that won't include e.g players who have never started if we're looking at expected minutes given starting. so should put in prior for that
      missingPlayerInfo = anti_join(currentTeamPlayer,
                                    byPlayerInfo,
                                    c('team', 'player')) %>%
        mutate(normExpectedValue = overallMeanValue) %>%
        select(team, player, gameForTeamNumber, normExpectedValue)
      byPlayerInfo = bind_rows(byPlayerInfo, missingPlayerInfo)
    }
    
    subgbgdf = join_on_overlap(subgbgdf,
                               byPlayerInfo %>%
                                 select(team, player, gameForTeamNumber, normExpectedValue),
                               c('team', 'player', 'gameForTeamNumber'))
    if ( (gi %% 20) == 0) message('Have calculated ', quantityChoice, ' for team-game number ', gi)
  }
  

  return(subgbgdf)
}

LikFunct = function(theta, quantityChoice, position, runMode) {
  
  # for debugging:
  # theta = c(log(0.05), log(1), log(5)); quantityChoice = 'goal'; position = 'FW'
  print(exp(theta))
  subgbgdf = CalculateHistoricSingleQuantity(theta, quantityChoice, position, gbgdf)
  subgbgdf$expectedValue = with(subgbgdf, normExpectedValue * teamoddsescored * minute2 / 94)
  
  if (runMode == 'fit') {
    toReturn = subgbgdf %>%
      select(season, teamgamenumber, team, player, expectedValue)
  }
  
  if (runMode == 'max') {
    if (quantityChoice == 'goal') {
      subgbgdf$logLik = with(subgbgdf, log(dpois(goal, expectedValue)))
    }
    if (quantityChoice == 'assist') {
      subgbgdf$logLik = with(subgbgdf, log(dpois(assist, expectedValue)))
    }
    
    sumLogLik = sum(subgbgdf$logLik)
    
    calibplot(subgbgdf$expectedValue, subgbgdf$goal)
    print(sumLogLik)
    
    toReturn = -sumLogLik
  }
  
  return(toReturn)
}

posToMax = c('FW', 'AM', 'M', 'DMC', 'D')

theta = c(log(0.05), log(1), log(5))
maxInfoGoalFW = nlm(LikFunct, p = theta, quantityChoice = 'goal', position = 'FW', runMode = 'max')
# -3.340669 -8.119093  2.063635
goalFWGbgdf = LikFunct(maxInfoGoalFW$estimate, quantityChoice = 'goal', position = 'FW', runMode = 'fit')

maxInfoGoalAM = nlm(LikFunct, p = theta, quantityChoice = 'goal', position = 'AM', runMode = 'max')
# -3.340669 -8.119093  2.063635
goalAMGbgdf = LikFunct(maxInfoGoalFW$estimate, quantityChoice = 'goal', position = 'FW', runMode = 'fit')
