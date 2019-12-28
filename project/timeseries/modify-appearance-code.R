
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

MakeTimeDownweight = function(subgbgdf, dayDownweightCoef, seasonDownweightCoef) {
  subgbgdf = subgbgdf %>%
    left_join(subgbgdf %>%
                group_by(team, player) %>%
                summarise(maxSeasonNumber = max(seasonNumber),
                          maxDaynum = max(daynum)),
              c('team', 'player'))

  subgbgdf$seasDelta = with(subgbgdf, maxSeasonNumber - seasonNumber)
  subgbgdf$dayDelta = with(subgbgdf, maxDaynum - daynum)
  subgbgdf$dayDownweight = with(subgbgdf, exp(-dayDownweightCoef * dayDelta))
  subgbgdf$seasonDownweight = with(subgbgdf, exp(-seasonDownweightCoef * seasDelta))
  subgbgdf$timeDownweight = with(subgbgdf, dayDownweight * seasonDownweight)
  
  # but we don't need all of the intermediate columns so get rid
  subgbgdf = within(subgbgdf, rm(maxSeasonNumber, maxDaynum, seasDelta, dayDelta, dayDownweight, seasonDownweight))
  
  return(subgbgdf)
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
  # theta = c(log(0.002), log(0.5), log(5)); quantityChoice = 'goal'; position = 'FW'
  print(exp(theta))
  subgbgdf = CalculateHistoricSingleQuantity(theta, quantityChoice, position, gbgdf)
  subgbgdf$expectedValue = with(subgbgdf, normExpectedValue * teamoddsescored * minute2 / 94)
  
  if (runMode == 'fit') {
    toReturn = subgbgdf %>%
      select(season, teamgamenumber, team, player, expectedValue, opgoal, penaltyscored)
  }
  
  if (runMode == 'max') {
    if (quantityChoice == 'goal') {
      subgbgdf$logLik = with(subgbgdf, log(dpois(opgoal, expectedValue)))
    }
    if (quantityChoice == 'assist') {
      subgbgdf$logLik = with(subgbgdf, log(dpois(assist, expectedValue)))
    }
    
    sumLogLik = sum(subgbgdf$logLik)
    
    calibplot(subgbgdf$expectedValue, subgbgdf$opgoal)
    print(sumLogLik)
    
    toReturn = -sumLogLik
  }
  
  return(toReturn)
}

posToMax = c('FW', 'AM', 'M', 'DMC', 'D')

maxThetaArr = expand.grid(mainpos = posToMax,
                          quantityChoice = c('goal', 'assist'),
                          stringsAsFactors = FALSE)
maxThetaArr = cbind(maxThetaArr, array(NA, dim = c(nrow(maxThetaArr), 3)))
theta = c(log(0.002), log(0.5), log(5))

for (mi in 1:nrow(maxThetaArr)) {
  maxInfo = nlm(LikFunct, p = theta,
                quantityChoice = maxThetaArr$quantityChoice[mi],
                position = maxThetaArr$mainpos[mi],
                runMode = 'max')
  # -6.888534 -9.606497  2.217833 for FW, goal - season DW non-existant effectively
  maxThetaArr[mi,3:5] = maxInfo$est
}


### hmmm, wondering if there's a problem with deserved goals, the big team strikers always seem to get more than they deserve

regularStriker = gbgdf %>%
  group_by(player) %>%
  summarise(sumMinute = sum(minute),
            sumGoalRate = 94 * sum(opgoal) / sum(minute)) %>%
  filter(sumMinute > 18 * 94 & sumGoalRate > 0.3)

regularInBetween = gbgdf %>%
  filter(mainpos != 'GK') %>%
  group_by(player) %>%
  summarise(sumMinute = sum(minute),
            sumGoalRate = 94 * sum(opgoal) / sum(minute)) %>%
  filter(sumMinute > 18 * 94 & between(sumGoalRate,0.05, 0.3))

regularNonStriker = gbgdf %>%
  filter(mainpos != 'GK') %>%
  group_by(player) %>%
  summarise(sumMinute = sum(minute),
            sumGoalRate = 94 * sum(opgoal) / sum(minute)) %>%
  filter(sumMinute > 18 * 94 & sumGoalRate <0.05)

# right, let's compute deserved versus observed for all three

strikBias = gbgdf %>%
  semi_join(regularStriker, 'player') %>%
  summarise(sumDeserved = sum(deservedgoal),
            sumGoal = sum(opgoal))
# A tibble: 1 x 2
# sumDeserved sumGoal
# <dbl>   <dbl>
#   1     1274.25    1365
inBetweenBias = gbgdf %>%
  semi_join(regularInBetween, 'player') %>%
  summarise(sumDeserved = sum(deservedgoal),
            sumGoal = sum(opgoal))
# A tibble: 1 x 2
# sumDeserved sumGoal
# <dbl>   <dbl>
#   1     1797.33    1455
nonStrikBias = gbgdf %>%
  semi_join(regularNonStriker, 'player') %>%
  summarise(sumDeserved = sum(deservedgoal),
            sumGoal = sum(opgoal))
# A tibble: 1 x 2
# sumDeserved sumGoal
# <dbl>   <dbl>
#   1     515.428     208
### crap. that's no good. no wonder we never want to sign kane/vardy etc
