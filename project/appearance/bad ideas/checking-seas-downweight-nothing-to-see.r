
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

CalculateSmoothFromPlayerGame = function(mygbgdf, overallMeanValue,
                                         gameDownweightCoef, seasonDownweightCoef, priorStrength) {
  mygbgdf = ffModel:::MakeTimeDownweight(mygbgdf, gameDownweightCoef, seasonDownweightCoef)
  
  byPlayerInfo = mygbgdf %>%
    group_by(team, player) %>%
    summarise(sumValue = sum(timeDownweight * value),
              sumNumOb = sum(timeDownweight)) %>%
    ungroup()
  
  ### now we can make our probabilities
  byPlayerInfo$topLine = with(byPlayerInfo, priorStrength * overallMeanValue + sumValue)
  byPlayerInfo$bottomLine = with(byPlayerInfo, priorStrength + sumNumOb)
  byPlayerInfo$expectedValue = with(byPlayerInfo, topLine / bottomLine)
  
  return(byPlayerInfo)
}

CalculateHistoricSingleQuantity = function(theta, quantityChoice, gbgdf) {
  
  # theta = c(-0.9439482, -0.1502391, -0.8147686) # is sensible
  gameDownweightCoef = exp(theta[1])
  seasonDownweightCoef = exp(theta[2])
  priorStrength = exp(theta[3])
  
  subgbgdf = ffModel:::PrepareGbgdfForSmoothing(gbgdf, quantityChoice)
  subgbgdf$expectedValue = NA
  
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
        select(team, player, gameForTeamNumber, expectedValue)
      byPlayerInfo = bind_rows(byPlayerInfo, missingPlayerInfo)
    }
    
    subgbgdf = join_on_overlap(subgbgdf,
                               byPlayerInfo %>%
                                 select(team, player, gameForTeamNumber, expectedValue),
                               c('team', 'player', 'gameForTeamNumber'))
    if ( (gi %% 20) == 0) message('Have calculated ', quantityChoice, ' for team-game number ', gi)
  }
  
  # get it back onto original DF
  gbgdfPlus = lazy_left_join(gbgdf,
                             subgbgdf,
                             c('seasonNumber', 'team', 'player', 'teamgamenumber'),
                             'expectedValue')
  
  gbgdfPlus = gbgdfPlus %>%
    rename(!!quantityChoice := expectedValue)
  
  return(gbgdfPlus)
}
}

## current (wrong) method:
source('c:/git/lineup/new-model-startup.r')

gbgdf = gbgdf %>%
  group_by(team, player) %>%
  arrange(seasonNumber, teamgamenumber) %>%
  mutate(gameForTeamNumber = 1:n()) %>%
  ungroup()

quantityChoice = 'probStart'
optThetaFile = system.file(paste0('opt-theta-', quantityChoice, '.dat'), package = 'ffModel')
theta = scan(optThetaFile, quiet = TRUE)

gbgdfPlusQuantity = ffModel:::CalculateHistoricSingleQuantity(theta, quantityChoice, gbgdf)

### and the loglik is:
gbgdfPlusQuantity$logLik = with(gbgdfPlusQuantity, log(dbinom(isStart, 1, probStart)))
meanLogLik = with(gbgdfPlusQuantity, mean(logLik[available]))
# -0.4353 # that's the target


MakeTimeDownweight2 = function(pastGbgDF, dayDownweightCoef, seasonDownweightCoef) {
  pastGbgDF2 = pastGbgDF %>%
    left_join(pastGbgDF %>%
                group_by(team, player) %>%
                summarise(maxSeasonNumber = max(seasonNumber),
                          maxDaynum = max(daynum)),
              c('team', 'player'))
  
  pastGbgDF2$seasDelta = with(pastGbgDF2, maxSeasonNumber - seasonNumber)
  pastGbgDF2$dayDelta = with(pastGbgDF2, maxDaynum - daynum)
  pastGbgDF2$dayDownweight = with(pastGbgDF2, exp(-dayDownweightCoef * dayDelta))
  pastGbgDF2$seasonDownweight = with(pastGbgDF2, exp(-seasonDownweightCoef * seasDelta))
  pastGbgDF2$timeDownweight = with(pastGbgDF2, dayDownweight * seasonDownweight)
  
  # but we don't need all of the intermediate columns so get rid
  pastGbgDF2 = within(pastGbgDF2, rm(maxSeasonNumber, maxDaynum, seasDelta, dayDelta, dayDownweight, seasonDownweight))
  
  return(pastGbgDF2)
}

CalculateSmoothFromPlayerGame2 = function(pastGbgDF, overallMeanValue,
                                          gameDownweightCoef, seasonDownweightCoef, priorStrength) {
  pastGbgDF = MakeTimeDownweight2(pastGbgDF, gameDownweightCoef, seasonDownweightCoef)
  
  byPlayerInfo = pastGbgDF %>%
    group_by(team, player) %>%
    summarise(sumValue = sum(timeDownweight * value),
              sumNumOb = sum(timeDownweight)) %>%
    ungroup()
  
  ### now we can make our probabilities
  byPlayerInfo$topLine = with(byPlayerInfo, priorStrength * overallMeanValue + sumValue)
  byPlayerInfo$bottomLine = with(byPlayerInfo, priorStrength + sumNumOb)
  byPlayerInfo$expectedValue = with(byPlayerInfo, topLine / bottomLine)
  
  return(byPlayerInfo)
}

CalculateHistoricSingleQuantity2 = function(theta, quantityChoice, gbgdf) {
  
  # theta = c(-0.9439482, -0.1502391, -0.8147686) # is sensible
  dayDownweightCoef = exp(theta[1])
  seasonDownweightCoef = exp(theta[2])
  priorStrength = exp(theta[3])
  
  subGbgDF = ffModel:::PrepareGbgdfForSmoothing(gbgdf, quantityChoice)
  subGbgDF$expectedValue = NA
  
  overallMeanValue = with(subGbgDF, mean(value[isValid]))
  for (gi in unique(subGbgDF$gameForTeamNumber)) {
    # first fill out the first game with the prior
    if (gi == 1) {
      byPlayerInfo = subGbgDF %>%
        group_by(team, player) %>%
        arrange(seasonNumber, teamgamenumber) %>%
        slice(1) %>%
        mutate(expectedValue = overallMeanValue)
    }
    if (gi > 1) {
      currentTeamPlayer = subGbgDF %>%
        filter(gameForTeamNumber == gi)
      currentgbgdf = semi_join(subGbgDF,
                               currentTeamPlayer,
                               c('team', 'player'))
      pastGbgDF = currentgbgdf %>%
        filter(isValid & gameForTeamNumber < gi)
      
      byPlayerInfo = CalculateSmoothFromPlayerGame2(pastGbgDF, overallMeanValue,
                                                    dayDownweightCoef, seasonDownweightCoef, priorStrength)
      
      byPlayerInfo = byPlayerInfo %>%
        mutate(gameForTeamNumber = gi)
      # but that won't include e.g players who have never started if we're looking at expected minutes given starting. so should put in prior for that
      missingPlayerInfo = anti_join(currentTeamPlayer,
                                    byPlayerInfo,
                                    c('team', 'player')) %>%
        mutate(expectedValue = overallMeanValue) %>%
        select(team, player, gameForTeamNumber, expectedValue)
      byPlayerInfo = bind_rows(byPlayerInfo, missingPlayerInfo)
    }
    
    subGbgDF = join_on_overlap(subGbgDF,
                                byPlayerInfo %>%
                                  select(team, player, gameForTeamNumber, expectedValue),
                                c('team', 'player', 'gameForTeamNumber'))
    if ( (gi %% 20) == 0) message('Have calculated ', quantityChoice, ' for team-game number ', gi)
  }
  
  # get it back onto original DF
  gbgdfPlus = lazy_left_join(gbgdf,
                             subGbgDF,
                             c('seasonNumber', 'team', 'player', 'teamgamenumber'),
                             'expectedValue')
  
  gbgdfPlus = gbgdfPlus %>%
    rename(!!quantityChoice := expectedValue)
  
  return(gbgdfPlus)
}

theta = c(log(0.01), log(0.01), log(0.5))
LikFunct = function(theta, quantityChoice) {
  gbgdfPlusQuantity = CalculateHistoricSingleQuantity2(theta, quantityChoice, gbgdf)

  ### and the loglik is:
  gbgdfPlusQuantity$logLik = with(gbgdfPlusQuantity, log(dbinom(isStart, 1, probStart)))
  meanLogLik = with(gbgdfPlusQuantity, mean(logLik[available]))

  print(meanLogLik)
  return(-meanLogLik)
}

maxInfo = nlm(LikFunct, p = theta, 'probStart')
# -0.437398 no better, nothing in this

# what about just downweighting by games back?



MakeTimeDownweight3 = function(pastGbgDF, gameDownweightCoef, gi) {

  pastGbgDF$gameDelta = with(pastGbgDF, gi - gameForTeamNumber)
  pastGbgDF$timeDownweight = with(pastGbgDF, exp(-gameDownweightCoef * gameDelta))
    
  return(pastGbgDF)
}

CalculateSmoothFromPlayerGame3 = function(pastGbgDF, overallMeanValue,
                                          gameDownweightCoef, priorStrength, gi) {
  pastGbgDF = MakeTimeDownweight3(pastGbgDF, gameDownweightCoef, gi)
  
  byPlayerInfo = pastGbgDF %>%
    group_by(team, player) %>%
    summarise(sumValue = sum(timeDownweight * value),
              sumNumOb = sum(timeDownweight)) %>%
    ungroup()
  
  ### now we can make our probabilities
  byPlayerInfo$topLine = with(byPlayerInfo, priorStrength * overallMeanValue + sumValue)
  byPlayerInfo$bottomLine = with(byPlayerInfo, priorStrength + sumNumOb)
  byPlayerInfo$expectedValue = with(byPlayerInfo, topLine / bottomLine)
  
  return(byPlayerInfo)
}

CalculateHistoricSingleQuantity3 = function(theta, quantityChoice, gbgdf) {
  
  # theta = c(log(0.05), log(3)) # is sensible
  gameDownweightCoef = exp(theta[1])
  priorStrength = exp(theta[2])
  
  subGbgDF = ffModel:::PrepareGbgdfForSmoothing(gbgdf, quantityChoice)
  subGbgDF$expectedValue = NA
  
  overallMeanValue = with(subGbgDF, mean(value[isValid]))
  for (gi in unique(subGbgDF$gameForTeamNumber)) {
    # first fill out the first game with the prior
    if (gi == 1) {
      byPlayerInfo = subGbgDF %>%
        group_by(team, player) %>%
        arrange(seasonNumber, teamgamenumber) %>%
        slice(1) %>%
        mutate(expectedValue = overallMeanValue)
    }
    if (gi > 1) {
      currentTeamPlayer = subGbgDF %>%
        filter(gameForTeamNumber == gi)
      currentgbgdf = semi_join(subGbgDF,
                               currentTeamPlayer,
                               c('team', 'player'))
      pastGbgDF = currentgbgdf %>%
        filter(isValid & gameForTeamNumber < gi)
      
      byPlayerInfo = CalculateSmoothFromPlayerGame3(pastGbgDF, overallMeanValue,
                                                    gameDownweightCoef, priorStrength,
                                                    gi)
      
      byPlayerInfo = byPlayerInfo %>%
        mutate(gameForTeamNumber = gi)
      # but that won't include e.g players who have never started if we're looking at expected minutes given starting. so should put in prior for that
      missingPlayerInfo = anti_join(currentTeamPlayer,
                                    byPlayerInfo,
                                    c('team', 'player')) %>%
        mutate(expectedValue = overallMeanValue) %>%
        select(team, player, gameForTeamNumber, expectedValue)
      byPlayerInfo = bind_rows(byPlayerInfo, missingPlayerInfo)
    }
    
    subGbgDF = join_on_overlap(subGbgDF,
                               byPlayerInfo %>%
                                 select(team, player, gameForTeamNumber, expectedValue),
                               c('team', 'player', 'gameForTeamNumber'))
    if ( (gi %% 20) == 0) message('Have calculated ', quantityChoice, ' for team-game number ', gi)
  }
  
  # get it back onto original DF
  gbgdfPlus = lazy_left_join(gbgdf,
                             subGbgDF,
                             c('seasonNumber', 'team', 'player', 'teamgamenumber'),
                             'expectedValue')
  
  gbgdfPlus = gbgdfPlus %>%
    rename(!!quantityChoice := expectedValue)
  
  return(gbgdfPlus)
}

theta = c(log(0.1), log(0.5))
LikFunct = function(theta, quantityChoice) {
  gbgdfPlusQuantity = CalculateHistoricSingleQuantity3(theta, quantityChoice, gbgdf)
  
  ### and the loglik is:
  gbgdfPlusQuantity$logLik = with(gbgdfPlusQuantity, log(dbinom(isStart, 1, probStart)))
  meanLogLik = with(gbgdfPlusQuantity, mean(logLik[available]))
  
  print(meanLogLik)
  return(-meanLogLik)
}

maxInfo = nlm(LikFunct, p = theta, 'probStart')

# -0.4330739 fkin get in there, fuck off season downweight
