
PrepareGbgdfForSmoothing = function(gbgdf, quantityChoice) {

	if (quantityChoice == 'probStart') {
		subgbgdf = gbgdf %>%
							mutate(isValid = available)
		subgbgdf$value = subgbgdf$isStart
	}
	if (quantityChoice == 'probOffBench') {
	  subgbgdf = gbgdf %>%
	    mutate(isValid = available & !isStart)
	  subgbgdf$value = with(subgbgdf, !isStart & played)
	}
	if (quantityChoice == 'eMinStart') {
		subgbgdf = gbgdf %>%
							mutate(isValid = (available & isStart))
		subgbgdf$value = subgbgdf$minute
	}
	if (quantityChoice == 'eMinBench') {
		subgbgdf = gbgdf %>%
							mutate(isValid = (available & !isStart & played))
		subgbgdf$value = subgbgdf$minute
	}

	subgbgdf$expectedValue = NA

  return(subgbgdf)
}

MakeTimeDownweight = function(pastGbgDF, gameDownweightCoef) {

  pastGbgDF$gameDelta = with(pastGbgDF, latestGameForTeamNumber - gameForTeamNumber)
  pastGbgDF$timeDownweight = with(pastGbgDF, exp(-gameDownweightCoef * gameDelta))
  
	return(pastGbgDF)
}

CalculateSmoothFromPlayerGame = function(mygbgdf, overallMeanValue,
																			gameDownweightCoef, priorStrength,
																			currentGameNumber) {
  mygbgdf = MakeTimeDownweight(mygbgdf, gameDownweightCoef)

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

SingleQuantityCalculateUpToDatePlayerSmooth = function(theta, quantityChoice, gbgdf) {
	# theta = c(-0.9439482, -0.8147686) # is sensible
	gameDownweightCoef = exp(theta[1])
	priorStrength = exp(theta[2])

	subgbgdf = ffModel:::PrepareGbgdfForSmoothing(gbgdf, quantityChoice)
	subgbgdf$expectedValue = NA

	overallMeanValue = with(subgbgdf, mean(value[isValid]))

	# but we're only actually interested in players who are active at the moment
	subgbgdf = semi_join(subgbgdf, playerDF, c('team', 'player'))

	# we need the total number of games ech player has done for their team, so do this
	subgbgdf = subgbgdf %>%
	  left_join(subgbgdf %>%
	              group_by(team, player) %>%
	              summarise(latestGameForTeamNumber = n()),
	            c('team', 'player'))
	
	byPlayerInfo = ffModel:::CalculateSmoothFromPlayerGame(subgbgdf %>% filter(isValid),
	                                                       overallMeanValue,
																	                        gameDownweightCoef, priorStrength)
	
	# but what about players who've eg never come off the bench, they're not in byPlayerInfo as things stand
	missingPlayer = anti_join(subgbgdf %>%
	                            distinct(team, player),
	                          byPlayerInfo,
	                          c('team', 'player'))
	byPlayerInfo = bind_rows(byPlayerInfo, missingPlayer %>% mutate(expectedValue = overallMeanValue), )
	
	byPlayerInfo[,quantityChoice] = byPlayerInfo$expectedValue
	# maybe should retain sumValue and sumNumOb?
	byPlayerInfo = remove_column(byPlayerInfo, c('sumValue', 'sumNumOb', 'topLine', 'bottomLine', 'expectedValue'))

	return(byPlayerInfo)
}

CalculateUpToDatePlayerSmooth = function(gbgdf) {

	quantityChoiceVector = c('probStart', 'probOffBench', 'eMinStart', 'eMinBench')
	for (qi in 1:length(quantityChoiceVector)) {
		optThetaFile = system.file(paste0('opt-theta-', quantityChoiceVector[qi], '.dat'), package = 'ffModel')
		theta = scan(optThetaFile, quiet = TRUE)
		currentSmoothDF = ffModel:::SingleQuantityCalculateUpToDatePlayerSmooth(theta, quantityChoiceVector[qi], gbgdf)
		playerDF = lazy_left_join(playerDF, currentSmoothDF, c('team', 'player'), quantityChoiceVector[qi])
		message('Have got latest smoothed values for ', quantityChoiceVector[qi],'...')
	}
	playerDF$eMin = with(playerDF, probStart * eMinStart + (1 - probStart) * probOffBench * eMinBench)

	return(playerDF)
}

CalculateHistoricSingleQuantity = function(theta, quantityChoice, gbgdf) {
  
  # theta = c(-0.9439482, -0.8147686) # is sensible
  gameDownweightCoef = exp(theta[1])
  priorStrength = exp(theta[2])
  
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
        filter(isValid & gameForTeamNumber < gi) %>%
        mutate(latestGameForTeamNumber = gi)
      
      byPlayerInfo = ffModel:::CalculateSmoothFromPlayerGame(pastgbgdf, overallMeanValue,
                                                             gameDownweightCoef, priorStrength)
      
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

CalculateHistoricExpectedMinute = function(gbgdf) {

  gbgdf = gbgdf %>%
    group_by(team, player) %>%
    arrange(seasonNumber, teamgamenumber) %>%
    mutate(gameForTeamNumber = 1:n()) %>%
    ungroup()
  
  quantityChoiceVector = c('probStart', 'probOffBench', 'eMinStart', 'eMinBench')
  for (qi in 1:length(quantityChoiceVector)) {
    optThetaFile = system.file(paste0('opt-theta-', quantityChoiceVector[qi], '.dat'), package = 'ffModel')
    theta = scan(optThetaFile, quiet = TRUE)
    
    gbgdfPlusQuantity = CalculateHistoricSingleQuantity(theta, quantityChoice = quantityChoiceVector[qi], gbgdf)
    gbgdf = lazy_left_join(gbgdf,
                           gbgdfPlusQuantity,
                           c('seasonNumber', 'team', 'player', 'teamgamenumber'),
                           quantityChoiceVector[qi])
  }

  return(gbgdf)
}
