### so, there's some great stuff going on in the other files. would be great if we could combine:
# prob of starting
# expected minutes given you start
# expected minutes given you don't start

# alll into the same place. That is what we'll try to do now.
# think they can be three independent models, but they share some code


source('c:/research/lineup/appearance model/appearance-model-startup.r')

# so probability that you start, downweight + prior:
# we're assuming you know about injuries and suspensions in advance, to remove noise from the preditions

appearanceDF$isStart = with(appearanceDF, startTime == '0')
appearanceDF$available = with(appearanceDF, !startTime %in% c('injury', 'suspension'))

CalculateQuantity = function(theta, quantityChoice, runMode = 'max') {
		
	# theta = c(-0.9439482, -0.1502391, -0.8147686) # is sensible
	gameDownweightCoef = exp(theta[1])
	seasonDownweightCoef = exp(theta[2])
	priorStrength = exp(theta[3])

	if (quantityChoice == 'probStart') {
		subAppearanceDF = appearanceDF %>%
							mutate(isValid = available)
		subAppearanceDF$value = subAppearanceDF$isStart
	}
	if (quantityChoice == 'minuteStart') {
		subAppearanceDF = appearanceDF %>% 
							mutate(isValid = (available & isStart))
		subAppearanceDF$value = subAppearanceDF$minute
	}
	if (quantityChoice == 'minuteBench') {
		subAppearanceDF = appearanceDF %>%
							mutate(isValid = (available & !isStart & played))
		subAppearanceDF$value = subAppearanceDF$minute
	}
	
	subAppearanceDF$expectedValue = NA
		
	overallMeanValue = with(subAppearanceDF, mean(value[isValid]))
	for (gi in unique(subAppearanceDF$gameForTeamNumber)) {
		# first fill out the first game with the prior
		if (gi == 1) {
			byPlayerInfo = subAppearanceDF %>%
							group_by(team, player) %>%
							arrange(seasonNumber, teamgamenumber) %>%
							slice(1) %>%
							mutate(expectedValue = overallMeanValue)	
		}
		if (gi > 1) {
			currentTeamPlayer = subAppearanceDF %>%
								filter(gameForTeamNumber == gi)
			currentAppearanceDF = semi_join(subAppearanceDF,
											currentTeamPlayer,
											c('team', 'player'))
			pastAppearanceDF = currentAppearanceDF %>%
								filter(isValid & gameForTeamNumber < gi)
			pastAppearanceDF = MakeTimeDownweight(pastAppearanceDF, gameDownweightCoef, seasonDownweightCoef)

			byPlayerInfo = pastAppearanceDF %>%
								group_by(team, player) %>%
								summarise(sumValue = sum(timeDownweight * value),
											sumNumOb = sum(timeDownweight)) %>%
								mutate(gameForTeamNumber = gi)
			
			### now we can make our probabilities
			byPlayerInfo$topLine = with(byPlayerInfo, priorStrength * overallMeanValue + sumValue)
			byPlayerInfo$bottomLine = with(byPlayerInfo, priorStrength + sumNumOb)
			byPlayerInfo$expectedValue = with(byPlayerInfo, topLine / bottomLine)

			# but that won't include e.g players who have never started if we're looking at expected minutes given starting. so should put in prior for that
			missingPlayerInfo = anti_join(currentTeamPlayer,
											byPlayerInfo,
											c('team', 'player')) %>%
								mutate(expectedValue = overallMeanValue) %>%
								select(team, player, gameForTeamNumber, expectedValue)
			byPlayerInfo = bind_rows(byPlayerInfo, missingPlayerInfo)
		}
		
		subAppearanceDF = join_on_overlap(subAppearanceDF,
									byPlayerInfo %>%
									select(team, player, gameForTeamNumber, expectedValue),
									c('team', 'player', 'gameForTeamNumber'))
		if ( (gi %% 20) == 0) message('Have calculated for team-game number ', gi)
	}

	# get it back onto original DF
	appearanceDF = lazy_left_join(appearanceDF,
									subAppearanceDF,
									c('seasonNumber', 'team', 'player', 'teamgamenumber'),
									'expectedValue')
	
	if (quantityChoice == 'probStart') {
		appearanceDF$probStart = appearanceDF$expectedValue
	}
	if (quantityChoice == 'minuteStart') {
		appearanceDF$eMinStart = appearanceDF$expectedValue
	}
	if (quantityChoice == 'minuteBench') {
		appearanceDF$eMinBench = appearanceDF$expectedValue
	}
	
	appearanceDF = within(appearanceDF, rm(expectedValue))
	
	if (runMode == 'fit') {
		toReturn = appearanceDF
	}
	
	return(toReturn)
}

theta = c(-0.9439482, -0.1502391, -0.8147686) # is sensible
appearanceDFPlusProbStart = CalculateQuantity(theta, quantityChoice = 'probStart', runMode = 'fit')

appearanceDFPlusEMinStart = CalculateQuantity(theta, quantityChoice = 'minuteStart', runMode = 'fit')

appearanceDFPlusEMinBench = CalculateQuantity(theta, quantityChoice = 'minuteBench', runMode = 'fit')

appearanceDFCombined = appearanceDF %>%
						lazy_left_join(appearanceDFPlusProbStart,
										c('seasonNumber', 'team', 'player', 'teamgamenumber'),
										'probStart') %>%
						lazy_left_join(appearanceDFPlusEMinStart,
										c('seasonNumber', 'team', 'player', 'teamgamenumber'),
										'eMinStart') %>%
						lazy_left_join(appearanceDFPlusEMinBench,
										c('seasonNumber', 'team', 'player', 'teamgamenumber'),
										'eMinBench')
appearanceDFCombined$eMin = with(appearanceDFCombined, probStart * eMinStart + (1 - probStart) * eMinBench)

ViewPlayer = function(playerString, mySeasonNumber) {
	 myInfo = appearanceDFCombined %>%
				filter(grepl(playerString, player) & seasonNumber == mySeasonNumber) %>%
				select(-c(playerid, teamgamenumber, seasonNumber, played, isStart, available))
	
	return(myInfo)
}

currentEMin = appearanceDFCombined %>% 
				filter(seasonNumber == max(seasonNumber)) %>%
				group_by(team) %>%
				filter(date == max(date)) %>%
				select(team, player, probStart, eMin) %>%
				arrange(-eMin) %>%
				ungroup()

# downweight looks crazy fast but otherwise that looks like it might be sensible
