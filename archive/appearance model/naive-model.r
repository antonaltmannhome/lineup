# let's start with the most basic model we can think of, only has an overall prior, and an overall muntes played given you started model, and a downweight

source('c:/research/lineup/appearance model/appearance-model-startup.r')

# so probability that you start, downweight + prior:
# we're assuming you know about injuries and suspensions in advance, to remove noise from the preditions

appearanceDF$isStart = with(appearanceDF, startTime == '0')
appearanceDF$available = with(appearanceDF, !startTime %in% c('injury', 'suspension'))

OptimiseParam = function(theta, runMode = 'max') {
	
	gameDownweightCoef = exp(theta[1])
	seasonDownweightCoef = exp(theta[2])
	priorStrength = exp(theta[3])

	appearanceDF$probStart = NA
	for (si in 1:length(unSeasonNumber)) {
		currentMaxNumGame = with(numGameBySeason, numGame[seasonNumber == unSeasonNumber[si]])
		# got the edge case for week 1 of each season, come to that later
		for (gi in 2:currentMaxNumGame) {
			pastAppearanceDF = appearanceDF %>%
								filter(seasonNumber < unSeasonNumber[si] |
										seasonNumber == unSeasonNumber[si] & teamgamenumber < gi)
			pastAppearanceDF = MakeTimeDownweight(pastAppearanceDF, gameDownweightCoef, seasonDownweightCoef)

			overallMeanStart = with(pastAppearanceDF, mean(isStart))
			byPlayerInfo = pastAppearanceDF %>%
								group_by(team, player) %>%
								summarise(sumStart = sum(timeDownweight * isStart),
											sumAvailable = sum(timeDownweight))
			
			### now we can make our probabilities
			byPlayerInfo$topLine = with(byPlayerInfo, priorStrength * overallMeanStart + sumStart)
			byPlayerInfo$bottomLine = with(byPlayerInfo, priorStrength + sumAvailable)
			byPlayerInfo$probStart = with(byPlayerInfo, topLine / bottomLine)
			
			appearanceDF$isCurrentGW = with(appearanceDF, seasonNumber == si & teamgamenumber == gi)
			appearanceDF = subset_join(appearanceDF,
										byPlayerInfo %>%
										select(team, player, probStart),
										c('team', 'player'),
										isCurrentGW)
		}
		message('Have calculated for season ', si)
	}

	# then override the injuries or suspensions wtih 0
	appearanceDF$probStart[!appearanceDF$available] = 0
	
	if (runMode == 'max') {
		## let's get the loglik of that
		appearanceDF$logLik = log(dbinom(appearanceDF$isStart, 1, appearanceDF$probStart))
		meanLogLik = mean(appearanceDF$logLik, na.rm = TRUE)

		message('gameDownweightCoef: ', gameDownweightCoef)
		message('seasonDownweightCoef: ', seasonDownweightCoef)
		message('priorStrength: ', priorStrength)
		print(meanLogLik)
	}
	
	if (runMode == 'fit') {
		return(appearanceDF)
	}
	
	return(-meanLogLik)
}

maxInfo = nlm(OptimiseParam, p = c(log(0.01), log(0.25), log(10)))

#$minimum
#[1] 0.3530556
#$estimate
#[1] -0.9439482 -0.1502391 -0.8147686
# all looks kind of sensible

# all looks kind of sensible

appearanceDF = OptimiseParam(maxInfo$est, runMode = 'fit')
