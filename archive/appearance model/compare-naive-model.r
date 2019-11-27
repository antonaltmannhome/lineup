# let's compare the two naive models, check one that accounts for injury is better:

source('c:/research/lineup/appearance model/appearance-model-startup.r')

# so probability that you start, downweight + prior:

appearanceDF$isStart = with(appearanceDF, startTime == '0')
appearanceDF$available = with(appearanceDF, !startTime %in% c('injury', 'suspension'))

OptimiseParam = function(theta, adjustForIS = FALSE, runMode = 'max') {
	
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

			if (!adjustForIS) {
				overallMeanStart = with(pastAppearanceDF, mean(isStart))
				byPlayerInfo = pastAppearanceDF %>%
								group_by(team, player) %>%
								summarise(sumStart = sum( (timeDownweight * isStart)),
											sumAvailable = sum(timeDownweight))
			}

			if (adjustForIS) {
				overallMeanStart = with(pastAppearanceDF, mean(isStart[available]))
				byPlayerInfo = pastAppearanceDF %>%
								group_by(team, player) %>%
								summarise(sumStart = sum( (timeDownweight * isStart)[available]),
											sumAvailable = sum(timeDownweight[available]))
			}
			
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

naiveTheta = c(-0.9439482, -0.1502391, -0.8147686)
appearanceDF = OptimiseParam(naiveTheta, adjustForIS = FALSE, runMode = 'fit')
appearanceDF$naiveProbStart = appearanceDF$probStart
appearanceDF$naiveLogLik = log(dbinom(appearanceDF$isStart, 1, appearanceDF$naiveProbStart))

naiveThetaPlusIS = c(-0.9825891, -0.2588052, -0.7399990)
appearanceDF = OptimiseParam(naiveThetaPlusIS, adjustForIS = TRUE, runMode = 'fit')
appearanceDF$naivePlusISProbStart = appearanceDF$probStart
appearanceDF$naivePlusISLogLik = log(dbinom(appearanceDF$isStart, 1, appearanceDF$naivePlusISProbStart))

# check logliks are as expected
shouldHaveProbIndex = with(appearanceDF, !is.na(naiveProbStart))
with(appearanceDF, mean(naiveLogLik[shouldHaveProbIndex]))
with(appearanceDF, mean(naivePlusISLogLik[shouldHaveProbIndex]))

# yes agreeing, what about conf int?

with(appearanceDF[shouldHaveProbIndex,], bootstrapci(naivePlusISLogLik - naiveLogLik))
# 0.00592 (-0.033201, 0.043805) # no significant difference, interesting

# is that because the plusCI is suffering when a player returns from injury? their start prob is surely lower than it's predicting. can confirm when we start looking into biases
