### let's look into expected minutes given you start, given you don't start. want expected minutes overall basically

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

naiveThetaPlusIS = c(-0.9825891, -0.2588052, -0.7399990)
appearanceDF = OptimiseParam(naiveThetaPlusIS, adjustForIS = TRUE, runMode = 'fit')

# firstly let's get histogram of overall minutes given started

appearanceDF$played = with(appearanceDF, !startTime %in% c('injury', 'suspension', 'U', 'UU'))
appearanceDF$minute = 0
appearanceDF$minute[appearanceDF$played] = with(appearanceDF, as.numeric(endTime[played]) - as.numeric(startTime[played]))

# mean if started in 88.3
with(appearanceDF[appearanceDF$isStart,], calibplot(probStart, minute))
# very light trend, barely worth bothering with. but i think if you did a player by player thing, you would find trends e.g aguero clearly plays less than this would estimate

# so we need to have a summed minutes given played by player too. that sounds like a job thought, sod it
