## try to get the whole expected time model working using the new setup
# source('c:/research/lineup/new-model-startup.r')


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
		if ( (gi %% 20) == 0) message('Have calculated for team-game number ', gi)
	}

	# get it back onto original DF
	gbgdf = lazy_left_join(gbgdf,
									subgbgdf,
									c('seasonNumber', 'team', 'player', 'teamgamenumber'),
									'expectedValue')

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

	if (runMode == 'fit') {
		toReturn = gbgdf
	}

	return(toReturn)
}

theta = c(-1.031, 0.785, -0.658) # is optimum (see project/optimise-theta-for-expected-minute-model)
gbgdfPlusProbStart = CalculateQuantity(theta, quantityChoice = 'probStart', runMode = 'fit')

theta = c(-2.2414, 0.0788, 0.9960)
gbgdfPlusProbOffBench = CalculateQuantity(theta, quantityChoice = 'probOffBench', runMode = 'fit')

theta = c(-2.7508, -0.0219,  0.8938)
gbgdfPlusEMinStart = CalculateQuantity(theta, quantityChoice = 'minuteStart', runMode = 'fit')

theta = c(-3.941, 0.483, 2.073)
gbgdfPlusEMinBench = CalculateQuantity(theta, quantityChoice = 'minuteBench', runMode = 'fit')

gbgdfCombined = gbgdf %>%
						lazy_left_join(gbgdfPlusProbStart,
        										c('seasonNumber', 'team', 'player', 'teamgamenumber'),
        										'probStart') %>%
            lazy_left_join(gbgdfPlusProbOffBench,
                           c('seasonNumber', 'team', 'player', 'teamgamenumber'),
                           'probOffBench') %>%
            lazy_left_join(gbgdfPlusEMinStart,
        										c('seasonNumber', 'team', 'player', 'teamgamenumber'),
        										'eMinStart') %>%
						lazy_left_join(gbgdfPlusEMinBench,
        										c('seasonNumber', 'team', 'player', 'teamgamenumber'),
        										'eMinBench')
gbgdfCombined$eMin = with(gbgdfCombined, probStart * eMinStart + probOffBench * eMinBench)

ViewPlayer = function(playerString, mySeasonNumber) {
	 myInfo = gbgdfCombined %>%
				filter(grepl(playerString, player) & seasonNumber == mySeasonNumber) %>%
				select(date, startTime, endTime, minute, available, probStart, eMinStart, eMinBench, eMin)

	return(myInfo)
}

with(gbgdfCombined[gbgdfCombined$available & !gbgdfCombined$isStart & gbgdfCombined$played,],mean(minute - eMinBench))
# [1] 0.135931

with(gbgdfCombined[gbgdfCombined$available & gbgdfCombined$isStart,],mean(minute - eMinStart))
# [1] -0.4788458
# no probs with that

with(gbgdfCombined[gbgdfCombined$available,],calibplot(probStart, isStart))
with(gbgdfCombined[gbgdfCombined$available & gbgdfCombined$isStart,],calibplot(eMinStart, minute))
with(gbgdfCombined[gbgdfCombined$available & !gbgdfCombined$isStart & gbgdfCombined$played,],
     calibplot(eMinBench, minute))
# all look fine

with(gbgdfCombined[gbgdfCombined$available,], calibplot(eMin, minute))

# the genuinely bad thing about all of this is that the four quantities are all modelled independently when they're clearly correlated. so it might occasionally do wacky things when there isn't much data or where somethin unusual happens. maybe. not sure
# would prefer it if each player had a single 'pecking order' value, so the top players would have high probstart, low probOff bench. the medium ones would have low probstart, medium proboffbench, the worst would have both low. then eminStart and eminBench would also be linked to those. lthough you have oddities like aguero often starts but is often subbed, while defenders always play 90 mins if they start basically

ViewCurrentTeamEMin = function(myTeam) {
  maxGame = with(resultdf, max(teamgamenumber[season == currentseason & team == myTeam]))
  toDisplay = gbgdfCombined %>%
    filter(season == currentseason & teamgamenumber == maxGame & team == myTeam) %>%
    select(player, probStart, probOffBench, eMin) %>%
    arrange(desc(eMin))
  return(toDisplay)
}

### but we've got a more pressing problem that these functions don't give us the estimate including the latest game. can we rejig them so that they do...?

# think i've cracked it, i think we can just do this
if (FALSE) {
# no it's more fiddly than that, you've got to set the overall prior, put in the season break adjustment etc. will come back to it, sure it can be done though
gbgdf2 = MakeTimeDownweight(gbgdf, gameDownweightCoef, seasonDownweightCoef)

byPlayerInfo = gbgdf2 %>%
					group_by(team, player) %>%
					summarise(sumValue = sum(timeDownweight * value),
								sumNumOb = sum(timeDownweight)) %>%
					mutate(gameForTeamNumber = gi)

### now we can make our probabilities
byPlayerInfo$topLine = with(byPlayerInfo, priorStrength * overallMeanValue + sumValue)
byPlayerInfo$bottomLine = with(byPlayerInfo, priorStrength + sumNumOb)
byPlayerInfo$expectedValue = with(byPlayerInfo, topLine / bottomLine)
}
# in the meantime let's do this rubbish idea:

latestExpectedMinute = gbgdfCombined %>%
											filter(season == currentseason) %>%
											group_by(team, player) %>%
											arrange(desc(gameForTeamNumber)) %>%
											slice(1) %>%
											ungroup()

## this whole code is silly, almost all of it should be in the project/optimise bit. we only need to retrieve the optimum values, then run it for the entire data. but that would require substantial rejigging and we haven't got time

## now maybe we have, let's do it
ffModelPath = paste0(USERPATH, 'ffModel')
devtools::load_all(ffModelPath)

gbgdf$isStart = with(gbgdf, startTime == '0')
gbgdf$available = with(gbgdf, !startTime %in% c('injury', 'suspension'))

theta = c(-1.031, 0.785, -0.658);quantityChoice = 'probStart'
