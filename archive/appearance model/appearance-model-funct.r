.SingleRowMakeFocusDF = function(myMatchRow) {
	myDF = inner_join(appearanceDF,
						myMatchRow,
						c('team', 'date')) %>%
						mutate(started = (startTime == 0),
								available = !startTime %in% c('injury', 'suspension')) %>%
						select(player, started, available)

	# think we want this:
	myModelDF = with(myDF,
					tibble(focusPlayer = rep(player, nrow(myDF)),
							focusStarted = rep(started, nrow(myDF)),
							focusAvailable = rep(available, nrow(myDF)),
							otherPlayer = rep(player, rep(nrow(myDF), nrow(myDF))),
							otherStarted = rep(started, rep(nrow(myDF), nrow(myDF))),
							otherAvailable = rep(available, rep(nrow(myDF), nrow(myDF)))))

	return(myModelDF)
}

MakeFocusDF = function(myTeam) {

	matchDF = appearanceDF %>%
				filter(team == myTeam) %>%
				distinct(team, date)

	myList = NULL
	for (j in 1:nrow(matchDF)) {
		myList[[j]] = .SingleRowMakeFocusDF(matchDF[j,])
	}

	teamAppDF = bind_rows(myList)

	return(teamAppDF)
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
