
LoadSoccerwayData = function() {

	fileIn = paste0(DATAPATH, 'soccerway_saved/appearance.csv')
	appearanceDF = read.csv(fileIn, as.is = TRUE)

	resultDF = ffDataLoading:::GetResultDF()
	## for some reason i don't know right now, soccerway code need seasonNumber, not season, let's make that
	seasondf = resultDF %>%
	            distinct(season) %>%
	            arrange(season) %>%
	            mutate(seasonNumber = 1:n())
	resultDF = left_join(resultDF, seasondf, 'season')

	appearanceDF = lazy_left_join(appearanceDF,
									resultDF,
									c('date', 'team'),
									c('season', 'seasonNumber', 'teamgamenumber'))
	appearanceDF = ffDataJoining:::.FillMissingAppearanceDF(appearanceDF, resultDF)
	appearanceDF = lazy_left_join(appearanceDF, seasondf, 'seasonNumber', 'season')

	return(appearanceDF)
}

.FindMissingGameDF = function(mySeasonNumber, myTeam, myPlayer, myPlayerId, myTeamGameNumber) {
	missingGame = setdiff(min(myTeamGameNumber): max(myTeamGameNumber), myTeamGameNumber)
	if (length(missingGame) > 0) {
		myMissingDF = tibble(seasonNumber = rep(mySeasonNumber[1], length(missingGame)),
							team = rep(myTeam[1], length(missingGame)),
							playerid = rep(myPlayerId[1], length(missingGame)),
							player = rep(myPlayer[1], length(missingGame)),
							teamgamenumber = missingGame,
							startTime = rep('UU', length(missingGame)))
	}
	if (length(missingGame) == 0) {
		myMissingDF = tibble(seasonNumber = integer(0),
							team = character(0),
							playerid = integer(0),
							player = character(0),
							teamgamenumber = integer(0),
							startTime = character(0))
	}
	return(myMissingDF)
}

.FillMissingAppearanceDF = function(appearanceDF, resultDF) {
	missingDF = appearanceDF %>%
				group_by(seasonNumber, team, player) %>%
				do(ffDataJoining:::.FindMissingGameDF(.$seasonNumber, .$team, .$player, .$playerid, .$teamgamenumber))
	# but now we need to join it to appearanceDF with its extra columns
	missingDF = lazy_left_join(missingDF, resultDF, c('seasonNumber', 'teamgamenumber', 'team'), 'date')
	missingDF$endTime = NA
	appearanceDF = bind_rows(appearanceDF, missingDF) %>%
					arrange(seasonNumber, team, player, teamgamenumber)
	return(appearanceDF)
}
