
GetResultDF = function() {
	resultlist = NULL
	for (si in 1:nrow(seasoninfo)) {
		if (seasoninfo$havegbg[si]) {
			resultlist[[si]] = read.csv(paste(DATAPATH,'fixture_result/result',seasoninfo$season[si],'.csv',sep=''),as.is=TRUE)
			resultlist[[si]]$season = seasoninfo$season[si]
		}
	}
	flatresultdf = bind_rows(resultlist)

	## also useful to have flat version of that
	homevertresult = flatresultdf %>%
						dplyr::rename(team = ht, scored = hsc, oppteam = at, conceded = asc) %>%
						mutate(isHome = TRUE)
	awayvertresult = flatresultdf %>%
						dplyr::rename(team = at, scored = asc, oppteam = ht, conceded = hsc) %>%
						mutate(isHome = FALSE)
	resultdf = bind_rows(homevertresult, awayvertresult)

	resultdf = resultdf %>%
				arrange(date)

	resultdf = resultdf %>%
				group_by(season, team) %>%
				arrange(date) %>%
				mutate(teamgamenumber = 1:n()) %>%
				ungroup()

	return(resultdf = resultdf)
}

GetFixtDF = function() {

	### then get the fixtures
	flatfixtdf=read.csv(paste(DATAPATH,'fixture_result/fixture',currentseason,'.csv',sep=''))
	### slot in the gameweeks
	gameweekdf=read.csv(paste(DATAPATH,'gameweek_deadline_',currentseason,'.csv',sep=''))
	gameweekdf$deadline = lubridate::ymd_hms(paste(gameweekdf$deadline, '11:59:59'))
	flatfixtdf$ymd_hms = lubridate::ymd_hms(paste(flatfixtdf$date, '12:01:01'))
	flatfixtdf$gameweek=gameweekdf$gameweek[findInterval(flatfixtdf$ymd_hms,gameweekdf$deadline)]
	### but we want to convert that to a weight - 1 for next fixture, 0.1 for ten games time, 0 thereafter
	currentgameweek=min(flatfixtdf$gameweek)
	flatfixtdf$gwweight=rep(NA,nrow(flatfixtdf))
	flatfixtdf$gwweight[which(flatfixtdf$gameweek>=currentgameweek+10)]=0
	flatfixtdf$gwweight[which(flatfixtdf$gameweek<currentgameweek+10)]=seq(1,0.1,-0.1)[flatfixtdf$gameweek[which(flatfixtdf$gameweek<currentgameweek+10)]-currentgameweek+1]

	flatfixtdf = within(flatfixtdf, rm(ymd_hms))

	# let's now hamke it like results, with a team and isHome column

	homevertfixt = flatfixtdf %>%
						dplyr::rename(team = ht, oppteam = at) %>%
						mutate(isHome = TRUE)
	awayvertfixt = flatfixtdf %>%
						dplyr::rename(team = at, oppteam = ht) %>%
						mutate(isHome = FALSE)
	fixtdf = bind_rows(homevertfixt, awayvertfixt)

	fixtdf = fixtdf %>%
				arrange(date)

	return(fixtdf)
}

CheckSpreadexUpToDate = function(fixtDF) {
	# very easy to forget to update the odds, this function will warn you if you appear not to have updated them
	oddsStatus = fixtDF %>%
											filter(gameweek == min(gameweek)) %>%
											summarise(oddsNeedUpdating = all(is.na(oddsescored)) & all(is.na(oddseconceded)),
																oddsMightNeedUpdating = !all(is.na(oddsescored) & is.na(oddseconceded)) &
																 												any(is.na(oddsescored) | is.na(oddseconceded)))

	if (oddsStatus$oddsNeedUpdating) {
		message('You do not have any odds entered for the upcoming set of fixtures. Do not forget to add them!')
	}
	if (oddsStatus$oddsMightNeedUpdating) {
		message('You have odds for some but not all of the upcoming set of fixtures. Do not forget to add them!')
	}
}

AlignOddsWithResultsAndFixtures = function(resultDF, fixtDF) {

	allseason = unique(resultDF$season)
	spreadexfilein = paste0(DATAPATH, 'spreadex_', allseason, '.csv')
	spreadexlist = NULL
	for (si in 1:length(allseason)) {
		spreadexlist[[si]] = read.csv(spreadexfilein[si], as.is = TRUE)
	}
	rawspreadexdf = bind_rows(spreadexlist)

	# need to match to the longer version of fixtDF and resultDF though
	homespreadexdf = rawspreadexdf %>%
						dplyr::rename(team = ht, oppteam = at) %>%
						mutate(isHome = TRUE,
								oddsescored = (totspread + supspread) / 2,
								oddseconceded = (totspread - supspread) / 2)
	awayspreadexdf = rawspreadexdf %>%
						dplyr::rename(team = at, oppteam = ht) %>%
						mutate(isHome = FALSE,
								oddsescored = (totspread - supspread) / 2,
								oddseconceded = (totspread + supspread) / 2)
	spreadexdf = bind_rows(homespreadexdf, awayspreadexdf)
	resultDF = lazy_left_join(resultDF, spreadexdf, c('date', 'team', 'oppteam'))
	fixtDF = lazy_left_join(fixtDF, spreadexdf, c('date', 'team', 'oppteam'))

	resultDF = resultDF %>%
				select(-c(supspread, totspread))
	fixtDF = fixtDF %>%
				select(-c(supspread, totspread))

	ffDataLoading:::CheckSpreadexUpToDate(fixtDF)

	return(list(resultDF = resultDF,
				fixtDF = fixtDF))
}

AlignGameweekAndSeasonWithResultDF = function(resultDF) {
	### then declare teh gameweek for each game
	resultDF[,c('season','gameweek')] = NA
	for (si in which(seasoninfo$havegbg)) {
		gameweekdf=read.csv(paste(DATAPATH,'gameweek_deadline_',seasoninfo$season[si],'.csv',sep=''),as.is=T)
		sax=with(resultdf, which(between(date, seasoninfo$start[si], seasoninfo$end[si])))
		resultDF$season[sax]=seasoninfo$season[si]
		resultDF$gameweek[sax]=gameweekdf$gameweek[findInterval(resultdf$date[sax],gameweekdf$deadline)]
	}

	return(resultDF = resultDF)
}
