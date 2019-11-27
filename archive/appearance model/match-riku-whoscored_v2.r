# let's make another attempt at matching soccerway and whoscored data

# so we luckily have a file with all unique soccerway players. we don't have that for whoscored unfortuantely because the player ids are so hard to get

# but we can try to auto match as many as we can, and manually collect the others

# but what are the unique whoscored players? think we have to divide up by season, so e.g arsenal-bellerin is three separate players, who all match to the since soccerway one

source('c:/research/lineup/appearance model/appearance-model-startup.r')

resultdf = getresultdf()
fixtdf = getfixtdf()

dum = matchupspreadexdata(resultdf, fixtdf)
resultdf = dum$resultdf
fixtdf = dum$fixtdf

gbgdf=getgbgdf(resultdf)

appearanceDF = lazy_left_join(appearanceDF,
								resultdf,
								c('date', 'team'),
								c('season', 'teamgamenumber'))


# let's do it team by team - want facility to update of course
MatchSoccerwayPlayerToWhoscoredByTeam = function(myTeam) {
	mySoccerwayPlayer = appearanceDF %>%
						filter(team == myTeam) %>%
						rename(rikuName = player) %>%
						group_by(season, playerid, rikuName) %>%
						summarise(rikuNumGame = sum(!is.na(endTime) & endTime != 'U')) %>%
						filter(rikuNumGame > 0) %>%
						ungroup %>% 
						mutate(rowIndex = 1:n())
						
	myWhoScoredPlayer = gbgdf %>%
						filter(team == myTeam) %>%
						rename(WSName = player) %>%
						group_by(season, WSName) %>%
						summarise(WSNumGame = sum(minute > 0)) %>%
						ungroup()
						
	mySoccerwayPlayer$nameMatched = '*'
	myWhoScoredPlayer$alreadyMatched = FALSE
	# so firstly, match by name if we can
	mapByName = match(with(mySoccerwayPlayer, paste(season, rikuName, rikuNumGame)),
						with(myWhoScoredPlayer, paste(season, WSName, WSNumGame)))
	mySoccerwayPlayer[,c('WSName', 'WSNumGame')] =
		myWhoScoredPlayer[mapByName, c('WSName', 'WSNumGame')]
	mySoccerwayPlayer$nameMatched[!is.na(mapByName)] = ' '
	myWhoScoredPlayer$alreadyMatched[mapByName] = TRUE
	
	# now the other ones, match by number of games if possible
	missingIndex = mySoccerwayPlayer %>% filter(is.na(WSName)) %>% pull(rowIndex)
	
	mySoccerwayPlayer$likelyMatchIndex = NA
	for (mi in missingIndex) {
		dum = mySoccerwayPlayer %>% filter(rowIndex == mi)
		likelyMatch = with(myWhoScoredPlayer,
							which(season == dum$season &
									WSNumGame == dum$rikuNumGame &
									!alreadyMatched))
		if (length(likelyMatch) == 1) {
			mySoccerwayPlayer$WSName[mi] = myWhoScoredPlayer$WSName[likelyMatch]
			mySoccerwayPlayer$likelyMatchIndex[mi] = likelyMatch
		}
	}
	
	# now ask me if i agree with all the matches
	print(mySoccerwayPlayer %>%
			filter(rowIndex %in% missingIndex & !is.na(WSName)) %>%
			select(rowIndex, season, rikuName, WSName))
	print('Enter all the ones that you think are wrong, separated by ENTER (blank when done)')
	finished = FALSE
	wrongIndex = NULL
	while(!finished) {
		myWrong = askcond(T, T)
		if (!is.null(myWrong)) {
			wrongIndex = c(wrongIndex, myWrong)
		}
		if (is.null(myWrong)) {
			finished = TRUE
		}
	}

	if (length(wrongIndex) > 0) {
		mySoccerwayPlayer$WSName[mySoccerwayPlayer$rowIndex %in% wrongIndex] = NA
		mySoccerwayPlayer$likelyMatchIndex[mySoccerwayPlayer$rowIndex %in% wrongIndex] = NA
	}
	
	# we can now say that the likelyMatch ones in myWhoScoredPlayer are alreadyMatched
	myWhoScoredPlayer$alreadyMatched[unique(mySoccerwayPlayer$likelyMatchIndex)]= TRUE
	
	# now loop through the unmatched ones, ask user to identify them
	if (any(is.na(mySoccerwayPlayer$WSName))) {
		myUnSeason = unique(mySoccerwayPlayer$season[is.na(mySoccerwayPlayer$WSName)])
		for (si in 1:length(myUnSeason)) {
			currentSeasonSoccerwayMissing = mySoccerwayPlayer %>%
									filter(season == myUnSeason[si] & is.na(WSName))
			currentSeasonWhoScoredUnmatched = myWhoScoredPlayer %>%
												filter(season == myUnSeason[si] & !alreadyMatched) %>%
												mutate(rowIndex = 1:n()) %>%
												select(rowIndex, WSName, WSNumGame)
			for (j in 1:nrow(currentSeasonSoccerwayMissing)) {
				cat('\n\n')
				print(as.data.frame(currentSeasonSoccerwayMissing[j,c('season', 'rikuName', 'rikuNumGame')]))
				print('Which player is this (enter for none)?')
				print(as.data.frame(currentSeasonWhoScoredUnmatched))
				myRowIndex = askcond(T, T)
				if (!is.null(myRowIndex)) {
					fullSoccerwayPlayerIndex = which(mySoccerwayPlayer$rowIndex == currentSeasonSoccerwayMissing$rowIndex[j])
					mySoccerwayPlayer$WSName[fullSoccerwayPlayerIndex] = currentSeasonWhoScoredUnmatched$WSName[myRowIndex]
				}
			}
		}
	}
	
	return(mySoccerwayPlayer)
}

## ok, it's storing/'updating the bastards that we've next got to worry about

unteam = unique(resultdf$team)

for (ui in 1:length(unteam)) {
	dum = MatchSoccerwayPlayerToWhoscoredByTeam(unteam[ui])
	### lots of rubbish in that, just select the important bits
	fileOut = paste0(DATAPATH, '/soccerway_saved/soccerway-whoscored-', unteam[ui], '.csv')
	write.csv(file = fileOut,
				dum %>%
				select(season, playerid, rikuName, WSName) %>%
				rename(soccerwayPlayer = rikuName,
						whoscoredPlayer= WSName),
				row.names = FALSE)
}

### awesome, can do the matching up next time we load in the data. still need to think about updating as new data comes in though
