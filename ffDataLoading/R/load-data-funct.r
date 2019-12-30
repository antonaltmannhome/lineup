
ReadCurrentSeasonPlayerDF = function() {

  currentSeasonPlayerIdFile = paste0(DATAPATH, 'current-season-player-id.csv')

  currentSeasonPlayerDF = readr::read_csv(currentSeasonPlayerIdFile, col_types = list(
    season = readr::col_integer(),
    playerid = readr::col_integer(),
    soccerwayPlayer = readr::col_character(),
    whoscoredPlayer = readr::col_character(),
    team = readr::col_character(),
    ffuseswholename = readr::col_logical(),
    hasleft = readr::col_logical(),
    adjustedwhoscoredPlayer = readr::col_character()
  ))

  return(currentSeasonPlayerDF)
}

ReadGbgDF = function() {
  gbgdf = readr::read_csv(paste(DATAPATH, 'player-result.csv'),
    col_types = list(
        date = readr::col_integer(),
        team = readr::col_character(),
        playerid = readr::col_integer(),
        soccerwayPlayer = readr::col_character(),
        player = readr::col_character(),
        startTime = readr::col_character(),
        endTime = readr::col_character(),
        seasonNumber = readr::col_integer(),
        teamgamenumber = readr::col_integer(),
        season = readr::col_integer(),
        goal = readr::col_integer(),
        assist = readr::col_integer(),
        shotoob = readr::col_integer(),
        shot6yd = readr::col_integer(),
        shotib = readr::col_integer(),
        openplay = readr::col_integer(),
        counter = readr::col_integer(),
        setpiece = readr::col_integer(),
        penaltytaken = readr::col_integer(),
        offt = readr::col_integer(),
        bar = readr::col_integer(),
        ont = readr::col_integer(),
        block = readr::col_integer(),
        penaltyscored = readr::col_integer(),
        longkp = readr::col_integer(),
        shortkp = readr::col_integer()
      ))

  return(gbgdf)
}

BolsterGbgDF = function(gbgdf, resultdf) {
  gbgdf$played = with(gbgdf, !grepl('[^0-9]', startTime))
  gbgdf$minute = 0
  gbgdf$minute[gbgdf$played] = with(gbgdf, as.numeric(endTime[played]) - as.numeric(startTime[played]))
  gbgdf$isStart = with(gbgdf, startTime == '0')
  gbgdf$available = with(gbgdf, !startTime %in% c('injury', 'suspension'))

  gbgdf = gbgdf %>%
			select(season, date, teamgamenumber, everything())

	propertitle = c('AM','D','DMC','FW','GK','M')
	gbgdf = gbgdf %>%
			mutate(mainpos = ifelse(mainpos %in% propertitle,
										mainpos,
										case_when(mainpos == 'Goalkeeper' ~ 'GK',
													mainpos == 'Defender' ~ 'D',
                          mainpos == 'Midfielder' ~ 'M',
													mainpos == 'Forward' ~ 'FW',
													)))
  message('The position mapping code is a bit fragile, worth firming this up at some point')

  ### due to occasional retrospective updates on whoscored, can get negative quantitaties, just set these to zero
	numericColumn = c('minute', 'goal', 'assist', 'shotoob', 'shot6yd', 'shotib', 'openplay', 'counter', 'setpiece', 'penaltytaken', 'offt', 'bar', 'ont', 'block', 'penaltyscored', 'longkp', 'shortkp')
  for (myColumn in numericColumn) {
    gbgdf = gbgdf %>%
            mutate(!!myColumn := pmax(get(myColumn), 0))
  }

  # another condition, if you've played 0 minutes but have a total of anything > 0, set your minutes to 1
	numericColumn2 = setdiff(numericColumn, 'minute')
	didSomething = apply(gbgdf[,numericColumn2] > 0, 1, function(x) any(x))
	overrideZeroMinute = which(gbgdf$minute == 0 & didSomething)
	gbgdf$minute[overrideZeroMinute] = 1

  # the mainpos column is missing for players who are unused at the start of the season, so fill those in with the most frequent position in current season. if they've not played so far this season, use whatever most frequent position was in previosu season. if they're still NA after that, ditch from analysis

  # so let's get each players stats on their position each year
  playerPosStat = gbgdf %>%
    filter(!is.na(mainpos)) %>%
    group_by(season, team, player) %>%
    count(mainpos) %>%
    ungroup()
  # ok so there are genuine examples of players playing in differnt positions. just go with the majority
  playerMainPos = playerPosStat %>%
    group_by(season, team, player) %>%
    summarise(mostFreqPos = mainpos[which.max(n)]) %>%
    ungroup() %>%
    complete(gbgdf %>% distinct(season, team, player),
              fill = list(mostFreqPos = NA)) %>%
    arrange(player) %>%
    group_by(player) %>%
    mutate(prevMostFreqPos = lag(mostFreqPos))

  gbgdf = subset_join(gbgdf,
            playerMainPos %>%
            rename(mainpos = mostFreqPos) %>%
            select(season, team, player, mainpos),
            c('season', 'team', 'player'),
            is.na(mainpos))
  # any left over? then try to use previous season if possible
  gbgdf = subset_join(gbgdf,
            playerMainPos %>%
            rename(mainpos = prevMostFreqPos) %>%
            select(season, team, player, mainpos),
            c('season', 'team', 'player'),
            is.na(mainpos))


	# however we also want data about goalkeeper saves, which is fiddly
	# although process is similar to getting hold of opponent and oppnenet's expected goals which are useful, so we'll get them at same time
	shotinfo = gbgdf %>%
			group_by(season, teamgamenumber, team) %>%
			summarise(shotont = sum(ont))

	resultdf = left_join(resultdf,
							shotinfo %>%
							dplyr::rename(oppteam = team,
											shotontconceded = shotont),
							by = c('season', 'teamgamenumber', 'oppteam'))

	resultdf = resultdf %>%
					mutate(gksave = shotontconceded - conceded)

	gbgdf = left_join(gbgdf,
						resultdf %>%
						select(date, daynum, team, oddsescored, oppteam, conceded, oddseconceded, gksave) %>%
						dplyr::rename(teamconceded = conceded,
										teamoddsescored = oddsescored,
										teamoddseconceded = oddseconceded),
						by = c('date', 'team'))
	gbgdf$gksave[which(gbgdf$gksave < 0)] = 0

  gbgdf = gbgdf %>%
    group_by(team, player) %>%
    arrange(seasonNumber, teamgamenumber) %>%
    mutate(gameForTeamNumber = 1:n()) %>%
    ungroup()
	  
	return(gbgdf)
}

GetSummaryDF = function(gbgdf, beforeseason=F) {
	summarydf = gbgdf %>%
				group_by(season, team, player) %>%
				summarise(minute = sum(minute),
							goal = sum(goal),
							assist = sum(assist),
							shotoob = sum(shotoob),
							shot6yd = sum(shot6yd),
							shotib = sum(shotib),
							openplay = sum(openplay),
							counter = sum(counter),
							setpiece = sum(setpiece),
							penaltytaken = sum(penaltytaken),
							offt = sum(offt),
							bar = sum(bar),
							ont = sum(ont),
							block = sum(block),
							penaltyscored = sum(penaltyscored),
							longkp = sum(longkp),
							shortkp = sum(shortkp)) %>%
				ungroup()
	if (beforeseason) {
		### won't have any of the current squad, so need to tack that on
		currentsquad = getcurrentsquad()
		currentsquad$season=currentseason
		missingcol = setdiff(names(summarydf),names(currentsquad))
		currentsquad[,missingcol]=NA
		currentsquad = currentsquad[,names(summarydf)]
		currentsquad = as_tibble(currentsquad)
		summarydf = bind_rows(summarydf, currentsquad)
	}
	if (!beforeseason) {
		print('probably a good idea to indicate which players have left in summarydf')
	}
	return(summarydf)
}
