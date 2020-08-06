
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

FillOutEndTimeColumn = function(gbgdf, resultDF) {
  resultDF2 = resultDF %>%
    mutate(matchKey = paste(date, ifelse(isHome, team, oppteam)))
  
  gbgdf2 = gbgdf %>%
    lazy_left_join(resultDF2, c('season', 'team', 'teamgamenumber'), 'matchKey')
  maxTimeByMatch = gbgdf2 %>%
    mutate(endTime2 = case_when(
      is.na(endTime) ~ '-100',
      (!is.na(endTime) & endTime == 'U') ~ '-100',
      (!is.na(endTime) & endTime == 'F') ~ '94',
      TRUE ~ endTime)) %>%
    mutate(endTime2 = as.integer(endTime2)) %>%
    group_by(matchKey) %>%
    summarise(maxEndTime = max(endTime2))
  
  gbgdf2 = gbgdf2 %>%
    left_join(maxTimeByMatch,
              'matchKey')
  gbgdf2 = gbgdf2 %>%
    mutate_cond(!is.na(endTime) & endTime == 'F',
                endTime = as.character(maxEndTime))
  
  gbgdf2 = within(gbgdf2, rm(matchKey, maxEndTime))
  
  return(gbgdf2)
}

BolsterGbgDF = function(gbgdf, resultDF) {
  
  gbgdf = FillOutEndTimeColumn(gbgdf, resultDF)
  
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

	resultDF = left_join(resultDF,
							shotinfo %>%
							dplyr::rename(oppteam = team,
											shotontconceded = shotont),
							by = c('season', 'teamgamenumber', 'oppteam'))

	resultDF = resultDF %>%
					mutate(gksave = shotontconceded - conceded)

	gbgdf = left_join(gbgdf,
						resultDF %>%
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
	summaryDF = gbgdf %>%
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
		missingcol = setdiff(names(summaryDF),names(currentsquad))
		currentsquad[,missingcol]=NA
		currentsquad = currentsquad[,names(summaryDF)]
		currentsquad = as_tibble(currentsquad)
		summaryDF = bind_rows(summaryDF, currentsquad)
	}
	if (!beforeseason) {
		print('probably a good idea to indicate which players have left in summaryDF')
	}
	return(summaryDF)
}
