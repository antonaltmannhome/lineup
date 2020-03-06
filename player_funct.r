
#ok the problem is files are stored vertically, but some functions want it horizontal. need to make a decision here. storing horizontally seems fine to me, just have to have easy way of transferring the objects from vertical to horizontal and back

processdeserved=function(mydf, getffpoint = FALSE) {

	### check all necessary columns are there
	if (getffpoint) {
		if (!'ffPosition' %in% names(mydf)) {
			stop('Need to have ffPosition columns, run getplayerprice to get it\n')
		}
	}

	goalmaxinfo=dget(file=paste(USERPATH,'whoscored/shotvalue.dat',sep=''))
	assistmaxinfo=dget(file=paste(USERPATH,'whoscored/passvalue.dat',sep=''))

	### now modify the goals scored to take away penalties scored
	mydf[,'opgoal']=mydf[,'goal']-mydf[,'penaltyscored']

	### process shot zones calculations
	mydf=mydf %>%
	      mutate(shotobcredit = goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotob']*shotoob, shotibcredit=goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotib']*(shot6yd + shotib))
	### then shots accuracy
	mydf=mydf %>%
	      mutate(goalcredit  = (1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotgoal']*opgoal,
	             ontargetcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshottarget']*ont,
	             misscredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotmiss']*(offt+bar),
	             blockcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotblock']*block)

	### then add all the bastards up
	mydf = mydf %>%
	      mutate(deservedgoal=shotobcredit + shotibcredit + goalcredit + ontargetcredit + misscredit + blockcredit,
	             deservedgoalpg=ifelse(minute>0,94*deservedgoal/minute,NA))

	### then add in deserved assists
	mydf=mydf %>%
	    mutate(assistcredit = assistmaxinfo$passtheta['oosassist']*assist,
	           keypasscredit=assistmaxinfo$passtheta['ooskeypass']*(longkp + shortkp))
	### then add all the bastards up
	mydf = mydf %>%
	    mutate(deservedassist=assistcredit + keypasscredit,
	           deservedassistpg=ifelse(minute>0,94*deservedassist/minute,NA))

	if (getffpoint) {
		### we then want expected fantasy points
		pointdf=data.frame(pos=c('g','d','m','f'),point=c(6,6,5,4))
		mydf$goalpoint = pointdf$point[match(mydf$ffPosition,pointdf$pos)]
		mydf$assistpoint=3
		mydf = mydf %>%
		      mutate(deservedpoint = goalpoint*deservedgoal + assistpoint*deservedassist,
		             deservedpointpg=deservedpoint*94/minute)

		### get rid of intermediate columns
		mydf = mydf %>% select(-c(goalpoint,assistpoint))
	}

	return(mydf)
}

modelexpectedsave = function(gbgdf) {
	isGoalkeeperIndex = with(gbgdf, mainpos == 'GK' & minute>89)
	mod = glm(gksave ~ teamoddseconceded, data = gbgdf[isGoalkeeperIndex,], family = 'poisson')
	return(coef(mod))
}

getplayerfixture=function(fixtdf, playerDF, gbgdf) {
	### want a data frame of future player/fixture combos, along with expected points in each match and the weight

	### now merge in the players data

  playerfixtdf=inner_join(playerDF,
                          fixtdf %>%
                            select(date, team, oppteam, isHome,
                                   teamgamenumber, gameweek, gwweight,
                                   escored, econceded), by = 'team') %>%
					dplyr::rename(eteamscored = escored,
									eteamconceded = econceded)
	
	
	
	### now calculate expected goals and assists
	playerfixtdf = playerfixtdf %>%
					arrange(team,player,gameweek, date) %>%
					mutate(egoal = chnormgoalrate * eteamscored,
							eassist = chnormassistrate * eteamscored)

	### then we need the goalkeeper predictions
	gksavecoef = modelexpectedsave(gbgdf)
	playerfixtdf = playerfixtdf %>%
					mutate(egksave = if_else(mainpos == 'GK',
											exp(gksavecoef[[1]] + gksavecoef[[2]] * eteamconceded),
											0))

	return(playerfixtdf)
}

getfixtureexpectedpoint=function(playerfixtdf) {

	playerfixtdf = playerfixtdf %>%
			mutate(
			appearancepoint = 2,
			goalpoint = case_when(
				ffPosition %in% c('g', 'd') ~ (6 + 1.5) * egoal,
				ffPosition == 'm' ~ (5 + 1.5) * egoal,
				ffPosition == 'f' ~ (4 + 1.5) * egoal),
			assistpoint = (3 + 0.75)*eassist,
			goalconcededpoint = case_when(
				ffPosition %in% c('g', 'd') ~ - 2*(dpois(2,eteamconceded) +
													dpois(3,eteamconceded)) -
												4*(dpois(4,eteamconceded) +
													dpois(5,eteamconceded)),
				ffPosition %in% c('m', 'f') ~ 0),
			cleansheetpoint = case_when(
				ffPosition %in% c('g', 'd') ~ (4 + 0.75)*dpois(0,eteamconceded),
				ffPosition == 'm' ~ 1 * dpois(0,eteamconceded),
				ffPosition == 'f' ~ 0),
			savepoint = case_when(ffPosition == 'g' ~ 0.25 * egksave,
									ffPosition != 'g' ~ 0),
			expectedpoint90min = appearancepoint + goalpoint + assistpoint + goalconcededpoint + cleansheetpoint + savepoint,
			expectedpoint = eMin/94 * expectedpoint90min) %>%
			select(-c(goalpoint, assistpoint, goalconcededpoint, cleansheetpoint, savepoint))

	return(playerfixtdf)
}

getlongtermplayerpoint=function(playerfixtdf) {

	## make a player by player summary
	playersummary=playerfixtdf %>%
					group_by(team, player, ffPosition) %>%
					summarise(epoint=sum(expectedpoint * gwweight)) %>%
					ungroup()

	return(playersummary)
}

getplayervalue=function(playerDF, playerfixtdf) {
	### firstly get expected points scord by each active player
	playerDF = left_join(playerDF,
												playerfixtdf %>%
													group_by(team, player) %>%
													summarise(epoint = sum(expectedpoint * gwweight)) %>%
													ungroup(),
													c('team', 'player'))

	### then get hold of players values
	### join up prices
	playerDF$value=with(playerDF,round(epoint/ffPrice,2))
	playerDF=playerDF %>%
					group_by(ffPosition) %>%
					mutate(valuerank=rank(-value)) %>%
					ungroup()
	### arrange in nice order
	playerDF = playerDF %>%
							arrange(team,
												match(ffPosition,c('g','d','m','f')),
													ffPrice)

	return(playerDF)
}

getcurrentplayervalue=function(playervalue) {
	currentteam=read.csv(paste(DATAPATH,'currentteam.csv',sep=''))
	mdum=match(with(currentteam,paste(team,player)),with(playervalue,paste(team,player)))
	coltocopy=c('epoint90min','active','epoint','price','value','valuerank')
	currentteam[,coltocopy]=playervalue[mdum,coltocopy]
	return(currentteam)
}

getcurrentexpectedpoint=function(playerfixtdf, mysquad) {
	currentplayerfixtepoint = playerfixtdf %>%
							filter(!is.na(player)) %>%
							filter(gameweek == min(gameweek)) %>%
							filter(paste(team, player) %in% with(mysquad, paste(team, player))) %>%
							select(team, player, expectedpoint, ffPosition)
	groupedcurrentfixtepoint = currentplayerfixtepoint %>%
								group_by(team, player) %>%
								summarise(expectedpoint = sum(expectedpoint),
											numfixt = n()) %>%
								mutate(expectedpoint = expectedpoint) %>%
								select(-numfixt)
	currentsquadepoint =  left_join(groupedcurrentfixtepoint,
									currentplayerfixtepoint %>%
									select(team, player, ffPosition),
									by = c('team', 'player')) %>%
							distinct() %>%
							arrange(match(ffPosition, c('g','d','m','f')), -expectedpoint)
	return(currentsquadepoint)
}

makeseasondeservedsummary = function(summarydf, gbgdf) {
	### check that processdeserved has been done
	if (!'deservedgoal' %in% names(summarydf)) stop('need to run processdeserved')
	seasonplayersummary=NULL
	seasonteamsummary=NULL
	for (si in 1:nrow(seasoninfo)) {
		if (!seasoninfo$havegbg[si]) {
			### need to load summary
			tempsummary = read.csv(paste(DATAPATH,seasoninfo$season[si],'/model.csv',sep=''),as.is=T)
			### only want the necessary columns
			tempdeservedsummary = tempsummary %>% select(team, player, minute, deservedgoal ,deservedassist)
			tempdeservedsummary$season=seasoninfo$season[si]
			# but also want total goals by the team
			tempteamsummary = tempsummary %>%
								group_by(team) %>%
								summarise(goal = sum(goal))
		}
		if (seasoninfo$havegbg[si]) {
			tempdeservedsummary = summarydf %>%
							filter(season == seasoninfo$season[si]) %>%
							select(season, team, player, minute, deservedgoal ,deservedassist)
			tempteamsummary = summarydf %>%
								filter(season == seasoninfo$season[si]) %>%
								group_by(team) %>%
								summarise(goal = sum(goal))
			# but we also want to acquire the info for the promoted teams
			if (seasoninfo$season[si] != currentseason) {
				promotedtempsummary = read.csv(paste(DATAPATH,seasoninfo$season[si],'/model.csv',sep=''),as.is=T)
				promoteddeservedsummary = promotedtempsummary %>%
										mutate(season = seasoninfo$season[si]) %>%
										select(season, team, player, minute, deservedgoal ,deservedassist)
				promotedteamsummary = promotedtempsummary %>%
										group_by(team) %>%
										summarise(goal = sum(goal))
				### join em up
				tempdeservedsummary = bind_rows(tempdeservedsummary, promoteddeservedsummary)
				tempteamsummary = bind_rows(tempteamsummary, promotedteamsummary)
			}
		}
		seasonplayersummary[[si]]=tempdeservedsummary
		tempteamsummary$season=seasoninfo$season[si]
		seasonteamsummary[[si]] = tempteamsummary
	}
	seasondeservedsummary = as_tibble(bind_rows(seasonplayersummary))
	seasonteamsummary = as_tibble(bind_rows(seasonteamsummary))
	seasondeservedsummary = left_join(seasondeservedsummary,
										seasonteamsummary,
										by = c('team', 'season'))

	# but then we want that joined to gbgdf in almost any situation i would have thought
	seasoninfo$previousseason = c(NA,seasoninfo$season[1:(nrow(seasoninfo)-1)])
	gbgdf$previousseason = seasoninfo$previousseason[match(gbgdf$season, seasoninfo$season)]
	# NB this only uses one previous season, a bit crap but it'll do for now
	gbgdftoseasondeservedsummarymap = match(
									with(gbgdf, paste(previousseason, player)),
									with(seasondeservedsummary, paste(season, player)))
	gbgdf$previoustotalminute=seasondeservedsummary$minute[gbgdftoseasondeservedsummarymap]
	gbgdf$previousdeservedgoal=seasondeservedsummary$deservedgoal[gbgdftoseasondeservedsummarymap]
	gbgdf$previousdeservedassist=seasondeservedsummary$deservedassist[gbgdftoseasondeservedsummarymap]
	gbgdf$previousteamgoal=seasondeservedsummary$goal[gbgdftoseasondeservedsummarymap]

	if (FALSE) {
		stop('totally wrong for when a player switches team\n')
		gbgdftoseasonteamsummarymap = match(
									with(gbgdf, paste(previousseason, team)),
									with(seasonteamsummary, paste(season, team)))
		gbgdf$previousteamgoal = seasonteamsummary$goal[gbgdftoseasonteamsummarymap]
	}

	return(list(seasondeservedsummary = seasondeservedsummary,
				gbgdf = gbgdf,
				seasonteamsummary = seasonteamsummary))
}
