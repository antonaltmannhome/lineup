# let's have dedicated file for all code relating to calculating deserved goals and assists

CalculateDeservedGoalAssist = function(mydf) {

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

	return(mydf)
}

CalculateSeasonDeservedGoalAssist = function(summarydf, gbgdf) {
	### check that processdeserved has been done
	if (!'deservedgoal' %in% names(summarydf)) stop('need to run processdeserved')

	summary1516 = read.csv(paste(DATAPATH,'1516/model.csv',sep=''),as.is=T) %>%
	              mutate(season = 1516)

	dum = vector('list', nrow(seasoninfo))
	for (si in 1:nrow(seasoninfo)) {
	  if (seasoninfo$havegbg[si] & seasoninfo$season[si] != currentseason) {
	    promotedtempsummary = read.csv(paste(DATAPATH,seasoninfo$season[si],'/model.csv',sep=''),as.is=T)
	    promoteddeservedsummary = promotedtempsummary %>%
	      mutate(season = seasoninfo$season[si]) %>%
	      select(season, team, player, minute, deservedgoal ,deservedassist, goal, assist)

	    dum[[si]] = promoteddeservedsummary
	  }
	}
	promotedTeamSummary = bind_rows(dum)

	combinedSummaryDF = bind_rows(summary1516, summarydf, promotedTeamSummary)
	seasonteamsummary = combinedSummaryDF %>%
	  group_by(team, season) %>%
	  summarise(teamgoal = sum(goal),
	            teamassist = sum(assist))

	seasondeservedsummary = left_join(combinedSummaryDF %>%
	                                    select(season, team, player, minute, deservedgoal ,deservedassist, goal),
										                  seasonteamsummary,
										                  by = c('team', 'season'))

	# but then we want that joined to gbgdf in almost any situation i would have thought
	# and we want to line up previous season information
	# which is a bit of a hssle to set up, remember a player can change teams mid season, so we need to find the info from the preceding season, so doing arrange(season) then lag won't work
	seasoninfo = seasoninfo %>%
								arrange(season) %>%
								mutate(previousseason = lag(season))
	seasondeservedsummary = lazy_left_join(seasondeservedsummary, seasoninfo, 'season', 'previousseason')
	seasondeservedsummary$previousinfomap = with(seasondeservedsummary,
															match(paste(previousseason, player), paste(season, player)))
	seasondeservedsummary$previoustotalgame = with(seasondeservedsummary, minute[previousinfomap]/94)
	seasondeservedsummary$previousdeservedgoal = with(seasondeservedsummary, deservedgoal[previousinfomap])
	seasondeservedsummary$previousdeservedassist = with(seasondeservedsummary, deservedassist[previousinfomap])
	seasondeservedsummary$previousteamgoal = with(seasondeservedsummary, teamgoal[previousinfomap])
	seasondeservedsummary$previousteamassist = with(seasondeservedsummary, teamassist[previousinfomap])

	gbgdf = gbgdf %>%
	        lazy_left_join(seasondeservedsummary,
	                       c('season', 'player', 'team'),
	                       c('previoustotalgame', 'previousdeservedgoal', 'previousdeservedassist', 'previousteamgoal', 'previousteamassist'))

	return(list(seasondeservedsummary = seasondeservedsummary,
				      gbgdf = gbgdf))
}

ImputeMissingMatchOdds = function(playerGameDF, resultDF) {

	### really annoying having those NAs for the missing ones, let's just put in average by team for those
	### will replace with proper glm values later
	meanegoalbyteam = resultdf %>%
						group_by(season,team) %>%
						summarise(meanegoal = mean(oddsescored,na.rm=T))
	sax=which(is.na(playerGameDF$teamoddsescored))
	playerGameDF$teamoddsescored[sax] = meanegoalbyteam$meanegoal[match(with(playerGameDF[sax,],paste(season,team)),
															with(meanegoalbyteam, paste(season,team)))]
	playerGameDF$teamoddseconceded[sax] = meanegoalbyteam$meanegoal[match(with(playerGameDF[sax,],paste(season,oppteam)),
	                                                                    with(meanegoalbyteam, paste(season,team)))]
	
	return(playerGameDF)
}

PositionBasedGoalAssistLikFunct = function(theta, runMode, goalassist, playerposition, playerGameDF) {
	names(theta) = playerposition
	playerGameDF$positionrate = with(playerGameDF, exp(theta[mainpos]))
	playerGameDF$positionexpected = with(playerGameDF, positionrate * teamoddsescored*minute/94)
	if (runMode == 'fit') {
		return(playerGameDF$positionrate)
	}
	if (runMode == 'max') {
		if (goalassist == 'goal') {
			playerGameDF$loglik = log(dpois(playerGameDF$goal, playerGameDF$positionexpected))
		}
		if (goalassist == 'assist') {
			playerGameDF$loglik = log(dpois(playerGameDF$assist, playerGameDF$positionexpected))
		}
		sumloglik = mean(playerGameDF$loglik)
		return(-sumloglik)
	}
}

CalculatePositionBasedExpectedGoalAssist = function(playerGameDF) {
	playerposition = c('GK','D','DMC','M','AM','FW')

	for (goalassist in c('goal','assist')) {
		if (goalassist == 'goal') {
			mylogmean = log(mean(playerGameDF$goal))
		}
		if (goalassist == 'assist') {
			mylogmean = log(mean(playerGameDF$assist))
		}
		theta = rep(mylogmean, length(playerposition))
		message('About to find out expected', goalassist,' rate for each position...')
		maxinfo1 = nlm(ffModel:::PositionBasedGoalAssistLikFunct, p = theta, runMode='max',
										goalassist = goalassist, playerposition = playerposition,
										playerGameDF = playerGameDF)

		### this bit is purely for display:
		expectedRate = exp(maxinfo1$est)
		names(expectedRate) = playerposition
		message('Done! The expected rates by position are:')
		print(round(expectedRate, 3))

		mypositionrate = ffModel:::PositionBasedGoalAssistLikFunct(maxinfo1$est, runMode = 'fit',
										goalassist = goalassist, playerposition = playerposition,
										playerGameDF = playerGameDF)

		if (goalassist == 'goal') playerGameDF$positiongoalrate = mypositionrate
		if (goalassist == 'assist') playerGameDF$positionassistrate = mypositionrate
	}

	return(playerGameDF)
}

CalculateHistoricPlayerDeservedGoalAssist = function(playerGameDF) {

	playerGameDF = playerGameDF %>%
									mutate_cond(is.na(previoustotalgame),
															previoustotalgame = 0,
															previousdeservedgoal = 0,
															previousteamgoal = -999,
															previousdeservedassist = 0,
															previousteamassist = -999
														)
	playerGameDF = playerGameDF %>%
									mutate(adjpreviousdeservedgoal = previousdeservedgoal / (previousteamgoal/38),
													adjpreviousdeservedassist = previousdeservedassist / (previousteamassist/38))

	playerGameDF = playerGameDF %>%
			group_by(season,team,player) %>%
			arrange(teamgamenumber) %>%
			mutate(chcumgame = cumsum(minute/94),
					chcumadjdeservedgoal = cumsum(deservedgoal/teamoddsescored),
					chcumadjdeservedassist = cumsum(deservedassist/teamoddsescored),
					legalcumgame = lag(chcumgame, default = 0),
					legalcumadjdeservedgoal = lag(chcumadjdeservedgoal, default = 0),
					legalcumadjdeservedassist = lag(chcumadjdeservedassist, default = 0)) %>%
										ungroup()

	return(playerGameDF)
}


CombinePositionPlayerHistoricExpectedGoalAssistLikFunct = function(theta, runMode, playerGameDF) {
	pospriorstr=exp(theta[1])
	prevseaspriorstr=exp(theta[2])
	# first add in the prior and the previous season's value
	playerGameDF$topline = with(playerGameDF, (pospriorstr*positionrate +
							prevseaspriorstr*adjpreviousdeserved))
	playerGameDF$bottomline = with(playerGameDF, pospriorstr +
								prevseaspriorstr*previoustotalgame)
	# now add in the player historic value
	playerGameDF$topline = playerGameDF$topline + playerGameDF$cumadjdeserved
	playerGameDF$bottomline = playerGameDF$bottomline + playerGameDF$cumgame

	playerGameDF$normalisedrate = playerGameDF$topline / playerGameDF$bottomline
	if (runMode == 'fit') {
		return(playerGameDF %>% pull(normalisedrate))
	}
	if (runMode == 'max') {
		playerGameDF$expectedrate = with(playerGameDF, minute/94 * teamoddsescored * normalisedrate)
		playerGameDF$loglik = log(dpois(playerGameDF$response, playerGameDF$expectedrate))
		sumloglik = mean(playerGameDF$loglik)
		return(-sumloglik)
	}
}

InitialisePositionPlayerHistoricMixColumn = function(playerGameDF, runMode, goalassist) {
	if (runMode == 'fit') {
		playerGameDF = playerGameDF %>%
									mutate(cumgame = chcumgame)
	}
	if (runMode == 'max') {
		playerGameDF = playerGameDF %>%
									mutate(cumgame = legalcumgame)
	}
	if (goalassist == 'goal') {
		playerGameDF = playerGameDF %>%
									mutate(adjpreviousdeserved = adjpreviousdeservedgoal,
													positionrate = positiongoalrate,
													response = goal)
	}
	if (goalassist == 'assist') {
		playerGameDF = playerGameDF %>%
									mutate(adjpreviousdeserved = adjpreviousdeservedassist,
													positionrate = positionassistrate,
													response = assist)
	}
	if (runMode == 'fit' & goalassist == 'goal') {
		playerGameDF = playerGameDF %>%
										mutate(cumadjdeserved = chcumadjdeservedgoal)
	}
	if (runMode == 'max' & goalassist == 'goal') {
		playerGameDF = playerGameDF %>%
										mutate(cumadjdeserved = legalcumadjdeservedgoal)
	}
	if (runMode == 'fit' & goalassist == 'assist') {
		playerGameDF = playerGameDF %>%
										mutate(cumadjdeserved = chcumadjdeservedassist)
	}
	if (runMode == 'max' & goalassist == 'assist') {
		playerGameDF = playerGameDF %>%
										mutate(cumadjdeserved = legalcumadjdeservedassist)
	}

	return(playerGameDF)
}

OptimisePositionPlayerHistoricMix = function(playerGameDF) {
	for (goalassist in c('goal', 'assist')) {
		# theta = c(log(5), log(0.5)); goalassist = 'goal'
		playerGameDF = ffModel:::InitialisePositionPlayerHistoricMixColumn(playerGameDF, runMode = 'max', goalassist)
		maxInfo = nlm(ffModel:::CombinePositionPlayerHistoricExpectedGoalAssistLikFunct,
										p = c(log(5), log(0.5)), runMode = 'max', playerGameDF,
										stepmax = 1)

		playerGameDF = ffModel:::InitialisePositionPlayerHistoricMixColumn(playerGameDF, runMode = 'fit', goalassist)
		normrate =	ffModel:::CombinePositionPlayerHistoricExpectedGoalAssistLikFunct(
																				maxInfo$est, runMode = 'fit', playerGameDF = playerGameDF)
		if (goalassist == 'goal') {
			goalTheta = maxInfo$est
			playerGameDF$chnormgoalrate = normrate
		}
		if (goalassist == 'assist') {
			assistTheta = maxInfo$est
			playerGameDF$chnormassistrate = normrate
		}
	}
	return(list(goalTheta = goalTheta,
							assistTheta = assistTheta,
							playerGameDF = playerGameDF))
}


CalculateLatestGoalAssistRate = function(playerDF, gbgdf, summarydf, resultDF) {
	gbgdf = ffModel:::CalculateDeservedGoalAssist(gbgdf)
	summarydf = ffModel::CalculateDeservedGoalAssist(summarydf)

	playerGameDF = gbgdf %>%
								filter(played)

	dum = ffModel:::CalculateSeasonDeservedGoalAssist(summarydf, playerGameDF)
	seasondeservedsummary = dum$seasondeservedsummary
	playerGameDF = dum$gbgdf

	playerGameDF = ffModel:::ImputeMissingMatchOdds(playerGameDF, resultDF)

	playerGameDF = ffModel:::CalculatePositionBasedExpectedGoalAssist(playerGameDF)
	### so at this point, why have i got previous teamgoal but not previousteamassist?
	### CalculateSeasonDeservedGoalAssist is a horror show in any case, it needs tidying
	playerGameDF = ffModel:::CalculateHistoricPlayerDeservedGoalAssist(playerGameDF)

	dum = ffModel:::OptimisePositionPlayerHistoricMix(playerGameDF)
	goalTheta = dum$goalTheta
	assistTheta = dum$assistTheta
	playerGameDF = dum$playerGameDF

	latestPlayerEstimate = playerGameDF %>%
			group_by(team, player) %>%
			filter(date == max(date)) %>%
			select(team, player, mainpos, chnormgoalrate, chnormassistrate, chcumgame) %>%
			ungroup()

	playerDF = left_join(playerDF,
							latestPlayerEstimate,
							c('team', 'player'))

	return(playerDF)
}
