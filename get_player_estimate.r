### version 2 is a nightmare to extract ratings including lastest match for.
### so will try to do a modified version that calculates everything cheatily but then moves to non-cheat for predictions at time

minuteInGame = 94

playerGameDF = gbgdf %>%
							filter(played)

dum = makeseasondeservedsummary(summarydf, playerGameDF)
seasondeservedsummary=dum$seasondeservedsummary
playerGameDF=dum$gbgdf

### really annoying having those NAs for the missing ones, let's just put in average by team for those
### will replace with proper glm values later
meanegoalbyteam = resultdf %>%
					group_by(season,team) %>%
					summarise(meanegoal = mean(oddsescored,na.rm=T))
sax=which(is.na(playerGameDF$teamoddsescored))
playerGameDF$teamoddsescored[sax] = meanegoalbyteam$meanegoal[match(with(playerGameDF[sax,],paste(season,team)),
														with(meanegoalbyteam, paste(season,team)))]


playerposition = c('GK','D','DMC','M','AM','FW')
validIndex = with(playerGameDF, which(!is.na(teamoddsescored)))
### model 1: just based on position
likfunct1 = function(theta, runmode, goalassist) {
	names(theta) = playerposition
	playerGameDF$positionrate = with(playerGameDF, exp(theta[mainpos]))
	playerGameDF$positionexpected = with(playerGameDF, positionrate * teamoddsescored*minute/94)
	if (runmode == 'fit') {
		return(playerGameDF$positionrate)
	}
	if (runmode == 'max') {
		if (goalassist == 'goal') {
			playerGameDF$loglik = log(dpois(playerGameDF$goal, playerGameDF$positionexpected))
		}
		if (goalassist == 'assist') {
			playerGameDF$loglik = log(dpois(playerGameDF$assist, playerGameDF$positionexpected))
		}
		sumloglik = mean(playerGameDF$loglik[validIndex])
		return(-sumloglik)
	}
}

for (goalassist in c('goal','assist')) {
	if (goalassist == 'goal') {
		mylogmean = log(mean(playerGameDF$goal))
	}
	if (goalassist == 'assist') {
		mylogmean = log(mean(playerGameDF$assist))
	}
	theta = rep(mylogmean, length(playerposition))
	maxinfo1 = nlm(likfunct1, p = theta, runmode='max', goalassist = goalassist)

	mypositionrate = likfunct1(maxinfo1$est, runmode = 'fit', goalassist = goalassist)
	if (goalassist == 'goal') playerGameDF$positiongoalrate = mypositionrate
	if (goalassist == 'assist') playerGameDF$positionassistrate = mypositionrate
}

playerGameDF$previoustotalminute[which(is.na(playerGameDF$previoustotalminute))]=0
#playerGameDF$previousdeservedgoal[which(is.na(playerGameDF$previousdeservedgoal))]=0
#playerGameDF$previousdeservedassist[which(is.na(playerGameDF$previousdeservedassist))]=0
#playerGameDF$adjpreviousdeservedgoal = with(playerGameDF, previousdeservedgoal / (previousteamgoal/38))
#playerGameDF$adjpreviousdeservedassist = with(playerGameDF, previousdeservedassist / (previousteamgoal/38))

playerGameDF = playerGameDF %>%
		mutate(adjpreviousdeservedgoal = ifelse(previoustotalminute > 0,
												previousdeservedgoal / (previousteamgoal/38),
												0),
				adjpreviousdeservedassist = ifelse(previoustotalminute > 0,
												previousdeservedassist / (previousteamgoal/38),
												0))

playerGameDF = playerGameDF %>%
		group_by(season,team,player) %>%
		arrange(teamgamenumber) %>%
		mutate(chcumgame = cumsum(minute/minuteInGame),
				chcumadjdeservedgoal = cumsum(deservedgoal/teamoddsescored),
				chcumadjdeservedassist = cumsum(deservedassist/teamoddsescored)) %>%
		ungroup()

playerGameDF$lastix = with(playerGameDF, match(paste(season, player, teamgamenumber - 1),
								paste(season, player, teamgamenumber)))
playerGameDF$legalcumgame = with(playerGameDF, chcumgame[lastix])
playerGameDF$legalcumadjdeservedgoal = with(playerGameDF, chcumadjdeservedgoal[lastix])
playerGameDF$legalcumadjdeservedassist = with(playerGameDF, chcumadjdeservedassist[lastix])

### no, can have multiple games in a gameweek or no games in a gameweek, need to be cleverer than this
## but ch is a better starting point, because e.g for double gameweeks, you get the update from previous match

playerGameDF = playerGameDF %>%
		group_by(season, player) %>%
		arrange(date) %>%
		mutate(legalcumgame = c(0, head(chcumgame, -1)),
				legalcumadjdeservedgoal = c(0, head(chcumadjdeservedgoal, -1)),
				legalcumadjdeservedassist = c(0, head(chcumadjdeservedassist, -1))) %>%
		ungroup()


likfunct3 = function(theta, runmode, goalassist) {
	pospriorstr=exp(theta[1])
	prevseaspriorstr=exp(theta[2])
	if (goalassist == 'goal') {
		topline = with(playerGameDF, (pospriorstr*positiongoalrate +
							prevseaspriorstr*adjpreviousdeservedgoal))
	}
	if (goalassist == 'assist') {
		topline = with(playerGameDF, (pospriorstr*positionassistrate +
							prevseaspriorstr*adjpreviousdeservedassist))
	}
	bottomline = with(playerGameDF, pospriorstr +
								prevseaspriorstr*previoustotalminute/minuteInGame)
	### fitting past matches or for all current games?
	if (runmode=='fit') {
		if (goalassist == 'goal') {
			topline = topline + playerGameDF$chcumadjdeservedgoal
		}
		if (goalassist == 'assist') {
			topline = topline + playerGameDF$chcumadjdeservedassist
		}
		bottomline = bottomline + playerGameDF$chcumgame
	}
	if (runmode=='max') {
		if (goalassist == 'goal') {
			topline = topline + playerGameDF$legalcumadjdeservedgoal
		}
		if (goalassist == 'assist') {
			topline = topline + playerGameDF$legalcumadjdeservedassist
		}
		bottomline = bottomline + playerGameDF$legalcumgame
	}
	playerGameDF$normrate3 = topline / bottomline
	if (runmode == 'fit') {
		return(playerGameDF %>% pull(normrate3))
	}
	if (runmode == 'max') {
		playerGameDF$expected3 = with(playerGameDF, minute/minuteInGame * teamoddsescored * normrate3)
		if (goalassist == 'goal') {
			playerGameDF$loglik = log(dpois(playerGameDF$goal, playerGameDF$expected3))
		}
		if (goalassist == 'assist') {
			playerGameDF$loglik = log(dpois(playerGameDF$assist, playerGameDF$expected3))
		}
		sumloglik = mean(playerGameDF$loglik[validIndex])
		return(-sumloglik)
	}
}

maxinfo3goal=nlm(likfunct3, p=c(log(5),log(0.5)), runmode='max', goalassist='goal')
playerGameDF$normgoalrate3 = likfunct3(maxinfo3goal$est, runmode='fit', goalassist='goal')
maxinfo3assist=nlm(likfunct3, p=c(log(5),log(0.5)), runmode='max', goalassist='assist')
playerGameDF$normassistrate3 = likfunct3(maxinfo3assist$est, runmode='fit', goalassist='assist')

### to check that for sense:
checkplayer = function(myplayer) {
	miniplayerGameDF = playerGameDF %>%
		filter(grepl(myplayer,player)) %>%
		arrange(date) %>%
		mutate(adjdeservedgoalrate = chcumadjdeservedgoal/chcumgame,
				previoustotalgame = previoustotalminute / minuteInGame,
				previousrate = adjpreviousdeservedgoal/previoustotalgame) %>%
		select(season,minute,positiongoalrate,previousrate,previoustotalgame,adjdeservedgoalrate,chcumgame,normgoalrate3,deservedgoal,goal)
		# bit confusing that epositiongoal include minutes and teamoddsescored but deservedrate doesn't
	return(miniplayerGameDF)
}

### to get latest players estimates

### only want to keep hold of players who are in playerdf, and only the latest estimate of them

latestPlayerEstimate = playerGameDF %>%
		group_by(team,player) %>%
		filter(date==max(date)) %>%
		select(team,player,mainpos,normgoalrate3,normassistrate3,chcumgame) %>%
		ungroup()

playerDF = left_join(playerDF,
						latestPlayerEstimate,
						c('team', 'player'))

### and to see top players
topGoalScorer = playerDF %>%
					arrange(-normgoalrate3) %>%
					head(20)
topAssister = playerDF %>%
					arrange(-normassistrate3) %>%
					head(20)

print(topGoalScorer)
print(topAssister)

### good stuff, almost there with that then
### but need to add in a time downweight if necessary
### a couple of big disagreements between adjdeserved at end of previous season and previousrate eg gabbiadini, mcauley
