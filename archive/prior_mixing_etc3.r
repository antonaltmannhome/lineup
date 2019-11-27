### version 2 is a nightmare to extract ratings including lastest match for.
### so will try to do a modified version that calculates everything cheatily but then moves to non-cheat for predictions at time

dum = makeseasondeservedsummary(summarydf, gbgdf)
seasondeservedsummary=dum$seasondeservedsummary
gbgdf=dum$gbgdf

# also awesome, now we can do a model
## first bit, need to adjust for minutes played and opponent's strength
resultdf = getresultexpectedgoal()
### but need to separate that into ht and at
haresultdf = bind_rows(resultdf %>% select(date,ht,hescore) %>% dplyr::rename(matchdate = date,
																		team = ht,
																		teamegoal = hescore),
						resultdf %>% select(date,at,aescore) %>% dplyr::rename(matchdate = date,
																				team = at,
																				teamegoal = aescore))
gbgdf = left_join(gbgdf, haresultdf, by = c('matchdate','team'))
### really annoying having those NAs for the missing ones, let's just put in average by team for those
### will replace with proper glm values later
haresultdf$season = resultdf$season[match(haresultdf$matchdate,resultdf$date)]
meanegoalbyteam = haresultdf %>% group_by(season,team) %>% summarise(meanegoal = mean(teamegoal,na.rm=T))
sax=which(is.na(gbgdf$teamegoal))
gbgdf$teamegoal[sax] = meanegoalbyteam$meanegoal[match(with(gbgdf[sax,],paste(season,team)),
														with(meanegoalbyteam, paste(season,team)))]

														
playerposition = c('GK','D','DMC','M','AM','FW')
validIndex = with(gbgdf, which(!is.na(teamegoal)))
### model 1: just based on position
likfunct1 = function(theta, runmode, goalassist) {
	names(theta) = playerposition
	gbgdf$positionrate = with(gbgdf, exp(theta[mainpos]))
	gbgdf$positionexpected = with(gbgdf, positionrate * teamegoal*minute/90)
	if (runmode == 'fit') {
		return(gbgdf$positionexpected)
	}
	if (runmode == 'max') {
		if (goalassist == 'goal') {
			gbgdf$loglik = log(dpois(gbgdf$goal, gbgdf$positionexpected))
		}
		if (goalassist == 'assist') {
			gbgdf$loglik = log(dpois(gbgdf$assist, gbgdf$positionexpected))
		}
		sumloglik = mean(gbgdf$loglik[validIndex])
		return(-sumloglik)
	}
}

for (goalassist in c('goal','assist')) {
	if (goalassist == 'goal') {
		mylogmean = log(mean(gbgdf$goal))
	}
	if (goalassist == 'assist') {
		mylogmean = log(mean(gbgdf$assist))
	}
	theta = rep(mylogmean, length(playerposition))
	maxinfo1 = nlm(likfunct1, p = theta, runmode='max', goalassist = goalassist)

	mypositionegoal = likfunct1(maxinfo1$est, runmode = 'fit', goalassist = goalassist)
	if (goalassist == 'goal') gbgdf$positionegoal = mypositionegoal
	if (goalassist == 'assist') gbgdf$positioneassist = mypositionegoal
}
	
gbgdf$previoustotalminute[which(is.na(gbgdf$previoustotalminute))]=0
gbgdf$previousdeservedgoal[which(is.na(gbgdf$previousdeservedgoal))]=0
gbgdf$previousdeservedassist[which(is.na(gbgdf$previousdeservedassist))]=0
gbgdf$adjpreviousdeservedgoal = with(gbgdf, previousdeservedgoal / (previousteamgoal/38))
gbgdf$adjpreviousdeservedassist = with(gbgdf, previousdeservedassist / (previousteamgoal/38))

gbgdf = gbgdf %>%
		group_by(season,team,player) %>%
		arrange(gameweek) %>%
		mutate(chcumgame = cumsum(minute/90),
				chcumadjdeservedgoal = cumsum(deservedgoal/teamegoal),
				chcumadjdeservedassist = cumsum(deservedassist/teamegoal)) %>%
		ungroup()

gbgdf$lastix = with(gbgdf, match(paste(season, player, gameweek - 1),
								paste(season, player, gameweek)))
gbgdf$legalcumgame = with(gbgdf, chcumgame[lastix])
gbgdf$legalcumadjdeservedgoal = with(gbgdf, chcumadjdeservedgoal[lastix])
gbgdf$legalcumadjdeservedassist = with(gbgdf, chcumadjdeservedassist[lastix])

### no, can have multiple games in a gameweek or no games in a gameweek, need to be cleverer than this
## but ch is a better starting point, because e.g for double gameweeks, you get the update from previous match

gbgdf = gbgdf %>%
		group_by(season, player) %>%
		arrange(matchdate) %>%
		mutate(legalcumgame = c(0, head(chcumgame, -1)),
				legalcumadjdeservedgoal = c(0, head(chcumadjdeservedgoal, -1)),
				legalcumadjdeservedassist = c(0, head(chcumadjdeservedassist, -1)))


likfunct3 = function(theta, runmode, goalassist) {
	pospriorstr=exp(theta[1])
	prevseaspriorstr=exp(theta[2])
	if (goalassist == 'goal') {
		topline = with(gbgdf, (pospriorstr*positionegoal +
							prevseaspriorstr*adjpreviousdeservedgoal))
	}
	if (goalassist == 'assist') {
		topline = with(gbgdf, (pospriorstr*positioneassist +
							prevseaspriorstr*adjpreviousdeservedassist))
	}
	bottomline = with(gbgdf, pospriorstr +
								prevseaspriorstr*previoustotalminute/90)
	### fitting past matches or for all current games?
	if (runmode=='fit') {
		if (goalassist == 'goal') {
			topline = topline + gbgdf$chcumadjdeservedgoal
		}
		if (goalassist == 'assist') {
			topline = topline + gbgdf$chcumadjdeservedassist
		}
		bottomline = bottomline + gbgdf$chcumgame
	}
	if (runmode=='max') {
		if (goalassist == 'goal') {
			topline = topline + gbgdf$legalcumadjdeservedgoal
		}
		if (goalassist == 'assist') {
			topline = topline + gbgdf$legalcumadjdeservedassist
		}
		bottomline = bottomline + gbgdf$legalcumgame
	}
	gbgdf$normrate3 = topline / bottomline
	if (runmode == 'fit') {
		return(gbgdf %>% pull(normrate3))
	}
	if (runmode == 'max') {
		gbgdf$expected3 = with(gbgdf, minute/90 * teamegoal * normrate3)
		if (goalassist == 'goal') {
			gbgdf$loglik = log(dpois(gbgdf$goal, gbgdf$expected3))
		}
		if (goalassist == 'assist') {
			gbgdf$loglik = log(dpois(gbgdf$assist, gbgdf$expected3))
		}
		sumloglik = mean(gbgdf$loglik[validIndex])
		return(-sumloglik)
	}
}

maxinfo3goal=nlm(likfunct3, p=c(log(5),log(0.5)), runmode='max', goalassist='goal')
gbgdf$normgoalrate3 = likfunct3(maxinfo3goal$est, runmode='fit', goalassist='goal')
maxinfo3assist=nlm(likfunct3, p=c(log(5),log(0.5)), runmode='max', goalassist='assist')
gbgdf$normassistrate3 = likfunct3(maxinfo3assist$est, runmode='fit', goalassist='assist')

### to check that for sense:
checkplayer = function(myplayer) {
	minigbgdf = gbgdf %>% 
		filter(grepl(myplayer,player)) %>%
		arrange(matchdate) %>%
		mutate(adjdeservedgoalrate = chcumadjdeservedgoal/chcumgame,
				previoustotalgame = previoustotalminute / 90,
				previousrate = adjpreviousdeservedgoal/previoustotalgame) %>%
		select(season,minute,positiongoalrate,previousrate,previoustotalgame,adjdeservedgoalrate,chcumgame,normgoalrate3,deservedgoal,goal)
		# bit confusing that epositiongoal include minutes and teamegoal but deservedrate doesn't
	return(minigbgdf)
}

### to get latest players estimates

dum = gbgdf %>%
		filter(season==currentseason) %>%
		group_by(team,player) %>%
		filter(gameweek==max(gameweek)) %>%
		select(team,player,normgoalrate3,normassistrate3,chcumgame)

### and to see top players
dum %>% filter(chcumgame>1) %>% arrange(-normgoalrate3) %>% head(20)
dum %>% filter(chcumgame>1) %>% arrange(-normassistrate3) %>% head(20)

### good stuff, almost there with that then
### but need to do for assists and add in a time downweight if necessary
