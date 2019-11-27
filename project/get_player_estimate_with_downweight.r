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
gbgdf$isPredValid = with(gbgdf, !is.na(teamegoal))
gbgdf$isRollingPredValid = with(gbgdf, !is.na(teamegoal) & gameweek > 1)
### model 1: just based on position
likfunct1 = function(theta, runmode, goalassist) {
	names(theta) = playerposition
	gbgdf$positionrate = with(gbgdf, exp(theta[mainpos]))
	gbgdf$positionexpected = with(gbgdf, positionrate * teamegoal*minute/90)
	if (runmode == 'fit') {
		return(gbgdf$positionrate)
	}
	if (runmode == 'max') {
		if (goalassist == 'goal') {
			gbgdf$loglik = log(dpois(gbgdf$goal, gbgdf$positionexpected))
		}
		if (goalassist == 'assist') {
			gbgdf$loglik = log(dpois(gbgdf$assist, gbgdf$positionexpected))
		}
		sumloglik = with(gbgdf, mean(loglik[isPredValid]))
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
	
	mypositionrate = likfunct1(maxinfo1$est, runmode = 'fit', goalassist = goalassist)
	if (goalassist == 'goal') gbgdf$positiongoalrate = mypositionrate
	if (goalassist == 'assist') gbgdf$positionassistrate = mypositionrate
}

gbgdf$previoustotalminute[which(is.na(gbgdf$previoustotalminute))]=0
#gbgdf$previousdeservedgoal[which(is.na(gbgdf$previousdeservedgoal))]=0
#gbgdf$previousdeservedassist[which(is.na(gbgdf$previousdeservedassist))]=0
#gbgdf$adjpreviousdeservedgoal = with(gbgdf, previousdeservedgoal / (previousteamgoal/38))
#gbgdf$adjpreviousdeservedassist = with(gbgdf, previousdeservedassist / (previousteamgoal/38))

gbgdf = gbgdf %>%
mutate(adjpreviousdeservedgoal = ifelse(previoustotalminute > 0,
previousdeservedgoal / (previousteamgoal/38),
0),
adjpreviousdeservedassist = ifelse(previoustotalminute > 0,
previousdeservedassist / (previousteamgoal/38),
0))

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


likfunct3 = function(theta, runmode, goalassist, subgbgdf) {
	pospriorstr=exp(theta[1])
	prevseaspriorstr=exp(theta[2])
	if (goalassist == 'goal') {
		topline = with(subgbgdf, (pospriorstr*positiongoalrate +
		prevseaspriorstr*adjpreviousdeservedgoal))
	}
	if (goalassist == 'assist') {
		topline = with(subgbgdf, (pospriorstr*positionassistrate +
		prevseaspriorstr*adjpreviousdeservedassist))
	}
	bottomline = with(subgbgdf, pospriorstr +
	prevseaspriorstr*previoustotalminute/90)
	### fitting past matches or for all current games?
	if (runmode=='fit') {
		if (goalassist == 'goal') {
			topline = topline + subgbgdf$chcumadjdeservedgoal
		}
		if (goalassist == 'assist') {
			topline = topline + subgbgdf$chcumadjdeservedassist
		}
		bottomline = bottomline + subgbgdf$chcumgame
	}
	if (runmode=='max') {
		if (goalassist == 'goal') {
			topline = topline + subgbgdf$legalcumadjdeservedgoal
		}
		if (goalassist == 'assist') {
			topline = topline + subgbgdf$legalcumadjdeservedassist
		}
		bottomline = bottomline + subgbgdf$legalcumgame
	}
	subgbgdf$normrate3 = topline / bottomline
	if (runmode == 'fit') {
		return(subgbgdf %>% pull(normrate3))
	}
	if (runmode == 'max') {
		subgbgdf$expected3 = with(subgbgdf, minute/90 * teamegoal * normrate3)
		if (goalassist == 'goal') {
			subgbgdf$loglik = log(dpois(subgbgdf$goal, subgbgdf$expected3))
		}
		if (goalassist == 'assist') {
			subgbgdf$loglik = log(dpois(subgbgdf$assist, subgbgdf$expected3))
		}
		sumloglik = with(subgbgdf, mean( (dwWeight*loglik)[isPredValid]))
		return(-sumloglik)
	}
}

dwParam = 0.05

FitRollingLikelihood = function(dwParam, goalassist) {
	
	maxgw = max(gbgdf$gameweek)
	gbgdf$rollingNormRate3 = NA
	for (gwi in 2:maxgw) {
		subgbgdf = gbgdf %>%
		filter(gameweek < gwi)
		subgbgdf$dwWeight = with(subgbgdf, exp(-dwParam*(gwi - gameweek)))
		maxinfo = nlm(likfunct3,
		p=c(log(5),log(0.5)),
		runmode='max',
		goalassist='goal',
		subgbgdf = subgbgdf)
		subgbgdf = gbgdf %>%
		filter(gameweek == gwi)
		dum = likfunct3(maxinfo$est,
		runmode='fit',
		goalassist='goal',
		subgbgdf = subgbgdf)
		currentGameWeekIndex = with(gbgdf, which(gameweek == gwi))
		gbgdfToSubGbgdfMap = match(
		with(gbgdf[currentGameWeekIndex,], paste(team, player, season)),
		with(subgbgdf, paste(team, player, season))
		)
		gbgdf$rollingNormRate3[currentGameWeekIndex] = dum[gbgdfToSubGbgdfMap]
		
		cat('Have calculated for gameweek', gwi,'\n')
	}
	
	# so what is the likelihood of that
	gbgdf$expected3 = with(gbgdf, minute/90 * teamegoal * rollingNormRate3)
	gbgdf$loglik = log(dpois(gbgdf$goal, gbgdf$expected3))
	sumloglik = with(gbgdf, mean(loglik[isRollingPredValid]))
	
	return(sumloglik)
}

### so let's try to tweak that code so that it uses purrr
### then allow to optimise by defs, mids and forwards


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

latestPlayerEstimate = gbgdf %>%
filter(season==currentseason) %>%
group_by(team,player) %>%
filter(gameweek==max(gameweek)) %>%
select(team,player,normgoalrate3,normassistrate3,chcumgame) %>%
ungroup()

### and to see top players
topGoalScorer = latestPlayerEstimate %>%
filter(chcumgame>1) %>%
arrange(-normgoalrate3) %>%
head(20)
topAssister = latestPlayerEstimate %>%
filter(chcumgame>1) %>%
arrange(-normassistrate3) %>%
head(20)

print(topGoalScorer)
print(topAssister)

### good stuff, almost there with that then
### but need to add in a time downweight if necessary
### a couple of big disagreements between adjdeserved at end of previous season and previousrate eg gabbiadini, mcauley
