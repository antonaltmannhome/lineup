### try to get predictions that depend on previous season as well as games so far

### try to predict today's goals and assists using data so far this season AND position AND previous year's data

dum = makeseasondeservedsummary(summarydf, gbgdf)
seasondeservedsummary=dum$seasondeservedsummary
gbgdf=dum$gbgdf

### awesome, now let's do some non-cheaty predictions


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
likfunct1 = function(theta, runmode) {
	names(theta) = playerposition
	gbgdf$positiongoalrate = with(gbgdf, exp(theta[mainpos]))
	gbgdf$positionegoal = with(gbgdf, positiongoalrate * teamegoal*minute/90)
	if (runmode == 'fit') {
		return(gbgdf)
	}
	if (runmode == 'max') {
		gbgdf$loglik = log(dpois(gbgdf$goal, gbgdf$positionegoal))
		sumloglik = mean(gbgdf$loglik[validIndex])
		return(-sumloglik)
	}
}
	
theta = rep(log(mean(gbgdf$goal)), length(playerposition))
maxinfo1 = nlm(likfunct1, p = theta, runmode='max')

gbgdf = likfunct1(maxinfo1$est, runmode = 'fit')

### so that's agood overall prior

gbgdf = gbgdf %>%
		group_by(season,team,player) %>%
		arrange(gameweek) %>%
		mutate(cumgame = cumsum(minute/90)-minute/90,
				cumadjdeservedgoal = cumsum(deservedgoal/teamegoal) - deservedgoal/teamegoal,
				cumadjdeservedassist = cumsum(deservedassist/teamegoal) - deservedassist/teamegoal) %>%
		ungroup()

likfunct2 = function(theta, runmode) {
	gbgdf$mixdeservedgoalrate2 = with(gbgdf, (exp(theta)*positiongoalrate + cumadjdeservedgoal) / (exp(theta) + cumgame))
	gbgdf$egoal2 = with(gbgdf, minute/90 * teamegoal * mixdeservedgoalrate2)
	if (runmode == 'fit') {
		return(gbgdf)
	}
	if (runmode == 'max') {
		gbgdf$loglik = log(dpois(gbgdf$goal, gbgdf$egoal2))
		sumloglik = mean(gbgdf$loglik[validIndex])
		return(-sumloglik)
	}
}

maxinfo2=optimise(likfunct2, interval=c(log(0.5),log(20)), runmode='max')
gbgdf = likfunct2(maxinfo2$min, runmode='fit')

### to check that for sense:
checkplayer = function(myplayer) {
	minigbgdf = gbgdf %>% 
		filter(grepl(myplayer,player)) %>%
		mutate(adjdeservedgoalrate = cumadjdeservedgoal/cumgame,
				edeservedgoal = adjdeservedgoalrate * minute/90 * teamegoal) %>%
		select(minute,positiongoalrate,cumadjdeservedgoal,adjdeservedgoalrate,edeservedgoal,positionegoal,egoal2,goal)
		# bit confusing that epositiongoal include minutes and teamegoal but deservedrate doesn't
	return(minigbgdf)
}
### looking pretty good. so next step, offer previous year's totals also into the mix. and then do the same for assists. and add a time downweight

### let's try adding last season, it doosn't look that difficult

gbgdf$previousdeservedgoal[which(is.na(gbgdf$previousdeservedgoal))]=0
gbgdf$previoustotalminute[which(is.na(gbgdf$previoustotalminute))]=0
gbgdf$adjpreviousdeservedgoal = with(gbgdf, previousdeservedgoal / (previousteamgoal/38))

likfunct3 = function(theta, runmode) {
	pospriorstr=exp(theta[1])
	prevseaspriorstr=exp(theta[2])
	topline = with(gbgdf, (pospriorstr*positiongoalrate +
							prevseaspriorstr*adjpreviousdeservedgoal +
							cumadjdeservedgoal))
	bottomline = with(gbgdf, pospriorstr +
								prevseaspriorstr*previoustotalminute/90 +
								cumgame)
	gbgdf$mixdeservedgoalrate3 = topline / bottomline
	gbgdf$egoal3 = with(gbgdf, minute/90 * teamegoal * mixdeservedgoalrate3)
	if (runmode == 'fit') {
		return(gbgdf)
	}
	if (runmode == 'max') {
		gbgdf$loglik = log(dpois(gbgdf$goal, gbgdf$egoal3))
		sumloglik = mean(gbgdf$loglik[validIndex])
		return(-sumloglik)
	}
}

maxinfo3=nlm(likfunct3, p=c(log(5),log(0.5)), runmode='max')
gbgdf = likfunct3(maxinfo3$est, runmode='fit')

### so that kind of works but i don't like the fact that previosu season total hasn't been rescaled for team ability? or is that comment out of date, i don't know

### to check that for sense:
checkplayer = function(myplayer) {
	minigbgdf = gbgdf %>% 
		filter(grepl(myplayer,player)) %>%
		arrange(matchdate) %>%
		mutate(adjdeservedgoalrate = cumadjdeservedgoal/cumgame,
				edeservedgoal = adjdeservedgoalrate * minute/90 * teamegoal,
				previoustotalgame = previoustotalminute / 90,
				previousrate = adjpreviousdeservedgoal/previoustotalgame) %>%
		select(season,minute,positiongoalrate,previousrate,previoustotalgame,adjdeservedgoalrate,cumgame,mixdeservedgoalrate3,edeservedgoal,positionegoal,egoal3,goal)
		# bit confusing that epositiongoal include minutes and teamegoal but deservedrate doesn't
	return(minigbgdf)
}

### ok, so what these columns mean:
# the egoal3 is mixdeservedgoalrate3 but factoring in supremacy and minutes played
# positionegoal is positiongoalrate factoring in supremacy and minutes played
# so if you want to know what a players expected goals rate if for a future fixture, you extract the mixdeservedgoalrate3 including the most recent fixtures, then multiply in future expected minutes and supremacies
## but it feels like i have to recalculate a lot of things just to get the most recent fixture in, let's try to recode this to avoid this

### generally awesome, but why has mane got no previous season data?
### that seems to be a general problem , players switching clubs, the previous rate should be transferred
### also, do we have a prediction that uses the latest game? i'm not sure we do, everything's non cheaty as far as i can see
