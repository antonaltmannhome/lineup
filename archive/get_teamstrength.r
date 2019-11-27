
getteamstrength = function(resultdf, fixtdf) {
	
	### want to include all relevant odds, to actually need odds for games that haven't happened yet
	### ie the first week of fixtdf

	teamoddsdf = bind_rows(resultdf %>%
	filter(isHome) %>%
	select(date, team, oppteam, oddsescored, oddseconceded),
	fixtdf %>%
	filter(isHome) %>%
	select(date, team, oppteam, oddsescored, oddseconceded))
	
	### now do the actual modelling
	### might miss odds occasionally, so this is necessary:
	keep=which(!is.na(teamoddsdf$oddsescored+teamoddsdf$oddseconceded))
	teamoddsdf=teamoddsdf[keep,]
	unteam=unique(c(teamoddsdf$team, teamoddsdf$oppteam))
	teamoddsdf$hmap=match(teamoddsdf$team,unteam)
	teamoddsdf$amap=match(teamoddsdf$oppteam,unteam)
	teamoddsdf$hprop=with(teamoddsdf,oddsescored/(oddsescored+oddseconceded))
	
	### want a bit of a time downweight really, let's put only 0.9 weight on data 1 month ago (so 40% weight on data at start of season)
		
	teamoddsdf$posixdate=lubridate::ymd(teamoddsdf$date)
	teamoddsdf$weight=with(teamoddsdf,exp(-0.015*as.numeric(difftime(posixdate[which.max(date)],posixdate,units='days'))))

	likfunct1=function(theta) {
		offtheta=theta[1:length(unteam)]
		deftheta=theta[(length(unteam)+1):(2*length(unteam))]
		homeeffect=theta[2*length(unteam)+1]
		ehgoal=with(teamoddsdf,exp(offtheta[hmap]+deftheta[amap]+homeeffect))
		eagoal=with(teamoddsdf,exp(offtheta[amap]+deftheta[hmap]))
		sqdiff=with(teamoddsdf,weight*( (oddsescored-ehgoal)^2 + (oddseconceded-eagoal)^2))
		priorpen=0.01*(mean(offtheta))^2
		meansqdiff=mean(sqdiff)
		return(meansqdiff)
	}
	thetainit=rep(0,2*length(unteam)+1)
	maxinfo=nlm(likfunct1,p=thetainit)

	offtheta=maxinfo$est[1:length(unteam)]
	deftheta=maxinfo$est[(length(unteam)+1):(2*length(unteam))]
	homeeffect=maxinfo$est[2*length(unteam)+1]

	teamability=data.frame(team=unteam,offtheta=offtheta,deftheta=deftheta)

	print(teamability %>% mutate(overalltheta=offtheta-deftheta) %>% arrange(-overalltheta))

	return(list(teamability = teamability, homeeffect = homeeffect))
}
