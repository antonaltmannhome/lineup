
oldmatchupspreadexdata=function(resultdf, fixtdf) {
	startdate=20160801
	SPREADEXPATH=paste(DATAPATH,'spreadex_saved/',sep='')
	dum=list.files(SPREADEXPATH)
	sxfile=dum[grep('^spreadex_[0-9]{8}',dum)]
	sxdate=as.numeric(gsub('(^spreadex_)([0-9]+)(.dat)','\\2',sxfile))

	keep=which(sxdate>startdate)
	sxdate=sxdate[keep]
	sxfile=sxfile[keep]

	## but we need to add the latest time on to the end for the fixtures
	latestspreadexdateFile = paste0(DATAPATH,'spreadex_saved/latestspreadexdate.dat')
	latestspreadexdate = as.numeric(scan(latestspreadexdateFile, quiet = TRUE))
	sxdateextended = c(sxdate, latestspreadexdate)

	pricelist=NULL
	for (j in 1:length(sxdate)) {
		pricelist[[j]]=read.csv(paste(SPREADEXPATH,sxfile[j],sep=''))
		pricelist[[j]]$date=sxdate[j]
		pricelist[[j]]$priceindex = j
		### need to clean team names
		pricelist[[j]]$team=tolower(pricelist[[j]]$team)
		pricelist[[j]]$team=gsub(' ','',cleanteam(pricelist[[j]]$team,'spreadex'))
	}

	pricedf=do.call(rbind,pricelist)

	### ah but we don't know who's playing who, that's a pain
	### we get that from results.csv

	resultdf$priceindex = cut(resultdf$date, sxdateextended,labels = FALSE, right = TRUE)
	resultdf = left_join(resultdf,
								pricedf %>%
								select(team, priceindex, egoal, econc) %>%
								dplyr::rename(oddsescored = egoal, oddseconceded = econc),
								by = c('team', 'priceindex')) %>%
								select(-priceindex)

	fixtdf$priceindex = cut(fixtdf$date, breaks = sxdateextended, labels = FALSE, right = TRUE)
	fixtdf = left_join(fixtdf,
						pricedf %>%
						select(team, priceindex, egoal, econc) %>%
						dplyr::rename(oddsescored = egoal, oddseconceded = econc),
						by = c('team', 'priceindex')) %>%
						select(-priceindex)

	# but there are some bad odds, set those to NA
	rawbadodds = read.csv(paste0(DATAPATH, 'spreadex_saved/bad_odds.csv'),as.is = TRUE)
	homebadodds = rawbadodds %>% dplyr::rename(team = ht, oppteam = at) %>% mutate(isHome = TRUE)
	awaybadodds = rawbadodds %>% dplyr::rename(team = at, oppteam = ht) %>% mutate(isHome = FALSE)
	badodds = bind_rows(homebadodds, awaybadodds)

	resultdf = indicate_overlapping_combination(
							resultdf,
							badodds,
							c('date', 'team', 'oppteam', 'isHome'),
							'isBadOdds')

	resultdf = resultdf %>%
				mutate_cond(isBadOdds,
							oddsescored = NA,
							oddseconceded = NA)

	return(list(resultdf = resultdf,
				fixtdf = fixtdf))
}

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

getfixturegoal=function(resultdf, fixtdf) {
	### so get list of upcoming fixtures and current team abilities according to spreadex, then get expected goals scored and conceded by each team

	dum = getteamstrength(resultdf, fixtdf)
	teamability = dum$teamability
	homeeffect = dum$homeeffect

	### now get expected goals for and against for each match
	fixtdf$offtheta=with(teamability,offtheta[match(fixtdf$team,team)])
	fixtdf$oppofftheta=with(teamability,offtheta[match(fixtdf$oppteam,team)])
	fixtdf$deftheta=with(teamability,deftheta[match(fixtdf$team,team)])
	fixtdf$oppdeftheta=with(teamability,deftheta[match(fixtdf$oppteam,team)])
	fixtdf$homeeffect = with(fixtdf, ifelse(isHome, homeeffect, 0))

	fixtdf$escored=with(fixtdf,exp(offtheta+oppdeftheta+homeeffect))
	fixtdf$econceded=with(fixtdf,exp(oppofftheta+deftheta))

	fixtdf = within(fixtdf, rm(offtheta, oppofftheta, deftheta, oppdeftheta, homeeffect))

	return(fixtdf)
}
