
gettotalplayerminute=function(mygbgdf=gbgdf) {
	mygbgdf=mygbgdf %>% select(team,player,gameweek,minute)
	mygbgdf=spreadmelt(mygbgdf,spreadOrMelt='melt',keyColumn=c('team','player','gameweek'))
	### want to restrict to 'active' players ie ones who've played quite a bit or played a lot in the most recent game
	teammaxweek=mygbgdf %>% group_by(team) %>% summarise(maxweek=max(gameweek))
	totalplayerminute=mygbgdf %>% filter(variable=='minute') %>% group_by(team,player) %>% summarise(totalminute=sum(value))
	mygbgdf=merge(mygbgdf,teammaxweek,by='team')
	latestgbgdf=mygbgdf %>% filter(gameweek == maxweek & variable=='minute') # might include last two weeks eg at some point
	totalplayerminute$lastweekminute = latestgbgdf$value[match(with(totalplayerminute,paste(team,player)),with(latestgbgdf,paste(team,player)))]
	return(totalplayerminute)
}

getlongtermteamgoal=function() {
	
	fixtdf=getfixturegoal()
	### now add em up weighted by week
	weightedhomegoal=fixtdf %>% group_by(ht) %>% summarise(hscored=sum(gwweight*ehgoal), hconceded=sum(gwweight*eagoal))
	names(weightedhomegoal)[names(weightedhomegoal)=='ht']='team'
	weightedawaygoal=fixtdf %>% group_by(at) %>% summarise(ascored=sum(gwweight*eagoal), aconceded=sum(gwweight*ehgoal))
	names(weightedawaygoal)[names(weightedawaygoal)=='at']='team'
	
	### then combine together
	weightedgoal=merge(weightedhomegoal,weightedawaygoal)
	weightedgoal = weightedgoal %>% mutate(scored=hscored + ascored, conceded=hconceded + aconceded) %>% select(team, scored, conceded) %>% arrange(-(scored-conceded))
	
	### what would be useful is to have something that displays how many goals they'd score against average teams in the same time, so we can see who's got a relatively easy run
	### average teams isn't a completely straightforward concept - will come back to this
	
	return(weightedgoal)
}

getplayerpoint=function(mysummarydf=summarydf) {
	currentteam=read.csv(paste(DATAPATH,'currentteam.csv',sep=''))
	### now get hold of expected goals scored and conceded by their teams
	mostrecentfile=getmostrecentdir(mysource='spreadex')
	filein=paste(DATAPATH,'spreadex_saved/',mostrecentfile,sep='')
	spreadexdata=read.csv(filein)
	### now map team names
	spreadexdata$team=gsub(' ','',cleanteam(tolower(spreadexdata$team),'spreadex'))
	summarydf=getsummary()
	currentteam$teamexpectedscored=spreadexdata$egoal[match(currentteam$team,spreadexdata$team)]
	currentteam$teamexpectedconceded=spreadexdata$econc[match(currentteam$team,spreadexdata$team)]
	### now the fiddly bit, get hold of each player's expected goals and assists proportion
	currentteam[,c('egoalprop','eassistprop')]=NA
	for (j in 1:nrow(currentteam)) {
		if (currentteam$position[j]!='g') {
			teaminfo=viewteam(currentteam$team[j],mysummarydf=mysummarydf)
			sax=grep(currentteam$player[j],tolower(teaminfo$player))
			currentteam[j,'egoalprop']=teaminfo[sax,'deservedgoalproportion']
			currentteam[j,'eassistprop']=teaminfo[sax,'deservedassistproportion']
		}
	}
	### now get expected points
	currentteam[,'expectedpoint']=NA
	gksax=which(currentteam$position=='g')
	defsax=which(currentteam$position=='d')
	midsax=which(currentteam$position=='m')
	fwsax=which(currentteam$position=='f')
	currentteam[gksax,'expectedpoint']=with(currentteam[gksax,],round(4*dpois(0,teamexpectedconceded),2))
	currentteam[defsax,'expectedpoint']=with(currentteam[defsax,],round((6*egoalprop + 3*eassistprop)*teamexpectedscored + 4*dpois(0,teamexpectedconceded),2) - 2*(dpois(2,teamexpectedconceded) + dpois(3,teamexpectedconceded)) - 4*(dpois(4,teamexpectedconceded) + dpois(5,teamexpectedconceded)))
	currentteam[midsax,'expectedpoint']=with(currentteam[midsax,],round((5*egoalprop + 3*eassistprop)*teamexpectedscored + 1*dpois(0,teamexpectedconceded),2))
	currentteam[fwsax,'expectedpoint']=with(currentteam[fwsax,],round((4*egoalprop + 3*eassistprop)*teamexpectedscored,2))
	
	minicurrentteam=currentteam %>% arrange(-expectedpoint) %>% select(player,position,expectedpoint)
	### then return in order
	return(list(currentteam=currentteam,minicurrentteam=minicurrentteam))
}

processproportion=function(summarydf) {
	teamtotal=summarydf %>% group_by(season,team) %>% summarise(teamtotalminute=round(sum(minute)/11), teamtotaldeservedgoal=sum(deservedgoal),teamtotaldeservedassist=sum(deservedassist))
	summarydf = left_join(summarydf, teamtotal, by=c('season','team'))
	summarydf = summarydf %>% mutate(deservedgoalproportion=deservedgoal/teamtotaldeservedgoal, deservedassistproportion=deservedassist/teamtotaldeservedassist)
	### but then rescale according to number of minutes played
	summarydf = summarydf %>% mutate(deservedgoalproportion = round(deservedgoalproportion*teamtotalminute/minute,2), deservedassistproportion = round(deservedassistproportion*teamtotalminute/minute,2))
	
	return(summarydf)
}
