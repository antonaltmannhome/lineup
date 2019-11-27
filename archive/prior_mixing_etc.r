### let's actually sort out this whole prior mixing thing

currentseason=1617
### first step, get in previous year's data
previousseason=paste(as.numeric(substr(currentseason,1,2))-1,as.numeric(substr(currentseason,3,4))-1,sep='')
unteam=unique(summarydf$team)

filein=paste(DATAPATH,previousseason,'/model.csv',sep='')
historicmodel=read.csv(filein)

### awesome, so that bit's done

### next step, predict how many goals are scored purely from fixture and position
resultdf=getresultexpectedgoal()

### so firstly let's try to predict the goals scored by each player only using resultdf and position
hsax=which(with(gbgdf,paste(team,gameweek)) %in% with(resultdf,paste(ht,gameweek)))
asax=which(with(gbgdf,paste(team,gameweek)) %in% with(resultdf,paste(at,gameweek)))
gbgdf$teamegoal=NA
gbgdf$teamegoal[hsax]=resultdf$hgoal[match(with(gbgdf,paste(team,gameweek))[hsax],with(resultdf,paste(ht,gameweek)))]
gbgdf$teamegoal[asax]=resultdf$agoal[match(with(gbgdf,paste(team,gameweek))[asax],with(resultdf,paste(at,gameweek)))]

### we'll need a summary by team/gameweek though
gameteamsummary=gbgdf %>% group_by(team,gameweek) %>% summarise(teamegoal=teamegoal[1])

gbgdf$ffposition=summarydf$ffposition[match(with(gbgdf,paste(team,player)),with(summarydf,paste(team,player)))]

validsax=with(gbgdf,which(!is.na(teamegoal) & !is.na(ffposition) & minute>0 & assist>=0 & goal>=0 & ffposition!='g'))

### fiddly to adjust proportion for minutes played and team expected goal, this function does fiddly bit of it
rawprobtoegoal=function(myrawprop) {
	gbgdf$rawpropminute=myrawprop*gbgdf$minute/90
	gamesum0=gbgdf %>% group_by(team,gameweek) %>% summarise(sumprop=sum(rawpropminute,na.rm=T))
	mygameteamsummary=merge(gameteamsummary,gamesum0,all.x=T)
	mygameteamsummary$multprop=with(mygameteamsummary,teamegoal/sumprop)
	gbgdf$multprop=mygameteamsummary$multprop[match(with(gbgdf,paste(team,gameweek)),with(mygameteamsummary,paste(team,gameweek)))]
	gbgdf$playeregoal=with(gbgdf,rawpropminute*multprop)
	return(gbgdf$playeregoal)
}

positionmodel=function(theta, goalassist, runmode='max') {
	proptheta=c(1,exp(theta))
	gbgdf$posprop=proptheta[match(gbgdf$ffposition,c('d','m','f'))]
	#gbgdf$posmult0=proptheta[match(gbgdf$ffposition,c('d','m','f'))]
	### but then need to rescale
	#dum=gbgdf %>% group_by(team,gameweek) %>% summarise(sumposmult0=sum(posmult0,na.rm=T))
	#gbgdf$sumposmult0=dum$sumposmult0[match(with(gbgdf,paste(team,gameweek)),with(dum,paste(team,gameweek)))]
	#gbgdf$posprop=with(gbgdf,posmult0/sumposmult0)
	gbgdf$playeregoal=rawprobtoegoal(gbgdf$posprop)
	if (runmode=='max') {
		if (goalassist=='goal') loglik=with(gbgdf,log(dpois(goal,playeregoal)))
		if (goalassist=='assist') loglik=with(gbgdf,log(dpois(assist,playeregoal)))
		meanloglik=mean(loglik[validsax])
		return(-meanloglik)
	}
	if (runmode=='fit') {
		return(gbgdf$playeregoal)
	}
}

maxinfo=nlm(positionmodel,p=c(0,0),goalassist='goal',runmode='max')
gbgdf$posegoal=positionmodel(maxinfo$est,goalassist='goal',runmode='fit')
maxinfo=nlm(positionmodel,p=rep(0,4),goalassist='assist',runmode='max')
gbgdf$poseassist=positionmodel(maxinfo$est,goalassist='assist',runmode='fit')

### fabulous - next, we want the number of goals scored basedon what you did last season

historicmodel=processproportion(historicmodel)
summarydf[,c('historicminute','historicgoalproportion','historicassistproportion')]=historicmodel[match(summarydf$player,historicmodel$player),c('minute','deservedgoalproportion','deservedassistproportion')]

historiccol=c('historicminute','historicgoalproportion','historicassistproportion')
gbgdf[,historiccol]=summarydf[match(paste(gbgdf$team,gbgdf$player),paste(summarydf$team,summarydf$player)),historiccol]
gbgdf$historicminute=with(gbgdf,ifelse(!is.na(historicminute),historicminute,0))

### then need to multiply in minutes, teamegoal and mix with proprotion
gbgdf[,'historicegoal0']=rawprobtoegoal(gbgdf[,'historicgoalproportion'])
### however that has the critical problem of awarding zero goals to players with NA for historicgoalproportion, so need to rescale again

sumposbyhist=gbgdf %>% filter(minute>0 & ffposition!='g') %>% group_by(team,gameweek) %>% summarise(sumgoal=sum(posegoal),sumgoalvalidhist=sum(posegoal[!is.na(historicgoalproportion)]),multprop=sumgoalvalidhist/sumgoal)
gbgdf[,'historicegoal']=gbgdf$historicegoal0*sumposbyhist$multprop[match(with(gbgdf,paste(team,gameweek)),with(sumposbyhist,paste(team,gameweek)))]
gbgdf[which(gbgdf$historicminute==0),'historicegoal']=-10E6
validmixsax=with(gbgdf,which(!is.na(teamegoal) & assist>=0 & goal>=0 & minute>0 & !is.na(ffposition) & ffposition!='g'))

### so now, we can do the predicting
mixposhistmodel=function(theta, runmode='max') {
	gbgdf$mixegoal=with(gbgdf, (exp(theta)*posegoal + historicminute/(38*90)*historicegoal)/(exp(theta) + historicminute/(38*90)))
	if (runmode=='fit') {
		return(gbgdf$mixegoal)
	}
	if (runmode=='max') {
		gbgdf$loglik=with(gbgdf,log(dpois(goal,mixegoal)))
		meanloglik=mean(gbgdf$loglik[validmixsax])
		return(-meanloglik)
	}
}

maxinfo=optimise(mixposhistmodel,interval=c(-5,5))
gbgdf$poshistegoal=mixposhistmodel(maxinfo$min,runmode='fit')

### let's set up the final stage, which is mixing in the current season data
csgbgdf = gbgdf %>% group_by(team,player) %>% arrange(gameweek) %>% mutate(minute=cumsum(minute)-minute, deservedgoal=cumsum(deservedgoal)-deservedgoal, deservedassist=cumsum(deservedassist)-deservedassist) %>% select(player,team,gameweek,minute,minute,deservedgoal,deservedassist)
### but need to do the proportions calculations

gbgdf[,c('currentminute','currentgoalproportion','currentassistproportion')]=NA
for (gi in 2:max(gbgdf$gameweek)) {
	mysummarydf=csgbgdf %>% filter(gameweek==gi)
	mysummarydf=processproportion(mysummarydf)
	sax=with(gbgdf,which(gameweek==gi))
	mdum=match(with(gbgdf[sax,],paste(team,player)),with(mysummarydf,paste(team,player)))
	gbgdf[sax,c('currentminute','currentgoalproportion','currentassistproportion')]=mysummarydf[mdum,c('minute','deservedgoalproportion','deservedassistproportion')]
}
### then need to rescale for minutes and expected
gbgdf$currentegoal0=rawprobtoegoal(gbgdf$currentgoalproportion)
### however that has the critical problem of awarding zero goals to players with NA for historicgoalproportion, so need to rescale again

sumposbycurrent=gbgdf %>% filter(minute>0 & ffposition!='g') %>% group_by(team,gameweek) %>% summarise(sumgoal=sum(posegoal),sumgoalvalidcurrent=sum(posegoal[!is.na(currentgoalproportion)]),multprop=sumgoalvalidcurrent/sumgoal)
gbgdf[,'currentegoal']=gbgdf$currentegoal0*sumposbycurrent$multprop[match(with(gbgdf,paste(team,gameweek)),with(sumposbycurrent,paste(team,gameweek)))]
gbgdf[which(gbgdf$currentminute==0),'currentegoal']=-10E6
validmixsax=with(gbgdf,which(!is.na(teamegoal) & assist>=0 & goal>=0 & minute>0 & !is.na(ffposition) & ffposition!='g'))

mixposhistcurrentmodel=function(theta, runmode='max') {
	poswgt=exp(theta[1])
	histwgt=exp(theta[2])
	gbgdf$mixegoal=with(gbgdf, (poswgt*posegoal + histwgt*historicminute/(38*90)*historicegoal + currentminute/(38*90)*currentegoal)/(poswgt + histwgt*historicminute/(38*90) + currentminute/(38*90)))
	if (runmode=='fit') {
		return(gbgdf$mixegoal)
	}
	if (runmode=='max') {
		gbgdf$loglik=with(gbgdf,log(dpois(goal,mixegoal)))
		meanloglik=mean(gbgdf$loglik[validmixsax])
		return(-meanloglik)
	}
}

maxinfo=nlm(mixposhistcurrentmodel,p=c(0,0))
gbgdf$poshistcurrentegoal=mixposhistcurrentmodel(maxinfo$est,runmode='fit')

### final step is to include time downweight for this season - that looks a little fiddly though - have to redo that propcessproportion loop


