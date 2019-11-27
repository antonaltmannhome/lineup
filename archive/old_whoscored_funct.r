
combinefile=function(datetouse) {
	### what we want is a file that lists all players and their minutes/goals/assists game by game
	previousdate=getpreviousdate()
	
	### so let's firstly get currentmodelfile
	dum=list.files(paste(DATAPATH,datetouse,sep=''))
	currentmodelfile=dum[grep('^model',dum)]
	### let's first scan that one in and subset down to columns we want
	currentmodel=read.csv(paste(DATAPATH,datetouse,'/',currentmodelfile,sep=''))
	subcurrentmodel=currentmodel %>% select(team,player,minute,deservedgoal,deservedassist)
	
	if (length(previousdate)==0) {
		### we're done - just write subcurrentmodel to disk
		subcurrentmodel$gameweek=1
		meltsubcurrentmodel= melt(subcurrentmodel,id.vars=c('team','player','gameweek'))
		meltsubcurrentmodel = meltsubcurrentmodel %>% arrange(team,player,gameweek)
		fileout=paste(DATAPATH,datetouse,'/gamebygame.csv',sep='')
		write.csv(file=fileout,meltsubcurrentmodel,row.names=F)
	}
	
	if (length(previousdate)>0) {
		### firstly let's scan in previous summary
		previoussummary=read.csv(paste(DATAPATH,previousdate,'/gamebygame.csv',sep=''))
		### now need to append current data to that
		### how many games has each team played so far?
		maxgw=as.data.frame(previoussummary %>% group_by(team) %>% summarise(maxgw=max(gameweek)))
		maxgw$gameweek=maxgw$maxgw+1
		subcurrentmodel=merge(subcurrentmodel,maxgw[,c('team','gameweek')])
		meltsubcurrentmodel= melt(subcurrentmodel,id.vars=c('team','player','gameweek'))
		### the value is accumulative however, let's indicate that
		meltsubcurrentmodel = dplyr::rename(meltsubcurrentmodel,cumvar=value)
		meltsubcurrentmodel = meltsubcurrentmodel %>% arrange(team,player,gameweek)
		### however... we need to subtract the sum of the previous week for the three variables
		sumvardf=as.data.frame(previoussummary %>% group_by(team,player,variable) %>% summarise(sumvar=sum(value)))
		meltsubcurrentmodel=merge(meltsubcurrentmodel,sumvardf,all=T)
		### however, if a player is newly arrived, then they won't have been in sumvardf, so need to replace NAs in sumvar with zero
		meltsubcurrentmodel[which(is.na(meltsubcurrentmodel$sumvar)),'sumvar']=0
		meltsubcurrentmodel$value=meltsubcurrentmodel$cumvar - meltsubcurrentmodel$sumvar
		### then drop cumvar and sumvar, no longer needed
		meltsubcurrentmodel = within(meltsubcurrentmodel,rm(sumvar,cumvar))
		### now mix previous and subcurrent
		combmodel=rbind(previoussummary,meltsubcurrentmodel)
		combmodel = combmodel %>% arrange(team,player,gameweek,match(variable,c('minute','deservedgoal','deservedassist')))
		### then write the bastard to disk
		fileout=paste(DATAPATH,datetouse,'/gamebygame.csv',sep='')
		write.csv(file=fileout,combmodel,row.names=F)
	}
}

combinesummaryfile=function(datetouse) {
	### what we want is a file that lists all players and their minutes/goals/assists game by game
	previousdate=getpreviousdate(datetouse)
	
	### so let's firstly get currentmodelfile
	dum=list.files(paste(DATAPATH,datetouse,sep=''))
	currentsummaryfile=dum[grep('^summary',dum)]
	### let's first scan that one in and subset down to columns we want
	currentsummary=read.csv(paste(DATAPATH,datetouse,'/',currentsummaryfile,sep=''))
	### no need for position and it messes up summing later so get rid
	currentsummary = currentsummary %>% select(-mainpos)

	if (length(previousdate)==0) {
		### we're done - just write subcurrentmodel to disk - and full model too
		summaryfileout=c('gamebygame_full')
		currentsummary$matchnumber=1
		meltsummary= melt(currentsummary,id.vars=c('team','player','matchnumber'))
		meltsummary = meltsummary %>% arrange(team,player,matchnumber)
		fileout=paste(DATAPATH,datetouse,'/gamebygame_full.csv',sep='')
		write.csv(file=fileout,meltsummary,row.names=F)
	}
	
	if (length(previousdate)>0) {
		### firstly let's scan in previous summary
		previoussummary=read.csv(paste(DATAPATH,previousdate,'/gamebygame_full.csv',sep=''))
		sumvardf=as.data.frame(previoussummary %>% group_by(team,player,variable) %>% summarise(sumvar=sum(value)))
		### now need to append current data to that
		### except, not every team has played a fixture since this last one - let's check for this
		previousteamminute=previoussummary %>% group_by(team) %>% filter(variable=='minute') %>% mutate(minute=as.numeric(value)) %>% summarise(totalminute=sum(minute))
		currentteamminute=currentsummary %>% group_by(team) %>% summarise(totalminute=sum(minute))
		currentteamminute$previousminute=previousteamminute$totalminute[match(currentteamminute$team,previousteamminute$team)]
		teamdidplay=with(currentteamminute,team[totalminute>previousminute])
		
		### how many games has each team played so far?
		maxmatchnumber=as.data.frame(previoussummary %>% group_by(team) %>% summarise(maxmatchnumber=max(matchnumber)))
		maxmatchnumber$matchnumber=maxmatchnumber$maxmatchnumber + 1
		fullsummary=merge(currentsummary,maxmatchnumber[,c('team','matchnumber')])
		meltsummary= melt(fullsummary,id.vars=c('team','player','matchnumber'))
		### the value is accumulative however, let's indicate that
		meltsummary = dplyr::rename(meltsummary,cumvar=value)
		meltsummary = meltsummary %>% arrange(team,player,matchnumber)
		### however... we need to subtract the sum of the previous week for the three variables
		#meltsummary=merge(meltsummary,sumvardf,all=T)
		meltsummary=merge(meltsummary,sumvardf,all.x=T)
		### however, if a player is newly arrived, then they won't have been in sumvardf, so need to replace NAs in sumvar with zero
		meltsummary[which(is.na(meltsummary$sumvar)),'sumvar']=0
		meltsummary$value=meltsummary$cumvar - meltsummary$sumvar
		### then drop cumvar and sumvar, no longer needed
		meltsummary = within(meltsummary,rm(sumvar,cumvar))
		### now mix previous and subcurrent
		combsummary=rbind(previoussummary,meltsummary)
		combsummary = combsummary %>% arrange(team,player,matchnumber)
		### then write the bastard to disk
		fileout=paste(DATAPATH,datetouse,'/gamebygame_full.csv',sep='')
		write.csv(file=fileout,combsummary,row.names=F)
	}
}

if (F) {
getsummary=function() {
	mostrecentdir=getmostrecentdir(mysource='whoscored')
	summarydf=read.csv(paste(DATAPATH,'/',mostrecentdir,'/summarydf.csv',sep=''),as.is=T)
	### have got to get hold of ff positions
	summarydf=getplayerprice(summarydf)
	summarydf=processdeserved3(summarydf)
	return(summarydf)
}

getgamebygame=function(summarydf) {
	mostrecentdir=getmostrecentdir(mysource='whoscored')
	verticalgamebygamedf=read.csv(paste(DATAPATH,'/',mostrecentdir,'/gamebygame_full.csv',sep=''),as.is=T)
	gbgdf=spreadmelt(verticalgamebygamedf,spreadOrMelt='spread',keyColumn=c('team','player','matchnumber'),variableName='variable',valueName='value')
	### get ffposition in
	gbgdf$ffposition=summarydf$ffposition[match(paste(gbgdf$team,gbgdf$player),paste(summarydf$team,summarydf$player))]
	### then attach model info
	gbgdf=processdeserved3(gbgdf)
	return(gbgdf)
}
}

processdeserved=function(summarydf) {
		
	goalmaxinfo=dget(file=paste(USERPATH,'whoscored/shotvalue.dat',sep=''))
	assistmaxinfo=dget(file=paste(USERPATH,'whoscored/passvalue.dat',sep=''))
	
	### now modify the goals scored to take away penalties scored
	summarydf[,'goal']=summarydf[,'goal']-summarydf[,'penaltyscored']
	
	### process shot zones calculations
	summarydf=summarydf %>% mutate(shotobcredit = goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotob']*shotoob, shotibcredit=goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotib']*(shot6yd + shotib))
	### then shots accuracy
	summarydf=summarydf %>% mutate(goalcredit  = (1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotgoal']*goal, ontargetcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshottarget']*ont, misscredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotmiss']*(offt+bar), blockcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotblock']*block)
	
	### then add all the bastards up
	summarydf = summarydf %>% mutate(deservedgoal=shotobcredit + shotibcredit + goalcredit + ontargetcredit + misscredit + blockcredit, deservedgoalpg=90*deservedgoal/minute)
	
	### then add in deserved assists
	summarydf=summarydf %>% mutate(assistcredit = assistmaxinfo$passtheta['oosassist']*assist, keypasscredit=assistmaxinfo$passtheta['ooskeypass']*(longkp + shortkp))
	### then add all the bastards up
	summarydf = summarydf %>% mutate(deservedassist=assistcredit + keypasscredit, deservedassistpg=90*deservedassist/minute)
	
	### obviously we then want expected fantasy points
	pointdf=data.frame(pos=c('G','D','DMC','DM','M','AM','F','FW'),point=c(6,6,5,5,5,5,4,4))
	sax=which(!summarydf$nicemainpos %in% pointdf[,'pos'])
	if (length(sax)>0) stop('You need to add type',unique(summarydf[sax,'nicemainpos']),'to the pointdf data frame\n')
	summarydf$goalpoint = pointdf$point[match(summarydf$mainpos,pointdf$pos)]
	summarydf$assistpoint=3
	summarydf = summarydf %>% mutate(deservedpoint = goalpoint*deservedgoal + assistpoint*deservedassist,deservedpointpg=deservedpoint*90/minute)
	
	### then present nicely
	summarydf = summarydf %>% mutate(shotsoutboxinfo=paste(shotoob,' (',round(shotobcredit,2),')',sep=''),shotsinboxinfo=paste(shotib,' (',round(shotibcredit,2),')',sep=''))
	### then present nicely
	summarydf = summarydf %>% mutate(goalinfo = paste(goal,' (',round(goalcredit,2),')',sep=''), ontargetinfo=paste(ont,' (',round(ontargetcredit,2),')',sep=''), offtargetinfo=paste(offt+bar,' (',round(misscredit,2),')',sep=''), blockedinfo=paste(block,' (',round(blockcredit,2),')',sep=''))
	### then present nicely
	summarydf = summarydf %>% mutate(assistinfo = paste(assist,' (',round(assistcredit,2),')',sep=''), keypassinfo=paste(longkp + shortkp,' (',round(keypasscredit,2),')',sep=''))
	
	### slightly annoying, got to put presented stuff into different data frame, otherwise have to use silly column names like 'goalinfo' etc
	presentdf=summarydf[,c('team','player','mainpos','minute')]
	colgrab=names(summarydf)[grep('info',names(summarydf))]
	presentdf[,colgrab]=summarydf[colgrab]
	names(presentdf)[grep('info$',names(presentdf))]=gsub('info$','',names(presentdf)[grep('info$',names(presentdf))])
	### and of course the best of the lot, deserved goals/assists/points
	deservedcol=c('deservedgoal','deservedassist','deservedpoint','deservedgoalpg','deservedassistpg','deservedpointpg')
	presentdf[,deservedcol]=round(summarydf[,deservedcol],2)
	
	return(presentdf)
}

processdeserved2=function(datetouse=NULL) {
	
	if (is.null(datetouse)) datetouse=getmostrecentdir('whoscored')
	goalmaxinfo=dget(file=paste(USERPATH,'whoscored/shotvalue.dat',sep=''))
	assistmaxinfo=dget(file=paste(USERPATH,'whoscored/passvalue.dat',sep=''))
	
	gamebygamedf=read.csv(paste(DATAPATH,datetouse,'/gamebygame_full.csv',sep=''))
	### want to be making it into a wide version so...
	gamebygamedf=gamebygamedf %>% mutate(key=paste(team,player,gameweek))
	### make it into wide version
	gamebygamedf2=gamebygamedf %>% select(-c(team,player,gameweek))
	summarydf=spread(gamebygamedf2,key=variable,value=value)
	
	### now modify the goals scored to take away penalties scored
	summarydf[,'goal']=summarydf[,'goal']-summarydf[,'penaltyscored']
	
	### process shot zones calculations
	summarydf=summarydf %>% mutate(shotobcredit = goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotob']*shotoob, shotibcredit=goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotib']*(shot6yd + shotib))
	### then shots accuracy
	summarydf=summarydf %>% mutate(goalcredit  = (1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotgoal']*goal, ontargetcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshottarget']*ont, misscredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotmiss']*(offt+bar), blockcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotblock']*block)
	
	### then add all the bastards up
	summarydf = summarydf %>% mutate(deservedgoal=shotobcredit + shotibcredit + goalcredit + ontargetcredit + misscredit + blockcredit, deservedgoalpg=90*deservedgoal/minute)
	
	### then add in deserved assists
	summarydf=summarydf %>% mutate(assistcredit = assistmaxinfo$passtheta['oosassist']*assist, keypasscredit=assistmaxinfo$passtheta['ooskeypass']*(longkp + shortkp))
	### then add all the bastards up
	summarydf = summarydf %>% mutate(deservedassist=assistcredit + keypasscredit, deservedassistpg=90*deservedassist/minute)
	
	### then we obviously want the key split back up
	keycol=c('team','player','gameweek')
	summarydf[,keycol]=gamebygamedf[match(summarydf$key,gamebygamedf$key),keycol]
	### and put into nice order
	summarydf = summarydf %>% arrange(team,player,gameweek) %>% select(-key)
	summarydf=summarydf[,c(keycol,setdiff(names(summarydf),keycol))]
	
	return(summarydf)
}

spreadmelt=function(mydf, spreadOrMelt, keyColumn, variableName, valueName) {
	### a bit of an arse to set up and delete the key constantly
	### check that all necessary columnns are actually there
	if (!all(keyColumn %in% names(mydf))) stop('Missing some of the key columns\n')
	mydf$key=apply(mydf[,keyColumn],1,paste,collapse='~')
	mydf2=mydf[,which(!names(mydf) %in% keyColumn)]
	if (spreadOrMelt=='spread') {
		comm=paste('spread(mydf2, key=',variableName,', value=',valueName,')',sep='')
	}
	if (spreadOrMelt=='melt') {
		comm='melt(mydf2, id.vars=\'key\')'
	}
	newmydf=eval(parse(text=comm))
	### now put key columns back in
	newmydf[,keyColumn]=mydf[match(newmydf$key,mydf$key),keyColumn]
	### get rid of key
	newmydf=newmydf %>% select(-key)
	### then move key column names to the front
	newmydf=cbind(newmydf[,keyColumn],newmydf[,setdiff(names(newmydf),keyColumn)])
	return(newmydf)
}

matchupspreadexdata=function(resultdf, fixturedf= NULL) {
	SPREADEXPATH=paste(DATAPATH,'spreadex_saved/',sep='')
	dum=list.files(SPREADEXPATH)
	sxfile=dum[grep('spreadex',dum)]
	sxfiledate=gsub('(^.+\\_)([0-9]{8})(\\.dat)','\\2',sxfile)
	### filter down to 16/17 ones
	keep=which(sxfiledate>20160800)
	sxfile=sxfile[keep]
	sxfiledate=sxfiledate[keep]
	# get corresponding gameweek for each file
	gameweekdf=read.csv(paste(DATAPATH,'gameweek_deadline_1617.csv',sep=''),as.is=T)
	sxfilegameweek=gameweekdf$gameweek[match(sxfiledate,gameweekdf$sxdate)]
	
	sxdata=NULL
	for (j in 1:length(sxfile)) {
		sxdata[[j]]=read.csv(paste(SPREADEXPATH,sxfile[j],sep=''),as.is=T)
		sxdata[[j]]$team=tolower(gsub(' ','',sxdata[[j]]$team))
		sxdata[[j]]$team=cleanteam(sxdata[[j]]$team,'spreadex')
		sxdata[[j]]$gameweek=sxfilegameweek[j]
	}
	sxdata=do.call(rbind,sxdata)
	resultdf[,c('hgoal','agoal')]=sxdata[match(paste(resultdf$ht,resultdf$gameweek),paste(sxdata$team,sxdata$gameweek)),c('egoal','econc')]
	return(resultdf)
}
