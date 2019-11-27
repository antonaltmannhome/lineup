
### just wrong the way it's stored - 'model' should be called 'full data' and there should just be a function to convert data into model summary

combinesummaryfile=function(datetouse) {
	### what we want is a file that lists all players and their minutes/goals/assists game by game
	allfile=list.files(DATAPATH)
	alldate=as.numeric(allfile[grep('^[0-9]{8}$',allfile)])
	sax=which(alldate>20160801 & alldate<datetouse)
	if (length(sax)==0) previousdate=NULL
	if (length(sax)>0) previousdate=max(alldate[sax])
	
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
		currentsummary$gameweek=1
		meltsummary= melt(currentsummary,id.vars=c('team','player','gameweek'))
		meltsummary = meltsummary %>% arrange(team,player,gameweek)
		fileout=paste(DATAPATH,datetouse,'/gamebygame_full.csv',sep='')
		write.csv(file=fileout,meltsummary,row.names=F)
	}
	
	if (length(previousdate)>0) {
		### firstly let's scan in previous summary
		previoussummary=read.csv(paste(DATAPATH,previousdate,'/gamebygame_full.csv',sep=''))
		sumvardf=as.data.frame(previoussummary %>% group_by(team,player,variable) %>% summarise(sumvar=sum(value)))
		### now need to append current data to that
		### how many games has each team played so far?
		maxgw=as.data.frame(previoussummary %>% group_by(team) %>% summarise(maxgw=max(gameweek)))
		maxgw$gameweek=maxgw$maxgw+1
		fullsummary=merge(currentsummary,maxgw[,c('team','gameweek')])
		meltsummary= melt(fullsummary,id.vars=c('team','player','gameweek'))
		### the value is accumulative however, let's indicate that
		meltsummary = dplyr::rename(meltsummary,cumvar=value)
		meltsummary = meltsummary %>% arrange(team,player,gameweek)
		### however... we need to subtract the sum of the previous week for the three variables
		meltsummary=merge(meltsummary,sumvardf,all=T)
		### however, if a player is newly arrived, then they won't have been in sumvardf, so need to replace NAs in sumvar with zero
		meltsummary[which(is.na(meltsummary$sumvar)),'sumvar']=0
		meltsummary$value=meltsummary$cumvar - meltsummary$sumvar
		### then drop cumvar and sumvar, no longer needed
		meltsummary = within(meltsummary,rm(sumvar,cumvar))
		### now mix previous and subcurrent
		combsummary=rbind(previoussummary,meltsummary)
		combsummary = combsummary %>% arrange(team,player,gameweek)
		### then write the bastard to disk
		fileout=paste(DATAPATH,datetouse,'/gamebygame_full.csv',sep='')
		write.csv(file=fileout,combsummary,row.names=F)
	}
}

processdeserved2=function(datetouse) {
	
	goalmaxinfo=dget(file=paste(USERPATH,'whoscored/shotvalue.dat',sep=''))
	assistmaxinfo=dget(file=paste(USERPATH,'whoscored/passvalue.dat',sep=''))
	
	gamebygamedf=read.csv(paste(DATAPATH,datetouse,'/gamebygame_full.csv',sep=''))
	### make it into wide version
	gamebygamedf2=gamebygamedf %>% mutate(key=paste(team,player,gameweek)) %>% select(-c(team,player,gameweek))
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
	
	return(summarydf)
}
