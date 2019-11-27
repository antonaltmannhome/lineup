### we've done the modeling in whoscore_model_plyr3.r, but now we want to produce deserved goals for each player.
### what is a bit of an arse is that it doesn't tell you when a penalty has been scored so need to enter those manually

rm(list=ls())
source('c:/research/general_funct.r')
USERPATH='c:/research/lineup/whoscored/'
setwd(USERPATH)
options(warn=2)

library(dplyr)

goalmaxinfo=dget(file=paste(USERPATH,'shotvalue.dat',sep=''))
assistmaxinfo=dget(file=paste(USERPATH,'passvalue.dat',sep=''))

### what is the latest date that we've recorded shots?
dum=list.dirs(paste(USERPATH,'data/',sep=''))
latestdate=max(as.numeric(gsub('^.+/','',dum)),na.rm=T)
currentseason='1617'

curdf=read.csv(paste(USERPATH,'data/',latestdate,'/combined_',latestdate,'.csv',sep=''),encoding='UTF-8',as.is=T)
curdf$nicemainpos=gsub('(^[0-9]+,)([A-Z]+)(.+$)','\\2',curdf$mainpos)

### however now correct for penalties scored
### to get info and update file:
getpeninfo=function() {
	filein=paste(USERPATH,'data/peninfo_',currentseason,'.csv',sep='')
	### new season? then do this line: dumdf=curdf[which(curdf$penaltytaken>0),c('team','player','penaltytaken')];dumdf[,c('penaltyscored','oldpenaltytaken','oldpenaltyscore')]=NA;write.csv(filein,dumdf,row.names=F)
	peninfo=read.csv(filein)
	### does it need updating?
	curpeninfo=curdf[which(curdf$penaltytaken>0),c('team','player','penaltytaken')]
	curpeninfokey=with(curpeninfo,paste(team,player))
	oldpeninfokey=with(peninfo,paste(team,player))
	mdum=match(oldpeninfokey,curpeninfokey)
	curpeninfo[,c('penaltyscored','oldpenaltytaken','oldpenaltyscored')]=rep(NA,nrow(curpeninfo))
	curpeninfo[mdum,c('oldpenaltytaken','oldpenaltyscored')]=peninfo[,c('penaltytaken','penaltyscored')]
	nochangesax=with(curpeninfo,which(!is.na(curpeninfo$oldpenaltytaken) & curpeninfo$oldpenaltytaken==curpeninfo$penaltytaken))
	curpeninfo[nochangesax,'penaltyscored']=curpeninfo[nochangesax,'oldpenaltyscored']
	anotherpenaltysax=with(curpeninfo,which(oldpenaltytaken!=penaltytaken))
	newpenaltytakersax=with(curpeninfo,which(is.na(curpeninfo$oldpenaltytaken)))
	if (length(anotherpenaltysax)>0) {
		cat('The following players have taken more penalty(s) since the file was last written, please update their info:\n')
		curpeninfo[anotherpenaltysax,'penaltyscored']=NA
		print(curpeninfo[anotherpenaltysax,])
	}
	if (length(newpenaltytakersax)>0) {
		cat('The following players have taken penalty(s) for the first time since the file was last written, please update their info:\n')
		print(curpeninfo[newpenaltytakersax,])
	}
	if (length(anotherpenaltysax)>0 | length(newpenaltytakersax)>0) {
		write.csv(file=filein,curpeninfo,row.names=F)
		print('Press ENTER when you have done so')
		dum=scan(what='',nmax=1,quiet=T)
	}
	peninfo=read.csv(filein)
	return(peninfo)
}

peninfo=getpeninfo()

### now modify the goals scored to take away penalties scored
mdum=match(paste(peninfo$team,peninfo$player),paste(curdf$team,curdf$player))
curdf[mdum,'goal']=curdf[mdum,'goal']-peninfo$penaltyscored

### process shot zones calculations
curdf=curdf %>% mutate(shotobcredit = goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotob']*shotoob, shotibcredit=goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotib']*(shot6yd + shotib))
### then shots accuracy
curdf=curdf %>% mutate(goalcredit  = (1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotgoal']*goal, ontargetcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshottarget']*ont, misscredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotmiss']*(offt+bar), blockcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotblock']*block)

### then add all the bastards up
curdf = curdf %>% mutate(deservedgoal=shotobcredit + shotibcredit + goalcredit + ontargetcredit + misscredit + blockcredit, deservedgoalpg=90*deservedgoal/minute)

### then add in deserved assists
curdf=curdf %>% mutate(assistcredit = assistmaxinfo$passtheta['oosassist']*assist, keypasscredit=assistmaxinfo$passtheta['ooskeypass']*(longkp + shortkp))
### then add all the bastards up
curdf = curdf %>% mutate(deservedassist=assistcredit + keypasscredit, deservedassistpg=90*deservedassist/minute)

### obviously we then want expected fantasy points
pointdf=data.frame(pos=c('G','D','DMC','DM','M','AM','F'),point=c(6,6,5,5,5,5,4))
sax=which(!curdf$nicemainpos %in% pointdf[,'pos'])
if (length(sax)>0) stop('You need to add type',unique(curdf[sax,'nicemainpos']),'to the pointdf data frame\n')
curdf$goalpoint = pointdf$point[match(curdf$nicemainpos,pointdf$pos)]
curdf$assistpoint=3
curdf = curdf %>% mutate(deservedpoint = goalpoint*deservedgoal + assistpoint*deservedassist,deservedpointpg=deservedpoint*90/minute)

### then present nicely
curdf = curdf %>% mutate(shotsoutboxinfo=paste(shotoob,' (',round(shotobcredit,2),')',sep=''),shotsinboxinfo=paste(shotib,' (',round(shotibcredit,2),')',sep=''))
### then present nicely
curdf = curdf %>% mutate(goalinfo = paste(goal,' (',round(goalcredit,2),')',sep=''), ontargetinfo=paste(ont,' (',round(ontargetcredit,2),')',sep=''), offtargetinfo=paste(offt+bar,' (',round(misscredit,2),')',sep=''), blockedinfo=paste(block,' (',round(blockcredit,2),')',sep=''))
### then present nicely
curdf = curdf %>% mutate(assistinfo = paste(assist,' (',round(assistcredit,2),')',sep=''), keypassinfo=paste(longkp + shortkp,' (',round(keypasscredit,2),')',sep=''))

### slightly annoying, got to put presented stuff into different data frame, otherwise have to use silly column names like 'goalinfo' etc
presentdf=curdf[,c('team','player','mainpos','minute')]
colgrab=names(curdf)[grep('info',names(curdf))]
presentdf[,colgrab]=curdf[colgrab]
names(presentdf)[grep('info$',names(presentdf))]=gsub('info$','',names(presentdf)[grep('info$',names(presentdf))])
### and of course the best of the lot, deserved goals/assists/points
deservedcol=c('deservedgoal','deservedassist','deservedpoint','deservedgoalpg','deservedassistpg','deservedpointpg')
presentdf[,deservedcol]=round(curdf[,deservedcol],2)

viewteam=function(myteam) {
	print(presentdf %>% filter(team==myteam & minute>max(minute)/5) %>% arrange(-deservedpointpg))
}

### write in desending order all players
dum=presentdf %>% filter(minute>max(minute)/5) %>% arrange(-deservedpointpg)
write.csv(file=paste(USERPATH,'data/',latestdate,'/deservedpoint.csv',sep=''),dum,row.names=F)
