### right, let's try to make sense of phil's files
### we do have a file that has got headers and it is this

USERPATH='c:/research/lineup/whoscored/'
setwd(USERPATH)
options(warn=2)

filedf1415=read.csv(paste(USERPATH,'data/AllPlayers1',sep=''),sep=';')
### some awful column names, let's fix
oldname=c('X..3','X..4','X..5','X..6','X..15','X01','X07','X08','X09')
newname=c('ht','at','time','date','team','player','substatus','subtime','pos')
for (j in 1:length(oldname)) names(filedf1415)[names(filedf1415)==oldname[j]]=newname[j]
### make a key which we'll be matching to later
filedf1415$key=with(filedf1415,paste(date,team,player))

### now let's scan in the newer files

cfiledf=NULL
for (j in 1:6) {
	cfiledf[[j]]=read.csv(paste(USERPATH,'data/AllPlayers1_ALL_',j,'.txt',sep=''),sep=';',head=F,as.is=T)
	### they've all got a blank line at the top, get rid
	cfiledf[[j]]=cfiledf[[j]][-1,]
	#### also, some blank lines appear too, get rid
	sax=which(apply(cfiledf[[j]][,paste('V',2:21,sep='')],1,function(x) all(x=='-')))
	if (length(sax)>0) cfiledf[[j]]=cfiledf[[j]][-sax,]
	### other miscellaneous crap appears occasionally too
	keep=grep('^[0-9]{2}:[0-9]{2}:[0-9]{2}$',cfiledf[[j]][,'V7'])
	cfiledf[[j]]=cfiledf[[j]][keep,]
	cat('Have scanned in file',j,'\n')
}

### right, although columns often differ, the first block, which identify the player/match combo, are all the same
### so let's build up a massive data frame for that, along witha  pointer to where we get the rest of the data

minncol=min(c(sapply(cfiledf,ncol),ncol(filedf1415)-1))
for (j in 1:6) {
	cfiledf[[j]]=cfiledf[[j]][,1:minncol]
}

alldatdf=do.call(rbind,cfiledf)
names(alldatdf)=names(filedf1415)[1:ncol(alldatdf)]

### let's have a cut down array to make it easier to find what we want
#dum=lapply(cfiledf,function(x) x[,c('V5','V6','V8','V17','V24')])
### but we also want to indicate which of the cfiledf contains this data
#for (j in 1:6) {
#	dum[[j]]$cf=j
#	dum[[j]]$ix=1:nrow(dum[[j]])
#}
#playerdf=do.call(rbind,dum)

### quite a lot of repetition in that, reduce down to uniqueness
alldatdf$key=with(alldatdf,paste(date,team,player))
alldatdf=alldatdf[match(unique(alldatdf$key),alldatdf$key),]

columndf=read.csv(paste(USERPATH,'whoscored_column_toget.csv',sep=''),as.is=T)

playerdf=alldatdf[,columndf$wsname]
names(playerdf)=columndf$myname

### i hate upper case letters for players/teams, get rid
playerdf$player=tolower(playerdf$player)
playerdf$team=tolower(playerdf$team)
playerdf$ht=tolower(playerdf$ht)
playerdf$at=tolower(playerdf$at)

### also makes date numeric
playerdf$date=gsub('([0-9]{2})(/)([0-9]{2})(/)([0-9]{4})','\\5\\3\\1',playerdf$date)

### which right now i think are shots, where they were from, and what happened
### like to know if they were headers from corners, they might be more random
### and key passes
### and knowing who took free kick/penalty shots

### let's do a bit of further tidying - we want to put the match total into a separate data frame for the team/game

playerdf$teamdate=paste(playerdf$team,playerdf$date)
tddf=data.frame(teamdate=unique(playerdf$teamdate),stringsAsFactors=F)
tddf$date=as.numeric(gsub('^.+ ','',tddf$teamdate))
tddf$team=gsub(' [0-9]+$','',tddf$teamdate)
tddf$ht=playerdf$ht[match(tddf$teamdate,playerdf$teamdate)]
tddf$at=playerdf$at[match(tddf$teamdate,playerdf$teamdate)]

### but we don't want to have teams who've only played 1 or 2 games, we can't pick up long term trends with that
mcutoff=5
satis=F
dum1=table(tddf$team)
goodteam=names(dum1)[dum1>mcutoff]
while( !satis ) {
	keep=which(tddf$ht %in% goodteam & tddf$at %in% goodteam)
	tddf=tddf[keep,]
	dum1=table(tddf$team)
	goodteam=names(dum1)[dum1>mcutoff]
	if (all(tddf$ht %in% goodteam) & all(tddf$at %in% goodteam)) satis=T
}
	
pkeep=which(playerdf$teamdate %in% tddf$teamdate)
playerdf=playerdf[pkeep,]

mdum=match(tddf$teamdate,playerdf$teamdate)
### now we want to get hold of total column for each of the istotal columns
yesistot=columndf$myname[which(columndf$istotal==1)]
dum=playerdf[mdum,yesistot]
dum=t(apply(dum,1,function(x) gsub('^.+\\|','',x)))
dum[dum=='---']=0
dum[dum=='']=0
tddf[,yesistot]=dum
tddf[,yesistot]=t(apply(tddf[,yesistot],1,function(x) as.numeric(x)))

### want quick link between playerdf and teamdf
tddf$mnum=1:nrow(tddf)
playerdf$mnum=match(playerdf$teamdate,tddf$teamdat)
### good, now get rid of totals info from playerdf column

dum=playerdf[,yesistot]
dum=t(apply(dum,1,function(x) gsub('\\|.+$','',x)))
dum[dum=='---']=0
dum[dum=='']=0
playerdf[,yesistot]=dum
playerdf[,yesistot]=t(apply(playerdf[,yesistot],1,function(x) as.numeric(x)))

### right this is going beautifully, now need to concoct a few more columns

### before treating any minutes issues, want to get rid of any games that go to extra time, too annoying and confusing
dum=rep(NA,nrow(playerdf))
dum[grep('^[0-9]+$',playerdf$minsplayed)]=as.numeric(playerdf$minsplayed[grep('^[0-9]+$',playerdf$minsplayed)])
dumf=function(x) {
	if (all(is.na(x))) return(NA)
	return(max(x,na.rm=T))
}
dum=tapply(dum,playerdf$mnum,dumf)
tddf$maxmin=as.numeric(dum)[match(tddf$mnum,names(dum))]

keep=which(tddf$maxmin==90)
pkeep=which(playerdf$mnum %in% tddf$mnum[keep])
tddf=tddf[keep,]
playerdf=playerdf[pkeep,]

### want a minute on, minute off column - assume 2 minutes of stoppage time in first half, 4 minutes second

playerdf$minon=playerdf$minoff=NA
playerdf$minon[which(playerdf$substatus %in% c(0,1))]=0
playerdf$minon[which(playerdf$substatus ==2)]=playerdf$subtime[which(playerdf$substatus ==2)]
playerdf$minoff[which(playerdf$substatus %in% c(0,2))]=90
playerdf$minoff[which(playerdf$substatus ==1)]=playerdf$subtime[which(playerdf$substatus ==1)]
### except if the player got sent off, then override using minsplayed column
playerdf$redcard=as.numeric(playerdf$redcard==1)
sax=which(playerdf$redcard==1)
playerdf$minoff[sax]=playerdf$minon[sax]+as.numeric(playerdf$minsplayed[sax])

### but then ad on tweaks for injury time
sax1=with(playerdf,which(minon>45 & minon<90))
sax2=with(playerdf,which(minoff>45 & minoff<90))
sax3=with(playerdf,which(minoff==90))
playerdf[sax1,'minon']=playerdf[sax1,'minon']+2
playerdf[sax2,'minoff']=playerdf[sax2,'minoff']+2
playerdf[sax3,'minoff']=playerdf[sax3,'minoff']+6

### then, we also have a bit of inconsistency with columns, eg no fkblock
playerdf$fkblock=with(playerdf,fktotal-(fkgoal+fkmiss+fktarget+fkpost))

### let's say post shots are on target
playerdf$shotibtarget=playerdf$shotibtarget + playerdf$shotibpost
playerdf$shotobtarget=playerdf$shotobtarget + playerdf$shotobpost
playerdf$fktarget=playerdf$fktarget + playerdf$fkpost

### similarly for tddf

tddf$fkblock=with(tddf,fktotal-(fkgoal+fkmiss+fktarget+fkpost))

### let's say post shots are on target
tddf$shotibtarget=tddf$shotibtarget + tddf$shotibpost
tddf$shotobtarget=tddf$shotobtarget + tddf$shotobpost
tddf$fktarget=tddf$fktarget + tddf$fkpost

### but we want to get penalty information too
### trouble is it overlaps with other stats in a not always consistent manner
playerdf$shotibgoal=playerdf$shotibgoal-playerdf$pengoal
### also want to remove free kicks from shots outside box
playerdf$shotobgoal=playerdf$shotobgoal-playerdf$fkgoal
playerdf$shotobmiss=playerdf$shotobmiss-playerdf$fkmiss
playerdf$shotobtarget=playerdf$shotobtarget-playerdf$fktarget
playerdf$shotobblock=playerdf$shotobblock-playerdf$fkblock

### occasionally get negative totals, probably due to whoscored being inconsistent, just set these to zero
playerdf[which(playerdf$shotobgoal<0),'shotobgoal']=0
playerdf[which(playerdf$shotobmiss<0),'shotobmiss']=0
playerdf[which(playerdf$shotobtarget<0),'shotobtarget']=0
playerdf[which(playerdf$shotobblock<0),'shotobblock']=0

### need to add this just to keep column names consistent
playerdf$penblock=0
tddf$penblock=0
### helpful to pick out specific match to verify info for it:
#sax=which(alldatdf$at=='Chelsea' & alldatdf$ht=='Newcastle United' & alldatdf$team=='Chelsea' & alldatdf$date=='26/09/2015')
sax=which(alldatdf$at=='West Bromwich Albion' & alldatdf$ht=='Stoke' & alldatdf$team=='Stoke' & alldatdf$date=='29/08/2015')

### now reduce the two dfs down to columns that are probably of interest - can tweak over time of course

goodstatshot=apply(expand.grid(c('shotib','shotob','fk','pen'),c('goal','target','block','miss')),1,paste,collapse='')
goodstatass=c('assist','cornertaken','cross','keypass')

tdnicecol=c('mnum','teamdate','team','date','goal',sort(goodstatshot),goodstatass)
tddf2=tddf[,tdnicecol]

playernicecol=c('mnum','team','ht','at','date','player','minon','minoff','redcard','pos','goal',sort(goodstatshot),goodstatass)
playerdf2=playerdf[,playernicecol]

### save their asses
write.csv(file=paste(USERPATH,'data/playerdata_tidy.csv',sep=''),playerdf2,row.names=F)
write.csv(file=paste(USERPATH,'data/teammatchdata_tidy.csv',sep=''),tddf2,row.names=F)
