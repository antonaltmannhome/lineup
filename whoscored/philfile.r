### right, let's try to make sense of phil's files
### we do have a file that has got headers and it is this

USERPATH='c:/research/lineup/whoscored/'

filedf1415=read.csv(paste(USERPATH,'data/AllPlayers1',sep=''),sep=';')
### some awful column names, let's fix
oldname=c('X..3','X..4','X..5','X..6','X..15','X01')
newname=c('ht','at','time','date','team','player')
for (j in 1:length(oldname)) names(filedf1415)[names(filedf1415)==oldname[j]]=newname[j]
### make a key which we'll be matching to later
filedf1415$key=with(filedf1415,paste(date,team,player))

### now let's scan in the newer files

cfiledf=NULL
for (j in 1:6) {
	cfiledf[[j]]=read.csv(paste(USERPATH,'data/AllPlayers1_ALL_',j,'.txt',sep=''),sep=';',head=F,as.is=T)
	### they've all got a blank line at the top, get rid
	cfiledf[[j]]=cfiledf[[j]][-1,]
	cat('Have scanned in file',j,'\n')
}

### right, although columns often differ, the first block, which identify the player/match combo, are all the same
### so let's build up a massive data frame for that, along witha  pointer to where we get the rest of the data

minncol=min(sapply(cfiledf,ncol))
for (j in 1:6) {
	cfiledf[[j]]=cfiledf[[j]][,1:minncol]
}

cfiledf=do.call(rbind,cfiledf)

### let's have a cut down array to make it easier to find what we want
#dum=lapply(cfiledf,function(x) x[,c('V5','V6','V8','V17','V24')])
### but we also want to indicate which of the cfiledf contains this data
#for (j in 1:6) {
#	dum[[j]]$cf=j
#	dum[[j]]$ix=1:nrow(dum[[j]])
#}
#playerdf=do.call(rbind,dum)
playerdf=cfiledf[,c('V5','V6','V8','V17','V24')]
names(playerdf)=c('ht','at','date','team','player')

### now make the horrible big string identifying the player/match combo
playerdf$key=with(playerdf,paste(date,team,player))

### but we have some duplication, get rid of that
mdum=match(unique(playerdf$key),playerdf$key)
playerdf=playerdf[mdum,]

### create the same with filedf1415...

sax=which(playerdf$key %in% filedf1415$key)
playerdf$fmatch=rep(NA,nrow(playerdf))
playerdf$fmatch[sax]=match(playerdf$key[sax],filedf1415$key)

### ok, we've got a link between something without column headers and something with them
### and they all seem to be the same, extra cols in cfile seem to be related to penalty shootouts

### so let's go grab some data and name the columns - the amount of data is excessive, let's reduce to what is currently seen to be useful

names(cfiledf)=names(filedf1415)[1:ncol(cfiledf)]

### which right now i think are shots, where they were from, and what happened
### like to know if they were headers from corners, they might be more random
### and key passes
### and knowing who took free kick/penalty shots

b=read.csv(paste(USERPATH,'whoscored_column_toget.csv',sep=''),head=F,as.is=T)
myname=b$V2
wsname=b$V1

### now bolster playerdf with all these new columns

playerdf[,myname]=NA
for (j in 1:6) {
	sax=which(playerdf$cf==j)
	playerdf[sax,myname]=cfiledf[[j]][playerdf$ix[sax],wsname]
}
	
### let's get hold of all stats for a chelsea game, will make it easier to piece everything together

sax=which(playerdf$at=='Chelsea' & playerdf$ht=='Newcastle United' & playerdf$team=='Chelsea' & playerdf$date=='26/09/2015')
