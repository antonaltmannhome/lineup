### we now have some good data frames for player data, now let's try to run a few simple models
### want to be able to say something like 'a shot out of the box on target = 0.15 goals'

rm(list=ls())
source('c:/research/general_funct.r')
USERPATH='c:/research/lineup/whoscored/'
setwd(USERPATH)
options(warn=2)

library(dplyr)

playerdf=read.csv('data/playerdata_tidy.csv',as.is=T)
tddf=read.csv('data/teammatchdata_tidy.csv',as.is=T)

cat('Have scanned in the player and team/date combination files\n')

tddf$season=rep(NA,nrow(tddf))
tddf$season[which(tddf$date<20150800)]=1
tddf$season[which(tddf$date>20150800)]=2
playerdf$season=tddf$season[match(playerdf$mnum,tddf$mnum)]

playerdf$gamprop=(playerdf$minoff+1-pmin(playerdf$minon,96))/96
### still get the odd bollox up, override if so
playerdf$gamprop[playerdf$gamprop<1/96]=1/96

playerdf$mainpos=gsub('[^A-Z].+$','',playerdf$pos)
### just put in midfield for the undescribed ones
playerdf[which(playerdf$mainpos==''),'mainpos']='M'

### there is the odd NA in this, ditch such lines
keep=which(!is.na(playerdf$gamprop))
playerdf=playerdf[keep,]

### let's build up some new columns
# playerdf=mutate(playerdf,shotob=shotobblock+shotobgoal+shotobmiss+shotobtarget,fkshot=fkblock+fkgoal+fkmiss+fktarget,shotib=shotibblock+shotibgoal+shotibmiss+shotibtarget,shotblock=fkblock+shotibblock+shotobblock,shottarget=fktarget+shotibtarget+shotobtarget,shotmiss=fkmiss+shotibmiss+shotobmiss,shotgoal=fkgoal+shotibgoal+shotobgoal)
### no that double counts fks, do them separately
playerdf=mutate(playerdf,
	shotob=shotobblock+shotobgoal+shotobmiss+shotobtarget,
	fkshot=fkblock+fkgoal+fkmiss+fktarget,
	shotib=shotibblock+shotibgoal+shotibmiss+shotibtarget,
	shotblock=shotibblock+shotobblock,
	shottarget=shotibtarget+shotobtarget,
	shotmiss=shotibmiss+shotobmiss,
	shotgoal=shotibgoal+shotobgoal
)

### now try to do some tapply type stuff
playerdf$pltmsn=with(playerdf,paste(player,team,season))
unpltmsn=unique(playerdf$pltmsn)

by_pltmsn=group_by(playerdf,pltmsn)

totshotwmin=summarise(by_pltmsn,
	totshotibgoal=sum(shotibgoal*gamprop),
	totshotibtarget=sum(shotibtarget*gamprop),
	totshotibblock=sum(shotibblock*gamprop),
	totshotibmiss=sum(shotibmiss*gamprop),
	totshotobgoal=sum(shotobgoal*gamprop),
	totshotobtarget=sum(shotobtarget*gamprop),
	totshotobblock=sum(shotobblock*gamprop),
	totshotobmiss=sum(shotobmiss*gamprop),
	totshotib=sum(shotib*gamprop),
	totshotob=sum(shotob*gamprop),
	totshotblock=sum(shotblock*gamprop),
	totshotgoal=sum(shotgoal*gamprop),
	totshottarget=sum(shottarget*gamprop),
	totshotmiss=sum(shotmiss*gamprop),
	totgamprop=sum(gamprop)
)

playerdf=inner_join(playerdf,totshotwmin,by='pltmsn')

### now create out of sample averages for each player/team/season combo

playerdf=mutate(playerdf,
	oosshotibgoal=totshotibgoal-gamprop*shotibgoal,
	oosshotibtarget=totshotibtarget-gamprop*shotibtarget,
	oosshotibblock=totshotibblock-gamprop*shotibblock,
	oosshotibmiss=totshotibmiss-gamprop*shotibmiss,
	oosshotobgoal=totshotobgoal-gamprop*shotobgoal,
	oosshotobtarget=totshotobtarget-gamprop*shotobtarget,
	oosshotobblock=totshotobblock-gamprop*shotobblock,
	oosshotobmiss=totshotobmiss-gamprop*shotobmiss,
	oosshotib=totshotib-gamprop*shotib,
	oosshotob=totshotob-gamprop*shotob,
	oosshottarget=totshottarget-gamprop*shottarget,
	oosshotblock=totshotblock-gamprop*shotblock,
	oosshotmiss=totshotmiss-gamprop*shotmiss,
	oosshotgoal=totshotgoal-gamprop*shotgoal,
	oostotgamprop=(totgamprop-gamprop)
)

musigmatoalphabeta=function(mu,sigma) {
	alphaparam=mu^2/sigma^2
	betaparam=mu/sigma^2
	return(list(alphaparam=alphaparam,betaparam=betaparam))
}

### yes, that looks totally sensible

### let's generalise this, make a function that lets us add them in easily

keep=with(playerdf,which(oostotgamprop>5 & mainpos=='AM'))

### so the baseline, just that prior mulitplied by proportion of game played
ovprior=sum(playerdf$goal[keep])/sum(playerdf$gamprop[keep])

### scan in the shot types from data/shottype.dat
# very often want to compare two methods, so set up two files
dum=data.frame(shottype=gsub('^oos','',names(playerdf)[grep('oosshot',names(playerdf))]),include=0)
write.table(file=paste(USERPATH,'data/shottype1.dat',sep=''),dum,sep=':',row.names=F,quote=F)
write.table(file=paste(USERPATH,'data/shottype2.dat',sep=''),dum,sep=':',row.names=F,quote=F)

adjfunctgeneral=function(theta,shotcolname,runmode,playerdf,position) {
	priorstr=exp(theta[1])
	# force prior to be non existant
	priorstr=100
	shottheta=exp(theta[2:(length(shotcolname)+1)])
	# print(paste(theta,collapse=','))
	dum=musigmatoalphabeta(ovprior,priorstr)
	alphaparam=dum$alphaparam
	betaparam=dum$betaparam
	newtl=alphaparam
	for (j in 1:length(shotcolname)) newtl=newtl + shottheta[j]*playerdf[,shotcolname[j]]
	newbl=betaparam + with(playerdf,oostotgamprop)
	playerdf=mutate(playerdf,egoal=gamprop*(newtl/newbl))
	### nlm might try something completely stupid, so...
	if (any(playerdf$egoal>10) | any(playerdf$egoal<1E-12)) return(10E6)
	playerdf=mutate(playerdf,loglik=log(dpois(shotgoal,egoal)))
	keep=with(playerdf,which(mainpos==position))
	if (runmode=='max') {
		return(-mean(playerdf$loglik[keep]))
	}
	if (runmode=='fit') {
		return(playerdf)
	}
}

makeparam=function(modelnumber,position) {
	### want to be able to combine shot types together
	b=read.csv(paste(USERPATH,'data/shottype',modelnumber,'.dat',sep=''),sep=':',as.is=T)
	numcat=max(b$include)
	mycolname=paste('shottype',1:numcat,sep='')
	playerdf[,mycolname]=NA
	shotlabel=rep(NA,numcat)
	for (j in 1:numcat) {
		sax1=which(b$include == j)
		sax2=which(b$include == -j)
		playerdf[,mycolname[j]]=rowSums(playerdf[,paste('oos',b$shottype[sax1],sep=''),drop=F])
		if (length(sax2)>0) playerdf[,mycolname[j]]=playerdf[,mycolname[j],drop=F] - rowSums(playerdf[,paste('oos',b$shottype[sax2],sep=''),drop=F])
		shotlabel[j]=paste(b$shottype[sax1],collapse='+')
		if (length(sax2)>0) shotlabel[j]=paste(shotlabel[j],'- (',paste(b$shottype[sax2],collapse='+'),')',sep='')
	}
	return(list(playerdf=playerdf,numcat=numcat,shotlabel=shotlabel))
}

getparam=function(modelnumber, position, fitmode=F) {
	modelinfo=makeparam(modelnumber, position)
	shotcolname=paste('shottype',1:modelinfo$numcat,sep='')
	### try to make sure we have sensible initial thetas
	initshottheta=ovprior/(colSums(modelinfo$playerdf[keep,shotcolname,drop=F])/sum(playerdf[keep,'oostotgamprop']))
	theta=c(log(0.1),log(initshottheta))
	maxinfo=nlm(adjfunctgeneral,p=theta,shotcolname=shotcolname,runmode='max',playerdf=modelinfo$playerdf, position=position)
	### now make that output look attractive
	cat('maxtheta:\n')
	print(paste(maxinfo$est,collapse=','))
	cat('Likelihood:',maxinfo$min,'\n')
	cat('Prior strength:',exp(maxinfo$est[1]),'\n')
	cat('Parameter values:\n')
	print(cbind(modelinfo$shotlabel,round(exp(maxinfo$est[2:(modelinfo$numcat+1)]),2)))
	### but we might want to return the egoal values
	if (fitmode) {
		playerdf=adjfunctgeneral(maxinfo$est,shotcolname,runmode='fit',playerdf=modelinfo$playerdf)
		return(playerdf$egoal)
	}
}

compmodel=function() {
	playerdf$egoal1=getparam(1,fitmode=T)
	playerdf$egoal2=getparam(2,fitmode=T)
	
	### do histograms of differences
	with(playerdf[keep,],hist(egoal2-egoal1,br=20))
	
	### and provide a few players who are very different
	
	bsquant=with(playerdf[keep,],quantile(egoal2-egoal1,pr=c(0.01,0.99)))
	biggain=with(playerdf[keep,],which(egoal2-egoal1>bsquant[2]))
	gainplay=rev(sort(table(playerdf[keep[biggain],'player'])))[1:10]
	bigloss=with(playerdf[keep,],which(egoal2-egoal1<bsquant[1]))
	lossplay=rev(sort(table(playerdf[keep[bigloss],'player'])))[1:10]
	
	print('big winners:')
	print(gainplay)
	print('big losers:')
	print(lossplay)
	
	assign('playerdf',playerdf,envir=globalenv())
}

### this is useful for exploration
viewplayer=function(playername) {
	dum1=dum2=list(rep(list(),2))
	for (modelnumber in 1:2) {
		modelinfo=makeparam(modelnumber)
		dum1[[modelnumber]]=modelinfo$shotlabel
		dum2[[modelnumber]]=paste('shottype',modelnumber,'.',1:modelinfo$numcat,sep='')
		### but bolster playerdf
		playerdf[,paste('shottype',modelnumber,'.',1:modelinfo$numcat,sep='')]=modelinfo$playerdf[,paste('shottype',1:modelinfo$numcat,sep='')]
	}
	shotlabel=sapply(dum1,function(x) paste(x,collapse='   '))
	shotcolname=do.call(c,dum2)
	shotcolnameprop=paste(shotcolname,'prop',sep='')
	for (j in 1:length(shotcolname)) {
		playerdf[,shotcolnameprop[j]]=playerdf[,shotcolname[j]]/playerdf[,'oostotgamprop']
	}
	sax=grep(playername,playerdf[,'player'])
	dum=playerdf[sax,c('player','gamprop','egoal1','egoal2',shotcolnameprop)]
	print(dum)
	print(shotlabel)
}

### but here's something my system doesn't allow - i often want to combine columns, or subtract one from the other eg i might want a single parameter for all shots in box that weren't goals - how do i do that?

### ok, this is good i think : this is for FWs:
[,1]                     [,2]  
[1,] "shotibgoal"             "0.3" 
[2,] "shotibtarget"           "0.27"
[3,] "shotibblock+shotibmiss" "0.14"
[4,] "shotobgoal"             "0.56"
[5,] "shotobtarget"           "0.12"
[6,] "shotobblock+shotobmiss" "0.02"
### and this is for AM:
     [,1]                     [,2]  
[1,] "shotibgoal"             "0.42"
[2,] "shotibtarget"           "0.16"
[3,] "shotibblock+shotibmiss" "0.15"
[4,] "shotobgoal"             "0.01"
[5,] "shotobtarget"           "0.14"
[6,] "shotobblock+shotobmiss" "0.03"
### what the hell is going on with shotobgoal
