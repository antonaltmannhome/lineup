### let's do what we did in v3 but for assists

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

### now try to do some tapply type stuff
playerdf$pltmsn=with(playerdf,paste(player,team,season))
unpltmsn=unique(playerdf$pltmsn)

by_pltmsn=group_by(playerdf,pltmsn)

totwmin=summarise(by_pltmsn,
	totassist=sum(assist*gamprop),
	totcornertaken=sum(cornertaken*gamprop),
	totcross=sum(cross*gamprop),
	totkeypass=sum(keypass*gamprop),
	totgamprop=sum(gamprop)
)

playerdf=inner_join(playerdf,totwmin,by='pltmsn')

### now create out of sample averages for each player/team/season combo

playerdf=mutate(playerdf,
	oosassist=totassist-gamprop*assist,
	ooscornertaken=totcornertaken-gamprop*cornertaken,
	ooscross=totcross-gamprop*cross,
	ooskeypass=totkeypass-gamprop*keypass,
	oostotgamprop=(totgamprop-gamprop)
)

### let's generalise this, make a function that lets us add them in easily

keep=with(playerdf,which(oostotgamprop>5 & mainpos!='GK'))

### so the baseline, just that prior mulitplied by proportion of game played
tdum=tapply(playerdf$assist[keep],playerdf$mainpos[keep],sum)
bdum=tapply(playerdf$gamprop[keep],playerdf$mainpos[keep],sum)
ovprior=tdum/bdum

### the only data we can get from the summary is shots on or off target, and where they're from ,but we can't get eg shots in the box that were on target
### so, we need to have two models, one for accuracy and one for zone

### make the columns in playerdf that are the same as the ones we get from the download

adjfunctgeneral=function(theta,passcolname,runmode) {
	npriorgame=exp(theta[1])
	passtheta=exp(theta[2:(length(passcolname)+1)])
	# print(paste(theta,collapse=','))
	newtl=ovprior[playerdf[,'mainpos']]*npriorgame
	newbl=rep(npriorgame,nrow(playerdf))
	for (j in 1:length(passcolname)) newtl=newtl + passtheta[j]*playerdf[,passcolname[j]]
	newbl=newbl + with(playerdf,oostotgamprop)
	playerdf=mutate(playerdf,eassist=gamprop*(newtl/newbl))
	### nlm might try something completely stupid, so...
	if (any(playerdf$eassist[keep]>20) | any(playerdf$eassist[keep]<1E-12)) return(10E6)
	playerdf=mutate(playerdf,loglik=log(dpois(assist,eassist)))
	if (runmode=='max') {
		return(-mean(playerdf$loglik[keep]))
	}
	if (runmode=='fit') {
		return(playerdf)
	}
}

passname=c('oosassist','ooscornertaken','ooscross','ooskeypass')
theta=log(c(10,rep(0.1,length(passname))))
maxinfo=nlm(adjfunctgeneral,p=theta,passcolname=passname,runmode='max')
dum=adjfunctgeneral(maxinfo$est,shotcolname=zoneshotname,runmode='fit')
playerdf$ezonegoal=dum$egoal

passtheta=exp(maxinfo$est[2:(length(passname)+1)])
names(passtheta)=passname

### interesting, all you need to look at is assists and key passes

passname=c('oosassist','ooskeypass')
theta=log(c(10,rep(0.1,length(passname))))
maxinfo=nlm(adjfunctgeneral,p=theta,passcolname=passname,runmode='max')
dum=adjfunctgeneral(maxinfo$est,passcolname=passname,runmode='fit')
playerdf$eassist=dum$eassist

passtheta=exp(maxinfo$est[2:(length(passname)+1)])
names(passtheta)=passname

dput(file=paste(USERPATH,'passvalue.dat',sep=''),list(passtheta=passtheta))
