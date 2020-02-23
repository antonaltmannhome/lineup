#### whoscored_model_playr2 is nice for setting up functions, but we want to have some way of storing estimates, mixing models etc

### let's have the same coef for all positions, just have different prior for each positions

source('ff_startup.r')

playerdf=read.csv(paste0(DATAPATH, 'playerdata_tidy.csv'),as.is=T)
tddf=read.csv(paste0(DATAPATH, 'teammatchdata_tidy.csv'),as.is=T)

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
	shotgoal=shotibgoal+shotobgoal+fkgoal
)

totshotwmin = playerdf %>%
  group_by(player, team, season) %>%
  summarise(
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
  ) %>%
  ungroup()

playerdf=inner_join(playerdf,totshotwmin,by=c('player', 'team', 'season'))

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

### let's generalise this, make a function that lets us add them in easily

keep=with(playerdf,which(oostotgamprop>5 & mainpos!='GK'))

### so the baseline, just that prior mulitplied by proportion of game played
tdum=tapply(playerdf$shotgoal[keep],playerdf$mainpos[keep],sum)
bdum=tapply(playerdf$gamprop[keep],playerdf$mainpos[keep],sum)
ovprior=tdum/bdum

### the only data we can get from the summary is shots on or off target, and where they're from ,but we can't get eg shots in the box that were on target
### so, we need to have two models, one for accuracy and one for zone

### make the columns in playerdf that are the same as the ones we get from the download

adjfunctgeneral=function(theta,shotcolname,runmode) {
	npriorgame=exp(theta[1])
	shottheta=exp(theta[2:(length(shotcolname)+1)])
	# print(paste(theta,collapse=','))
	newtl=ovprior[playerdf[,'mainpos']]*npriorgame
	newbl=rep(npriorgame,nrow(playerdf))
	for (j in 1:length(shotcolname)) newtl=newtl + shottheta[j]*playerdf[,shotcolname[j]]
	newbl=newbl + with(playerdf,oostotgamprop)
	playerdf=mutate(playerdf,egoal=gamprop*(newtl/newbl))
	### nlm might try something completely stupid, so...
	if (any(playerdf$egoal[keep]>10) | any(playerdf$egoal[keep]<1E-12)) return(10E6)
	playerdf=mutate(playerdf,loglik=log(dpois(shotgoal,egoal)))
	if (runmode=='max') {
		return(-mean(playerdf$loglik[keep]))
	}
	if (runmode=='fit') {
		return(playerdf)
	}
}

zoneshotname=c('oosshotob','oosshotib')
theta=log(c(10,rep(0.1,length(zoneshotname))))
maxinfo=nlm(adjfunctgeneral,p=theta,shotcolname=zoneshotname,runmode='max')
dum=adjfunctgeneral(maxinfo$est,shotcolname=zoneshotname,runmode='fit')
playerdf$ezonegoal=dum$egoal

zoneshottheta=exp(maxinfo$est[2:(length(zoneshotname)+1)])
names(zoneshottheta)=zoneshotname

### now the same for accuracy:
accshotname=c('oosshotgoal','oosshottarget','oosshotmiss','oosshotblock')
theta=log(c(10,rep(0.1,length(accshotname))))
maxinfo=nlm(adjfunctgeneral,p=theta,shotcolname=accshotname,runmode='max')
dum=adjfunctgeneral(maxinfo$est,shotcolname=accshotname,runmode='fit')
playerdf$eaccgoal=dum$egoal

accshottheta=exp(maxinfo$est[2:(length(accshotname)+1)])
names(accshottheta)=accshotname

### what is best combination of them?
mixfunct=function(theta) {
	playerdf$egoal = invlogit(theta)*playerdf$ezonegoal + (1-invlogit(theta))*playerdf$eaccgoal
	playerdf=mutate(playerdf,loglik=log(dpois(shotgoal,egoal)))
	return(-mean(playerdf$loglik[keep]))
}
maxinfo=optimise(mixfunct,interval=c(-10,10))
zoneprop=invlogit(maxinfo$min)

dput(file=paste(USERPATH,'shotvalue.dat',sep=''),list(zoneprop=zoneprop, zoneshottheta=zoneshottheta, accshottheta=accshottheta))

### but can't we mix them togather, as we want to do in practice?
### aarrgh, no we can't, it gets all confused with huge coefs for shots and tiny proportions for the mix

zoneshotname=c('oosshotob','oosshotib')
accshotname=c('oosshotgoal','oosshottarget','oosshotmiss','oosshotblock')

adjfunctgeneral=function(theta) {
	npriorgame=exp(theta[1])
	zoneprop=invlogit(theta[2])
	zoneshottheta=exp(theta[3:4])
	accshottheta=exp(theta[5:8])
	# print(paste(theta,collapse=','))
	zonetl=acctl=ovprior[playerdf[,'mainpos']]*npriorgame
	overallbl=rep(npriorgame,nrow(playerdf))
	for (j in 1:length(zoneshotname)) zonetl=zonetl + zoneshottheta[j]*playerdf[,zoneshotname[j]]
	for (j in 1:length(accshotname)) acctl=acctl + accshottheta[j]*playerdf[,accshotname[j]]
	overallbl=overallbl + with(playerdf,oostotgamprop)
	playerdf=mutate(playerdf, egoalzone=gamprop*(zonetl/overallbl), egoalacc=gamprop*(acctl/overallbl))
	playerdf$egoal=zoneprop*playerdf$egoalzone + (1-zoneprop)*playerdf$egoalacc
	### nlm might try something completely stupid, so...
	if (any(playerdf$egoal[keep]>10) | any(playerdf$egoal[keep]<1E-12)) return(10E6)
	playerdf=mutate(playerdf,loglik=log(dpois(shotgoal,egoal)))
	return(-mean(playerdf$loglik[keep]))
}

theta=c(log(5),logit(0.5),rep(log(0.1),length(c(zoneshotname,accshotname))))
maxinfo=nlm(adjfunctgeneral,p=theta)

### won't be able to use this, but i'm interested in coefs if you do separate out into ontarget/inthebox

shotname=c('oosshotibgoal','oosshotibtarget','oosshotibblock','oosshotibmiss','oosshotobgoal','oosshotobtarget','oosshotobblock','oosshotobmiss')
niceshotname=c('goal-in-the-box','on-target-in-the-box','blocked-in-the-box','miss-in-the-box','goal-out-the-box','on-target-out-the-box','blocked-out-the-box','miss-out-the-box')
theta=log(c(10,rep(0.1,length(shotname))))
maxinfo=nlm(adjfunctgeneral,p=theta,shotcolname=shotname,runmode='max')

shottheta=exp(maxinfo$est[2:(length(shotname)+1)])
names(shottheta)=niceshotname
dum=cbind(names(shottheta),round(shottheta,2))
rownames(dum)=NULL
dum[order(-as.numeric(dum[,2])),]
[1,] "goal-in-the-box"       "0.36"
[2,] "goal-out-the-box"      "0.19"
[3,] "on-target-in-the-box"  "0.18"
[4,] "blocked-in-the-box"    "0.14"
[5,] "on-target-out-the-box" "0.09"
[6,] "miss-in-the-box"       "0.08"
[7,] "blocked-out-the-box"   "0.03"
[8,] "miss-out-the-box"      "0.01"

### i suspect goal-out-the-box is boosted by free kicks - could look into that further one day

### 