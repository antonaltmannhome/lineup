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

### make sure playerdf 

### let's build up some new columns
# playerdf=mutate(playerdf,shotob=shotobblock+shotobgoal+shotobmiss+shotobtarget,fkshot=fkblock+fkgoal+fkmiss+fktarget,shotib=shotibblock+shotibgoal+shotibmiss+shotibtarget,shotblock=fkblock+shotibblock+shotobblock,shottarget=fktarget+shotibtarget+shotobtarget,shotmiss=fkmiss+shotibmiss+shotobmiss,shotgoal=fkgoal+shotibgoal+shotobgoal)
### no that double counts fks, do them separately
playerdf=mutate(playerdf,shotob=shotobblock+shotobgoal+shotobmiss+shotobtarget,fkshot=fkblock+fkgoal+fkmiss+fktarget,shotib=shotibblock+shotibgoal+shotibmiss+shotibtarget,shotblock=shotibblock+shotobblock,shottarget=shotibtarget+shotobtarget,shotmiss=shotibmiss+shotobmiss,shotgoal=shotibgoal+shotobgoal)

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
	totwmin=sum(gamprop)
)

playerdf=inner_join(playerdf,totshotwmin,by='pltmsn')

### now create out of sample averages for each player/team/season combo

playerdf=mutate(playerdf,
	oosshotibgoal=(totshotibgoal-gamprop*shotibgoal)/(totwmin-gamprop),
	oosshotibtarget=(totshotibtarget-gamprop*shotibtarget)/(totwmin-gamprop),
	oosshotibblock=(totshotibblock-gamprop*shotibblock)/(totwmin-gamprop),
	oosshotibmiss=(totshotibmiss-gamprop*shotibmiss)/(totwmin-gamprop),
	oosshotobgoal=(totshotobgoal-gamprop*shotobgoal)/(totwmin-gamprop),
	oosshotobtarget=(totshotobtarget-gamprop*shotobtarget)/(totwmin-gamprop),
	oosshotobblock=(totshotobblock-gamprop*shotobblock)/(totwmin-gamprop),
	oosshotobmiss=(totshotobmiss-gamprop*shotobmiss)/(totwmin-gamprop),
	oosshotib=(totshotib-gamprop*shotib)/(totwmin-gamprop),
	oosshotob=(totshotob-gamprop*shotob)/(totwmin-gamprop),
	oosshottarget=(totshottarget-gamprop*shottarget)/(totwmin-gamprop),
	oosshotblock=(totshotblock-gamprop*shotblock)/(totwmin-gamprop),
	oosshotmiss=(totshotmiss-gamprop*shotmiss)/(totwmin-gamprop),
	oosshotgoal=(totshotgoal-gamprop*shotgoal)/(totwmin-gamprop),
	oostotgamprop=(totwmin-gamprop)
)

keep=with(playerdf,which(oostotgamprop>0))
with(playerdf[keep,],calibplot(oosshotgoal,goal))
	
### now, we can do a little glm now
### no, we absolutely cannot - got to take into account number of minutes played

### and we can, thanks to the offset method i've just leart about - wehey

### let's restrict to strikers firstly who've played a decent number of games

keep=with(playerdf,which(oostotgamprop>5 & mainpos=='FW'))
mod=with(playerdf[keep,],glm(goal~oosshotgoal,offset=log(gamprop),family='poisson'))

keep=with(playerdf,which(oostotgamprop>5 & mainpos=='AM'))
mod=with(playerdf[keep,],glm(goal~oosshotgoal,offset=log(gamprop),family='poisson'))

### ok, i'm finding it a little tricky to get my head around what is happening in the models - let's attach predictions to playerdf, add variables in one by one and see what happens

keep=with(playerdf,which(oostotgamprop>5 & mainpos=='FW'))
mod1=with(playerdf[keep,],glm(goal~oosshotgoal,offset=log(gamprop),family='poisson'))
mod2=with(playerdf[keep,],glm(goal~oosshotgoal+oosshotibtarget,offset=log(gamprop),family='poisson'))
playerdf[,c('mod1','mod2')]=NA
playerdf[keep,'mod1']=fitted(mod1)
playerdf[keep,'mod2']=fitted(mod2)

### easiest to browse by looking at all data for all prem teams over one weekend, not at the same player all at the same time

### need to create a gameweek column though
playerdf$posixdate=strptime(playerdf[,'date'],format='%Y%m%d')
playerdf=mutate(playerdf,gw=as.numeric(difftime(posixdate,min(posixdate),units='weeks')))

### the models' coefs are all over the shop. i think we need to repeat the method used the first time we tried this. that is as follows:
### come up with an initial prior based on position and gamprop - adjust for quality of opposition
### then start with just using goals in other games. what is the value of theta that optimises prior to posterior update where we mix in theta*goals (should be about 1 of course - but we're going to extend to other statistics where it won't be 1)

musigmatoalphabeta=function(mu,sigma) {
	alphaparam=mu^2/sigma^2
	betaparam=mu/sigma^2
	return(list(alphaparam=alphaparam,betaparam=betaparam))
}

## eg prior is 0.18 per game, but player has scored 5 goals in seven games:
dum=musigmatoalphabeta(0.18,0.01)
(dum$alphaparam + 5)/(dum$betaparam+7)
[1] 0.3296
### but we can change the 5 to 5*theta
