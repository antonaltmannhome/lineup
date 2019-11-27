### we now have some good data frames for player data, now let's try to run a few simple models
### want to be able to say something like 'a shot out of the box on target = 0.15 goals'

source('c:/research/general_funct.r')
USERPATH='c:/research/lineup/whoscored/'
setwd(USERPATH)
options(warn=2)

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

### so step 1, make a list of list of points for each player/team/season combo

playerdf$pltmsn=with(playerdf,paste(player,team,season))
unpltmsn=unique(playerdf$pltmsn)
### then build up index points for all of their data points
ixlist=sapply(unpltmsn,function(x) which(playerdf$pltmsn==x))

### next step, we want to recalibrate the totals to reflect (1) minutes played and (2) supremacy of team
### (1) is easy enough, just multiply by 96/minutes played
playerdf$minmult=1/playerdf$gamprop

### for supremacy, calculate mean conceded goals by each team firstly
### break down by season - worry about what to do at start of season some other time - use sportingindex

### need a column matching up the numbers of goals conceded by the team

tddf$ht=playerdf$ht[match(tddf$mnum,playerdf$mnum)]
tddf$at=playerdf$at[match(tddf$mnum,playerdf$mnum)]
tddf$oppteam=with(tddf,ifelse(ht==team,at,ht))
tddf$concgoal=tddf$goal[match(paste(tddf$oppteam,tddf$date),paste(tddf$team,tddf$date))]

### should tidy this up to use season, maybe use a loop too
sax1415=which(tddf$date<20150800)
sax1516=which(tddf$date>20150800)
sax1415h=which(tddf$date<20150800 & tddf$ht==tddf$team)
sax1516h=which(tddf$date>20150800 & tddf$ht==tddf$team)
sax1415a=which(tddf$date<20150800 & tddf$at==tddf$team)
sax1516a=which(tddf$date>20150800 & tddf$at==tddf$team)
mconc1415=with(tddf[sax1415,],tapply(concgoal,team,mean))
mconc1516=with(tddf[sax1516,],tapply(concgoal,team,mean))
### ah but take into account home and away
hmconcgoal=mean(tddf$concgoal[tddf$team==tddf$ht])
amconcgoal=mean(tddf$concgoal[tddf$team==tddf$at])

tddf$gsmult=rep(NA,nrow(tddf))
tddf$gsmult[sax1415h]=hmconcgoal/mconc1415[match(tddf$oppteam[sax1415h],names(mconc1415))]
tddf$gsmult[sax1516h]=hmconcgoal/mconc1516[match(tddf$oppteam[sax1516h],names(mconc1516))]
tddf$gsmult[sax1415a]=amconcgoal/mconc1415[match(tddf$oppteam[sax1415a],names(mconc1415))]
tddf$gsmult[sax1516a]=amconcgoal/mconc1516[match(tddf$oppteam[sax1516a],names(mconc1516))]

### then match that over to the players database
playerdf$gsmult=tddf$gsmult[match(playerdf$mnum,tddf$mnum)]
playerdf$totmult=playerdf$gsmult * playerdf$minmult

### now rescale all the stats for this
goodstatshot=c('goal',apply(expand.grid(c('shotib','shotob','fk'),c('goal','target','block','miss')),1,paste,collapse=''))
goodstatass=c('cornertaken','assistattopenplay','assistattsetpiece','assistmiss','assisttarget','assistgoal','passf3')

rsgoodstatshot=paste('rs',goodstatshot,sep='')
rsgoodstatass=paste('rs',goodstatass,sep='')

playerdf[,rsgoodstatshot]=playerdf$totmult*playerdf[,goodstatshot]

cat('Have calcualted the defense rescaled match stats\n')

### to investigate a specific match:
sax888=with(playerdf,which(at=='chelsea' & ht=='newcastle united' & team=='chelsea' & date==20150926))

plsumdf=data.frame(pltm=names(ixlist))
plsumdf$ngam=sapply(ixlist,function(x) sum(playerdf[x,'gamprop']))
### now start making the average columns
for (myvar in rsgoodstatshot) {
	plsumdf[,myvar]=sapply(ixlist,function(x) sum(playerdf[x,myvar]*playerdf[x,'gamprop']))
	cat('Have calculated time rescaled stats for ',goodstatshot[rsgoodstatshot==myvar],' (',which(rsgoodstatshot==myvar),' out of ',length(rsgoodstatshot),')\n',sep='')
}
### next step, match to player's actual matches, then take away that match's stats
### we want minutes played firstly
playerdf$oosngam=plsumdf$ngam[match(playerdf$pltmsn,plsumdf$pltm)] - playerdf$gamprop
### then loop throught the performance stats
oosgoodstatshot=paste('oos',goodstatshot,sep='')
for (j in 1:length(oosgoodstatshot)) {
	playerdf[,oosgoodstatshot[j]]=plsumdf[match(playerdf$pltmsn,plsumdf$pltm),rsgoodstatshot[j]]
	### but of course we then need to subtract the value for that match
	playerdf[,oosgoodstatshot[j]]=playerdf[,oosgoodstatshot[j]] - playerdf[,rsgoodstatshot[j]]*playerdf[,'gamprop']
	cat('Have out of sample stats for ',goodstatshot[j],' (',j,' out of ',length(goodstatshot),')\n',sep='')
}
### but then let's now convert that to average by game
oosmeanshot=paste('oosmean',goodstatshot,sep='')
for (j in 1:length(oosmeanshot)) {
	playerdf[,oosmeanshot[j]]=playerdf[,oosgoodstatshot[j]]/playerdf[,'oosngam']
}

### right, complete doddle, now let's do some plots

sax=which(playerdf$oosngam>1)
calibplot(playerdf$oosmeangoal[sax],playerdf$goal[sax])

### hmm, massive regression to the mean issue
### ok, let's get modelling

liksax=which(playerdf$gamprop>0)
### first model - use overall average:
playerdf$egoal1=rep(mean(playerdf$goal),nrow(playerdf))
loglik1=mean(log(dpois(playerdf$goal[liksax],playerdf$egoal1[liksax])))

### then factor in number of minutes played
playerdf$egoal2=playerdf$gamprop*sum(playerdf$goal)/sum(playerdf$gamprop)
loglik2=mean(log(dpois(playerdf$goal[liksax],playerdf$egoal2[liksax])))

### ok, massive bias in terms of substituting players who've scored it seems
### so we see that we underestimate badly players who get subbed late on in the game
### but we can't really correct for this, sinced we don't know that it will happen in advance
### ugh, but what a complicated problem
### don't want to go down route of calculating expected number of minutes based on expected prob of winning game - think we just have to leave this horrible feature in

### now let's add in position as a factor
tldum=with(playerdf,tapply(goal,mainpos,sum))
bldum=with(playerdf,tapply(gamprop,mainpos,sum))
playerdf$meanbypos=(tldum/bldum)[match(playerdf$mainpos,names(tldum))]
playerdf$egoal3=playerdf$gamprop*playerdf$meanbypos
loglik3=mean(log(dpois(playerdf$goal[liksax],playerdf$egoal3[liksax])))

### next step, include quality of this team and opposing team
playerdf$egoal4=with(playerdf,gamprop * meanbypos / gsmult)
loglik4=mean(log(dpois(playerdf$goal[liksax],playerdf$egoal4[liksax])))

#### so an easy one to start with - average goals. but we want to do a gamma prior to posterior update

fitmix=function(chosevar, runmode='lik') {
	playerdf$galpha=playerdf$meanbypos^2/chosevar
	playerdf$gbeta=playerdf$meanbypos/chosevar
	### now update the parameter for oosmeangoal
	playerdf$updgalpha=playerdf$galpha + playerdf$oosgoal
	playerdf$updgbeta=playerdf$gbeta + playerdf$oosngam
	playerdf$updmeangoal=playerdf$updgalpha/playerdf$updgbeta
	
	playerdf$egoal5=with(playerdf,updmeangoal*gamprop/gsmult)
	loglik5=mean(log(dpois(playerdf$goal[liksax],playerdf$egoal5[liksax])))
	if (runmode=='lik') return(loglik5)
	if (runmode=='fit') return(playerdf)
}

### 0.005 seems to be rough maximum
### but it wants very little of the data - here are a few things that i think need considering:
### using data from other seasons
### downweighting
### somehow adjusting for when a player changes positions when calculating the oos values
