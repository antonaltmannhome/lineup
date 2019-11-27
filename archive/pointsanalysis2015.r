source('c:/research/general_funct.r')

pointdf=read.csv('c:/research/lineup/FP points_to_2015.csv',as.is=T)
pricedf=read.csv('c:/research/lineup/FP prices_2015.csv',as.is=T)

### ugh, upper case headings, sort that
names(pointdf)=tolower(names(pointdf))
names(pricedf)=tolower(names(pricedf))

names(pointdf)[names(pointdf)=='x2015.ref']='pid'
names(pricedf)[names(pricedf)=='ref']='pid'

### only interested in 2014/15 season
keep=which(pointdf$season==2015)
pointdf=pointdf[keep,]

dum1=tapply(pointdf$tot,pointdf$pid,sum)
pricedf$tot=as.numeric(dum1)[match(pricedf$pid,names(dum1))]
dum1=tapply(pointdf$mins,pointdf$pid,sum)
pricedf$min=as.numeric(dum1)[match(pricedf$pid,names(dum1))]
dum1=tapply(pointdf$gls,pointdf$pid,sum)
pricedf$goal=as.numeric(dum1)[match(pricedf$pid,names(dum1))]
dum1=tapply(pointdf$ass,pointdf$pid,sum)
pricedf$ass=as.numeric(dum1)[match(pricedf$pid,names(dum1))]
dum1=tapply(pointdf$mins>60,pointdf$pid,sum)
pricedf$gt60min=as.numeric(dum1)[match(pricedf$pid,names(dum1))]
dum1=tapply(pointdf$mins>0,pointdf$pid,sum)
pricedf$totgam=as.numeric(dum1)[match(pricedf$pid,names(dum1))]

pricedf$totbymin=with(pricedf,tot/min)
keep=which(pricedf$min>10*90)
with(pricedf[keep,],calibplot(value,totbymin))
mod=with(pricedf[keep,],lm(totbymin~value))
summary(mod)

pricedf$ppv=pricedf$totbymin/pricedf$value

viewteam=function(myteam,mypos) {
	mypos2=c('Defender','Midfielder','Forward')[match(mypos,c('D','M','F'))]
	sax=with(pricedf,which(team==myteam & pos==mypos2 & !is.na(min)))
	dumdf=pricedf[sax,][order(pricedf[sax,'ppv']),]
	print(dumdf)
}

### question is, were some teams spawny when it came to clean sheets?
ungam=unique(paste(pointdf$game,pointdf$team))
pointdf$gn=match(paste(pointdf$game,pointdf$team),ungam)
gamedf=data.frame(gn=unique(pointdf$gn))
gamedf$team=pointdf$team[match(gamedf$gn,pointdf$gn)]

dum=with(pointdf,tapply(gc,gn,max))
gamedf$gc=as.numeric(dum)[match(gamedf$gn,names(dum))]

### so what we want is an expected number of clean sheets versus actual
teamdf=data.frame(team=unique(gamedf$team))
dum1=tapply(gamedf$gc,gamedf$team,sum)
teamdf$gc=as.numeric(dum1)[match(teamdf$team,names(dum1))]
dum1=tapply(gamedf$gc==0,gamedf$team,sum)
teamdf$cs=as.numeric(dum1)[match(teamdf$team,names(dum1))]

mod=lm(teamdf$cs~teamdf$gc)
teamdf$ecs=predict(mod,teamdf)

### interesting, west brom supremely lucky

### ok, here is a question - do defenders/midfielders/strikers from big teams return better ppv than ones from small teams?

### we need to be careful - assume (although it's clearly wrong) that you know in advance whether a player is going to be selected for the match

sax90=with(pointdf,which(mins>60))

dum1=with(pointdf[sax90,],tapply(tot,pid,sum))
pricedf$tot90=as.numeric(dum1)[match(pricedf$pid,names(dum1))]
dum1=with(pointdf[sax90,],tapply(mins,pid,sum))
pricedf$min90=as.numeric(dum1)[match(pricedf$pid,names(dum1))]
### but dioviding by minute inflates players who are often subbed e.g strikers - so divide by number of games
dum1=with(pointdf[sax90,],table(pid))
pricedf$gam90=as.numeric(dum1)[match(pricedf$pid,names(dum1))]
pricedf$ppg90=with(pricedf,tot90/gam90)

## also count up number of clean sheets
dum1=with(pointdf[sax90,],tapply(cs,pid,sum))
pricedf$cs90=as.numeric(dum1)[match(pricedf$pid,names(dum1))]

### and also bonus points
dum1=with(pointdf[sax90,],tapply(bp,pid,sum))
pricedf$bp=as.numeric(dum1)[match(pricedf$pid,names(dum1))]

### but we really want these as a proportion of games played
pricedf$grat=round(pricedf$goal/pricedf$gam90,2)
pricedf$assrat=round(pricedf$ass/pricedf$gam90,2)
pricedf$csrat=round(pricedf$cs90/pricedf$gam90,2)
pricedf$bprat=round(pricedf$bp/pricedf$gam90,2)

### now filter down to players that actually played a decent number of games
keep=with(pricedf,which(gam90>15))

pricedf2=pricedf[keep,c('pid','name','pos','team','value','grat','assrat','csrat','bprat','tot90','gam90','ppg90')]
pricedf2$vfm=with(pricedf2,ppg90/value)

viewteam2=function(myteam,mypos) {
	mypos2=c('Defender','Midfielder','Forward')[match(mypos,c('D','M','F'))]
	sax=with(pricedf2,which(team==myteam & pos==mypos2))
	dumdf=pricedf2[sax,][order(pricedf2[sax,'vfm']),]
	print(dumdf)
}

### but how do we answer The Question - after all, we don't want to include players who you'd obviously not have selected
### plus, you can only select 15 players, so no use concluding you should buy 100 players all worth 4.5 m

### is this the time to try out the knapsack?

library(knapsack)

### oddly, has to have integer only values, so mulitply everything up for that

### so let's say we're going to spend 30m on defenders:
sax=with(pricedf2,which(pos=='Defender'))
with(pricedf2[sax,],knapsack(round(10*value),round(10*ppg90),300))

### ok, that runs but is is quite useless tbh, just tells you who spawned a lot of points. we need to have an 'expected points' column really, but that is much harder to work out

### or is it - for defenders i reckon it's the easiest in fact
### initially assume that goals/assists/yellows/reds/bonus are all luck

### no ignore all of that, that's where it gets complicated: this is more useful:
dum=sort(with(pricedf2[sax,],tapply(vfm,team,mean)))
dum2=cbind(names(dum),round(dum,2))
rownames(dum2)=NULL
print(dum2)

### general pattern - better value as team gets better

### but maybe we see that with other positions. let's move away from average, let's eg pick two most valuable player by squad/position combo - no don't do that, you might just pick the ones that had best luck in terms of clean sheets

### let's pick out highest scorers in midfield - that should pick out main breadwinners
### that's probably a good idea, but let's do a quick manual check - i'll name what i think is the representative number for each team

### no, just tried that, it's a terrible idea, just pick up the players who did unexpectedly well - no use

### have just manually done the viewteam2 for all teams - no real pattern - all teams have potential to have players with high value - although midfield there seem to be more players who get really high ppg averages. but how easy it is to spot who these players are before they get their points hauls i'm not sure. e.g was it predictable that walters/jedinak/mane were going to be as useful as they were as it is predictable that sanchez/toure are going t o score lots

### NB have got basic bonus points calculation as follows:
### goal: 1.5
### assist: 0.75
### clean sheet: 0.75
