source('c:/research/general_startup.r')

library(dplyr)
b=scan('c:/research/lineup/spreadexseason.txt',sep='\n','')

### narrow down to season points
b1=b[(grep('SEASON POINTS',b)+1):(grep('INDEX \\(60',b)-1)]

dum=b1[grep('^([A-Z][a-z]).+[0-9\\.]+',b1)]
### now strip that for info

teamdf=data.frame(team=gsub('(^[A-Za-z ]+)(.+$)','\\1',dum))
teamdf$lowval=as.numeric(gsub('(^[^0-9]+)([0-9\\.]+)(.+$)','\\2',dum))
teamdf$highval=as.numeric(gsub('(^[^0-9]+[0-9\\.]+ - )([0-9\\.]+)(.+$)','\\2',dum))
teamdf$ptsofar=as.numeric(gsub('(^.+[^0-9])([0-9]+)(/[0-9]+$)','\\2',dum))
teamdf$playedsofar=as.numeric(gsub('(^.+[^0-9][0-9]+/)([0-9]+$)','\\2',dum))

teamdf$sxepoint=with(teamdf,0.5*(highval+lowval)-ptsofar)

dum=b[grep('Total Goals',b)]
totlowval=as.numeric(gsub('(^[^0-9]+)([0-9]+)([^0-9].+$)','\\2',dum))
tothighval=as.numeric(gsub('(^[^0-9]+[0-9]+ - )([0-9]+)([^0-9].+$)','\\2',dum))
totgsofar=as.numeric(gsub('(^[^0-9]+[0-9]+ - [0-9]+[^0-9])([0-9]+)([^0-9].+$)','\\2',dum))
sxetotgoal=0.5*(tothighval+totlowval)-totgsofar

### now find out what has been played so far
### get this, in chrome, from http://www.premierleague.com/en-gb/matchday/results.html?paramComp_8=true&view=.dateSeason
b=scan('c:/research/lineup/allresult.txt','',sep='\n')
b1=b[grep('^[0-9]{2}:[0-9]{2}.+ - .+$',b)]
b1=gsub('\t','X',b1)
htplayedsofar=gsub('(^[^X]+X)([^X]+)(X.+$)','\\2',b1)
atplayedsofar=gsub('(^.+X)([^X]+)(X.+$)','\\2',b1)
badname=c('Spurs','Man City','Man Utd')
goodname=c('Tottenham','Manchester City','Manchester Utd')
htplayedsofar[htplayedsofar %in% badname]=goodname[match(htplayedsofar[htplayedsofar %in% badname],badname)]
atplayedsofar[atplayedsofar %in% badname]=goodname[match(atplayedsofar[atplayedsofar %in% badname],badname)]
hgoalsofar=as.numeric(gsub('(^.+X)([0-9])( .+$)','\\2',b1))
agoalsofar=as.numeric(gsub('(^.+X[0-9]+ - )([0-9]+)(X.+$)','\\2',b1))

fixtureunteam=unique(c(htplayedsofar,atplayedsofar))
### so what fixtures remain?
dum=expand.grid(ht=fixtureunteam,at=fixtureunteam,stringsAsFactors=F)
dum=dum[dum$ht!=dum$at,]
fixture=dum[which(!paste(dum$ht,dum$at) %in% paste(htplayedsofar,atplayedsofar)),]

fixture$hn=match(fixture$ht,teamdf$team)
fixture$an=match(fixture$at,teamdf$team)

### if total goals not available from spreadex: sxetotgoal=mean(c(hgoalsofar+agoalsofar))*nrow(fixture)

if (F) {
b=read.csv('c:/research/lineup/fixtures.csv',as.is=T)
### lots of crap in the file
fixture=b[which(b$X.4!='-'),]
names(fixture)=c('date','venue','time','fixt','venue2')
### now convert dates
fixture$date=strptime(fixture$date,format="%A %d %B %Y")
fixture$ht=gsub(' v .+$','',fixture$fixt)
fixture$at=gsub('^.+ v ','',fixture$fixt)

goodcol=c('date','ht','at')
fixture=fixture[,goodcol]

### convert a couple of names
badname=c('Spurs','Man City','Man Utd')
goodname=c('Tottenham','Manchester City','Manchester Utd')

fixture$ht[fixture$ht %in% badname]=goodname[match(fixture$ht[fixture$ht %in% badname],badname)]
fixture$at[fixture$at %in% badname]=goodname[match(fixture$at[fixture$at %in% badname],badname)]

## but now we want to eliminate fixtures already played
fixture=fixture[which(fixture$date>as.POSIXlt(Sys.time(), "GMT")),]

postponed=read.csv('c:/research/lineup/postponed.csv',as.is=T)
postponed=postponed[which(postponed$X.1=='P'),]
 
### let's add in the postponed fixtures, unless they've since happened
postponed$ht=gsub(' v .+$','',postponed$X.3)
postponed$at=gsub('^.+ v ','',postponed$X.3)
postponed$ht[postponed$ht %in% badname]=goodname[match(postponed$ht[postponed$ht %in% badname],badname)]
postponed$at[postponed$at %in% badname]=goodname[match(postponed$at[postponed$at %in% badname],badname)]
postponed$date=NA

toadd=which(!with(postponed,paste(ht,at)) %in% with(fixture,paste(ht,at)))

fixture=rbind(fixture,postponed[,goodcol])
}

### good, got the data now

hgag=expand.grid(hg=0:10,ag=0:10)
hgag$hpoint=with(hgag,3*(hg>ag)+(hg==ag))
hgag$apoint=with(hgag,3*(ag>hg)+(hg==ag))
rawcol=c('hpoint','apoint','hg','ag')
margcol=paste('marg',rawcol,sep='')
CalcExpectedGoalPoint=function(htheta,atheta) {
	hgag$prob=dpois(hgag$hg,htheta) * dpois(hgag$ag,atheta)
	### now create the expected sums
	hgag[,margcol]=hgag$prob*hgag[,rawcol]
	dum=colSums(hgag[,margcol])
	return(dum)
}

### now code up the likelihood function - use tapply first then the dplyr stuff

likfunct=function(theta, verbose=F) {
	fixture$ehgoal=exp(theta[fixture$hn]-theta[fixture$an])
	fixture$eagoal=exp(theta[fixture$an]-theta[fixture$hn])

	### very slow:
	fixture[,margcol]=t(sapply(1:nrow(fixture),function(x) CalcExpectedGoalPoint(fixture[x,'ehgoal'],fixture[x,'eagoal'])))
	
	### now add up the number of points
	hetotpoint=with(fixture,tapply(marghpoint,ht,sum))
	aetotpoint=with(fixture,tapply(margapoint,at,sum))
	
	teamdf$etotpoint=as.numeric(hetotpoint[teamdf$team] + aetotpoint[teamdf$team])
	
	teamdf$sqdiff=(teamdf$sxepoint - teamdf$etotpoint)^2
		
	sqdiff=sum( teamdf$sqdiff)
	totgoalsqdiff=0.25*(sxetotgoal-sum(fixture$marghg+fixture$margag))^2
	
	if (verbose) {
		print(teamdf)
		print(sqdiff)
		print(totgoalsqdiff)
	}

	totsqdiff=sqdiff+totgoalsqdiff
	cat('Totsqdiff is',totsqdiff,'\n')
	return(totsqdiff)
}

### now get decent thetainit
totscoredsofar=tapply(c(hgoalsofar,agoalsofar),c(htplayedsofar,atplayedsofar),mean)
totconcsofar=tapply(c(agoalsofar,hgoalsofar),c(htplayedsofar,atplayedsofar),mean)

maxinfo=nlm(likfunct,p=rep(0,nrow(teamdf)),iterlim=500)
