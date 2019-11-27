### want to make team strengths using spreadex data - a bit screwed at the start of the season but we'll worry about that later

### so firstly scan in all spreadexdata

startdate=20160801
filedir=paste(DATAPATH,'spreadex_saved/',sep='')
dum=list.files(filedir)
sxfile=dum[grep('^spreadex_[0-9]{8}',dum)]
sxdate=as.numeric(gsub('(^spreadex_)([0-9]+)(.dat)','\\2',sxfile))
keep=which(sxdate>startdate)

sxdate=sxdate[keep]
sxfile=sxfile[keep]

pricelist=NULL
for (j in 1:length(sxdate)) {
	pricelist[[j]]=read.csv(paste(filedir,sxfile[j],sep=''))
	pricelist[[j]]$date=sxdate[j]
	### need to clean team names
	pricelist[[j]]$team=tolower(pricelist[[j]]$team)
	pricelist[[j]]$team=gsub(' ','',cleanteam(pricelist[[j]]$team,'spreadex'))
}

pricedf=do.call(rbind,pricelist)

### ah but we don't know who's playing who, that's a pain
### we get that from results.csv
resultdf=read.csv(paste(DATAPATH,'fixture_result/all_result.csv',sep=''))
# but then want to add on fixtures so we can include latest odds
latestfixturedf = read.csv(paste(DATAPATH,'fixture_result/latest_fixture.csv',sep=''))
### but don't want to include any beyond where the current odds extend to
cat('what date should i use to cut off where the odds extend to?\n')
cutoffdate = lubridate::now() + days(10)
latestfixturedf %>% filter(ymd(date)<cutoffdate)
sxcutoffdate=scan(nmax=1,quiet=T)
latestfixturedf = latestfixturedf %>% filter(ymd(date)<ymd(sxcutoffdate))
latestfixturedf[,c('hsc','asc')]=NA

resultdf = rbind(resultdf, latestfixturedf)
resultdf=resultdf[order(resultdf$date),]
resultdf$aescore=resultdf$hescore=rep(NA,nrow(resultdf))
### now match date of results to date of pricedf
resultdf$priceindex=findInterval(resultdf$date,sxdate)

for (j in 1:length(sxdate)) {
	sax=which(resultdf$priceindex==j)
	resultdf$hescore[sax]=with(pricelist[[j]],egoal[match(resultdf$ht[sax],team)])
	resultdf$aescore[sax]=with(pricelist[[j]],egoal[match(resultdf$at[sax],team)])
}

### now do the actual modelling
### might miss odds occasionally, so this is necessary:
keep=which(!is.na(resultdf$hescore+resultdf$aescore))
resultdf=resultdf[keep,]
unteam=unique(c(resultdf[,'ht'],resultdf[,'at']))
resultdf$hmap=match(resultdf$ht,unteam)
resultdf$amap=match(resultdf$at,unteam)
resultdf$hprop=with(resultdf,hescore/(hescore+aescore))

### want a bit of a time downweight really, let's put only 0.9 weight on data 1 month ago (so 40% weight on data at start of season)
resultdf$posixdate=strptime(resultdf[,'date'],format='%Y%m%d')
resultdf$weight=with(resultdf,exp(-0.0035*as.numeric(difftime(posixdate[which.max(date)],posixdate,units='days'))))

likfunct1=function(theta) {
	offtheta=theta[1:length(unteam)]
	deftheta=theta[(length(unteam)+1):(2*length(unteam))]
	homeeffect=theta[2*length(unteam)+1]
	ehgoal=with(resultdf,exp(offtheta[hmap]+deftheta[amap]+homeeffect))
	eagoal=with(resultdf,exp(offtheta[amap]+deftheta[hmap]))
	sqdiff=with(resultdf,weight*( (hescore-ehgoal)^2 + (aescore-eagoal)^2))
	priorpen=0.01*(mean(offtheta))^2
	meansqdiff=mean(sqdiff)
	return(meansqdiff)
}
thetainit=rep(0,2*length(unteam)+1)
maxinfo=nlm(likfunct1,p=thetainit)

offtheta=maxinfo$est[1:length(unteam)]
deftheta=maxinfo$est[(length(unteam)+1):(2*length(unteam))]
homeeffect=maxinfo$est[2*length(unteam)+1]

teamdf=data.frame(team=unteam,offtheta=offtheta,deftheta=deftheta)

print(teamdf %>% mutate(overalltheta=offtheta-deftheta) %>% arrange(-overalltheta))

write.csv(file=paste(DATAPATH,'spreadex_saved/teamability_',numericdate(),'.csv',sep=''),teamdf,row.names=F)
write(file=paste(DATAPATH,'spreadex_saved/homeeffect_',numericdate(),'.csv',sep=''),homeeffect)
