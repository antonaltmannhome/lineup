fulldf=read.csv(file=paste('c:/research/lineup/whoscored/data/combined_',numericdate(),'.csv',sep=''))

dum=round(tapply(fulldf[,'minute']/(11*90),fulldf$team,sum))
fulldf$teammatch=as.numeric(dum)[match(fulldf$team,names(dum))]
dum=round(tapply(fulldf[,'shotib'],fulldf$team,sum))
fulldf$teamshot=as.numeric(dum)[match(fulldf$team,names(dum))]
dum=round(tapply(fulldf[,'shortkp'],fulldf$team,sum))
fulldf$teamkp=as.numeric(dum)[match(fulldf$team,names(dum))]
dum=round(tapply(fulldf[,'goal'],fulldf$team,sum))
fulldf$teamgoal=as.numeric(dum)[match(fulldf$team,names(dum))]
fulldf$adjteamshot=round(fulldf$teamshot*fulldf$minute/(90*fulldf$teammatch),3)
fulldf$shotprop=round(fulldf$shotib/fulldf$adjteamshot,3)
fulldf$adjteamkp=round(fulldf$teamkp*fulldf$minute/(90*fulldf$teammatch),3)
fulldf$kpprop=round(fulldf$shortkp/fulldf$adjteamkp,3)

### now compute the expected points from goals
fulldf$egoalpt=rep(NA,dim(fulldf)[1])
saxf=which(fulldf$mainpos=='FW')
saxm=which(fulldf$mainpos %in% c('AM','M','DM'))
saxd=which(fulldf$mainpos=='D')

#fulldf$egoalpt[saxf]=round(with(fulldf[saxf,],4*shotprop*teamgoal + 3*kpprop*teamgoal),3)
#fulldf$egoalpt[saxm]=round(with(fulldf[saxm,],5*shotprop*teamgoal + 3*kpprop*teamgoal),3)
#fulldf$egoalpt[saxd]=round(with(fulldf[saxd,],6*shotprop*teamgoal + 3*kpprop*teamgoal),3)
fulldf$egoalpt[saxf]=round(with(fulldf[saxf,],4*shotprop + 3*kpprop),3)
fulldf$egoalpt[saxm]=round(with(fulldf[saxm,],5*shotprop + 3*kpprop),3)
fulldf$egoalpt[saxd]=round(with(fulldf[saxd,],6*shotprop + 3*kpprop),3)

### we can record a 'luckiness' rating, which is how many more points they have than they deserve
fulldf$adjgoalpt=rep(NA,dim(fulldf)[1])
fulldf$adjgoalpt[saxf]=round(with(fulldf[saxf,],90*teammatch/minute*(4*goal + 3*assist)),3)
fulldf$adjgoalpt[saxm]=round(with(fulldf[saxm,],90*teammatch/minute*(5*goal + 3*assist)),3)
fulldf$adjgoalpt[saxd]=round(with(fulldf[saxd,],90*teammatch/minute*(6*goal + 3*assist)),3)

### now have a nice data frame, only players who've played at least half the minutes, then by expected points
keep=which(fulldf$minute > 0.5*90*fulldf$teammatch & !is.na(fulldf$egoalpt))
shortdf=fulldf[keep,c('team','player','mainpos','minute','egoalpt')]
shortdf=shortdf[order(-shortdf$egoalpt),]
