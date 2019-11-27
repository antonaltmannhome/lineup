### turn fixtures csv into neat file

convertdate=function(datedata) {
	dumday=gsub('(^[a-z]+ )([0-9]{1,2})( .+$)','\\2',datedata)
	dumwordmonth=gsub('(^[a-z]+ [0-9]{1,2} )([a-z]+)( .+$)','\\2',datedata)
	dummonth=match(dumwordmonth,tolower(month.name))
	dumyear=gsub('(^[a-z]+ [0-9]{1,2} [a-z]+ )(.+$)','\\2',datedata)
	dumday=ifelse(nchar(dumday)==1,paste('0',dumday,sep=''),dumday)
	dummonth=ifelse(nchar(dummonth)==1,paste('0',dummonth,sep=''),dummonth)
	dumdate=paste(dumyear,dummonth,dumday,sep='')
	return(dumdate)
}

stripawayteam=function(atdata) {
	
	firsttry=gsub(' .+$','',atdata)
	firstmatch=which(firsttry %in% fixtureteam)
	stillleft=setdiff(1:length(atdata),firstmatch)
	secondtry=gsub('(^[a-z]+ [a-z]+)( .+$)','\\1',atdata[stillleft])
	secondmatch=stillleft[which(secondtry %in% fixtureteam)]
	stillleft=setdiff(1:length(atdata),c(firstmatch,secondmatch))
	if (length(stillleft)>0) stop('Got some weird issue with away team names, investigate\n')

	ateam=rep(NA,length(atdata))
	ateam[firstmatch]=firsttry[firstmatch]
	ateam[secondmatch]=secondtry
	return(ateam)
}

rawdata=scan('c:/research/lineup/fixtures1617.txt',sep='\n','')
rawdata=tolower(rawdata)

### try to pick out all the date lines
dategrep=grep('^[a-z]+ [0-9]{1,2} [a-z]+ [0-9]{4}',rawdata)
### good, let's convert those to dates firstly
alldate=convertdate(rawdata[dategrep])

### ok, now the tricker bit, try to get the fixtures
### need a list of all team names
fixtureteam=scan('c:/research/lineup/fixtures_teamlist_1617.txt','',quiet=T,sep='\n')
### easiest way to pick up fixtures is via the 'quick view' line
qvgrep=grep('quick view',rawdata)
fixtht=gsub('^ +','',rawdata[qvgrep-3])
### right arse to find the away teams unfortunately
dumat=gsub('^ +','',rawdata[qvgrep-1])
fixtat=stripawayteam(dumat)

### but make sure team names are as we want them to be
fixtht=gsub(' ','',matchteam(fixtht,'premierleague'))
fixtat=gsub(' ','',matchteam(fixtat,'premierleague'))

### now match dates to teams
fixtdate=alldate[findInterval(qvgrep,dategrep)]

### now bind em together and write em out
fixtdf=data.frame(date=fixtdate, ht=fixtht, at=fixtat)
write.csv(file=paste(DATAPATH,'fixtures1617.csv',sep=''),fixtdf,row.names=F)

### next, get hold of results

rawdata=scan('c:/research/lineup/results1617.txt',sep='\n','')
rawdata=tolower(rawdata)

### try to pick out all the date lines
dategrep=grep('^[a-z]+ [0-9]{1,2} [a-z]+ [0-9]{4}',rawdata)
### good, let's convert those to dates firstly
alldate=convertdate(rawdata[dategrep])

scoregrep=grep('[a-z]+ [0-9]\\-[0-9] .+',rawdata)
resht=gsub('^ *','',gsub(' *$','',gsub('(^.+)([0-9]\\-[0-9])(.+$)','\\1',rawdata[scoregrep])))
dumat=gsub('^.+ [0-9]-[0-9] *','',rawdata[scoregrep])
resat=stripawayteam(dumat)
reshsc=as.numeric(gsub('(^.+)([0-9])(\\-)([0-9])(.+$)','\\2',rawdata[scoregrep]))
resasc=as.numeric(gsub('(^.+)([0-9])(\\-)([0-9])(.+$)','\\4',rawdata[scoregrep]))
### then get the dates
resdate=alldate[findInterval(scoregrep,dategrep)]

### but make sure team names are as we want them to be
resht=gsub(' ','',matchteam(resht,'premierleague'))
resat=gsub(' ','',matchteam(resat,'premierleague'))

resdf=data.frame(date=resdate, ht=resht, at=resat, hsc=reshsc, asc=resasc)
write.csv(file=paste(DATAPATH,'results1617.csv',sep=''),resdf,row.names=F)
