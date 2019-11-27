
source(paste(USERPATH,'data fetching/fixture_result_funct.r',sep=''))

browserToUse='Firefox'
haveJustFinishedSeason = FALSE

resultPage='https://www.premierleague.com/results'
fixturePage='https://www.premierleague.com/fixtures'

if (haveJustFinishedSeason) {
	pageList = 'result'
}
if (!haveJustFinishedSeason) {
	pageList=c('result','fixture')
}
for (page in pageList) {
	if (page=='result') {
		webPage=resultPage
		fileName=paste('result',currentseason,'.txt',sep='')
	}
	if (page=='fixture') {
		webPage=fixturePage
		fileName=paste('fixture',currentseason,'.txt',sep='')
	}
	savePageInfo(webPage, fileName, thiscomputer = thiscomputer)
}

fixtureteam=scan(paste(DATAPATH,'fixture_result/fixtures_teamlist_',currentseason,'.txt',sep=''),'',quiet=T,sep='\n')

if (!haveJustFinishedSeason) {
	rawdata=scan(paste(DATAPATH,'fixture_result/fixture',currentseason,'.txt',sep=''),sep='\n','',quiet=T)
	rawdata=tolower(rawdata)

	### try to pick out all the date lines
	dategrep=grep('^[a-z]+ [0-9]{1,2} [a-z]+ [0-9]{4}',rawdata)
	### good, let's convert those to dates firstly
	fixtdate=convertdate(rawdata[dategrep])

	### but there might be postponed fixtures to deal with
	tbcgrep=grep('date to be confirmed',rawdata)
	tbcdate=rep(max(fixtdate),length(tbcgrep))

	alldate=c(tbcdate,fixtdate)
	allgrep=c(tbcgrep,dategrep)
	odum=order(allgrep)
	alldate=alldate[odum]
	allgrep=allgrep[odum]

	### ok, now the tricker bit, try to get the fixtures
	### need a list of all team names
	### easiest way to pick up fixtures is via the 'quick view' line
	qvgrep=grep('quick view',rawdata)
	fixtht=gsub('^ +','',rawdata[qvgrep-4])
	# this doesn't seem like a particularly stable solution, never mind
	fixtat=gsub('  +.+$', '', gsub('^ +', '', rawdata[qvgrep - 2]))

	### but make sure team names are as we want them to be
	fixtht=gsub(' ','',cleanteam(fixtht,'premierleague'))
	fixtat=gsub(' ','',cleanteam(fixtat,'premierleague'))

	### now match dates to teams
	fixtdate=alldate[findInterval(qvgrep,allgrep)]

	### now bind em together and write em out
	fixtdf=data.frame(date=fixtdate, ht=fixtht, at=fixtat)
	write.csv(file=paste(DATAPATH,'fixture_result/fixture',currentseason,'.csv',sep=''),fixtdf,row.names=F)
}
### next, get hold of results

rawdata=scan(paste(DATAPATH,'fixture_result/result',currentseason,'.txt',sep=''),sep='\n','',quiet=T)
rawdata=tolower(rawdata)

### try to pick out all the date lines
dategrep=grep('^[a-z]+ [0-9]{1,2} [a-z]+ [0-9]{4}',rawdata)
### good, let's convert those to dates firstly
alldate=convertdate(rawdata[dategrep])

scoregrep=grep('[a-z]+ [0-9]\\-[0-9] .+',rawdata)
resht=gsub('^ *','',gsub(' *$','',gsub('(^.+)([0-9]\\-[0-9])(.+$)','\\1',rawdata[scoregrep])))
resat=gsub('(^.+ [0-9]+\\-[0-9]+ )(.+)(  .+$)','\\2',rawdata[scoregrep])
reshsc=as.numeric(gsub('(^.+)([0-9])(\\-)([0-9])(.+$)','\\2',rawdata[scoregrep]))
resasc=as.numeric(gsub('(^.+)([0-9])(\\-)([0-9])(.+$)','\\4',rawdata[scoregrep]))
### then get the dates
resdate=alldate[findInterval(scoregrep,dategrep)]

### but make sure team names are as we want them to be
resht=gsub(' ','',cleanteam(resht,'premierleague'))
resat=gsub(' ','',cleanteam(resat,'premierleague'))

resdf=data.frame(date=resdate, ht=resht, at=resat, hsc=reshsc, asc=resasc)
write.csv(file=paste(DATAPATH,'fixture_result/result',currentseason,'.csv',sep=''),resdf,row.names=F)
