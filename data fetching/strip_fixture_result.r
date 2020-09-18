
source(paste(USERPATH,'data fetching/fixture_result_funct.r',sep=''))

browserToUse='Firefox'

resultPage='https://www.premierleague.com/results'
fixturePage='https://www.premierleague.com/fixtures'

if (aboutToStartSeason) {
  pageList = 'fixture'
}
if (haveJustFinishedSeason) {
	pageList = 'result'
}
if (!haveJustFinishedSeason & !aboutToStartSeason) {
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
	### but there might be postponed fixtures to deal with
	tbcgrep=grep('date to be confirmed',rawdata)
	### good, let's convert those to dates firstly
	dum=as.integer(convertdate(rawdata[dategrep]))
  # but the safest thing to do is put hte maximum date in for the postponed fixtures
	uniquefixtdate = c(max(dum), dum)

	### ok, now the tricker bit, try to get the fixtures
	### need a list of all team names
	### easiest way to pick up fixtures is via the 'quick view' line
	qvgrep=grep('quick view',rawdata)
	fixtdate = c(uniquefixtdate, 30000101)[cut(qvgrep, c(tbcgrep, dategrep, 10E10), labels = FALSE)]
	
	# right, next we have to clear away all the rubbish surrounding the fixtures
	reduceddata = rawdata[tbcgrep:max(qvgrep)]
	reduceddata = gsub('^ +', '', reduceddata)
	tbcIndex = which(reduceddata == 'tbc')
	if (length(tbcIndex) > 0) {
	  reduceddata = reduceddata[-tbcIndex]
	}
	timeIndex = grep('[0-9]+:[0-9]+', reduceddata)
	if (length(timeIndex) > 0) {
	  reduceddata = reduceddata[-timeIndex]
	}
	dateIndex = grep('^[a-z]+ [0-9]+ [a-z]+ [0-9]+$', reduceddata)
	if (length(dateIndex) > 0) {
	  reduceddata = reduceddata[-dateIndex]
	}
	qvgrep = grep('quick view', reduceddata)
	
	fixtht=reduceddata[qvgrep-2]
	fixtat = reduceddata[qvgrep-1]

	fixtDF = tibble(date = fixtdate, ht = fixtht, at = fixtat)
	
	### but make sure team names are as we want them to be
	fixtDF$ht=gsub(' ','',cleanteam(fixtDF$ht,'premierleague'))
	fixtDF$at=gsub(' ','',cleanteam(fixtDF$at,'premierleague'))

	write.csv(file=paste(DATAPATH,'fixture_result/fixture',currentseason,'.csv',sep=''),fixtDF,row.names=F)
}
### next, get hold of results

if (!aboutToStartSeason) {
  rawdata=scan(paste(DATAPATH,'fixture_result/result',currentseason,'.txt',sep=''),sep='\n','',quiet=T)
  rawdata=tolower(rawdata)
  
  ### try to pick out all the date lines
  dategrep=grep('^[a-z]+ [0-9]{1,2} [a-z]+ [0-9]{4}',rawdata)
  ### good, let's convert those to dates firstly
  alldate=convertdate(rawdata[dategrep])
  
  scoregrep=grep('[a-z]+ [0-9]\\-[0-9] .+',rawdata)
  resht=gsub('^ *','',gsub(' *$','',gsub('(^.+)([0-9]\\-[0-9])(.+$)','\\1',rawdata[scoregrep])))
  resat=gsub('(^.+ [0-9]+\\-[0-9]+ )(.+$)','\\2',rawdata[scoregrep])
  reshsc=as.numeric(gsub('(^.+)([0-9])(\\-)([0-9])(.+$)','\\2',rawdata[scoregrep]))
  resasc=as.numeric(gsub('(^.+)([0-9])(\\-)([0-9])(.+$)','\\4',rawdata[scoregrep]))
  ### then get the dates
  resdate=alldate[findInterval(scoregrep,dategrep)]
  
  ### but make sure team names are as we want them to be
  resht=gsub(' ','',cleanteam(resht,'premierleague'))
  resat=gsub(' ','',cleanteam(resat,'premierleague'))
  
  resdf=data.frame(date=resdate, ht=resht, at=resat, hsc=reshsc, asc=resasc)
  write.csv(file=paste(DATAPATH,'fixture_result/result',currentseason,'.csv',sep=''),resdf,row.names=F)
}
