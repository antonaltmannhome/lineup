### want to save all results/fixtures for season

makematchresult=function() {
	## so save the meaty bit of this webpage into a text file called 'allresult.txt':
	
	cat('Go to this web page:\n')
	print('http://www.premierleague.com/en-gb/matchday/results.html?paramComp_8=true&view=.dateSeason')
	print('and then save it as allresult.txt')
	
	print('press ENTER when you have done this')
	dum=scan(what='',nmax=1,quiet=T)
	
	b=scan('c:/research/lineup/allresult.txt','',sep='\n')
	
	mline=grep('^[0-9]{2}:[0-9]{2}.+ - .+$',b)
	dateline=grep('[A-Z][a-z]+ [0-9]+ [A-Z][a-z]+ [0-9]{4}',b)
	
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
	
	alldate=strptime(gsub('^[^ ]+ ','',b[dateline]),format='%d %B %Y',tz='GMT')
	### but we want to align with the matches of course
	datesofar=alldate[findInterval(mline,dateline)]
	
	resDF=data.frame(date=datesofar,ht=htplayedsofar,at=atplayedsofar,hsc=hgoalsofar,asc=agoalsofar)
	write.csv(file='c:/research/lineup/allresult.csv',resDF,row.names=F)
}

dicol.lik=function(theta) {
	

rundicol=function() {
	resDF=read.csv(file='c:/research/lineup/allresult.csv')
	