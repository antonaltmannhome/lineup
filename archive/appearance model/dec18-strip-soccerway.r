source('c:/research/lineup/ff_startup.r')
source(paste(USERPATH,'soccerway_data_funct.r',sep=''))
source(paste(USERPATH,'admin_funct.r',sep=''))

seastodo=1617
dinfo=makedaylist(seastodo)
keep=1:length(dinfo$fulldatelist)
	dinfo$fulldatelist=dinfo$fulldatelist[keep]
	dinfo$fulldaylist=dinfo$fulldaylist[keep]

	### so, now grab the info from soccerway
	
	pagelist=paste('http://uk.soccerway.com/matches/',dinfo$fulldaylist,sep='')

	di = which(dinfo$fulldatelist == 20161231)
	
	for (di in 1:length(dinfo$fulldaylist)) {

		b=scanrobust(pagelist[di],'',sep='\n')
		mgreg=grep('a href.+matches.+england/premier-league.+ICID',b)

		if (length(mgreg)==0) {
			cat('No matches on',dinfo$fulldatelist[di],'...\n')
		}

		if (length(mgreg)>0) {

			cat('About to pick up',length(mgreg),'matches on',dinfo$fulldatelist[di],'\n')
			matchpagelist=paste('http://uk.soccerway.com',gsub('(^.+")([^"]+)(".+$)','\\2',b[mgreg]),sep='')
			### also want to grab team and score though
			thisht=tolower(gsub('(^.+title=\")([^"]+)(".+$)','\\2',b[mgreg-6]))
			thisat=tolower(gsub('(^.+title=\")([^"]+)(".+$)','\\2',b[mgreg+7]))
			## but want nice team abbreviations
			thisht=cleanteam(thisht, 'soccerway')
			thisat=cleanteam(thisat, 'soccerway')
			thishgl=gsub(' \\-.+$','',gsub('^ +','',b[mgreg+2]))
			thisagl=gsub('^.+\\- ','',gsub('^ +','',b[mgreg+2]))
			thisdate=rep(dinfo$fulldatelist[di],length(thisht))

			thismkey=paste(thisdate,thisht,sep='')

	
myList=list(NULL)
for (i in 1:length(thismkey)) {
	myList[[i]]=getsidelinedinfo(matchpagelist[i],thismkey[i])
	myList[[i]]$mkey = thismkey[i]
}
sidelinedDF = bind_rows()
