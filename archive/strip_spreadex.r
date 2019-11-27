#### let's turn spreadex data into expected goals for each side

pagelabel=c('supremacy','total')
filelabel=c('sup','tot')
for (k in 1:2) {
	if (k==1) cat('Open',spreadexBrowser,', ')
	cat('go to spreadex',pagelabel[k],'page then press enter\n')
	dum=scan(what='',quiet=T,nmax=1)
	fileout=paste(TEMPPATH,'spreadex',filelabel[k],'.txt',sep='')
	if (file.exists(fileout)) file.remove(fileout)
	initfile()
	insertselectwindow(spreadexBrowser)
	selectalltonotepad(file=fileout)
	insertselectwindow('R')
	insertabort()
	runscript()
}
	
for (filein in c('sup','tot')) {
	b=scan(paste(TEMPPATH,'/spreadex',filein,'.txt',sep=''),'',sep='\n', quiet = TRUE)
	b = tolower(b)
	#b2=b[grep('^premier league$',tolower(b)):grep('long term markets',tolower(b))[1]]
	if (filein=='sup') {
		print('What is the first listed home team in lower case?')
		firsthometeam = scan(what='',quiet=T,nmax=1,sep='\n')
		print('What is the first non-match phrase at the end of the fixture list, in lower case?')
		stopphrase=scan(what='',quiet=T,nmax=1,sep='\n')
	}

	if (spreadexBrowser == 'Firefox') {

		b2=b[grep(paste(firsthometeam, 'v'),b)[1]:grep(stopphrase,b)[1]]
		sax=grep('^ *$',b2)
		if (length(sax)>0) b2=b2[-sax]
	
		### now pick out the teams
		sax=grep(' v ',b2)
		ht=gsub(' v .+$','',b2[sax])
		at=gsub('^.+ v ','',b2[sax])
	
		dum=b2[grep('[0-9]+ mkts',b2)-1]
		lspread=as.numeric(gsub(' \\- .+$','',dum))
		uspread=as.numeric(gsub('^.+ \\- ','',dum))
		if (filein=='sup') {
			### is it an away fav?
			isawfav=grepl('/',b2[sax+3])
			sup=ifelse(isawfav,-1,1)*0.5*(lspread+uspread)
		}
	}
	if (spreadexBrowser == 'Chrome') {
		b2=b[grep('^premier league$',tolower(b))[1]:(grep(tolower(stopphrase),b)[1]-1)]

		gameboundaryline = c(grep(' v ',b2), length(b2))
		gameboundarylinearr = cbind(head(gameboundaryline, -1),tail(gameboundaryline, -1))
		gameinfo = apply(gameboundarylinearr,1,function(x) paste(b2[x[1]:(x[2]-1)],collapse='~'))
		gameinfolist = lapply(gameinfo, function(x) strsplit(x, split = '~')[[1]])
		getteam = function(mygameinfo) {
			teamindex = grep(' v ', mygameinfo)
			ht = gsub(' v .+$', '', mygameinfo[teamindex])
			at = gsub('^.+ v ', '', mygameinfo[teamindex])
			return(c(ht, at))
		}
		getspread = function(mygameinfo) {
			spreadindex = grep('^\\-*[0-9]+\\.*[0-9]* \\- [0-9]\\.*[0-9]*',mygameinfo)
			lspread = as.numeric(gsub('(^\\-*[0-9]\\.*[0-9]*)( .+$)','\\1',mygameinfo[spreadindex]))
			uspread = as.numeric(gsub('(^\\-*[0-9]\\.*[0-9]* \\- )([0-9]\\.*[0-9]*)(.*$)','\\2',mygameinfo[spreadindex]))
			return(c(lspread, uspread))
		}
		getawayfav = function(mygameinfo) {
			isawayfav = any(grepl('^[a-zA-Z\\.]+/[a-zA-Z\\.]+$', mygameinfo))
			return(isawayfav)
		}
		teamarr = t(sapply(gameinfolist, getteam))
		spreadarr = t(sapply(gameinfolist, getspread))
		if (filein == 'sup') {
			isawayfav = sapply(gameinfolist, getawayfav)
			supgamearr = tibble(ht = teamarr[,1],
							at = teamarr[,2],
							suplspread = spreadarr[,1],
							supuspread = spreadarr[,2],
							isawayfav = isawayfav)
		}
		if (filein == 'tot') {
			totgamearr = tibble(ht = teamarr[,1],
							at = teamarr[,2],
							totlspread = spreadarr[,1],
							totuspread = spreadarr[,2])
		}
	}
}

gamearr = left_join(supgamearr, totgamearr, by = c('ht', 'at'))
gamearr = gamearr %>%
			mutate(sup = 0.5*(suplspread + supuspread),
					sup = if_else(isawayfav, - sup, sup),
					tot = 0.5*(totlspread + totuspread))

### but we want to have expected goals scored/conceded by each team:
team=c(gamearr$ht, gamearr$at)
egoal=econc=rep(NA,length(team))
sax1=1:(length(team)/2)
sax2=(length(team)/2+1):length(team)
egoal[sax1]=0.5*(gamearr$tot[sax1] + gamearr$sup[sax1])
egoal[sax2]=0.5*(gamearr$tot[sax1] - gamearr$sup[sax1])
econc[sax1]=0.5*(gamearr$tot[sax1] - gamearr$sup[sax1])
econc[sax2]=0.5*(gamearr$tot[sax1] + gamearr$sup[sax1])

fileout=paste(DATAPATH,'spreadex_saved/spreadex','_',numericdate(),'.dat',sep='')
write.csv(file=fileout,cbind(team,egoal,econc),row.names=F)

### then list in order of supremacy
print(cbind(team,egoal,econc)[order(econc-egoal),])

### useful to know what the latest date of the matches that these apply to is
cat('What is the date of the final match included in this group of games (yyyymmdd)?\n')
latestspreadexdate=scan(nmax=1, quiet=T)
# write that to disk
write(file=paste(DATAPATH,'spreadex_saved/latestspreadexdate.dat',sep=''),latestspreadexdate)
