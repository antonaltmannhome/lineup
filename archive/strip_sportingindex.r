#### abandoned, the matches were horrible to try to strip, went back to spreadex


pagelabel=c('Match Supremacies','Match Goals')
filelabel=c('sup','tot')
for (k in 1:2) {
	if (k==1) cat('Open',sportingIndexBrowser,', ')
	cat('go to sportingindex, go to\nfootball/premier league/quick filter\nchoose "', pagelabel[k],'"\n search\nthen press enter\n')
	dum=scan(what='',quiet=T,nmax=1)
	fileout=paste(TEMPPATH,'sportingindex',filelabel[k],'.txt',sep='')
	if (file.exists(fileout)) file.remove(fileout)
	initfile()
	insertselectwindow(sportingIndexBrowser)
	selectalltonotepad(file=fileout)
	insertselectwindow('R')
	insertabort()
	runscript()
}
	
for (filein in c('sup','tot')) {
	b=scan(paste(TEMPPATH,'/sportingindex',filein,'.txt',sep=''),'',sep='\n', quiet = TRUE)
	b = tolower(b)

	gameinfo = b[grep('0-0',b)]
	
	
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
