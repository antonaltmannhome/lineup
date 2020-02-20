
gotocurrentpage=function(myteam, thiscomputer) {

	### use this function if you want to set a loop going over a set of countries, though only works for current season
	teamdf=read.csv(paste(DATAPATH,'teampage.csv',sep=''))

	initfile()
	### firstly navigate to main page of team
	mypage=paste('http://www.whoscored.com/Teams/',with(teamdf,pageno[match(myteam,team)]),sep='')
	gotowebadd(mypage,waittime=teamwaittime,browserchoice=browserToUse,thiscomputer = thiscomputer)

	insertabort()
	runscript()
}

getlocationinfo=function(ishistoric=F) {
	print('Please go tthe whoscored page, open the window spy, press PageDown, then DOWN arrow four times, and get y-location of \'detailed\' tab\n')
	detailedtabylocation=scan(quiet=T,nmax=1)
	print('And now go back to top of page!')
	if (!ishistoric) {
		print('And how many games are there on the page?')
		numgam=scan(quiet=T,nmax=1)
		print('And how many tournaments are listed on the page?')
		numtournament=scan(quiet=T,nmax=1)
	}
	if (ishistoric) {
		numgam=NULL
		numtournament=NULL
	}
	print('Does it say \'No upcoming matches found?\n')
	dum = scan(what = '', quiet = TRUE, nmax = 1)
	noupcomingmatchesfound = (dum == 'y')

	# at this point we can see if whoscored has adjusted something. is the detailed tab where we would expect?


	return(list(detailedtabylocation=detailedtabylocation,
				numgam=numgam,
				numtournament=numtournament,
				noupcomingmatchesfound=noupcomingmatchesfound))
}

adjustlinkdf=function(linkdf, numgam, numtournament, noupcomingmatchesfound, thiscomputer) {
	### need to tweak y position of everything based on number of games and current position of things
	numgamlist=c(2,3, 4, 5, 6, 7, 8)
	if (!numgam %in% numgamlist) {
		message('Have not got an adjustment for this number of games')
		message('You need to add it to whoscored_funct. but remember to take into account the number of tournaments too')
		message('Then add it to the adjustlinkdf function')
		stop('Exiting now\n')
	}
	if (thiscomputer == 'NOVATECHLAPTOP') {
		gamadjlist = c(-180, -150, -120, -90, -60, -30, 0)
		#gamadjlist = c(-30,0, 30, 60, 90, 120, 150)
	}
	if (thiscomputer == 'ANTONDESKTOP') {
		gamadjlist = c(-180, -150, -120, -90, -50, -30, 0)
		#gamadjlist = c(-30,0, 30, 60, 90, 120, 150)
	}
	if (is.na(gamadjlist[numgamlist==numgam])) stop('need to fill out numgamlist\n')

	numtournamentlist = c(1, 2, 3, 4, 5, 6, 7, 8)
	if (thiscomputer == 'NOVATECHLAPTOP') {
		tournadjlist = c(0, 0, 0, 10, 30, 55, 82, 100)
	}
	if (thiscomputer == 'ANTONDESKTOP') {
		tournadjlist = c(0, 0, 0, 10, 25, 55, 85)
	}
	if (is.na(tournadjlist[numtournamentlist==numtournament])) stop('need to fill out numtournamentlist\n')

	if (!noupcomingmatchesfound) {
		noupcomingmatchesfoundadj = 0
	}
	if (noupcomingmatchesfound) {
		noupcomingmatchesfoundadj = 30
	}

	isYCoordIndex = grep('\\_Y$', linkdf$scriptlabel)
	### final step, move everything to account for number of games, ie is it differnt from number in locationinfo
	linkdf$location[isYCoordIndex] = linkdf$location[isYCoordIndex] + gamadjlist[numgamlist==numgam] - gamadjlist[numgamlist==defaultnumgam] + noupcomingmatchesfoundadj

	### final step, move everything to account for number of games, ie is it differnt from number in locationinfo
	linkdf$location[isYCoordIndex] = linkdf$location[isYCoordIndex] + tournadjlist[numtournamentlist==numtournament] - tournadjlist[numtournamentlist==defaultnumtournament]

	return(linkdf)
}

replacescriptwithlocation = function(comm, linkdf) {
	for (j in 1:nrow(linkdf)) {
		# want to replace whole word, not partial word, so put space around term
		subterm = paste('\\$',linkdf$scriptlabel[j],'\\$',sep='')
		if (any(grepl(subterm, comm))) {
			comm = gsub(subterm, linkdf$location[j], comm)
		}
	}
	return(comm)
}

removefourdownpressesfordesktop = function(comm) {
	### on desktop, don't need to press down arrow four times, so don't
	fourdownindex = which(comm == 'send {Down 4}')
	if (length(fourdownindex) > 0) {
		comm = comm[-c(fourdownindex, fourdownindex + 1)]
	}
	return(comm)
}

stripcurrentpage=function(myteam, DIRTOUSE, ahkfile, ishistoric=F, beforeseason = F) {

	filename=paste(DIRTOUSE,'/',myteam,'_',allfiletype,'.txt',sep='')
	for (j in 1:length(filename)) if (file.exists(filename[j])) file.remove(filename[j])

	### get hold of links locations  - they need reseting each time
	if (!ishistoric) {
		linkdf=read.csv(paste(USERPATH,'whoscored/location_',browserToUse,'_',thiscomputer,'.csv',sep=''))
		mydeselectpoint = linkdf %>%
							filter(grepl('deselect-all',scriptlabel)) %>%
							pull(location) # don't want to type that too often

	}
	if (ishistoric) {
		linkdf=read.csv(paste(USERPATH,'whoscored/historic_location_',browserToUse,'.csv',sep=''))
		mydeselectpoint=linkdf[linkdf$type=='deselect-all',c('xloc','yloc')]
	}

	### you should be on summary page for team in question - now save to notepad++
	initfile(ahkfile)
	insertselectwindow(browserToUse)
	selectalltonotepad(filename[allfiletype=='summary'], deselectpoint=mydeselectpoint)

	insertabort()
	runscript()

	if (!beforeseason) {

		### now scan that in and check how many fixtures lines there are
		if (!ishistoric) {
			b=scan(file=filename[allfiletype=='summary'],what='',quiet=T,sep='\n')

			curwsteam=teamdf$wsteam[teamdf$team==myteam]

			fixtline=grep(paste(curwsteam,'fixtures'),tolower(b))
			squadline=grep(paste(curwsteam,'squad'),tolower(b))

			combline=paste(b[(fixtline+1):(squadline-1)],collapse='')
			combline=gsub('\\\t',' ',combline)
			### number of dates in that is a good guide to how many fixtures there are
			dateinfo=gregexpr('[0-9]{2}-[0-9]{2}-[0-9]{4}',combline)

			numgam=length(dateinfo[[1]])

			# then need to adjust for the number of tournaments the team is in
			tournamentstartline = grep('Tournament \tApps\tGoals\tShots pg',b)
			tournamentendline = grep('Total / Average',b)
			#print('Need to remove this line when site is fixed!')
			#tournamentendline = grep('stats_table_clmn_total_avg',b)
			numtournament = tournamentendline - tournamentstartline - 1

			noupcomingmatchesfound = any(grepl('No upcoming matches found',b))

			### now adjust depending on number of games	and tournaments entered
			linkdf=adjustlinkdf(linkdf,
								numgam, numtournament, noupcomingmatchesfound,
								thiscomputer)

		}

		if (ishistoric) {
			### linkdf should be spot on
		}

		initfile()

		for (si in 2:length(allfiletype)) {

			scriptfilename = paste('whoscored/', allfiletype[si], '_script.ahk', sep = '')
			comm = scan(file = scriptfilename, what = '', quiet = TRUE, sep = '\n')
			comm = replacescriptwithlocation(comm, linkdf)
			if (thiscomputer == 'ANTONDESKTOP') {
				comm = removefourdownpressesfordesktop(comm)
			}

			write(file=ahkfile,comm,append=T)

			randompausecode = paste('sleep', 1000*sample(1:4, 1))
			write(file=ahkfile, randompausecode, append=T)

			selectalltonotepad(filename[si], deselectpoint=mydeselectpoint)
		}

		insertabort()
		runscript()

	}
}

checkpage=function(myteam, myfiletype, mycolhead) {
	if (myfiletype=='summary') {
		truecolhead="R\t\tPlayer\tCM\tKG\tApps\tMins\tGoals\tAssists\tYel\tRed\tSpG\tPS%\tAerialsWon\tMotM\tRating"
	}
	if (myfiletype=='shotzone') {
		truecolhead="R\t\tPlayer\tCM\tKG\tApps\tMins\tTotal\tOutOfBox\tSixYardBox\tPenaltyArea\tRating"
	}
	if (myfiletype=='shotsit') {
		truecolhead="R\t\tPlayer\tCM\tKG\tApps\tMins\tTotal\tOpenPlay\tCounter\tSetPiece\tPenaltyTaken\tRating"
	}
	if (myfiletype=='shotacc') {
	truecolhead="R\t\tPlayer\tCM\tKG\tApps\tMins\tTotal\tOffTarget\tOnPost\tOnTarget\tBlocked\tRating"
	}
	if (myfiletype=='goalsit') {
		truecolhead="R\t\tPlayer\tCM\tKG\tApps\tMins\tTotal\tOpenPlay\tCounter\tSetPiece\tPenaltyScored\tOwn\tNormal\tRating"
	}
	if (myfiletype=='keypass') {
		truecolhead="R\t\tPlayer\tCM\tKG\tApps\tMins\tTotal\tLong\tShort\tRating"
	}
	#print('Need to remove this line when site is fixed!')
	#truecolhead = gsub('KG', 'stats_table_clmn_weight', truecolhead)
	if (mycolhead==truecolhead) {
		#cat(myfiletype,'for',myteam,'is ok!\n')
		return(0)
	}
	if (!mycolhead==truecolhead) {
		cat('Have an error with',myteam,', file:',myfiletype,'\n')
		return(1)
	}
}

checkteam=function(myteam,DIRTOUSE,ishistoric=F,beforeseason) {
	if (!beforeseason) {
		filename=paste(DIRTOUSE,'/',myteam,'_',allfiletype,'.txt',sep='')
	}
	if (beforeseason) {
		filename=paste(DIRTOUSE,'/',myteam,'_summary.txt',sep='')
	}
	wsteam=teamdf$wsteam[teamdf$team==myteam]
	iserror=rep(NA,length(allfiletype))
	for (j in 1:length(filename)) {
		b=scan(filename[j],'',sep='\n',quiet=T,encoding='UTF-8')
		### have we got the correct team firstly?
		if (!ishistoric) {
			pageteam=tolower(gsub(' Top Players','',b[grep('Top Players',b)]))
		}
		if (ishistoric) {
			pageteam=tolower(gsub(' Squad Archive','',b[grep('Squad Archive',b)]))
		}
		if (pageteam!=wsteam) {
			cat('Have picked up data for',pageteam,'when we want data for',wsteam,', recording error\n')
			iserror[1:length(allfiletype)]=1
		}
		if (pageteam==wsteam) {
			if (!ishistoric) sax=grep('^[0-9]{2}, [A-Z\\(\\)]+',b)
			if (ishistoric) sax=grep('^[A-Z][A-Za-z ]+, [0-9]{2}, [A-Z\\(\\)]+',b)
			### first check we've got the correct file - it can go wrong at times:
			iserror[j]=checkpage(myteam,allfiletype[j],gsub(' ','',b[sax[1]-3]))
			tabdat1=t(sapply(b[sax],function(x) gsub(' ','',strsplit(x,split='\t')[[1]])))
			rownames(tabdat1)=b[sax-1]
			### let's get the first listed position and use that
			dum=as.character(gsub('(^.+, )([^\\(]+)(.*$)','\\2',tabdat1[,1]))
			### what do we want to keep? depends on the file
			if (allfiletype[j]=='summary') {
				tabdat=cbind(rep(myteam,dim(tabdat1)[1]),rownames(tabdat1),dum)
				colnames(tabdat)=c('team','player','mainpos')
			}
			if (allfiletype[j]=='summary') {
				keep=c(5,6,7)
				colname=c('minute','goal','assist')
			}
			if (allfiletype[j]=='shotzone') {
				keep=7:9
				colname=c('shotoob','shot6yd','shotib')
			}
			if (allfiletype[j]=='shotsit') {
				keep=7:10
				colname=c('openplay','counter','setpiece','penaltytaken')
			}
			if (allfiletype[j]=='shotacc') {
				keep=7:10
				colname=c('offt','bar','ont','block')
			}
			if (allfiletype[j]=='keypass') {
				keep=c(7,8)
				colname=c('longkp','shortkp')
			}
			tabdat2=tabdat1[,keep]
			colnames(tabdat2)=colname
			tabdat2[tabdat2=='-']=0
			if (!all(apply(tabdat2,2,function(x) all(round(as.numeric(x))==as.numeric(x))))) {
				cat('For',myteam,', seem to have average, not totals for',allfiletype[j],'\n')
				iserror[j]=1
			}
			tabdat=cbind(tabdat,tabdat2)
		}
	}
	return(iserror)
}

processpage=function(myteam,DIRTOUSE,ishistoric=F,beforeseason) {
	cat('About to strip',myteam,'data...\n')
	if (beforeseason) {
		filename=paste(DIRTOUSE,'/',myteam,'_summary.txt',sep='')
	}
	if (!beforeseason) {
		filename=paste(DIRTOUSE,'/',myteam,'_',allfiletype,'.txt',sep='')
	}
	mywsteam=teamdf[which(teamdf$team==myteam),'wsteam']
	for (j in 1:length(filename)) {
		b=scan(filename[j],'',sep='\n',quiet=T,encoding='UTF-8')
		### have we got the correct team firstly?
		if (!ishistoric) pageteam=tolower(gsub(' Top Players','',b[grep('Top Players',b)]))
		if (ishistoric) pageteam=tolower(gsub(' Squad Archive','',b[grep('Squad Archive',b)]))
		if (pageteam!=mywsteam) {
			cat('Have picked up data for',pageteam,'when we want data for',mywsteam,', recording error\n')
		}
		if (pageteam==mywsteam) {
			if (ishistoric) {
				### for some reason is puts the team name by the players stats, get rid
				sax=grep('^[A-z ]+, [0-9]{2}, [A-Z\\(\\)]+',b)
				b[sax]=gsub('^[A-z ]+, ','',b[sax])
			}
			sax=grep('^[0-9]{2}, [A-Z\\(\\)]+',b)
			tabdat1=t(sapply(b[sax],function(x) gsub(' ','',strsplit(x,split='\t')[[1]])))
			rownames(tabdat1)=b[sax-1]
			### let's get the first listed position and use that
			dum=as.character(gsub('(^.+, *)([^\\(]+)(.*$)','\\2',tabdat1[,1]))
			### what do we want to keep? depends on the file
			if (allfiletype[j]=='summary') {
				tabdat=cbind(rep(myteam,dim(tabdat1)[1]),rownames(tabdat1),dum)
				colnames(tabdat)=c('team','player','mainpos')
			}
			if (allfiletype[j]=='summary') {
				keep=c(5,6,7)
				colname=c('minute','goal','assist')
			}
			if (allfiletype[j]=='shotzone') {
				keep=7:9
				colname=c('shotoob','shot6yd','shotib')
			}
			if (allfiletype[j]=='shotsit') {
				keep=7:10
				colname=c('openplay','counter','setpiece','penaltytaken')
			}
			if (allfiletype[j]=='shotacc') {
				keep=7:10
				colname=c('offt','bar','ont','block')
			}
			if (allfiletype[j]=='goalsit') {
				keep=10
				colname=c('penaltyscored')
			}
			if (allfiletype[j]=='keypass') {
				keep=c(7,8)
				colname=c('longkp','shortkp')
			}
			tabdat2=tabdat1[,keep,drop=F]
			colnames(tabdat2)=colname
			tabdat2[tabdat2=='-']=0
			if (!all(apply(tabdat2,2,function(x) all(round(as.numeric(x))==as.numeric(x))))) {
				stop('Seem to have average, not totals for',allfiletype[j],'\n')
			}
			tabdat=cbind(tabdat,tabdat2)
		}
	}
	tabdat=as.data.frame(tabdat)
	### make player names sensible
	tabdat$player=iconv(tolower(tabdat$player),from='UTF-8',to='ascii//translit')
	tabdat$player=gsub('\'','',tabdat$player)
	tabdat$player=gsub('-',' ',tabdat$player)
	### make numeric columns numeric
	numcol=setdiff(names(tabdat),c('team','player','mainpos'))
	tabdat[,numcol]=apply(tabdat[,numcol],2,function(x) as.numeric(x))
	rownames(tabdat)=NULL
	return(as.data.frame(tabdat))
}

addmissingteamtosummarydf=function(gotteam, allteam, datetouse) {
	teamtoadd=setdiff(allteam, gotteam)
	previousdate=getpreviousdate(datetouse)
	filein=paste(DATAPATH,previousdate,'/summarydf.csv',sep='')
	previoussummarydf=read.csv(filein)
	missingsummarydf=previoussummarydf %>% filter(team %in% teamtoadd)
	return(missingsummarydf)
}

combineteamfile=function(DIRTOUSE, datetouse, beforeseason=FALSE,ishistoric=FALSE) {
	currentfile=list.files(DIRTOUSE)
	currentsummaryfile=currentfile[grep('^[a-z]+_summary.txt',currentfile)]
	currentteam=gsub('\\_summary.txt','',currentsummaryfile)
	combinedteamsummarylist=NULL
	for (ti in 1:length(currentteam)) {
		currentteamsummary = processpage(currentteam[ti],DIRTOUSE,ishistoric=ishistoric,beforeseason=beforeseason)
		combinedteamsummarylist[[ti]] = currentteamsummary
	}
	combinedteamsummary=do.call(rbind,combinedteamsummarylist)
	fileout=paste0(DATAPATH, 'summarised_whoscored_data/combined_data_', datetouse, '.csv')
	write.csv(file=fileout, combinedteamsummary, row.names=FALSE)
	cat('Have created',fileout,'...\n')
}
