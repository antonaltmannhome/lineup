
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
		gamadjlist = c(-145, -120, -95, -70, -45, -25, 0)
		#gamadjlist = c(-30,0, 30, 60, 90, 120, 150)
	}
	if (thiscomputer == 'ANTONDESKTOP') {
		gamadjlist = c(-180, -150, -120, -90, -50, -30, 0)
		#gamadjlist = c(-30,0, 30, 60, 90, 120, 150)
	}
	if (is.na(gamadjlist[numgamlist==numgam])) stop('need to fill out numgamlist\n')

	numtournamentlist = c(1, 2, 3, 4, 5, 6, 7, 8)
	if (thiscomputer == 'NOVATECHLAPTOP') {
		tournadjlist = c(0, 0, 0, 10, 30, 52, 75, 98)
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
	linkdf$location[isYCoordIndex] = linkdf$location[isYCoordIndex] +
	  tournadjlist[numtournamentlist==numtournament] - tournadjlist[numtournamentlist==defaultnumtournament] +
	  gamadjlist[numgamlist==numgam] - gamadjlist[numgamlist==defaultnumgam] +
	  noupcomingmatchesfoundadj

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

stripcurrentpage=function(myteam, DIRTOUSE, ahkfile, ishistoric=F, beforeseason = F, isFirstTeam) {

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
	saveDelay = ifelse(isFirstTeam, 3, 1)
  selectalltonotepad(filename[allfiletype=='summary'], deselectpoint=mydeselectpoint, savedelay = saveDelay)

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
			dateinfo=gregexpr('[0-9]{2}-[0-9]{2}-[0-9]{2}',combline)

			numgam=length(dateinfo[[1]])

			# then need to adjust for the number of tournaments the team is in
			tournamentstartline = grep('^Tournament$', b)
			tournamentendline = grep('Total / Average',b)
			#print('Need to remove this line when site is fixed!')
			#tournamentendline = grep('stats_table_clmn_total_avg',b)
			numtournament = (tournamentendline - tournamentstartline - 2) / 2


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

			selectalltonotepad(filename[si], deselectpoint=mydeselectpoint, savedelay = 1)
		}

		insertabort()
		runscript()

	}
}

checkpage=function(myteam, myfiletype, mycolhead) {
	if (myfiletype=='summary') {
		truecolhead="\t\tCM\tKG\tApps\tMins\tGoals\tAssists\tYel\tRed\tSpG\tPS%\tAerialsWon\tMotM\tRating"
	}
	if (myfiletype=='shotzone') {
		truecolhead="\t\tCM\tKG\tApps\tMins\tTotal\tOutOfBox\tSixYardBox\tPenaltyArea\tRating"
	}
	if (myfiletype=='shotsit') {
		truecolhead="\t\tCM\tKG\tApps\tMins\tTotal\tOpenPlay\tCounter\tSetPiece\tPenaltyTaken\tRating"
	}
	if (myfiletype=='shotacc') {
	truecolhead="\t\tCM\tKG\tApps\tMins\tTotal\tOffTarget\tOnPost\tOnTarget\tBlocked\tRating"
	}
	if (myfiletype=='goalsit') {
		truecolhead="\t\tCM\tKG\tApps\tMins\tTotal\tOpenPlay\tCounter\tSetPiece\tPenaltyScored\tOwn\tNormal\tRating"
	}
	if (myfiletype=='keypass') {
		truecolhead="\t\tCM\tKG\tApps\tMins\tTotal\tLong\tShort\tRating"
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

ProcessPage = function(myteam, DIRTOUSE) {
  filename=paste(DIRTOUSE,'/',myteam,'_',allfiletype,'.txt',sep='')
  wsteam=teamdf$wsteam[teamdf$team==myteam]
  iserror=rep(NA,length(allfiletype))
  for (j in 1:length(filename)) {
    b=scan(filename[j],'',sep='\n',quiet=T,encoding='UTF-8')
    pageteam=tolower(gsub(' Top Players','',b[grep('Top Players',b)]))
    if (pageteam!=wsteam) {
      stop('Have picked up data for',pageteam,'when we want data for',wsteam,', please re-fetch this\n')
    }
    if (pageteam==wsteam) {
      sax=grep('^[0-9]{2}, [A-Z\\(\\)]+',b)
      ### first check we've got the correct file - it can go wrong at times:
      iserror[j]=checkpage(myteam,allfiletype[j],gsub(' ','',b[sax[1]-3]))
      # now initialise the player DF
      
      statMatrix = t(sapply(b[sax+1], function(x) strsplit(x, split = ' *\t')[[1]]))
      statMatrix[which(statMatrix == '-')] = '0'
      rownames(statMatrix) = NULL
      statDF = as.data.frame(statMatrix)
      names(statDF) = strsplit(b[sax[1]-3], split = ' *\t')[[1]][-1]
      
      if (allfiletype[j] == 'summary') {
        myTeamPlayerDF = tibble(team = myteam, player = b[sax-1])
        dummainpos = b[sax]
        dummainpos = gsub('(^[0-9]+, )(.+$)', '\\2', dummainpos)
        dummainpos = gsub('([^\\(]+)(\\(.+$))*', '\\1', dummainpos)
        myTeamPlayerDF$mainpos = dummainpos
        
        myTeamPlayerDF[,c('minute', 'goal', 'assist')] = statDF[,c('Mins', 'Goals', 'Assists')]
      }
      if (allfiletype[j]=='shotzone') {
        myTeamPlayerDF[,c('shotoob', 'shot6yd', 'shotib')] = statDF[,c('OutOfBox', 'SixYardBox', 'PenaltyArea')]
      }
      if (allfiletype[j]=='shotsit') {
        myTeamPlayerDF[,c('openplay','counter','setpiece','penaltytaken')] =
          statDF[,c('OpenPlay', 'Counter', 'SetPiece', 'PenaltyTaken')]
      }
      if (allfiletype[j]=='shotacc') {
        myTeamPlayerDF[,c('offt','bar','ont','block')] =
          statDF[,c('OffTarget', 'OnPost', 'OnTarget', 'Blocked')]
      }
      if (allfiletype[j] == 'goalsit') {
        myTeamPlayerDF[,'penaltyscored'] = statDF[,'PenaltyScored']
      }
      if (allfiletype[j]=='keypass') {
        myTeamPlayerDF[,c('longkp','shortkp')] =
          statDF[,c('Long', 'Short')]
      }
    }
  }
  
  ### make player names sensible
  myTeamPlayerDF$player=iconv(tolower(myTeamPlayerDF$player),from='UTF-8',to='ascii//translit')
  myTeamPlayerDF$player=gsub('\'','',myTeamPlayerDF$player)
  myTeamPlayerDF$player=gsub('-',' ',myTeamPlayerDF$player)
  ## check we didn't get the per90 mins rther than total:
  
  statColumn = setdiff(names(myTeamPlayerDF), c('team', 'player', 'mainpos'))
  valueIsInteger = apply(myTeamPlayerDF[statColumn], 2, function(x) all(round(as.numeric(x))==as.numeric(x)))
  allValueAreInteger = all(valueIsInteger)
  if (!allValueAreInteger) {
    cat('For',myteam,', seem to have average, not totals for',allfiletype[j],'\n')
    iserror[j]=1
  }
  
  ### make numeric columns numeric
  for (ci in statColumn) {
    myTeamPlayerDF = myTeamPlayerDF %>%
      mutate(!!ci := as.integer(get(ci)))
  }
 
  return(lst(myTeamPlayerDF, iserror))
}

addmissingteamtosummarydf=function(gotteam, allteam, datetouse) {
	teamtoadd=setdiff(allteam, gotteam)
	previousdate=getpreviousdate(datetouse)
	filein=paste(DATAPATH,previousdate,'/summarydf.csv',sep='')
	previoussummarydf=read.csv(filein)
	missingsummarydf=previoussummarydf %>% filter(team %in% teamtoadd)
	return(missingsummarydf)
}
