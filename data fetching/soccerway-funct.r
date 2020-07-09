### several massive functions that will be used when combining all the different data sources together, shove em all in here

getwebaddressfordate = function(mydate, resultDF) {
	myday = gsub('([0-9]{4})([0-9]{2})([0-9]{2})', '\\1/\\2/\\3', mydate)
	coverpageaddress = paste0('http://uk.soccerway.com/matches/', myday)
	message('To investigate soccerway web page for ', mydate, ', see ', coverpageaddress)
	covshHtml = scanrobust(coverpageaddress, '', sep = '\n', quiet = TRUE)
	# get rid of lines that are just white space
	allWhiteSpaceIndex = which(nchar(gsub(' ', '', covshHtml)) == 0)
	if (length(allWhiteSpaceIndex) > 0) {
		covshHtml = covshHtml[-allWhiteSpaceIndex]
	}

	## need to do this to check for postponed games
	allMatchIndex = grep('team team-a', covshHtml)
	allMatchLink = covshHtml[grep('team team-a', covshHtml)+6]
	GetMatchVectorFromMatchLink = function(myMatchLink) {
	  splitMatchLink = strsplit(myMatchLink, split = '/')[[1]]
	  matchVector = c(splitMatchLink[c(6, 7)])
	  names(matchVector) = c('country', 'league')
	  return(matchVector)
	}
	matchMatrix = t(sapply(allMatchLink, GetMatchVectorFromMatchLink))
	rownames(matchMatrix) = NULL
	matchDF = as.data.frame(matchMatrix)
	# then we want th long team names (not to be confused with web address team names)
	matchDF$longHTeam = tolower(gsub('[^a-zA-Z]', '', gsub('(^.+title=\")(.+)', '\\2', covshHtml[allMatchIndex + 2])))
	matchDF$longATeam = tolower(gsub('[^a-zA-Z]', '', gsub('(^.+title=\")(.+)', '\\2', covshHtml[allMatchIndex + 12])))
	# let's also tack on the score to that
	matchDF$score = gsub('(^ *)(.+)( *$)', '\\2', covshHtml[allMatchIndex + 7])
	# and of course the web address
	matchDF$address = paste0('http://uk.soccerway.com',
	                              gsub('(.+a href=\")(/matches[^\"]+)(\".+$)', '\\2', allMatchLink))

	# want to whittle down to those that are just premier league and not postponed
	matchDF = matchDF %>%
	  filter(country == 'england' & league == 'premier-league' & !score %in% c('PSTP', 'Postponed'))

	numGameAccordingtoSoccerway = nrow(matchDF)
	if (numGameAccordingtoSoccerway==0) {
		stop('No matches on',mydate,'...\n')
	}

	# also, number of matches for this date HAS to agree with resultDF, so check that
	numGameAccordingToResultDF = with(resultDF, sum(date == mydate & isHome))
	if (numGameAccordingtoSoccerway != numGameAccordingToResultDF) {
	  stop('Aaargh, resultDF says there are ', numGameAccordingToResultDF, ' games on ', mydate, ' but getwebaddressfordate says there are ', nrow(matchDF), '\n')
	}

	cat('About to pick up',nrow(matchDF),'matches on',mydate,'\n')

	# can now reduce to the tidy team names
	matchDF$hteam = cleanteam(matchDF$longHTeam, 'soccerway')
	matchDF$ateam = cleanteam(matchDF$longHTeam, 'soccerway')

	matchDF$key = paste0(mydate, matchDF$hteam)

	return(matchDF[,c('key', 'address')])
}

getappearanceinfofordate = function(mydate, resultDF) {

	addressinfo = getwebaddressfordate(mydate, resultDF)

	myAppearanceList = list(NULL)
	for (i in 1:nrow(addressinfo)) {
		dum = getappearanceinfo(addressinfo$address[i],addressinfo$key[i])
		myAppearanceList[[i]] = dum
		myAppearanceList[[i]]$key = addressinfo$key[i]
		message('Have got info for ', addressinfo$key[i])
	}
	appearanceDF = bind_rows(myAppearanceList)

	return(appearanceDF)
}

coachproblem=function(probpaste) {
	fileout=paste(DATAPATH,'soccerway_saved/playercoachnightmare.dat',sep='')
	if (file.exists(fileout)) {
		gotprobpaste=scan(fileout,'',sep='\n',quiet=T)
		toadd=which(!probpaste %in% gotprobpaste)
	}
	if (!file.exists(fileout)) {
		toadd=1:length(probpaste)
	}
	if (length(toadd)>0) {
		write(file=fileout,probpaste[toadd],append=T)
	}
}

CleanPlayer = function(messyPlayer) {
	# some palyesr have a hyphen at the front of their name, get rid of those
	cleanPlayer = gsub('^-', '', messyPlayer)
	# then replace hyphens with spaces
	cleanPlayer = gsub('-+', ' ', cleanPlayer)

	return(cleanPlayer)
}

CleanPlusMinute = function(myTime) {
	hasPlusSignIndex = grep('\\+', myTime)
	if (length(hasPlusSignIndex) > 0) {
		messyMinute = myTime[hasPlusSignIndex]
		messyLHMinute = as.numeric(gsub('\\+.+$', '', messyMinute))
		messyRHMinute = as.numeric(gsub('^.+\\+', '', messyMinute))
		cleanMinute = as.character(messyLHMinute + messyRHMinute)
		myTime[hasPlusSignIndex] = cleanMinute
	}
	return(myTime)
}

ExtractSoccerwayTeam = function(htmlLine) {
	soccerwayTeam = gsub('(^.+teams/[^/]+/)([^/]+)(/.+$)','\\2',htmlLine)
	return(soccerwayTeam)
}

ExtractSidelinedInfo = function(sidelinedHtml, homeStatus) {
	sidelinedIndex = grep('\"player\"><a href=',sidelinedHtml)
	sidelinedPlayer = gsub('(^.+players/)(.+)(/[0-9]+.+$)', '\\2', sidelinedHtml[sidelinedIndex])
	sidelinedPlayer = CleanPlayer(sidelinedPlayer)
	sidelinedPlayerId = gsub('(^.+players/[^/]+/)([0-9]+)(/.+$)', '\\2', sidelinedHtml[sidelinedIndex])

	injurySuspension = gsub('(^.+icons )(.+)(\">.+$)','\\2',sidelinedHtml[sidelinedIndex+2])
	mySidelinedDF = tibble(playerid = sidelinedPlayerId,
							player = sidelinedPlayer,
							injurySuspension = injurySuspension,
							homeStatus = homeStatus)
	return(mySidelinedDF)
}

RemoveDoubleListedPlayer = function(appearanceDF) {
  appearanceDFUniqued = appearanceDF %>%
    count(playerid, player, homeStatus)

  # rule: if they've got two non-appearance lines, paste together
  # if they've got an appearance line, take that and ignore other one
  appearanceDF$isNumeric = !grepl('[^0-9]', appearanceDF$startTime)
  appearanceDF$toKeep = TRUE
  appearanceDF$isCurrent = FALSE
  shortDupIndex = which(appearanceDFUniqued$n > 1)
  if (length(shortDupIndex) > 0) {
    for (di in 1:length(shortDupIndex)) {
      appearanceDF$isCurrent = appearanceDF$playerid == appearanceDFUniqued$playerid[shortDupIndex[di]]
      if (any(with(appearanceDF, isNumeric[isCurrent]))) {
        # the one in which they player you keepo, the other you ditch
        appearanceDF$toKeep[which(appearanceDF$isCurrent & !appearanceDF$isNumeric)] = FALSE
      }
      if (!any(with(appearanceDF, isNumeric[isCurrent]))) {
        # paste the reasons together and ditch the second one
        appearanceDF$toKeep[which(appearanceDF$isCurrent)[-1]] = FALSE
        appearanceDF$startTime[which(appearanceDF$isCurrent)[1]] = paste(unique(with(appearanceDF, startTime[isCurrent])), collapse = '/')
      }
    }
  }

  appearanceDF = appearanceDF %>%
    filter(toKeep) %>%
    select(-c(isNumeric, toKeep, isCurrent))

  return(appearanceDF)
}

getappearanceinfo = function(webadd, mykey) {
	### firstly check to see if we've already got the filename
	message('webadd = \'', webadd, '\'')
	message('mykey = \'', mykey, '\'')
	filen=paste(DATAPATH,'soccerwayhtml/',mykey,'.html',sep='')

	if (!file.exists(filen)) {
		b1=scanrobust(webadd,'',sep='\n',quiet=T,encoding='UTF-8')
		write(file=filen,b1)
	}

	b1=scan(filen,'',sep='\n',quiet=T,encoding='latin1')
	b1=iconv(b1,from='latin1',to='ASCII//TRANSLIT')
	b1=tolower(b1)

	dum = b1[grep('team-logo',b1)]
	soccerwayHomeTeam = ExtractSoccerwayTeam(dum[1])
	soccerwayAwayTeam = ExtractSoccerwayTeam(dum[2])

	### right, we have a problem with player links changing to coach links - need to override when this happens
	coachIndex=grep('/coaches/',b1)
	probsax=coachIndex[!grepl('coach:',b1[coachIndex-1])]
	if (length(probsax)>0) {
		b1[probsax]=gsub('/coaches/','/players/',b1[probsax])
		### also record it where it happens
		probplayer=gsub('(^.+players/)([^/]+)(/.+$)','\\2',b1[probsax])
		probplayerid=gsub('(^.+players/[^/]+.)([0-9]+)(/.+$)','\\2',b1[probsax])
		probpaste=paste(probplayerid,probplayer,sep='~')
		coachproblem(probpaste)
	}

	linb=b1[(grep('header-label.+lineups',b1)+1):(grep('substitutes',b1)-1)]
	teamb=grep('a href="/teams',linb)
	startline=grep('<a href="/players/.+/[0-9]',linb)
	### occasioanlly that picks up a coach, get rid of them
	dum=grep('coach:',linb[startline-1])
	if (length(dum)>0) startline=startline[-dum]
	startplayer=gsub('(^.+players/)([^/]+)(/.+$)','\\2',linb[startline])
	startplayer = CleanPlayer(startplayer)
	startplayerid=gsub('(^.+players/[^/]+/)([0-9]+)(/.+$)','\\2',linb[startline])
	startplayerhastatus=rep(1:2,rep(11,2))

	subb=b1[(grep('substitutes',b1)+1):(grep('additional info',b1)[1]-1)]
	startline=grep('<a href="/players/.+/[0-9]',subb)
	### but separate into those who were subs to start with, and those who were subbed off
	substatus=rep(3,length(startline))
	substatus[grep('substituted',subb[startline+1])]=1
	substatus[grep('substitute substitute-out',subb[startline])]=2
	subtime=rep(NA,length(startline))
	subtime[substatus==1]=gsub("(^.+> )([0-9\\+]+)('</p>)","\\2",subb[startline[substatus==2]])
	subtime[substatus==2]=gsub("(^.+> )([0-9\\+]+)('</p>)","\\2",subb[startline[substatus==2]])
	subplayer=gsub('(^.+players/)([^/]+)(/.+$)','\\2',subb[startline])
	subplayer = CleanPlayer(subplayer)
	subplayerid=gsub('(^.+players/[^/]+/)([0-9]+)(/.+$)','\\2',subb[startline])
	subplayerhastatus=rep(NA,length(subplayer))
	# but which side did they play for? this should be right hopefully
	dum=grep('<div class=\"container right\">',subb)
	subplayerhastatus[startline<dum]=1
	subplayerhastatus[startline>dum]=2

	## however, we don't want the doubling up of players, so let's convert things to minute on and minute off
	unplayerid = unique(c(startplayerid, subplayerid))
	unplayer=c(startplayer,subplayer)[match(unplayerid, c(startplayerid, subplayerid))]
	startix=which(unplayerid %in% startplayerid)
	mstart=match(unplayerid[startix],startplayerid)
	subonix=which(unplayerid %in% subplayerid[substatus==1] & !unplayerid %in% subplayerid[substatus==2])
	msubon=match(unplayerid[subonix],subplayerid)
	suboffix=which(unplayerid %in% subplayerid[substatus==2])
	msuboff=match(unplayerid[suboffix],subplayerid)
	subonoffix=which(unplayerid %in% subplayerid[substatus==1] & unplayerid %in% subplayerid[substatus==2])
	msubonoffon=which(substatus==1)[match(unplayerid[subonoffix],subplayerid[substatus==1])]
	msubonoffoff=which(substatus==2)[match(unplayerid[subonoffix],subplayerid[substatus==2])]

	starttime=endtime=hastatus=rep('U',length(unplayerid))
	starttime[startix]=0
	starttime[subonix]=subtime[msubon]
	endtime[subonix]=94
	endtime[which(unplayerid %in% startplayerid & !unplayerid %in% subplayerid)]=94
	endtime[suboffix]=subtime[msuboff]
	starttime[subonoffix]=subtime[msubonoffon]
	endtime[subonoffix]=subtime[msubonoffoff]
	starttime = CleanPlusMinute(starttime)
	endtime = CleanPlusMinute(endtime)

	hastatus=c(startplayerhastatus,subplayerhastatus)[match(unplayerid,c(startplayerid,subplayerid))]
	homeStatus = hastatus == 1

	playingDF = tibble(playerid = unplayerid,
							player = unplayer,
							homeStatus = homeStatus,
							startTime = starttime,
							endTime = endtime)

	sidelinedHeaderIndex = grep('sidelined table (right|left)', b1)
	matchOfficialHeaderIndex = grep('match officials', b1)
	if (length(sidelinedHeaderIndex) == 0) {
		message('Getting no sidelined info for this, might be worth checking:')
		sidelinedDF = tibble(player = character(0),
									injurySuspension = character(0),
									homeStatus = logical(0))
	}
	if (length(sidelinedHeaderIndex) == 1) {
		# which team has sidelined data?
		sidelinedHtml = b1[(sidelinedHeaderIndex+1):(matchOfficialHeaderIndex-1)]
		sidelinedTeamHtml = sidelinedHtml[grep('<a href=\"/teams/',sidelinedHtml)]
		sidelinedTeam = ExtractSoccerwayTeam(sidelinedTeamHtml)
		if (sidelinedTeam == soccerwayHomeTeam) homeStatus = TRUE
		if (sidelinedTeam == soccerwayAwayTeam) homeStatus = FALSE
		if (!sidelinedTeam %in% c(soccerwayHomeTeam, soccerwayAwayTeam)) {
			stop('can not find which team sidelined players play for, check\n')
		}
		sidelinedDF = ExtractSidelinedInfo(sidelinedHtml, homeStatus)
	}
	if (length(sidelinedHeaderIndex) == 2) {
		# have to find out what order they've been listed firstly
		homeAwaySidelined = ExtractSoccerwayTeam(b1[sidelinedHeaderIndex+4])
		if (homeAwaySidelined[1] == soccerwayHomeTeam & homeAwaySidelined[2] == soccerwayAwayTeam) {
			homeSidelinedHtml = b1[(sidelinedHeaderIndex[1]+1):(sidelinedHeaderIndex[2]-1)]
			awaySidelinedHtml = b1[(sidelinedHeaderIndex[2]+1):(matchOfficialHeaderIndex-1)]
		}
		if (homeAwaySidelined[1] == soccerwayAwayTeam & homeAwaySidelined[2] == soccerwayHomeTeam) {
			awaySidelinedHtml = b1[(sidelinedHeaderIndex[1]+1):(sidelinedHeaderIndex[2]-1)]
			homeSidelinedHtml = b1[(sidelinedHeaderIndex[2]+1):(matchOfficialHeaderIndex-1)]
		}
		homeSidelinedDF = ExtractSidelinedInfo(homeSidelinedHtml, TRUE)
		awaySidelinedDF = ExtractSidelinedInfo(awaySidelinedHtml, FALSE)
		sidelinedDF = bind_rows(homeSidelinedDF, awaySidelinedDF)
	}

	# but now let's merge the playingDF and sidelineDF
	appearanceDF = bind_rows(playingDF,
							sidelinedDF %>%
							rename(startTime = injurySuspension) %>%
							mutate(endTime = NA))

	appearanceDF = RemoveDoubleListedPlayer(appearanceDF)

	return(appearanceDF)
}

FindDateToDo = function(resultDF) {
	undate = unique(resultDF$date)

	allFile = list.files(paste0(DATAPATH, 'soccerway_saved/'))
	allFile = allFile[grep('appearance-info-[0-9]{8}.csv', allFile)]
	doneDate = gsub('(^.+-)([0-9]{8})(.csv)', '\\2', allFile)

	dateToDo = setdiff(undate, doneDate)

	return(dateToDo)
}
