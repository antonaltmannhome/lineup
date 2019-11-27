
getffplayerpricedf=function() {
	# NB obsolete now, delete this once it's under git
	stop('This function is obsolete\n')
	rawdata=scan('https://fantasy.premierleague.com/player-list/','',sep='\n',quiet=T,encoding='UTF-8')
	rawdata=tolower(rawdata)
	### get positional markers
	longallposition=c('goalkeepers','defenders','midfielders','forwards')
	allposition=c('g','d','m','f')
	positiongrep=rep(NA,length(allposition))
	for (j in 1:length(allposition)) positiongrep[j]=grep(longallposition[j],rawdata)

	pricegrep=grep('<td>.+[0-9]+\\.[0-9]',rawdata)

	price=as.numeric(gsub('(^[^0-9]+)([0-9]+\\.[0-9])(.+$)','\\2',rawdata[pricegrep]))
	team=gsub(' ','',gsub('(^.+>)([^<]+)(<.+$)','\\2',rawdata[pricegrep-2]))
	team=cleanteam(team,'premierleague')
	player=gsub('(^.+>)([^<]+)(<.+$)','\\2',rawdata[pricegrep-3])
	player=iconv(player,from='UTF-8',to='ascii//translit')
	player = gsub('\\?', 'ss', player)
	player=gsub('\'','',player)
	player=gsub('-',' ',player)
	position=allposition[findInterval(pricegrep,positiongrep)]
	currentpoint = gsub('(^.+>)([^<]+)(<.+$)','\\2',rawdata[pricegrep-1])

	ffplayerpricedf=tibble(ffteam=team,ffplayer=player,price=price,ffposition=position, ffpoint = currentpoint)

	ffplayerpricedf = matchffnametoplayerid(ffplayerpricedf)

	### but that leaves us with some horrible redundant columns
	ffplayerpricedf = ffplayerpricedf %>%
						select(-c(teamsurname, playeridmap)) %>%
						select(ffteam, ffplayer, team, player, everything())

	return(ffplayerpricedf)
}

updateplayeridcsv = function(gbgdf) {
	playeridfilename = paste0(DATAPATH, 'playerid.csv')
	existingplayerdf = read.csv(playeridfilename, as.is = TRUE)

	latestplayerinfo = gbgdf %>%
						group_by(player) %>%
						filter(date == max(date)) %>%
						ungroup()

	playerdf = latestplayerinfo %>%
					select(player, team) %>%
					dplyr::rename(whoscoredname = player,
									whoscoredlatestteam = team) %>%
					mutate(probablelatestteam = whoscoredlatestteam,
							playerid = whoscoredname) %>% # NB not sure what point of playerid is atm
					arrange(probablelatestteam)

	# but keep the good stuff from existingplayerdf
	playerdf = lazy_left_join(playerdf,
								existingplayerdf,
								c('probablelatestteam', 'playerid'),
								c('ffuseswholename', 'adjustedwhoscoredname'))

	# but by default, the ffuseswholename is FALSE and adjustedwhoscoredname is none
	playerdf = mutate_cond(playerdf, is.na(ffuseswholename), ffuseswholename = FALSE)
	playerdf = mutate_cond(playerdf, is.na(adjustedwhoscoredname), adjustedwhoscoredname = 'none')

	write.csv(x = playerdf, file = playeridfilename, row.names = FALSE)
}

matchffnametoplayerid = function(ffplayerpricedf) {

	### awesome, only problem now is matching up player names to whoscored of course
	ffplayerpricedf$teamsurname=with(ffplayerpricedf,paste(ffteam,ffplayer))

	playeridfilename = paste0(DATAPATH, 'playerid.csv')

	playerdf = read.csv(playeridfilename, as.is = TRUE)

	### only need to match to current season of summarydf of course
	### so let's get in the whoscored names
	playerdf = playerdf %>%
				mutate(surname = case_when(
					adjustedwhoscoredname == 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',whoscoredname),
					#adjustedwhoscoredname != 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',adjustedwhoscoredname),
					#adjustedwhoscoredname != 'none' & ffuseswholename ~ adjustedwhoscoredname,
					adjustedwhoscoredname == 'none' & ffuseswholename ~ whoscoredname,
					adjustedwhoscoredname != 'none' ~ adjustedwhoscoredname))
	playerdf$teamsurname=with(playerdf,paste(probablelatestteam,surname))
	#playerdf$teamplayer=with(playerdf,paste(whoscoredlatestteam,player))
	#playerdf$seasonteamplayer=with(playerdf,paste(season,whoscoredlatestteam,player))
	### now try to match, mark exceptions
	ffplayerpricedf$playeridmap=match(ffplayerpricedf$teamsurname, playerdf$teamsurname)

	onestomatchup = which(with(ffplayerpricedf, ffpoint > 0 & is.na(playeridmap)))

	if (length(onestomatchup) > 0) {
		message('Warning: there are: ', length(onestomatchup), ' players who have scored points but are not matched')

		print(ffplayerpricedf[onestomatchup,])
		message('rerun updateplayeridcsv(gbgdf) in order to upate playerid.csv, then rerun model-startup, fill in playerid.csv after that if necessary')
	}

	### then of the ones that are matched, we ought to port across the playerid, but we haven't got that anywhere yet, so for now rely on whoscored player name being unique
	ffplayerpricedf$player = playerdf$whoscoredname[ffplayerpricedf$playeridmap]
	ffplayerpricedf$team = playerdf$probablelatestteam[ffplayerpricedf$playeridmap]

	return(ffplayerpricedf)
}

getcurrentsquad = function() {
	mostrecentdir = getmostrecentdir('whoscored')
	combineddatafile=paste(DATAPATH,mostrecentdir,'/combined_data.csv',sep='')
	combineddata = read.csv(combineddatafile, as.is = T)
	currentsquad = combineddata %>% select(team,player,mainpos)
	return(currentsquad)
}

makegbgdf = function(resultdf) {
	alldate = getalldate()

	### scan them all in
	tempbigdf=NULL
	for (di in 1:length(alldate)) {
		combinedteamfile=paste(DATAPATH,alldate[di],'/combined_data.csv',sep='')
		tempbigdf[[di]]=read.csv(combinedteamfile)
		tempbigdf[[di]]$filedate = alldate[di]
	}
	combineddata=do.call(rbind,tempbigdf)

	# this is horrible. sometimes i update on the same day after all matches are finished, sometimes early in the day before matches happen. so you can't compare the date of the result to the date of the file to know which happened first
	# but we do know that if you do an update before e.g lunchtime, then that must be before any matches take place that day. so we can effectively move such times back to 23:59 of the day before
	# if you do an update after e.g. 20:00 at night then of course that's after the last match has finished.
	# but what if you do an update at e.g 13:00? that's certainly before any matches that day.
	# i think there must be a grey area e.g 17:00 where that could be after the game that day has finished or before one has started. and we don't have times of days.
	# although having said that, i'd say it's highly unlikely you'd update for that day any time before 20:00. if you do an update after that, it's surely for games that have happened that day (or there are no games so we don't care). so yes, i think 20:00 is reliable.
	# before 20:00 => no games have yet happened that day, so move to 23:59 of day before.
	# after 20:00 => any games have finished that day. so move to 23:59 of same day.
	# then assign e.g. 17:00 as the time all games took place
	# ignore all of above, for the older games, the file info is meanignless. but surely the number of games on file will equal the number of games in results. check and stop if it doesn't

	combineddata$matchdate=NA
	teamlist=unique(combineddata$team)
	for (ti in 1:length(teamlist)) {
		sax=which(combineddata$team==teamlist[ti])
		currentteamfiledate=unique(combineddata$filedate[sax])
		currentteamresultdate=with(resultdf,sort(date[team==teamlist[ti]]))

		if (length(currentteamfiledate) != length(currentteamresultdate)) {
			message('For ', teamlist[ti],', I have ', length(currentteamresultdate), ' matches in resultdf but ', length(currentteamfiledate), ' matches in the whoscored data')
			message('Cannot match date to matches, check what is happening in makegbgdf, in player_funct.r')
			stop()
		}

		combineddata$matchdate[sax]=currentteamresultdate[match(combineddata$filedate[sax],currentteamfiledate)]
	}
	seasoninfo = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''))
	combineddata$season=with(seasoninfo[seasoninfo$havegbg,],
							season[findInterval(combineddata$matchdate,start,end)])

	### then, each quantity is just current minus previous
	matchstatcol = setdiff(names(combineddata), c('team','player','mainpos','filedate','matchdate'))

	gbgdf = combineddata

	# now fix broken names
	gbgdf = fixplayername(gbgdf)

	gbgdf = gbgdf %>%
					group_by(season,team,player) %>%
					arrange(matchdate) %>%
					mutate(minute = c(minute[1], diff(minute)),
						goal = c(goal[1], diff(goal)),
						assist = c(assist[1], diff(assist)),
						shotoob = c(shotoob[1], diff(shotoob)),
						shot6yd = c(shot6yd[1], diff(shot6yd)),
						shotib = c(shotib[1], diff(shotib)),
						openplay = c(openplay[1], diff(openplay)),
						counter = c(counter[1], diff(counter)),
						setpiece = c(setpiece[1], diff(setpiece)),
						penaltytaken = c(penaltytaken[1], diff(penaltytaken)),
						offt = c(offt[1], diff(offt)),
						bar = c(bar[1], diff(bar)),
						ont = c(ont[1], diff(ont)),
						block = c(block[1], diff(block)),
						penaltyscored = c(penaltyscored[1], diff(penaltyscored)),
						longkp = c(longkp[1], diff(longkp)),
						shortkp = c(shortkp[1], diff(shortkp)))

	write.csv(file=paste(DATAPATH,'gamebygame_data.csv',sep=''),gbgdf,row.names=F)
}

alignactiveplayer = function(gbgdf) {
	alldate = getalldate()

	### scan them all in
	gbgdf$active = NA
	for (di in 1:length(alldate)) {
		activeteamfile=paste(DATAPATH,alldate[di],'/activeplayer.csv',sep='')
		if (file.exists(activeteamfile)) {
			myactiveplayer=read.csv(activeteamfile) %>%
								select(team, player, active)
			### now matchup to all matches that took place after this date
			afternowindex = with(gbgdf, which(date > alldate[di]))
			gbgdftomyactiveplayermap = match(with(gbgdf[afternowindex,], paste(team, player)),
												with(myactiveplayer, paste(team, player)))
			gbgdf$active[afternowindex] = myactiveplayer$active[gbgdftomyactiveplayermap]
		}
	}

	return(gbgdf)
}

getgbgdf = function(resultdf) {
	gbgdf = read.csv(paste(DATAPATH,'gamebygame_data.csv',sep=''), as.is = TRUE)
	### let's join the gameweek file to it
	seasoninfo = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''))
	gbgdf$season=NA
	for (si in 1:nrow(seasoninfo)) {
		if (seasoninfo$havegbg[si]) {
			sax=with(gbgdf, which(between(matchdate,seasoninfo$start[si],seasoninfo$end[si])))
			gbgdf$season[sax] = seasoninfo$season[si]
			### now get in gameweek data
			if (FALSE) {
			if (length(sax)>0) {
				gwdeadlinefile = paste(DATAPATH,'gameweek_deadline_',seasoninfo$season[si],'.csv',sep='')
				gwdeadline = read.csv(gwdeadlinefile)
				gbgdf$gameweek[sax]=gwdeadline$gameweek[findInterval(gbgdf$matchdate[sax],gwdeadline$deadline)]
			}
			}
		}
	}

	gbgdf = gbgdf %>%
			dplyr::rename(date = matchdate) %>%
			left_join(resultdf %>%
						select(date, team, teamgamenumber),
						by = c('date', 'team')
						)
	gbgdf = gbgdf %>%
			select(season, date, teamgamenumber, everything())

	propertitle = c('AM','D','DMC','FW','GK','M')
	gbgdf = gbgdf %>%
			mutate(mainpos = ifelse(mainpos %in% propertitle,
										mainpos,
										case_when(.$mainpos == 'Goalkeeper' ~ 'GK',
													.$mainpos == 'Defender' ~ 'D',
													.$mainpos == 'Forward' ~ 'FW',
													.$mainpos == 'Midfielder' ~ 'M'))
													)
	### due to occasional retrospective updates on whoscored, can get negative quantitaties, just set these to zero
	numericColumn = c('minute', 'goal', 'assist', 'shotoob', 'shot6yd', 'shotib', 'openplay', 'counter', 'setpiece', 'penaltytaken', 'offt', 'bar', 'ont', 'block', 'penaltyscored', 'longkp', 'shortkp')
	for (myColumn in numericColumn) {
		gbgdf[,myColumn] = pmax(gbgdf[,myColumn], 0)
	}
	# another condition, if you've played 0 minutes but have a total of anything > 0, set your minutes to 1
	numericColumn2 = setdiff(numericColumn, 'minute')
	didSomething = apply(gbgdf[,numericColumn2] > 0, 1, function(x) any(x))
	overrideZeroMinute = which(gbgdf$minute == 0 & didSomething)
	gbgdf$minute[overrideZeroMinute] = 1

	# however we also want data about goalkeeper saves, which is fiddly
	# although process is similar to getting hold of opponent and oppnenet's expected goals which are useful, so we'll get them at same time
	shotinfo = gbgdf %>%
			group_by(season, teamgamenumber, team) %>%
			summarise(shotont = sum(ont))

	resultdf = left_join(resultdf,
							shotinfo %>%
							dplyr::rename(oppteam = team,
											shotontconceded = shotont),
							by = c('season', 'teamgamenumber', 'oppteam'))

	resultdf = resultdf %>%
					mutate(gksave = shotontconceded - conceded)

	gbgdf = left_join(gbgdf,
						resultdf %>%
						select(date, team, oddsescored, oppteam, conceded, oddseconceded, gksave) %>%
						dplyr::rename(teamconceded = conceded,
										teamoddsescored = oddsescored,
										teamoddseconceded = oddseconceded),
						by = c('date', 'team'))
	gbgdf$gksave[which(gbgdf$gksave < 0)] = 0

	return(gbgdf)
}

getsummary = function(gbgdf, beforeseason=F) {
	summarydf = gbgdf %>%
				group_by(season, team, player) %>%
				summarise(minute = sum(minute),
							goal = sum(goal),
							assist = sum(assist),
							shotoob = sum(shotoob),
							shot6yd = sum(shot6yd),
							shotib = sum(shotib),
							openplay = sum(openplay),
							counter = sum(counter),
							setpiece = sum(setpiece),
							penaltytaken = sum(penaltytaken),
							offt = sum(offt),
							bar = sum(bar),
							ont = sum(ont),
							block = sum(block),
							penaltyscored = sum(penaltyscored),
							longkp = sum(longkp),
							shortkp = sum(shortkp)) %>%
				ungroup()
	if (beforeseason) {
		### won't have any of the current squad, so need to tack that on
		currentsquad = getcurrentsquad()
		currentsquad$season=currentseason
		missingcol = setdiff(names(summarydf),names(currentsquad))
		currentsquad[,missingcol]=NA
		currentsquad = currentsquad[,names(summarydf)]
		currentsquad = as_tibble(currentsquad)
		summarydf = bind_rows(summarydf, currentsquad)
	}
	if (!beforeseason) {
		print('probably a good idea to indicate which players have left in summarydf')
	}
	return(summarydf)
}

#ok the problem is files are stored vertically, but some functions want it horizontal. need to make a decision here. storing horizontally seems fine to me, just have to have easy way of transferring the objects from vertical to horizontal and back

processdeserved=function(mydf, getffpoint = FALSE) {

	### check all necessary columns are there
	if (getffpoint) {
		if (!'ffposition' %in% names(mydf)) {
			stop('Need to have ffposition columns, run getplayerprice to get it\n')
		}
	}

	goalmaxinfo=dget(file=paste(USERPATH,'whoscored/shotvalue.dat',sep=''))
	assistmaxinfo=dget(file=paste(USERPATH,'whoscored/passvalue.dat',sep=''))

	### now modify the goals scored to take away penalties scored
	mydf[,'opgoal']=mydf[,'goal']-mydf[,'penaltyscored']

	### process shot zones calculations
	mydf=mydf %>% mutate(shotobcredit = goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotob']*shotoob, shotibcredit=goalmaxinfo$zoneprop*goalmaxinfo$zoneshottheta['oosshotib']*(shot6yd + shotib))
	### then shots accuracy
	mydf=mydf %>% mutate(goalcredit  = (1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotgoal']*opgoal, ontargetcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshottarget']*ont, misscredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotmiss']*(offt+bar), blockcredit=(1-goalmaxinfo$zoneprop)*goalmaxinfo$accshottheta['oosshotblock']*block)

	### then add all the bastards up
	mydf = mydf %>% mutate(deservedgoal=shotobcredit + shotibcredit + goalcredit + ontargetcredit + misscredit + blockcredit, deservedgoalpg=ifelse(minute>0,90*deservedgoal/minute,NA))

	### then add in deserved assists
	mydf=mydf %>% mutate(assistcredit = assistmaxinfo$passtheta['oosassist']*assist, keypasscredit=assistmaxinfo$passtheta['ooskeypass']*(longkp + shortkp))
	### then add all the bastards up
	mydf = mydf %>% mutate(deservedassist=assistcredit + keypasscredit, deservedassistpg=ifelse(minute>0,90*deservedassist/minute,NA))

	if (getffpoint) {
		### we then want expected fantasy points
		pointdf=data.frame(pos=c('g','d','m','f'),point=c(6,6,5,4))
		mydf$goalpoint = pointdf$point[match(mydf$ffposition,pointdf$pos)]
		mydf$assistpoint=3
		mydf = mydf %>% mutate(deservedpoint = goalpoint*deservedgoal + assistpoint*deservedassist,deservedpointpg=deservedpoint*90/minute)

		### get rid of intermediate columns
		mydf = mydf %>% select(-c(goalpoint,assistpoint))
	}

	return(mydf)
}

viewteam=function(myteam, mysummarydf=summarydf) {

	info=mysummarydf %>% filter(team==myteam)
	### want to get approximate proportion of goals and assists each player gets, so...
	totaldeserved=mysummarydf %>% filter(team==myteam) %>% summarise(totaldeservedgoal=sum(deservedgoal),totaldeservedassist=sum(deservedassist))
	info = info %>% mutate(deservedgoalproportion=deservedgoal/totaldeserved[['totaldeservedgoal']], deservedassistproportion=deservedassist/totaldeserved[['totaldeservedassist']])
	### but then rescale according to number of minutes played
	totalminute=round(sum(info$minute)/11)
	info = info %>% mutate(deservedgoalproportion = round(deservedgoalproportion*totalminute/minute,2), deservedassistproportion = round(deservedassistproportion*totalminute/minute,2))
	info = info %>% arrange(-deservedpointpg) %>% select(player,minute,ffposition,deservedgoalpg,deservedassistpg,deservedpointpg,deservedgoalproportion,deservedassistproportion)
	### but let's add another column indicating expected points if expected goals is 1
	return(info)
}

viewplayer=function(playername, mygbgdf=gbgdf) {
	### firstly select the bits we want
	minigamebygame = mygbgdf %>%
						filter(grepl(playername,tolower(gbgdf$player)) & season == currentseason)
	if (nrow(minigamebygame)==0) stop('Can\'t find that player, exiting\n')
	if (length(unique(minigamebygame[,'player']))>1) stop('not unique player\n')
	### paste the minutes/goals/assists into a single cell
	### or just plot it?
	plot(minigamebygame$teamgamenumber, minigamebygame$minute,ylim=c(0,90),pch=16,xlab='teamgamenumber',ylab='minutes',main=minigamebygame[1,'player'])
	points(minigamebygame$teamgamenumber,minigamebygame$deservedgoal*90/1.5,pch=15,col='blue')
	points(minigamebygame$teamgamenumber,minigamebygame$deservedassist*90/1.5,pch=15,col='green')
	### overlay average, weighted by minutes played
	meangoal=with(minigamebygame,weighted.mean(90/minute * deservedgoal,minute))
	meanassist=with(minigamebygame,weighted.mean(90/minute * deservedassist,minute))
	abline(h=meangoal*90/1.5,col='blue')
	abline(h=meanassist*90/1.5,col='green')
	text(max(minigamebygame$teamgamenumber),meangoal*90/1.5,round(meangoal,2),col='blue',pos=3,cex=0.8)
	text(max(minigamebygame$teamgamenumber),meanassist*90/1.5,round(meanassist,2),col='green',pos=3,cex=0.8)
	legend(min(minigamebygame$teamgamenumber),90,c('minutes','goals','assists'),pch=c(16,15,15),col=c('black','blue','green'))
}

viewgeneral=function(pos=NULL,mincutoff=NULL,pointcutoff=NULL,teamlist=NULL,todisplay=20, mysummarydf=summarydf) {
	minimodel=mysummarydf
	if (!is.null(pos)) minimodel = minimodel %>% filter(mainpos==pos)
	if (!is.null(mincutoff)) minimodel = minimodel %>% filter(minute>max(minute)*mincutoff)
	if (!is.null(pointcutoff)) minimodel = minimodel %>% filter(deservedpointpg>pointcutoff)
	if (!is.null(teamlist)) minimodel = minimodel %>% filter(team %in% teamlist)
	minimodel = minimodel %>% arrange(-deservedpointpg) %>% select(team,player,minute,deservedgoalpg,deservedassistpg,deservedpointpg)
	print(minimodel[1:todisplay,])
}

modelexpectedsave = function(gbgdf) {
	isGoalkeeperIndex = with(gbgdf, mainpos == 'GK' & minute>89)
	mod = glm(gksave ~ teamoddseconceded, data = gbgdf[isGoalkeeperIndex,], family = 'poisson')
	return(coef(mod))
}

getplayerfixture=function(fixtdf, playedf, summarydf, gbgdf) {
	### want a data frame of future player/fixture combos, along with expected points in each match and the weight

	### now merge in the players data
	playerfixtdf=inner_join(playerdf, fixtdf, by = 'team') %>%
					dplyr::rename(eteamscored = escored,
									eteamconceded = econceded)
	### now calculate expected goals and assists
	playerfixtdf = playerfixtdf %>%
					arrange(team,player,gameweek) %>%
					mutate(egoal = normgoalrate3 * eteamscored,
							eassist = normassistrate3 * eteamscored)

	### then we need the goalkeeper predictions
	gksavecoef = modelexpectedsave(gbgdf)
	playerfixtdf = playerfixtdf %>%
					mutate(egksave = if_else(mainpos == 'GK',
											exp(gksavecoef[[1]] + gksavecoef[[2]] * eteamconceded),
											0))

	#### and then their active status
	playerfixtdf = addplayeractivecolumn(playerfixtdf)

	return(playerfixtdf)
}

getfixtureexpectedpoint=function(fixtdf, playerdf, summarydf, gbgdf) {
	playerfixtdf=getplayerfixture(fixtdf, playerdf, summarydf, gbgdf)

	playerfixtdf = playerfixtdf %>%
			mutate(
			appearancepoint = 2,
			goalpoint = case_when(
				ffposition %in% c('g', 'd') ~ (6 + 1.5) * egoal,
				ffposition == 'm' ~ (5 + 1.5) * egoal,
				ffposition == 'f' ~ (4 + 1.5) * egoal),
			assistpoint = (3 + 0.75)*eassist,
			goalconcededpoint = case_when(
				ffposition %in% c('g', 'd') ~ - 2*(dpois(2,eteamconceded) +
													dpois(3,eteamconceded)) -
												4*(dpois(4,eteamconceded) +
													dpois(5,eteamconceded)),
				ffposition %in% c('m', 'f') ~ 0),
			cleansheetpoint = case_when(
				ffposition %in% c('g', 'd') ~ (4 + 0.75)*dpois(0,eteamconceded),
				ffposition == 'm' ~ 1 * dpois(0,eteamconceded),
				ffposition == 'f' ~ 0),
			savepoint = case_when(ffposition == 'g' ~ 0.25 * egksave,
									ffposition != 'g' ~ 0),
			expectedpoint90min = appearancepoint + goalpoint + assistpoint + goalconcededpoint + cleansheetpoint + savepoint,
			expectedpoint = active * expectedpoint90min) %>%
			select(-c(goalpoint, assistpoint, goalconcededpoint, cleansheetpoint, savepoint))

	return(playerfixtdf)
}

addplayeractivecolumn = function(mydf) {

	mostrecentactive=getmostrecentdir('whoscored')
	activeplayerfile=paste(DATAPATH,mostrecentactive,'/activeplayer.csv',sep='')
	activeplayer=read.csv(activeplayerfile)

	### firstly get rid of anyone not in activeplayer
	mdum=match(with(mydf, paste(team,player)) , with(activeplayer,paste(team,player)))
	sax=which(!is.na(mdum))
	mydf$active = NA
	mydf$active[sax]=activeplayer[mdum[sax],'active']

	return(mydf)
}

getlongtermplayerpoint=function(fixtdf, playerdf, summarydf, gbgdf) {

	playerfixtdf=getfixtureexpectedpoint(fixtdf, playerdf, summarydf, gbgdf)
	## make a player by player summary
	playersummary=playerfixtdf %>%
					group_by(ffteam, ffplayer, team, player, ffposition, price) %>%
					summarise(epoint=sum(expectedpoint * gwweight)) %>%
					ungroup()

	return(playersummary)
}

getstartingplayer=function(gbgdf, summarydf, playerdf) {
	mostrecentdir=getmostrecentdir('whoscored')
	activeplayerfile=paste(DATAPATH,mostrecentdir,'/activeplayer.csv',sep='')
	# activeplayer=read.csv(activeplayerfile)
	### so let's get in current values, because most will stay the same
	### then provide how many minutes played by each player
	### filter down to last five games for each team
	minigbgdf = gbgdf %>%
				group_by(season,team) %>%
				filter(season == currentseason) %>%
				filter(teamgamenumber > max(teamgamenumber)-5) %>%
				ungroup() %>%
				select(team,player,minute,teamgamenumber) %>%
				dplyr::rename(game = teamgamenumber)
	### now spread that out
	horizgw=spread(minigbgdf,key='game',value='minute',sep='')
	### then get a crude version of who is currently active

	horizgw$summin = horizgw %>%
						select(contains('game')) %>%
						mutate(summin = rowSums(.,na.rm=T)) %>%
						pull(summin)

	activeplayerdf = left_join(playerdf %>%
							filter(!is.na(player)) %>%
							select(team, player, ffposition),
							horizgw,
							c('team', 'player'))

	### this is a right arse, we want to find the most recent but one directory name
	dum=list.files(DATAPATH)
	dum=dum[grep('^[0-9]{8}$',dum)]
	sortedmostrecent=dum[rev(order(dum))]
	havegotprevious = FALSE
	while(!havegotprevious) {
		for (fi in 2:length(sortedmostrecent)) {
			filein = paste(DATAPATH,sortedmostrecent[fi],'/activeplayer.csv',sep='')
			if (file.exists(filein)) {
				havegotprevious = TRUE
				previousactiveplayer=read.csv(filein)
				### match the active column of that to the active one of the new df
				mdum=match(with(activeplayerdf,paste(team,player)),with(previousactiveplayer,paste(team,player)))
				activeplayerdf$active=previousactiveplayer[mdum,'active']
				break
			}
		}
	}
	### now put in order
	gwcolname = names(activeplayerdf)[grep('game',names(activeplayerdf))]
	reverseordergwcolname = gwcolname[order(-as.numeric(gsub('game','',gwcolname)))]
	activeplayerdf=activeplayerdf[,c('team','player','ffposition','active',reverseordergwcolname)]
	activeplayerdf=activeplayerdf %>% arrange(team,match(ffposition,c('g','d','m','f')))

	### then this becomes the new active palyer file
	write.csv(file=activeplayerfile,activeplayerdf,row.names=F)

	cat('Most recent player file is here:',activeplayerfile,'\n')
}

getplayervalue=function(fixtdf, playerdf, summarydf, gbgdf) {
	### firstly get expected points scord by each active player
	playervalue=getlongtermplayerpoint(fixtdf, playerdf, summarydf, gbgdf)
	### then get hold of players values
	### join up prices
	playervalue$value=with(playervalue,round(epoint/price,2))
	playervalue=playervalue %>%
					group_by(ffposition) %>%
					mutate(valuerank=rank(-value)) %>%
					ungroup()
	### arrange in nice order
	playervalue = playervalue %>% arrange(team,match(ffposition,c('g','d','m','f')),price)

	return(playervalue)
}

getcurrentplayervalue=function(playervalue) {
	currentteam=read.csv(paste(DATAPATH,'currentteam.csv',sep=''))
	mdum=match(with(currentteam,paste(team,player)),with(playervalue,paste(team,player)))
	coltocopy=c('epoint90min','active','epoint','price','value','valuerank')
	currentteam[,coltocopy]=playervalue[mdum,coltocopy]
	return(currentteam)
}

getcurrentexpectedpoint=function(playerfixtdf, mysquad) {
	currentplayerfixtepoint = playerfixtdf %>%
							filter(!is.na(player)) %>%
							filter(gameweek == min(gameweek)) %>%
							filter(paste(team, player) %in% with(mysquad, paste(team, player))) %>%
							select(team, player, expectedpoint, ffposition)
	groupedcurrentfixtepoint = currentplayerfixtepoint %>%
								group_by(team, player) %>%
								summarise(expectedpoint = sum(expectedpoint),
											numfixt = n()) %>%
								mutate(expectedpoint = expectedpoint) %>%
								select(-numfixt)
	currentsquadepoint =  left_join(groupedcurrentfixtepoint,
									currentplayerfixtepoint %>%
									select(team, player, ffposition),
									by = c('team', 'player')) %>%
							distinct() %>%
							arrange(match(ffposition, c('g','d','m','f')), -expectedpoint)
	return(currentsquadepoint)
}

makeseasondeservedsummary = function(summarydf, gbgdf) {
	### check that processdeserved has been done
	if (!'deservedgoal' %in% names(summarydf)) stop('need to run processdeserved')
	seasoninfo = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''),as.is=T)
	seasonplayersummary=NULL
	seasonteamsummary=NULL
	for (si in 1:nrow(seasoninfo)) {
		if (!seasoninfo$havegbg[si]) {
			### need to load summary
			tempsummary = read.csv(paste(DATAPATH,seasoninfo$season[si],'/model.csv',sep=''),as.is=T)
			### only want the necessary columns
			tempdeservedsummary = tempsummary %>% select(team, player, minute, deservedgoal ,deservedassist)
			tempdeservedsummary$season=seasoninfo$season[si]
			# but also want total goals by the team
			tempteamsummary = tempsummary %>%
								group_by(team) %>%
								summarise(goal = sum(goal))
		}
		if (seasoninfo$havegbg[si]) {
			tempdeservedsummary = summarydf %>%
							filter(season == seasoninfo$season[si]) %>%
							select(season, team, player, minute, deservedgoal ,deservedassist)
			tempteamsummary = summarydf %>%
								filter(season == seasoninfo$season[si]) %>%
								group_by(team) %>%
								summarise(goal = sum(goal))
			# but we also want to acquire the info for the promoted teams
			if (seasoninfo$season[si] != currentseason) {
				promotedtempsummary = read.csv(paste(DATAPATH,seasoninfo$season[si],'/model.csv',sep=''),as.is=T)
				promoteddeservedsummary = promotedtempsummary %>%
										mutate(season = seasoninfo$season[si]) %>%
										select(season, team, player, minute, deservedgoal ,deservedassist)
				promotedteamsummary = promotedtempsummary %>%
										group_by(team) %>%
										summarise(goal = sum(goal))
				### join em up
				tempdeservedsummary = bind_rows(tempdeservedsummary, promoteddeservedsummary)
				tempteamsummary = bind_rows(tempteamsummary, promotedteamsummary)
			}
		}
		seasonplayersummary[[si]]=tempdeservedsummary
		tempteamsummary$season=seasoninfo$season[si]
		seasonteamsummary[[si]] = tempteamsummary
	}
	seasondeservedsummary = as_tibble(bind_rows(seasonplayersummary))
	seasonteamsummary = as_tibble(bind_rows(seasonteamsummary))
	seasondeservedsummary = left_join(seasondeservedsummary,
										seasonteamsummary,
										by = c('team', 'season'))

	# but then we want that joined to gbgdf in almost any situation i would have thought
	seasoninfo$previousseason = c(NA,seasoninfo$season[1:(nrow(seasoninfo)-1)])
	gbgdf$previousseason = seasoninfo$previousseason[match(gbgdf$season, seasoninfo$season)]
	# NB this only uses one previous season, a bit crap but it'll do for now
	gbgdftoseasondeservedsummarymap = match(
									with(gbgdf, paste(previousseason, player)),
									with(seasondeservedsummary, paste(season, player)))
	gbgdf$previoustotalminute=seasondeservedsummary$minute[gbgdftoseasondeservedsummarymap]
	gbgdf$previousdeservedgoal=seasondeservedsummary$deservedgoal[gbgdftoseasondeservedsummarymap]
	gbgdf$previousdeservedassist=seasondeservedsummary$deservedassist[gbgdftoseasondeservedsummarymap]
	gbgdf$previousteamgoal=seasondeservedsummary$goal[gbgdftoseasondeservedsummarymap]

	if (FALSE) {
		stop('totally wrong for when a player switches team\n')
		gbgdftoseasonteamsummarymap = match(
									with(gbgdf, paste(previousseason, team)),
									with(seasonteamsummary, paste(season, team)))
		gbgdf$previousteamgoal = seasonteamsummary$goal[gbgdftoseasonteamsummarymap]
	}

	return(list(seasondeservedsummary = seasondeservedsummary,
				gbgdf = gbgdf,
				seasonteamsummary = seasonteamsummary))
}

makeoldseasonsummary=function(season) {
	if (FALSE) {
	teamdf=read.csv(paste(DATAPATH,'/teampage.csv',sep=''))
	allfiletype=c('summary','shotzone','shotsit','shotacc','goalsit','keypass')
	### we're assuming you have put all teams of interest into this file:
	teamtousedf=read.csv(paste(DATAPATH,season,'/summary_coverpage.csv',sep=''), sep = '~')
	teamdata=NULL
	for (ti in 1:nrow(teamtousedf)) {
		teamdata[[ti]]=processpage(teamtousedf$team[ti],season,ishistoric=TRUE,beforeseason=FALSE)
	}
	allteamdata = bind_rows(teamdata)
	}
	allteamdata = read.csv(paste0(DATAPATH, season, '/combined_data.csv'),as.is=T)
	allteamdata=processdeserved(allteamdata)
	fileout=paste(DATAPATH,season,'/model.csv',sep = '')
	write.csv(file=fileout, allteamdata, row.names=FALSE)
}
