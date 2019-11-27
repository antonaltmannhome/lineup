
suppressWarnings(library('Rglpk'))


RunKnapsack = function(playerDF, forcedInclusionExclusion, currentmoney) {
	# quick fudge, get rid of NAs
	playerDF = playerDF %>%
					filter(!is.na(epoint))

	goalkeeperindex=with(playerDF,which(ffPosition=='g'))
	defenderindex=with(playerDF,which(ffPosition=='d'))
	midfielderindex=with(playerDF,which(ffPosition=='m'))
	forwardindex=with(playerDF,which(ffPosition=='f'))

	forcedincludedf = forcedInclusionExclusion %>% filter(includeOrExclude == 'include')
	forcedexcludedf = forcedInclusionExclusion %>% filter(includeOrExclude == 'exclude')

	anyForcedInclusion = nrow(forcedincludedf) > 0
	anyForcedExclusion = nrow(forcedexcludedf) > 0

	requiredgknum = 2
	requireddefnum = 5
	requiredmidnum = 5
	requiredfornum = 3


	if (anyForcedInclusion) {

		forcedincludedf = left_join(forcedincludedf,
								playerDF %>%
								select(team, player, ffPrice, ffPosition),
								by = c('team', 'player'))
		# check you haven't tried to include any non-existant players
		if (any(is.na(forcedincludedf$ffPrice))) {
			print(forcedincludedf)
			stop('You have tried to force in a player who does not exist or is not playing')
		}
		# make it select forced players by making them ultra desirable
		playerDF = indicate_overlapping_combination(playerDF, forcedincludedf, c('team', 'player'), 'isForcedInclusion')
		maxPointAnyPlayer = max(playerDF$epoint, na.rm= TRUE)
		playerDF = mutate_cond(playerDF, isForcedInclusion, epoint = 2 * maxPointAnyPlayer)

	}

	if (anyForcedExclusion) {
		sax = which(with(playerDF, paste(team, player)) %in%
					with(forcedexcludedf, paste(team, player)))
		if (length(sax) >0) playerDF[sax,'epoint'] = 0
	}

	subplayerDF=rbind(playerDF[goalkeeperindex,],
							playerDF[defenderindex,],
							playerDF[midfielderindex,],
							playerDF[forwardindex,])
	nsubplayer=nrow(subplayerDF)
	subunteam = unique(subplayerDF$team)

	#var.types <- c(rep("B", nsubplayer),'C')
	obj=subplayerDF$epoint
	var.types <- c(rep("B", nsubplayer), rep('B', 4), rep('B', length(subunteam)), 'C')
	mat10=matrix(0,nrow=nsubplayer,ncol=nsubplayer)
	diag(mat10)=1

	numgk = length(goalkeeperindex)
	numdef = length(defenderindex)
	nummid = length(midfielderindex)
	numfw = length(forwardindex)
	posmat = rbind(
		c(rep(1, numgk), rep(0, numdef), rep(0, nummid), rep(0, numfw)),
		c(rep(0, numgk), rep(1, numdef), rep(0, nummid), rep(0, numfw)),
		c(rep(0, numgk), rep(0, numdef), rep(1, nummid), rep(0, numfw)),
		c(rep(0, numgk), rep(0, numdef), rep(0, nummid), rep(1, numfw))
				)

	teammat = sapply(subplayerDF$team, function(x) as.numeric(subunteam == x))

	conmat=rbind(
		mat10,
		posmat,
		teammat,
		subplayerDF$ffPrice
	)
	direction=c(rep('<=',nsubplayer),rep('==',4),rep('<=', length(subunteam)), '<=')
	rhs=c(rep(1,nsubplayer),requiredgknum, requireddefnum, requiredmidnum, requiredfornum, rep (3, length(subunteam)), currentmoney)
	sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)

	### i think we should restrict to 80 mill, and only take 4 def/mids, assume you can always drop one

	# subplayerDF[which(sol$solution==1),]

	idealteam = subplayerDF[which(sol$solution==1),] %>%
				select(team, player)

	return(idealteam)
}

calculateexpectedpoint = function(playerfixtdf, myteam) {

	myteamfixtdf = inner_join(playerfixtdf %>%
								filter(gwweight > 0),
								myteam %>%
								select(team, player),
								by = c('team', 'player'))
	myteamfixtdf[,c('selected', 'captain')] = NA
	allgameweek = unique(myteamfixtdf$gameweek)
	for (mygameweek in allgameweek) {
		### let's try to pick out the best team for a given week using knapsack
		submyteamfixtdf = myteamfixtdf %>%
							filter(gameweek == mygameweek)
		### one thing to watch out for, double gameweeks
		gwtotaldf = submyteamfixtdf %>%
							group_by(team,player) %>%
							summarise(ffPosition = ffPosition[1],
										expectedpoint = sum(expectedpoint))
		### need to insert any missing players too
		missingplayer = anti_join(myteam,
									gwtotaldf %>%
									select(team, player),
									by = c('team', 'player'))
		if (nrow(missingplayer) > 0) {
			message('Warning, for gameweek ', mygameweek,', I have the following players with no fixture:')
			print(missingplayer)
			message('Press ENTER to continue')
			dum = askcond(FALSE, TRUE)
			missinginfo = missingplayer %>%
							mutate(expectedpoint = 0)
			missinginfo = lazy_left_join(missinginfo, playerDF, c('team', 'player'), 'ffPosition')
			gwtotaldf = bind_rows(gwtotaldf, missinginfo)
		}

		if (any(is.na(gwtotaldf$expectedpoint))) {
			stop('Have NAs within calculateexpectedpoint for gameweek ', mygameweek,', investigate!\n')
		}

		mynumplayer=nrow(myteam)
		#var.types <- c(rep("B", nsubplayer),'C')
		obj=gwtotaldf$expectedpoint
		var.types <- c(rep("B", mynumplayer),'I','I','I','C')
		mat10=matrix(0,nrow=mynumplayer,ncol=mynumplayer)
		diag(mat10)=1
		conmat=rbind(
			mat10,
			as.numeric(gwtotaldf$ffPosition == 'g'),
			as.numeric(gwtotaldf$ffPosition == 'd'),
			as.numeric(gwtotaldf$ffPosition == 'f'),
			rep(1,mynumplayer)
			)
		direction=c(rep('<=',mynumplayer),'==',rep('>=',2),'==')
		rhs=c(rep(1,mynumplayer),1,3,1,11)
		sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)
		gwtotaldf$selected = sol$solution
		gwtotaldf$captain = gwtotaldf$expectedpoint == max(gwtotaldf$expectedpoint)
		sax = with(myteamfixtdf, which(gameweek == mygameweek))
		mdum = match(with(myteamfixtdf[sax,], paste(team, player, gameweek)),
						paste(gwtotaldf$team, gwtotaldf$player, mygameweek))
		myteamfixtdf[sax,c('selected', 'captain')] = gwtotaldf[mdum, c('selected','captain')]
	}

	### then the fun bit, what are the total points?
	myteamfixtdf = myteamfixtdf %>%
					mutate(adjexpectedpoint = gwweight * selected*ifelse(captain, 2*expectedpoint, expectedpoint))

	totalexpectedpoint = sum(myteamfixtdf$adjexpectedpoint)

	### worth including some stats about how often each player actually plays
	pointsummarydf = myteamfixtdf %>%
						group_by(team, player) %>%
						summarise(ffPosition = ffPosition[1],
									sumSelected = sum(selected),
									sumCaptain = sum(captain),
									sumExpectedPoint = sum(expectedpoint),
									sumAdjExpectedPoint = sum(adjexpectedpoint)) %>%
						arrange(match(ffPosition, c('g', 'd', 'm', 'f')))

	return(list(pointsummarydf = pointsummarydf,
				totalexpectedpoint = totalexpectedpoint))
}

addextrainfo = function(fullexpectedpoint, gameweekexpectedpoint, playerDF) {

	fullexpectedpoint = left_join(fullexpectedpoint,
											gameweekexpectedpoint %>%
											select(team, player, expectedpoint),
											by = c('team', 'player')) %>%
									dplyr::rename(currentgameweek = expectedpoint)

	fullexpectedpoint = left_join(fullexpectedpoint,
									playerDF %>%
									select(team, player, eMin, ffPrice, value, valuerank),
									by = c('team', 'player'))

	return(fullexpectedpoint)
}
