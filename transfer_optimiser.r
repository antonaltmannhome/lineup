
calculateexpectedpoint2 = function(playerfixtdf, mycurrentteam, mytransfer) {
	
	allgameweek = sort(unique(playerfixtdf$gameweek))
	mycurrentteam = left_join(mycurrentteam,
	playervalue %>%
	select(team, player, ffposition),
	by = c('team', 'player'))
	mytransfer = left_join(mytransfer %>%
	dplyr::rename(player = playerout, team = teamout),
	playervalue %>%
	select(team, player, ffposition),
	by = c('team', 'player')) %>%
	dplyr::rename(playerout = player, teamout = team)
	
	dum = mytransfer %>%
	dplyr::rename(player = playerin, team = teamin)%>%
	select(team, player, ffposition)
	myfullteam = rbind(mycurrentteam, dum) %>%
	arrange(match(ffposition, c('g', 'd', 'm', 'f')))
	myfullteamfixtdf = inner_join(playerfixtdf,
	myfullteam %>%
	select(team, player),
	by = c('team', 'player'))
	myfullteamfixtdf[,c('selected', 'captain')] = NA
	for (mygameweek in allgameweek) {
		### let's try to pick out the best team for a given week using knapsack
		myteam = mycurrentteam
		transferindex = which(mytransfer$week <= mygameweek)
		if (length(transferindex) > 0) {
			toremove = mytransfer[transferindex,] %>%
			dplyr::rename(player = playerout, team = teamout) %>%
			select(team, player)
			toadd = mytransfer[transferindex,] %>%
			dplyr::rename(player = playerin, team = teamin) %>%
			select(team, player, ffposition)
			myteam = rbind(anti_join(myteam, toremove, by = c('team', 'player')), toadd)
		}
		submyteamfixtdf = inner_join(playerfixtdf %>%
		filter(gameweek == mygameweek),
		myteam %>%
		select(team, player),
		by = c('team', 'player'))
		submyteamfixtdf[,c('selected', 'captain')] = NA
		### one thing to watch out for, double gameweeks
		gwtotaldf = submyteamfixtdf %>%
		group_by(team,player) %>%
		summarise(ffposition = ffposition[1],
		expectedpoint = sum(expectedpoint))
		### need to insert any missing players too
		missingplayer = anti_join(myteam,
		gwtotaldf %>%
		select(team, player),
		by = c('team', 'player'))
		if (nrow(missingplayer) > 0) {
			missingplayer$expectedpoint = 0
			gwtotaldf = bind_rows(gwtotaldf, missingplayer)
		}
		
		mynumplayer=nrow(myteam)
		#var.types <- c(rep("B", nsubplayer),'C')
		obj=gwtotaldf$expectedpoint
		var.types <- c(rep("B", mynumplayer),'I','I','I','C')
		mat10=matrix(0,nrow=mynumplayer,ncol=mynumplayer)
		diag(mat10)=1
		conmat=rbind(
		mat10,
		as.numeric(gwtotaldf$ffposition == 'g'),
		as.numeric(gwtotaldf$ffposition == 'd'),
		as.numeric(gwtotaldf$ffposition == 'f'),
		rep(1,mynumplayer)
		)
		direction=c(rep('<=',mynumplayer),'==',rep('>=',2),'==')
		rhs=c(rep(1,mynumplayer),1,3,1,10)
		sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)
		gwtotaldf$selected = sol$solution
		gwtotaldf$captain = gwtotaldf$expectedpoint == max(gwtotaldf$expectedpoint)
		sax = with(myfullteamfixtdf, which(gameweek == mygameweek))
		mdum = match(with(myfullteamfixtdf[sax,], paste(team, player, gameweek)),
		paste(gwtotaldf$team, gwtotaldf$player, mygameweek))
		myfullteamfixtdf[sax,c('selected', 'captain')] = gwtotaldf[mdum, c('selected','captain')]
		# except that yields NA for the playyer whos been dropped, so cover for that
		sax2 = sax[which(is.na(mdum))]
		myfullteamfixtdf[sax2,c('selected', 'captain')] = 0
	}
	
	### then the fun bit, what are the total points?
	myfullteamfixtdf = myfullteamfixtdf %>%
	mutate(adjexpectedpoint = selected*ifelse(captain, 2*expectedpoint, expectedpoint))
	
	totalexpectedpoint = sum(myfullteamfixtdf$adjexpectedpoint)
	
	### worth including some stats about how often each player actually plays
	pointsummarydf = myfullteamfixtdf %>%
	group_by(team, player) %>%
	summarise(ffposition = ffposition[1],
	sumSelected = sum(selected),
	sumCaptain = sum(captain),
	sumExpectedPoint = sum(expectedpoint),
	sumAdjExpectedPoint = sum(adjexpectedpoint)) %>%
	arrange(match(ffposition, c('g', 'd', 'm', 'f')))
	
	return(list(pointsummarydf = pointsummarydf,
	totalexpectedpoint = totalexpectedpoint))
}

checklegal = function(currentstrategy, inthebank) {
	# get currentprice of all players
	currentstrategy = left_join(currentstrategy %>%
									dplyr::rename(player = playerout, team = teamout),
								playervalue %>%
									select(team, player, price),
								by = c('team', 'player')) %>%
						dplyr::rename(playerout = player, teamout = team, priceout = price)
	currentstrategy = left_join(currentstrategy %>%
									dplyr::rename(player = playerin, team = teamin),
								playervalue %>%
									select(team, player, price),
								by = c('team', 'player')) %>%
						dplyr::rename(playerin = player, teamin = team, pricein = price)
	sax = which(is.na(currentstrategy$pricein))
	if (length(sax) > 0) {
		print('Error, have not matched this player:')
		print(currentstrategy$playerin[sax])
		stop()
	}
	sax = which(is.na(currentstrategy$priceout))
	if (length(sax) > 0) {
		print('Error, have not matched this player:')
		print(currentstrategy$playerout[sax])
		stop()
	}
	currentstrategy  = currentstrategy %>%
						mutate(balance = inthebank - cumsum(pricein) + cumsum(priceout))
	if (all(currentstrategy$balance > 0)) legal = TRUE
	if (!all(currentstrategy$balance > 0)) legal = FALSE
	return(legal)
}

calculatepenaltypoint = function(currentstrategy, playerfixtdf, numfreetransfer) {
	ungameweek = sort(unique(playerfixtdf$gameweek))
	penaltypoint = 0
	for (wi in ungameweek) {
		thisweektransfer = sum(currentstrategy$week == wi)
		if (thisweektransfer > numfreetransfer) {
			penaltypoint = penaltypoint + 4 * (thisweektransfer - numfreetransfer)
		}
		numfreetransfer = pmin(numfreetransfer + 1, 2)
	}
	return(penaltypoint)
}

print('How many free transfers do you have remaining?\n')
numfreetransfer = scan(quiet = TRUE, nmax = 1)
print('How much money is in the bank?\n')
inthebank = scan(quiet = TRUE, nmax = 1)

checkstrategy = function() {
	
	transferdf = read.csv(paste(USERPATH, 'transfer_test.csv', sep = ''), as.is = TRUE)
	
	numstrat = length(unique(transferdf$strategy))
	strategyinfo = NULL
	stratdf = data.frame(rawpoint = rep(NA, numstrat),
	legal = rep(NA, numstrat),
	penalty = rep(NA, numstrat),
	adjpoint = rep(NA, numstrat))
	
	for (j in 1:numstrat) {
		# first check strategy is affordable, then count cost of transfers
		currentstrategy = transferdf[which(transferdf$strategy == j),]
		stratdf$legal[j] = checklegal(currentstrategy, inthebank)
		if (stratdf$legal[j]) {
			stratdf$penalty[j] = calculatepenaltypoint(currentstrategy, playerfixtdf, numfreetransfer)
			dum = calculateexpectedpoint2(playerfixtdf, currentteam, currentstrategy)
			strategyinfo[[j]] = dum$pointsummarydf
			stratdf$rawpoint[j] = dum$totalexpectedpoint
			stratdf$adjpoint[j] = stratdf$rawpoint[j] - stratdf$penalty[j]
		}
	}
	
	return(stratdf)
}
