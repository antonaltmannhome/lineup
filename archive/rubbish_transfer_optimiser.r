getrid = c('ayew', 'cedric', 'gomez')
getridpos = c('f', 'd' ,'d')
getridprice = c(5,5,5)

buy = c('kompany', 'kolasiinac', 'diouf')
buypos = c('d', 'd', 'f')
buyprice = c(6,4,3)

currentfund = 100.1

weeklimit = 5

tradedf0 = cbind(
			rep(getrid, rep(weeklimit, rep(length(getrid)))),
			rep(1:weeklimit, length(getrid))
			)

## no, forget that

getweeklyoption = function(myvec) {
	comblist = NULL
	for (j in 1:length(myvec)) {
		dum = combn(myvec, j)
		if (j < length(myvec)) dum = rbind(dum, array('none', dim = c(length(myvec) - j, ncol(dum))))
		comblist[[j]] = t(dum)
	}
	# comblist[[j+1]] = rep('none', length(myvec))
	combarr = do.call(rbind, comblist)
	
	return(combarr)
}

# that is all available options in a given week. but then you hvae to combine across availasble weeks

### no, also wrong...

makeallstrategy = function(getrid, buy) {

	### find all valid transfer combinations
	transcombdf = expand.grid(getrid = getrid, buy = buy)
	transcombdf$getridpos = getridpos[match(transcombdf$getrid, getrid)]
	transcombdf$buypos = buypos[match(transcombdf$buy, buy)]
	transcombdf = transcombdf %>%
					filter(getridpos == buypos) %>%
					select(getrid, buy)
	
	getridbuy = apply(transcombdf, 1, paste, collapse = '-')

	comblist = NULL
	numstrat = 0
	for (numtrans in 1:length(getridbuy)) {
		currentnumchoice = choose(length(getridbuy), numtrans) * (numtrans ^ weeklimit)
		currentplayercomb = as.data.frame(combn(getridbuy, numtrans))
		currentweekcomb = as.data.frame(matrix(rep(1:weeklimit, numtrans), ncol = numtrans))
	
		currentcombdf = NULL
		for (j in 1:ncol(currentplayercomb)) {
			combbyplayer = expand.grid(player = currentplayercomb[,j], week = 1:weeklimit)
			splitcombbyplayer = split(combbyplayer,f = combbyplayer$player)
			pastesplitcombbyplayer = lapply(splitcombbyplayer,
									function(x) apply(x, 1, paste, collapse = ','))
			currentcomb = expand.grid(pastesplitcombbyplayer, stringsAsFactors = FALSE)
		
			comblinetodf = function(x) {
				as.data.frame(matrix(unlist(strsplit(as.character(x), split= ',')),
							ncol = 2,
							byrow = TRUE))
			}
			currentcomblist = apply(currentcomb, 1, comblinetodf)
			for (k in 1:length(currentcomblist)) {
				currentcomblist[[k]]$numstrat = numstrat + k
			}
			numstrat = numstrat + length(currentcomblist)
			currentcombdf[[j]] = do.call(rbind, currentcomblist)
		}
	
		currenttranscombdf = do.call(rbind, currentcombdf)
		comblist[[numtrans]] = currenttranscombdf
	}
	combdf = do.call(rbind, comblist)
	names(combdf) = c('getridbuy', 'week', 'numstrat')
	
	# now for each strategy, have to work out amount in bank
	combdf$sell = gsub('\\-.+$', '', combdf$getridbuy)
	combdf$buy = gsub('^.+\\-', '', combdf$getridbuy)
	
		currenttranstab = expand.grid(week = 1:weeklimit, strategy = 1:nrow(currentcomb))
			# but that is a bastard to work with, better to have an array with y
		currenttranstab[, paste('trans', 1:numtrans, sep = '')] = 'none'
		
	}
	
	
	
	pasteplayer = apply(currentplayercomb, 2, paste, collapse = ',')
	currentchoice = expand.grid(week = 1:weeklimit,
								pasteplayer= pasteplayer,
								stringsAsFactors = FALSE)
	
### no, going nowhere with that either

remplayer = getrid
for (weeki in 1:weeklimit) {
	numtransoption = 0:length(remplayer)
	for (
	
# no this is just too hard

### try instead just calculating for a suggested strategy

transferdf = data.frame(
				strategy = c(1,2,3),
				week = c(15, 16, 17),
				playerout = rep('lukasz fabianski', 3),
				teamout = rep('swansea', 3),
				playerin = rep('simon mignolet', 3),
				teamin = rep('liverpool', 3),
				position = 'g')

calculateexpectedpoint2 = function(playerfixtdf, mycurrentteam, mytransfer) {
	
	allgameweek = sort(unique(playerfixtdf$gameweek))
	dum = mytransfer %>%
					dplyr::rename(player = playerin, team = teamin) %>%
					select(team, player, position)
	myfullteam = rbind(mycurrentteam, dum) %>%
					arrange(match(position, c('g', 'd', 'm', 'f')))
	myfullteamfixtdf = inner_join(playerfixtdf,
								myfullteam,
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
						select(team, player, position)
			myteam = rbind(anti_join(myteam, toremove, by = c('team', 'player')), toadd)
		}
		submyteamfixtdf = inner_join(playerfixtdf %>%
							filter(gameweek == mygameweek),
								myteam,
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
			missingplayerindex=match(with(missingplayer, paste(team, player)),
										with(myteamfixtdf, paste(team, player)))
			missinginfo = myteamfixtdf[missingplayerindex,] %>%
							select(team, player, ffposition, expectedpoint) %>%
							mutate(expectedpoint = 0)
			gwtotaldf = bind_rows(gwtotaldf, missinginfo)
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
