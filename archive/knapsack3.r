### let's try the knapsack approach to ff
### but now let's force a certain player e.g aguero into the squad and see what happens to overall actual expected points

suppressWarnings(library('Rglpk'))

playervalue=getplayervalue(fixtdf, latestPlayerEstimate, summarydf, gbgdf)

playerfixtdf = getfixtureexpectedpoint(fixtdf, latestPlayerEstimate, summarydf, gbgdf)
mdum = match(with(playerfixtdf,paste(team,player)), with(summarydf,paste(team,player)))
playerfixtdf$price = summarydf$price[mdum]
### filter out players who have no active position or are goalies
playerfixtdf = playerfixtdf %>%
				filter(!is.na(expectedpoint))

### ok, let's try doing defenders and midfielders together

currentmoney = 100.4

### so let's say we've got e.g 30 million to spend on defenders, let's get best set
goalkeeperindex=with(playervalue,which(ffposition=='g'))
defenderindex=with(playervalue,which(ffposition=='d'))
midfielderindex=with(playervalue,which(ffposition=='m'))
forwardindex=with(playervalue,which(ffposition=='f'))

currentteam=read.csv(paste(DATAPATH,'currentteam.csv',sep=''))
currentdf = data.frame(currentteam)
currentdf = left_join(currentdf,
						playervalue %>%
						select(player, price, ffposition),
						by = 'player')

forceinclude = FALSE

if (!forceinclude) {
	remainingmoney = currentmoney

	requiredgknum = 2
	requireddefnum = 5
	requiredmidnum = 5
	requiredfornum = 3
}	

if (forceinclude) {

	forcedincludedf = data.frame(team = c('chelsea'),
									player = c('victor moses'))
	forcedincludedf = left_join(forcedincludedf,
							playervalue %>%
							select(team, player, price, ffposition),
							by = c('team', 'player'))
	# don't want to end up selecting forced players twice so make them undesirable
	sax = which(with(playervalue, paste(team, player)) %in%
				with(forcedincludedf, paste(team, player)))
	if (length(sax) >0) playervalue[sax,'epoint'] = 0

	totalforcedincludeprice = with(forcedincludedf, sum(price))

	
	remainingmoney = currentmoney - totalforcedincludeprice

	requiredgknum = 2 - sum(forcedincludedf$ffposition == 'g')
	requireddefnum = 5 - sum(forcedincludedf$ffposition == 'd')
	requiredmidnum = 5 - sum(forcedincludedf$ffposition == 'm')
	requiredfornum = 3 - sum(forcedincludedf$ffposition == 'f')

	forcedincludedf = forcedincludedf %>%
						select(-c(price, ffposition))
}

forceexclude = FALSE
if (forceexclude) {
	forcedexcludedf = data.frame(team = c('chelsea', 'chelsea'), player = c('andreas christensen', 'alvaro morata'))
	sax = which(with(playervalue, paste(team, player)) %in%
				with(forcedexcludedf, paste(team, player)))
	if (length(sax) >0) playervalue[sax,'epoint'] = 0
}

subplayervalue=rbind(playervalue[goalkeeperindex,],
						playervalue[defenderindex,],
						playervalue[midfielderindex,],
						playervalue[forwardindex,])
nsubplayer=nrow(subplayervalue)

#var.types <- c(rep("B", nsubplayer),'C')
obj=subplayervalue$epoint
var.types <- c(rep("B", nsubplayer),'I','I','I','C')
mat10=matrix(0,nrow=nsubplayer,ncol=nsubplayer)
diag(mat10)=1
numgk = length(goalkeeperindex)
numdef = length(defenderindex)
nummid = length(midfielderindex)
numfw = length(forwardindex)
conmat=rbind(
	mat10,
	c(rep(1, numgk), rep(0, numdef), rep(0, nummid), rep(0, numfw)),
	c(rep(0, numgk), rep(1, numdef), rep(0, nummid), rep(0, numfw)),
	c(rep(0, numgk), rep(0, numdef), rep(1, nummid), rep(0, numfw)),
	c(rep(0, numgk), rep(0, numdef), rep(0, nummid), rep(1, numfw)),
	subplayervalue$price
)
direction=c(rep('<=',nsubplayer),rep('==',4),'<=')
rhs=c(rep(1,nsubplayer),requiredgknum, requireddefnum, requiredmidnum, requiredfornum, remainingmoney)
sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)

### so as expected, picks out injured or unselected players...
### but it's a start - intesting results
### i think we should restrict to 80 mill, and only take 4 def/mids, assume you can always drop one

subplayervalue[which(sol$solution==1),]

if (!forceinclude) {
	idealteam = subplayervalue[which(sol$solution==1),] %>%
			select(team, player)
}
			
if (forceinclude) {
	idealteam = rbind(subplayervalue[which(sol$solution==1),] %>%
						select(team, player),
					forcedincludedf)
}
			
calculateexpectedpoint = function(playerfixtdf, myteam) {
	
	myteamfixtdf = inner_join(playerfixtdf,
								myteam,
								by = c('team', 'player'))
	myteamfixtdf[,c('selected', 'captain')] = NA
	allgameweek = unique(myteamfixtdf$gameweek)
	for (mygameweek in allgameweek) {
		print(mygameweek)
		### let's try to pick out the best team for a given week using knapsack
		submyteamfixtdf = myteamfixtdf %>%
							filter(gameweek == mygameweek)
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
		sax = with(myteamfixtdf, which(gameweek == mygameweek))
		mdum = match(with(myteamfixtdf[sax,], paste(team, player, gameweek)),
						paste(gwtotaldf$team, gwtotaldf$player, mygameweek))
		myteamfixtdf[sax,c('selected', 'captain')] = gwtotaldf[mdum, c('selected','captain')]
	}

	### then the fun bit, what are the total points?
	myteamfixtdf = myteamfixtdf %>%
					mutate(adjexpectedpoint = selected*ifelse(captain, 2*expectedpoint, expectedpoint))
	
	totalexpectedpoint = sum(myteamfixtdf$adjexpectedpoint)
	
	### worth including some stats about how often each player actually plays
	pointsummarydf = myteamfixtdf %>%
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

currentteamgameweekexpectedpoint = getcurrentexpectedpoint(playerfixtdf, currentteam)
idealteamgameweekexpectedpoint = getcurrentexpectedpoint(playerfixtdf, idealteam)

currentteamfullinfo = calculateexpectedpoint(playerfixtdf, currentteam)
idealteamfullinfo = calculateexpectedpoint(playerfixtdf, idealteam)

### then calculate the info for the rival teams as well
rivalteamdir = paste(DATAPATH,'rivalteam/',sep='')
rivalteamfile = list.files(rivalteamdir)
rivalteamname = gsub('.csv', '', rivalteamfile)

rivalteaminfo = NULL
for (j in 1:length(rivalteamname)) {
	myrivalteam = read.csv(paste(rivalteamdir,rivalteamfile[j], sep = ''))
	sax = which(!with(myrivalteam, paste(team, player)) %in% 
				with(playervalue, paste(team, player)))
	if (length(sax) > 0) {
		print(myrivalteam[sax,])
		stop('Cannot find these players in playervalue, please correct:\n')
	}
	rivalteaminfo[[j]] = calculateexpectedpoint(playerfixtdf, myrivalteam)
	names(rivalteaminfo[[j]]) = rivalteamname[j]
}
	
addextrainfo = function(fullexpectedpoint, gameweekexpectedpoint, playervalue) {

	fullexpectedpoint = left_join(fullexpectedpoint,
											gameweekexpectedpoint %>%
											select(team, player, expectedpoint),
											by = c('team', 'player')) %>%
									dplyr::rename(currentgameweek = expectedpoint)
									
	fullexpectedpoint = left_join(fullexpectedpoint,
									playervalue %>%
									select(team, player, value, valuerank),
									by = c('team', 'player'))

	return(fullexpectedpoint)
}

currentteampointsummarydf = addextrainfo(
								currentteamfullinfo$pointsummarydf,
								currentteamgameweekexpectedpoint,
								playervalue)
idealteampointsummarydf = addextrainfo(
								idealteamfullinfo$pointsummarydf,
								idealteamgameweekexpectedpoint,
								playervalue)

### next step, start taking players out and see if you can find better alternatives
cat('Current squad expected points:\n')
print(currentteampointsummarydf)
print(currentteamfullinfo$totalexpectedpoint)
cat('Expected points with ideal squad and forced inclusions/exclusions:\n')
print(idealteampointsummarydf)
print(idealteamfullinfo$totalexpectedpoint)

cat('Type \'rivalteaminfo\' to view other teams situations\n')
