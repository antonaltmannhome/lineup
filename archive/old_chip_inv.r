### let's try the knapsack approach to ff
### but now let's force a certain player e.g aguero into the squad and see what happens to overall actual expected points

source('c:/research/lineup/ff_startup.r')
source('c:/research/lineup/model_startup.r')
suppressWarnings(library('Rglpk'))
currentmoney = 100

remainingmoney = currentmoney

requiredgknum = 2
requireddefnum = 5
requiredmidnum = 5
requiredfornum = 3
source('get_player_estimate.r')

### then update all the players if you have time

nextgw = min(fixtdf$gameweek)

freehitchoice = function(freehitweek) {

	for (situation in c('freehitweek', 'otherweeks')) {

		if (situation == 'freehitweek') {
			situationfixtdf = fixtdf %>% filter(gameweek == freehitweek)
		}
		if (situation == 'otherweeks') {
			situationfixtdf = fixtdf %>% filter(gameweek != freehitweek)
		}
		if (situation == 'freehitweek') {
			freehitpointdf = subplayervalue[which(sol$solution==1),]
		}
		if (situation == 'otherweeks') {
			otherweekspointdf = subplayervalue[which(sol$solution==1),]
		}
	}
	
	totalpoint = sum(freehitpointdf$epoint) + sum(otherweekspointdf$epoint)
	
	return(list(freehitpointdf = freehitpointdf,
				otherweekspointdf = otherweekspointdf,
				totalpoint = totalpoint))

getcurrentteamfixtpoint = function(currentteam, weeksubset) {
	situationfixtdf = fixtdf %>% filter(gameweek %in% weeksubset)
	### and then we're good to get the expected player points and values
	playervalue=getplayervalue(situationfixtdf, latestPlayerEstimate, summarydf, gbgdf)

	subplayervalue = inner_join(playervalue,
								currentteam %>%
								select(team, player),
								by = c('team', 'player'))
								
	totalpoint = sum(subplayervalue$epoint)
}

getoptteam = function(weeksubset) {
	
	situationfixtdf = fixtdf %>% filter(gameweek %in% weeksubset)
	### and then we're good to get the expected player points and values
	playervalue=getplayervalue(situationfixtdf, latestPlayerEstimate, summarydf, gbgdf)

	# but teams with no fixtures will be NA so get rid
	playervalue = playervalue %>%
					filter(!is.na(epoint90min))

	### so let's say we've got e.g 30 million to spend on defenders, let's get best set
	goalkeeperindex=with(playervalue,which(ffposition=='g'))
	defenderindex=with(playervalue,which(ffposition=='d'))
	midfielderindex=with(playervalue,which(ffposition=='m'))
	forwardindex=with(playervalue,which(ffposition=='f'))

	subplayervalue=rbind(playervalue[goalkeeperindex,],
						playervalue[defenderindex,],
						playervalue[midfielderindex,],
						playervalue[forwardindex,])
							
	
	nsubplayer=nrow(subplayervalue)
	subunteam = unique(subplayervalue$team)

	#var.types <- c(rep("B", nsubplayer),'C')
	obj=subplayervalue$epoint
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

	teammat = sapply(subplayervalue$team, function(x) as.numeric(subunteam == x))

	conmat=rbind(
		mat10,
		posmat,
		teammat,
		subplayervalue$price
	)
	direction=c(rep('<=',nsubplayer),rep('==',4),rep('<=', length(subunteam)), '<=')
	rhs=c(rep(1,nsubplayer),requiredgknum, requireddefnum, requiredmidnum, requiredfornum, rep (3, length(subunteam)), remainingmoney)
	sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)

	optteamdf = subplayervalue[which(sol$solution==1),]

	return(optteamdf)
}

### but, fixtures aren't up to date yet. But seems to be doing the right thing. DOesn't seem to have a problem with gameweeks downweighting but worth checking
# now let's loop over all possible combinations of free chips and wildcards
wildhitdf = expand.grid(wcweek = nextgw:38,
						freehitweek = nextgw:38) %>%
			filter(wcweek != freehitweek) %>%
			mutate(prewcpoint = NA,
					freehitpoint = NA,
					postwcpoint = NA)

currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))

for (j in 1:nrow(wildhitdf)) {
	if (wildhitdf$wcweek[j] == nextgw) wildhitdf$prewcpoint[j] = 0
	if (wildhitdf$wcweek[j] > nextgw) {
		currentteamweek = with(wildhitdf[j,], setdiff(nextgw:(wcweek - 1), freehitweek))
		if (length(currentteamweek) == 0) wildhitdf$prewcpoint[j] = 0
		if (length(currentteamweek) > 0) {
			wildhitdf$prewcpoint[j] = getcurrentteamfixtpoint(currentteam, currentteamweek)
		}
	}
	dum = getoptteam(wildhitdf$freehitweek[j])
	wildhitdf$freehitpoint[j] = sum(dum$epoint)
	postwcweek = with(wildhitdf[j,], setdiff(wcweek:38, freehitweek))
	dum = getoptteam(postwcweek)
	wildhitdf$postwcpoint[j] = sum(dum$epoint)
	if ( (j %% 10) == 0) cat('Have calculated',j,'out of',nrow(wildhitdf),'so far\n')
}

wildhitdf$totalpoint = with(wildhitdf, prewcpoint + freehitpoint + postwcpoint)

# so play wc straight away, really?
# but need to add the missing fixtures, plus could do the selecting and captaining as well, improtant for double gws

### have played the free hit, now time to see when best wc week is
wilddf = tibble(wcweek = nextgw:38) %>%
			mutate(prewcpoint = NA,
					postwcpoint = NA)

currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))

for (j in 1:nrow(wilddf)) {
	if (wilddf$wcweek[j] == nextgw) wilddf$prewcpoint[j] = 0
	if (wilddf$wcweek[j] > nextgw) {
		currentteamweek = with(wilddf[j,], nextgw:(wcweek - 1))
		if (length(currentteamweek) == 0) wildhitdf$prewcpoint[j] = 0
		if (length(currentteamweek) > 0) {
			wilddf$prewcpoint[j] = getcurrentteamfixtpoint(currentteam, currentteamweek)
		}
	}
	postwcweek = with(wilddf[j,], wcweek:38)
	dum = getoptteam(postwcweek)
	wilddf$postwcpoint[j] = sum(dum$epoint)
}

wilddf$totalpoint = with(wilddf, prewcpoint + postwcpoint)
# heavily favouring an early wc. but it is optimistic about how steady everything will be in terms of who starts/injuries etc
