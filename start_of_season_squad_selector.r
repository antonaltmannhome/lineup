### the process for selecting players for the first few weeks is a little different to the rest of the season. high chance of playing wildcard very soon, so should have different downweight to normal. also need to tinker with active players quite a lot, this file makes that workflow easier

if (FALSE) {
	source('c:/research/lineup/ff_startup.r')
	source('model_startup.r')

	source('get_player_estimate.r')

	suppressWarnings(library('Rglpk'))

}

playerfixtdf = getfixtureexpectedpoint(fixtdf, playerdf, latestPlayerEstimate, summarydf, gbgdf)

gwweightdf = tibble(gameweek = 1:38)
gwweightdf$gwweight = c(1, 0.5, 0.25, rep(0,35))

playerfixtdf$gwweight = gwweightdf$gwweight[match(playerfixtdf$gameweek, gwweightdf$gameweek)]

playervalue=playerfixtdf %>%
					group_by(ffteam, ffplayer, team, player, ffposition, price) %>%
					summarise(epoint90min=sum(expectedpoint * gwweight)) %>%
					ungroup()
	
playervalue = addplayeractivecolumn(playervalue)
playervalue$epoint=with(playervalue,epoint90min*active)

playervalue$value=with(playervalue,round(epoint/price,2))
playervalue=playervalue %>%
					group_by(ffposition) %>%
					mutate(valuerank=rank(-value)) %>%
					ungroup()

playervalue = playervalue %>%
				arrange(team, match(ffposition, c('g', 'd', 'm', 'f')), -epoint)

currentmoney = 100

### so let's say we've got e.g 30 million to spend on defenders, let's get best set

# quick fudge, get rid of NAs
playervalue = playervalue %>%
				filter(!is.na(epoint))

goalkeeperindex=with(playervalue,which(ffposition=='g'))
defenderindex=with(playervalue,which(ffposition=='d'))
midfielderindex=with(playervalue,which(ffposition=='m'))
forwardindex=with(playervalue,which(ffposition=='f'))

remainingmoney = currentmoney
requiredgknum = 2
requireddefnum = 5
requiredmidnum = 5
requiredfornum = 3

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

### so as expected, picks out injured or unselected players...
### but it's a start - intesting results
### i think we should restrict to 80 mill, and only take 4 def/mids, assume you can always drop one

optimalsquad = subplayervalue[which(sol$solution==1),]

print(optimalsquad)

currentplayerfixtepoint = playerfixtdf %>%
							filter(gameweek == 1) %>%
							filter(paste(team, player) %in% with(optimalsquad, paste(team, player))) %>%
							select(team, player, expectedpoint, ffposition) %>%
							arrange(match(ffposition, c('g', 'd', 'm', 'f')), expectedpoint)

print(currentplayerfixtepoint)
