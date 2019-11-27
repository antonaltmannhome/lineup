### let's try the knapsack approach to ff

suppressWarnings(library('Rglpk'))
playerfixtdf = getfixtureexpectedpoint(fixtdf, latestPlayerEstimate, summarydf)
mdum = match(with(playerfixtdf,paste(team,player)), with(summarydf,paste(team,player)))
playerfixtdf$price = summarydf$price[mdum]

### ok, let's try doing defenders and midfielders together

### so let's say we've got e.g 30 million to spend on defenders, let's get best set
dsax=with(playervalue,which(ffposition=='d'))
msax=with(playervalue,which(ffposition=='m'))
fsax=with(playervalue,which(ffposition=='f'))

subplayervalue=rbind(playervalue[dsax,],playervalue[msax,],playervalue[fsax,])
nsubplayer=nrow(subplayervalue)

#var.types <- c(rep("B", nsubplayer),'C')
obj=subplayervalue$epoint
var.types <- c(rep("B", nsubplayer),'I','I','I','C')
mat10=matrix(0,nrow=nsubplayer,ncol=nsubplayer)
diag(mat10)=1
conmat=rbind(
	mat10,
	c(rep(1,length(dsax)),rep(0,length(msax)),rep(0,length(fsax))),
	c(rep(0,length(dsax)),rep(1,length(msax)),rep(0,length(fsax))),
	c(rep(0,length(dsax)),rep(0,length(msax)),rep(1,length(fsax))),
	subplayervalue$price
)
direction=c(rep('<=',nsubplayer),rep('==',3),'<=')
rhs=c(rep(1,nsubplayer),5,5,3,90)
sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)

### so as expected, picks out injured or unselected players...
### but it's a start - intesting results
### i think we should restrict to 80 mill, and only take 4 def/mids, assume you can always drop one

subplayervalue[which(sol$solution==1),]

squad = subplayervalue[which(sol$solution==1),'player'] %>%
		pull(player)

calculateexpectedpoint = function(playerfixtdf, squad) {
	
	squadfixtdf = playerfixtdf %>% filter(player %in% squad)
	squadfixtdf[,c('selected', 'captain')] = NA
	allgameweek = unique(squadfixtdf$gameweek)
	for (mygameweek in allgameweek) {
		### let's try to pick out the best team for a given week using knapsack
		subsquadfixtdf = squadfixtdf %>% filter(gameweek == mygameweek)
		### one thing to watch out for, double gameweeks
		gwtotaldf = subsquadfixtdf %>%
							group_by(team,player) %>%
							summarise(ffposition = ffposition[1],
										expectedpoint = sum(expectedpoint))
		### need to insert any missing players too
		missingplayer = setdiff(squad, gwtotaldf$player)
		if (length(missingplayer) > 0) {
			missingplayerindex=match(missingplayer, squadfixtdf$player)
			missinginfo = squadfixtdf[missingplayerindex,] %>%
							select(team, player, ffposition, expectedpoint) %>%
							mutate(expectedpoint = 0)
			gwtotaldf = bind_rows(gwtotaldf, missinginfo)
		}

		nsquad=length(squad)
		#var.types <- c(rep("B", nsubplayer),'C')
		obj=gwtotaldf$expectedpoint
		var.types <- c(rep("B", nsquad),'I','I','I','C')
		mat10=matrix(0,nrow=nsquad,ncol=nsquad)
		diag(mat10)=1
		conmat=rbind(
			mat10,
			as.numeric(gwtotaldf$ffposition == 'd'),
			as.numeric(gwtotaldf$ffposition == 'f'),
			rep(1,nsquad)
			)
		direction=c(rep('<=',nsquad),rep('>=',2),'==')
		rhs=c(rep(1,nsquad),3,1,10)
		sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)
		gwtotaldf$selected = sol$solution
		gwtotaldf$captain = gwtotaldf$expectedpoint == max(gwtotaldf$expectedpoint)
		sax = with(squadfixtdf, which(gameweek == mygameweek))
		mdum = match(with(squadfixtdf[sax,], paste(player, gameweek)),
						paste(gwtotaldf$player, mygameweek))
		squadfixtdf[sax,c('selected', 'captain')] = gwtotaldf[mdum, c('selected','captain')]
	}

	### then the fun bit, what are the total points?
	squadfixtdf = squadfixtdf %>%
					mutate(adjexpectedpoint = selected*ifelse(captain, 2*expectedpoint, expectedpoint))
	
	totalexpectedpoint = sum(squadfixtdf$adjexpectedpoint)
	return(totalexpectedpoint)
}

### next step, start taking players out and see if you can find better alternatives
