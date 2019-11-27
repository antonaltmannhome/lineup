# chip_inv.r is a messy little file, let's try to get something that isn't separate from everything else and uses knapsack-funct.r

# the idea is it lets you try various combinations of which week to WC, which week to free hit
source('knapsack_funct.r')


GetWCFHExpectedPoint = function(currentteam, WCWeek, FHWeek, FHForcedInclusionExclusion, forcedInclusionExclusion) {
	
	# step 1, calculate expected points in the weeks prior to the wild card
	nextGW = min(fixtdf$gameweek)
	if (WCWeek == nextGW) {
		preWCNotFHTotalPoint = 0
	}
	if (WCWeek > nextGW) {
		preWCNotFHWeek = setdiff(nextGW:(WCWeek - 1), FHWeek)
		subFixtDF = fixtdf %>%
					filter(gameweek %in% preWCNotFHWeek) %>%
					mutate(gwweight = 1) # we want total points absolutely, not relative to other potential teams (as with usual optimisation)
		subPlayerFixtDF = getfixtureexpectedpoint(subFixtDF, playerdf, summarydf, gbgdf)

		dum = calculateexpectedpoint(subPlayerFixtDF, currentteam)
		preWCNotFHPlayerPointDF = dum$pointsummarydf
		preWCNotFHTotalPoint = dum$totalexpectedpoint
	}
	
	# then the free hit
	subFixtDF = fixtdf %>%
				filter(gameweek == FHWeek) %>%
				mutate(gwweight = 1)
	subPlayerValue=getplayervalue(subFixtDF, playerdf, summarydf, gbgdf)

	subPlayerFixtDF = getfixtureexpectedpoint(subFixtDF, playerdf, summarydf, gbgdf)

	FHTeam = RunKnapsack(subPlayerValue, FHForcedInclusionExclusion, currentmoney)
		
	dum = calculateexpectedpoint(subPlayerFixtDF, FHTeam)
	FHPlayerPointDF = dum$pointsummarydf
	FHTotalPoint = dum$totalexpectedpoint

	postWCNotFHWeek = setdiff(WCWeek:38, FHWeek)
	subFixtDF = fixtdf %>% filter(gameweek %in% postWCNotFHWeek) %>%
					mutate(gwweight = 1)
	subPlayerValue=getplayervalue(subFixtDF, playerdf, summarydf, gbgdf)

	subPlayerFixtDF = getfixtureexpectedpoint(subFixtDF, playerdf, summarydf, gbgdf)

	postWCNotFHTeam = RunKnapsack(subPlayerValue, forcedInclusionExclusion, currentmoney)
		
	dum = calculateexpectedpoint(subPlayerFixtDF, postWCNotFHTeam)
	postWCNotFHPlayerPointDF = dum$pointsummarydf
	postWCNotFHTotalPoint = dum$totalexpectedpoint
	
	print('free hit team:')
	print(FHPlayerPointDF)
	
	print('post-wild card team:')
	print(postWCNotFHPlayerPointDF)
	
	print('points summary:')
	message('pre-wild-card points: ', preWCNotFHTotalPoint)
	message('free hit points: ', FHTotalPoint)
	message('post-wild-card points: ', postWCNotFHTotalPoint)
	
	message('total points: ', preWCNotFHTotalPoint + FHTotalPoint + postWCNotFHTotalPoint)
}


FHForcedInclusionExclusion = read.csv(paste0(DATAPATH, 'free-hit-forced-inclusion-exclusion.csv'))

### so in 1819 season, do FH33, WC34 (rubbish idea but let's just try it:
GetWCFHExpectedPoint(currentteam, 34, 33, FHForcedInclusionExclusion, forcedInclusionExclusion)
pre-wild-card points: 82.9108867999734
free hit points: 59.2580383862963
post-wild-card points: 347.735353090156
total points: 489.904278276426

## or, FH 32, WC34:
GetWCFHExpectedPoint(currentteam, 34, 32, FHForcedInclusionExclusion, forcedInclusionExclusion)
pre-wild-card points: 54.8251381929024
free hit points: 115.974346854238
post-wild-card points: 347.735353090156
total points: 518.534838137296
# forcibly include aguero in the free hit:
total points: 522.541249499098
# forcibly include kane in the free hit:
total points: 517.321920014683
# forcibly include sterling in the free hit:
total points: 518.171111231521

## or, WC32, FH33:
GetWCFHExpectedPoint(currentteam, 32, 33, FHForcedInclusionExclusion, forcedInclusionExclusion)
pre-wild-card points: 0
free hit points: 59.2580383862963
post-wild-card points: 452.507623700725
total points: 511.765662087021
# so don't do that

### NB ought to add bench boost to this but there's no flexibility about when to use it this year, so leave that til next season
