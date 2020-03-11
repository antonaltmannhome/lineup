# chip_inv.r is a messy little file, let's try to get something that isn't separate from everything else and uses knapsack-funct.r

# the idea is it lets you try various combinations of which week to WC, which week to free hit
source('knapsack_funct.r')


GetWCFHExpectedPoint = function(currentteam, WCWeek, FHWeek, BBWeek, FHForcedInclusionExclusion, forcedInclusionExclusion) {
	
	# step 1, calculate expected points in the weeks prior to the wild card
	nextGW = min(fixtdf$gameweek)
	if (WCWeek == nextGW) {
		preWCNotFHTotalPoint = 0
	}
	if (WCWeek > nextGW) {
		preWCNotFHWeek = setdiff(nextGW:(WCWeek - 1), FHWeek)
		subPlayerFixtDF = playerfixtdf %>%
		  filter(gameweek %in% preWCNotFHWeek) %>%
		  mutate(gwweight = 1)

		dum = calculateexpectedpoint(subPlayerFixtDF, currentteam, BBWeek, warnAboutMissingPlayer = FALSE)
		preWCNotFHPlayerPointDF = dum$pointsummarydf
		preWCNotFHTotalPoint = dum$totalexpectedpoint
	}
	
	# then the free hit
	subPlayerFixtDF = playerfixtdf %>%
				filter(gameweek == FHWeek) %>%
				mutate(gwweight = 1)
	subPlayerValue=getplayervalue(playerDF, subPlayerFixtDF)

	FHTeam = RunKnapsack(subPlayerValue, FHForcedInclusionExclusion, currentmoney)
		
	dum = calculateexpectedpoint(subPlayerFixtDF, FHTeam, warnAboutMissingPlayer = FALSE)
	FHPlayerPointDF = dum$pointsummarydf
	FHTotalPoint = dum$totalexpectedpoint

	postWCNotFHWeek = setdiff(WCWeek:38, FHWeek)
	subPlayerFixtDF = playerfixtdf %>%
	  filter(gameweek %in% postWCNotFHWeek) %>%
		mutate(gwweight = 1)
	subPlayerValue=getplayervalue(playerDF, subPlayerFixtDF)

	postWCNotFHTeam = RunKnapsack(subPlayerValue, forcedInclusionExclusion, currentmoney)
		
	dum = calculateexpectedpoint(subPlayerFixtDF, postWCNotFHTeam, BBWeek, warnAboutMissingPlayer = FALSE)
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

### so in 1920 season, do FH33, WC34 (rubbish idea but let's just try it):

GetWCFHExpectedPoint(currentteam, 34, 33, FHForcedInclusionExclusion, forcedInclusionExclusion)
pre-wild-card points: 152.477098540508
free hit points: 66.3594194591453
post-wild-card points: 396.795686043531
total points: 615.632204043184

## or, FH 31, WC34:
GetWCFHExpectedPoint(currentteam, 34, 31, FHForcedInclusionExclusion, forcedInclusionExclusion)
pre-wild-card points: 166.849691223368
free hit points: 60.7208693947299
post-wild-card points: 396.795686043531
total points: 624.36624666163

# all nonsense for now, need to wait til fixtures are decided.
# but need to add bench boost possibility - not sure how easy that actually is though - think it might be possible to do if you don't try to optimise with it, just add in afterwards
