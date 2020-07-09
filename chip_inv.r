# chip_inv.r is a messy little file, let's try to get something that isn't separate from everything else and uses knapsack-funct.r

# the idea is it lets you try various combinations of which week to WC, which week to free hit
source('c:/git/lineup/new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

# might be worth updating the prices? not completely convneient to do that right now though, so do manually

playerDF = ffModel:::CalculateUpToDatePlayerSmooth(gbgdf)
# NB this is a bit slow but you can get the game by game calculations this way:
# gbgdf = ffModel::CalculateHistoricExpectedMinute(gbgdf)

# this is a bit dangerous: it'll overwrite manual changes, which would be very annoying - need to think about how we do that
# ffModel:::UpdateManualActiveSpreadsheet(gbgdf, playerDF, seasoninfo, resultdf)

playerDF = ffModel::ReadManualEMinFile(playerDF, resultDF)

fixtDF = getfixturegoal(resultDF, fixtDF)

# who's got a kind and tricky schedule to come:
fixtDF %>%
  filter(gameweek <= min(gameweek) + 9) %>%
  group_by(team) %>%
  summarise(sumEScored = sum(gwweight * escored),
            sumEConceded = sum(gwweight * econceded)) %>%
  arrange(desc(sumEScored - sumEConceded))

gbgdf = processdeserved(gbgdf)
summaryDF=processdeserved(summaryDF)

playerDF = ffModel:::CalculateLatestGoalAssistRate(playerDF, gbgdf, summaryDF, resultDF)

# might want to do this:
# source(paste0(USERPATH, 'data fetching/strip_ffprice.r')); StripFFPrice()
playerDF = ffDataJoining:::MatchFFPlayerData(playerDF)

playerfixtDF = getplayerfixture(fixtDF, playerDF, gbgdf)
playerfixtDF = getfixtureexpectedpoint(playerfixtDF)
source('knapsack_funct.r')

currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))
forcedInclusionExclusion = read.csv(paste0(DATAPATH, 'forced-inclusion-exclusion.csv'))

FHForcedInclusionExclusion = read.csv(paste0(DATAPATH, 'free-hit-forced-inclusion-exclusion.csv'))
currentmoney = 97.7

GetWCFHExpectedPoint = function(currentteam, WCWeek, FHWeek, BBWeek, FHForcedInclusionExclusion, forcedInclusionExclusion) {
	
	# step 1, calculate expected points in the weeks prior to the wild card
	nextGW = min(fixtDF$gameweek)
	if (WCWeek == nextGW | WCWeek > 38) {
		preWCNotFHTotalPoint = 0
	}
	if (WCWeek > nextGW & WCWeek <= 38) {
		preWCNotFHWeek = setdiff(nextGW:(WCWeek - 1), FHWeek)
		subplayerfixtDF = playerfixtDF %>%
		  filter(gameweek %in% preWCNotFHWeek) %>%
		  mutate(gwweight = 1)

		dum = calculateexpectedpoint(subplayerfixtDF, currentteam, BBWeek, warnAboutMissingPlayer = FALSE)
		preWCNotFHPlayerPointDF = dum$pointsummarydf
		preWCNotFHTotalPoint = dum$totalexpectedpoint
	}
	
	if (FHWeek > 38) {
	  FHTotalPoint = 0
	}
	if (FHWeek <=38) {
	  # then the free hit
	  subplayerfixtDF = playerfixtDF %>%
	    filter(gameweek == FHWeek) %>%
	    mutate(gwweight = 1)
	  subPlayerValue=getplayervalue(playerDF, subplayerfixtDF)
	  
	  FHTeam = RunKnapsack(subPlayerValue, FHForcedInclusionExclusion, currentmoney)
	  
	  dum = calculateexpectedpoint(subplayerfixtDF, FHTeam, warnAboutMissingPlayer = FALSE)
	  FHPlayerPointDF = dum$pointsummarydf
	  FHTotalPoint = dum$totalexpectedpoint
	}
	
	if (WCWeek > 38) {
	  postWCNotFHWeek = setdiff(nextGW:38, FHWeek)
	}
	if (WCWeek <=38) {
  	postWCNotFHWeek = setdiff(WCWeek:38, FHWeek)
	}
	subplayerfixtDF = playerfixtDF %>%
	  filter(gameweek %in% postWCNotFHWeek) %>%
		mutate(gwweight = 1)
	subPlayerValue=getplayervalue(playerDF, subplayerfixtDF)

	postWCNotFHTeam = RunKnapsack(subPlayerValue, forcedInclusionExclusion, currentmoney)
		
	dum = calculateexpectedpoint(subplayerfixtDF, postWCNotFHTeam, BBWeek, warnAboutMissingPlayer = FALSE)
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
	
	totalPoint = preWCNotFHTotalPoint + FHTotalPoint + postWCNotFHTotalPoint
	message('total points: ', totalPoint)
	return(totalPoint)
}

### post corona, have DGW in first week, plus free wild card. so bench boost it and WC on the next week
GetWCFHExpectedPoint(currentteam, 31, 30, 30, FHForcedInclusionExclusion, forcedInclusionExclusion)

GetWCFHExpectedPoint(currentteam, 34, 33, 35, FHForcedInclusionExclusion, forcedInclusionExclusion)
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

# try everything out in remainder of season:

remainingCombo = expand_grid(WCWeek = 39, # already played it
                             FHWeek = min(fixtDF$gameweek):38,
                             BBWeek = min(fixtDF$gameweek):38) %>%
  filter(FHWeek != BBWeek) %>%
  mutate(totalPoint = NA)

for (j in 1:nrow(remainingCombo)) {
  remainingCombo$totalPoint[j] = with(remainingCombo[j,], 
    GetWCFHExpectedPoint(currentteam, WCWeek = WCWeek, FHWeek = FHWeek, BBWeek = BBWeek,
                         FHForcedInclusionExclusion, forcedInclusionExclusion))
}
