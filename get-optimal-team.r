### let's try the knapsack approach to ff
### but now let's force a certain player e.g aguero into the squad and see what happens to overall actual expected points

source('knapsack_funct.r')

if (FALSE) {
	playerfixtdf = getfixtureexpectedpoint(fixtdf, playerdf, summarydf, gbgdf)
	mdum = match(with(playerfixtdf,paste(team,player)), with(summarydf,paste(team,player)))
	playerfixtdf$price = summarydf$price[mdum]
	### filter out players who have no active position or are goalies
	playerfixtdf = playerfixtdf %>%
				filter(!is.na(expectedpoint))
}
### ok, let's try doing defenders and midfielders together

idealteam = RunKnapsack(playerDF, forcedInclusionExclusion, currentmoney)

currentteamgameweekexpectedpoint = getcurrentexpectedpoint(playerfixtdf, currentteam)
idealteamgameweekexpectedpoint = getcurrentexpectedpoint(playerfixtdf, idealteam)

currentteamfullinfo = calculateexpectedpoint(playerfixtdf, currentteam)
idealteamfullinfo = calculateexpectedpoint(playerfixtdf, idealteam)

if (FALSE) {
### then calculate the info for the rival teams as well
rivalteamdir = paste(DATAPATH,'rivalteam/',sep='')
rivalteamfile = list.files(rivalteamdir)
rivalteamname = gsub('.csv', '', rivalteamfile)

rivalteaminfo = NULL
for (j in 1:length(rivalteamname)) {
	print(j)
	myrivalteam = read.csv(paste(rivalteamdir,rivalteamfile[j], sep = ''))
	sax = which(!with(myrivalteam, paste(team, player)) %in%
				with(playervalue, paste(team, player)[!is.na(price)]))
	if (length(sax) > 0) {
		print(myrivalteam[sax,])
		stop('Cannot find these players in playervalue, please correct:\n')
	}
	rivalteaminfo[[j]] = calculateexpectedpoint(playerfixtdf, myrivalteam)
	myrivalteamgameweekexpectedpoint = getcurrentexpectedpoint(playerfixtdf, myrivalteam)
	rivalteaminfo[[j]]$pointsummarydf = addextrainfo(rivalteaminfo[[j]]$pointsummarydf,
										myrivalteamgameweekexpectedpoint,
										playervalue)
	names(rivalteaminfo[[j]]) = rivalteamname[j]
}
}
currentteampointsummarydf = addextrainfo(
								currentteamfullinfo$pointsummarydf,
								currentteamgameweekexpectedpoint,
								playerDF)
idealteampointsummarydf = addextrainfo(
								idealteamfullinfo$pointsummarydf,
								idealteamgameweekexpectedpoint,
								playerDF)

### next step, start taking players out and see if you can find better alternatives
cat('Current squad expected points:\n')
print(currentteampointsummarydf)
print(currentteamfullinfo$totalexpectedpoint)
cat('Expected points with ideal squad and forced inclusions/exclusions:\n')
print(idealteampointsummarydf)
print(idealteamfullinfo$totalexpectedpoint)

cat('Type \'rivalteaminfo\' to view other teams situations\n')

message('ADvice for current week:')
# currentteampointsummarydf %>% ungroup() %>% select(player, ffPosition, currentgameweek, eMin) %>% arrange(desc(currentgameweek))
print(currentteampointsummarydf %>% mutate(adj94currentgameweek = 94/eMin * currentgameweek) %>% select(team, player, currentgameweek, eMin, adj94currentgameweek))
