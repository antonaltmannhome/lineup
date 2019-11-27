currentgameweek = min(fixtdf$gameweek)
fixtdf2 = fixtdf %>% filter(gameweek > currentgameweek)
playerfixtdf2 = getfixtureexpectedpoint(fixtdf2, playerdf, summarydf, gbgdf)

# but need to recalculate weight
fixtdf2$gwweight = 0
next10weekindex = with(fixtdf, which(between(gameweek, currentgameweek, currentgameweek + 9)))
fixtdf2$gwweight[next10weekindex] = seq(1,0.1,-0.1)[fixtdf2$gameweek[next10weekindex]-currentgameweek]
playervalue=getplayervalue(fixtdf2, playerdf, summarydf, gbgdf)
idealteam = RunKnapsack(playervalue)

message('Summary of value of current team:')
inner_join(currentteam %>% select(team, player), playervalue, c('team','player'))

currentteamfullinfo2 = calculateexpectedpoint(playerfixtdf2, currentteam)
idealteamfullinfo2 = calculateexpectedpoint(playerfixtdf2, idealteam)

print(currentteamfullinfo2)
print(idealteamfullinfo2)
