### quite complicated process to initialise the player_id data base, so let's make dedicated file to do and potentially modify down the line

### current spec is this:
### have column for:
# ffname (NA for non-current players)
# ffteam (NA for non-current players)
# whoscored default name (can have dups)
# whoscored current team
# definitive id (no dups - this should be what you see in gbgdf, summarydf etc)

### not sure how we go from whoscored eefault name to definitive id eg. realising there are two different john smiths in order to create johnsmith2. don't think it's happened yet so maybe worry about it if it seems to have happened

### so, let's initialise the file. before the season, i think this is the best way to get the definitive player list:

if (FALSE) {
playeridfilename = paste0(DATAPATH, 'playerid.csv')

dum = makeseasondeservedsummary(summarydf, gbgdf)
seasondeservedsummary = dum$seasondeservedsummary

playerdf = seasondeservedsummary %>%
				group_by(player) %>%
				arrange(season) %>%
				summarise(whoscoredname = tail(player, 1),
						playerid = tail(player, 1),
						whoscoredlatestteam = tail(team, 1),
						probablelatestteam = whoscoredlatestteam,
						ffuseswholename = FALSE,
						adjustedwhoscoredname = 'none') %>%
				select(-player) %>%
				arrange(probablelatestteam)
# now write that to disk, and modify as required
write.csv(file = playeridfilename, playerdf, row.names= FALSE)
}
