source(paste0(USERPATH, 'data fetching/soccerway-funct.r'))

resultdf = ffDataLoading:::GetResultDF()

dateToDo = FindDateToDo(resultdf)
# which one have we not yet got?

message('If you are starting a new season, PLEASE make sure you add any necessary team name adjustments to team_abbn.dat')

for (di in 1:length(dateToDo)) {
	appearanceDF = getappearanceinfofordate(dateToDo[di], resultdf)
	appearanceFileOut = paste0(DATAPATH, 'soccerway_saved/appearance-info-', dateToDo[di],'.csv')
	write.csv(x = appearanceDF, file = appearanceFileOut, row.names = FALSE)
}

### now loop through and make it a little friendlier to use

undate = unique(resultdf$date)
startTimeRanking = c(as.character(0:94), 'U', 'injury', 'suspension')
myList = NULL
for (di in 1:length(undate)) {
	appearanceFileOut = paste0(DATAPATH, 'soccerway_saved/appearance-info-', undate[di],'.csv')
	myList[[di]] = read.csv(appearanceFileOut, as.is = TRUE)
	myResultDF = resultdf %>%
					filter(date == undate[di] & isHome) %>%
					mutate(key = paste(date, team, sep = ''))
	myList[[di]] = lazy_left_join(myList[[di]], myResultDF, 'key', c('date', 'team', 'oppteam')) %>%
					rename(homeTeam = team, awayTeam = oppteam) %>%
					mutate(team = ifelse(homeStatus, homeTeam, awayTeam)) %>%
					select(date, team, playerid, player, startTime, endTime) %>%
					mutate(startTimeOrder = match(startTime, startTimeRanking)) %>%
					arrange(date, team, startTimeOrder) %>%
					select(-startTimeOrder)
	if ( (di %% 10) == 0) {
		message('Have processed ', di, ' out of ', length(undate))
	}
}

appearanceDF = bind_rows(myList)
# write unique list of players too

fileOut = paste0(DATAPATH, 'soccerway_saved/appearance.csv')
write.csv(x = appearanceDF, file = fileOut, row.names = FALSE)

fileOut = paste0(DATAPATH, 'soccerway_saved/playerid.csv')
write.csv(x = appearanceDF %>% distinct(playerid, player), file = fileOut, row.names = FALSE)
