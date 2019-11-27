# have the job of joining together the soccerway data and the whoscored data
# but maybe we can do this automatically by seeing who played for same team in same games

# so, let's get hold of the game by game data for both

source('c:/research/lineup/appearance model/appearance-model-startup.r')

resultdf = getresultdf()
fixtdf = getfixtdf()

dum = matchupspreadexdata(resultdf, fixtdf)
resultdf = dum$resultdf
fixtdf = dum$fixtdf

gbgdf=getgbgdf(resultdf)

gbgdf$wholeGame = with(gbgdf, near(minute, 90))
appearanceDF$wholeGame = with(appearanceDF, startTime == '0' & endTime == '94')

appearanceDF$key = with(appearanceDF, paste(date, team, wholeGame))
gbgdf$key = with(gbgdf, paste(date, team, wholeGame))

appearanceByPlayerKey = appearanceDF %>%
						filter(wholeGame) %>%
						group_by(team, player) %>%
						summarise(key = paste(date, collapse = ',')) %>%
						ungroup()

gbgByPlayerKey = gbgdf %>%
					filter(wholeGame) %>%
					group_by(team, player) %>%
					summarise(key = paste(date, collapse = ',')) %>%
						ungroup()

### right, can we match them?

appearanceByPlayerKey$WSMatch = gbgByPlayerKey$player[match(with(appearanceByPlayerKey, paste(team, key)),
														with(gbgByPlayerKey, paste(team, key)))]


# fairly low matching rate, might as well do manually, this is too complicated if it's not 100% effective

### 