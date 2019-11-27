source('c:/research/lineup/ff_startup.r')
source('c:/research/lineup/appearance model/appearance-model-funct.r')
fileIn = paste0(DATAPATH, 'soccerway_saved/appearance.csv')
appearanceDF = read.csv(fileIn, as.is = TRUE)

resultdf = getresultdf()
seasondf = resultdf %>% distinct(season) %>% arrange(season) %>% mutate(seasonNumber = 1:n())
resultdf = left_join(resultdf, seasondf, 'season')

appearanceDF = lazy_left_join(appearanceDF,
								resultdf,
								c('date', 'team'),
								c('seasonNumber', 'teamgamenumber'))

appearanceDF = FillMissingAppearanceDF(appearanceDF, resultdf)
# but let's just have a integer for the season number, not the 1718 type thing

								
# all set. just need to model the damn thing now

unSeasonNumber = unique(appearanceDF$seasonNumber)
numGameBySeason = resultdf %>%
				group_by(seasonNumber) %>%
				summarise(numGame = max(teamgamenumber))

appearanceDF$played = with(appearanceDF, !startTime %in% c('injury', 'suspension', 'U', 'UU'))
appearanceDF$minute = 0
appearanceDF$minute[appearanceDF$played] = with(appearanceDF, as.numeric(endTime[played]) - as.numeric(startTime[played]))

appearanceDF = appearanceDF %>%
				group_by(team, player) %>%
				arrange(seasonNumber, teamgamenumber) %>%
				mutate(gameForTeamNumber = 1:n()) %>%
				ungroup()
