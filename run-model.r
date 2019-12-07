source('c:/git/lineup/new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

# might be worth updating the prices? not completely convneient to do that right now though, so do manually

playerDF = ffModel:::CalculateUpToDatePlayerSmooth(gbgdf)
# NB this is a bit slow but you can get the game by game calculations this way:
# gbgdf = CalculateHistoricExpectedMinute(gbgdf)

fixtdf = getfixturegoal(resultdf, fixtdf)

gbgdf = processdeserved(gbgdf)
summarydf=processdeserved(summarydf)

playerDF = ffModel:::CalculateLatestGoalAssistRate(playerDF, gbgdf, summarydf, resultDF)

playerfixtdf = getplayerfixture(fixtdf, playerDF, gbgdf)
playerfixtdf = getfixtureexpectedpoint(playerfixtdf)
playerDF = getplayervalue(playerDF, playerfixtdf)

currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))
forcedInclusionExclusion = read.csv(paste0(DATAPATH, 'forced-inclusion-exclusion.csv'))

## hmm, it seems by adding heavy hitters, you don't actually lose anything like the number of points you might expect
# first issue is to sort out the minutes though
currentmoney = 99.6
source('get-optimal-team.r')
