source('c:/git/lineup/new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

# might be worth updating the prices? not completely convneient to do that right now though, so do manually

playerDF = ffModel:::CalculateUpToDatePlayerSmooth(gbgdf)
# NB this is a bit slow but you can get the game by game calculations this way:
# gbgdf = ffModel::CalculateHistoricExpectedMinute(gbgdf)

# this is a bit dangerous: it'll overwrite manual changes, which would be very annoying - need to think about how we do that
# ffModel:::UpdateManualActiveSpreadsheet(gbgdf, playerDF, seasoninfo, resultdf)

playerDF = ffModel::ReadManualEMinFile(playerDF, resultdf)

fixtdf = getfixturegoal(resultdf, fixtdf)

# who's got a kind and tricky schedule to come:
fixtdf %>%
  filter(gameweek <= min(gameweek) + 9) %>%
  group_by(team) %>%
  summarise(sumEScored = sum(gwweight * escored),
            sumEConceded = sum(gwweight * econceded)) %>%
  arrange(desc(sumEScored - sumEConceded))

gbgdf = processdeserved(gbgdf)
summarydf=processdeserved(summarydf)

playerDF = ffModel:::CalculateLatestGoalAssistRate(playerDF, gbgdf, summarydf, resultDF)

# might want to do this:
# source(paste0(USERPATH, 'data fetching/strip_ffprice.r')); StripFFPrice()
playerDF = ffDataJoining:::MatchFFPlayerData(playerDF)

playerfixtdf = getplayerfixture(fixtdf, playerDF, gbgdf)
playerfixtdf = getfixtureexpectedpoint(playerfixtdf)
playerDF = getplayervalue(playerDF, playerfixtdf)

currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))
forcedInclusionExclusion = read.csv(paste0(DATAPATH, 'forced-inclusion-exclusion.csv'))

## hmm, it seems by adding heavy hitters, you don't actually lose anything like the number of points you might expect
# first issue is to sort out the minutes though
currentmoney = 97.9
source('get-optimal-team.r')
