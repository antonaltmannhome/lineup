source('c:/git/lineup/new-model-startup.r')

# might be worth updating the prices? not completely convneient to do that right now though, so do manually

# playerDF = ffModel:::CalculateUpToDatePlayerSmooth(gbgdf)
# NB this is a bit slow but you can get the game by game calculations this way:
# gbgdf = ffModel::CalculateHistoricExpectedMinute(gbgdf)

# this is a bit dangerous: it'll overwrite manual changes, which would be very annoying - need to think about how we do that
# ffModel:::UpdateManualActiveSpreadsheet(gbgdf, playerDF, seasonInfoDF, resultDF)

# playerDF = ffModel::ReadManualEMinFile(playerDF, resultDF)

biasedUrnOutputDF = read_csv(paste0(DATAPATH, 'active_player/biased_urn_output/playing-prob.csv'), col_types = list(
  player = col_character(),
  expectedPropGame = col_double(),
  team = col_character(),
  mainpos2 = col_character()
))
playerDF = playerDF %>%
  lazy_left_join(biasedUrnOutputDF,
                 c('team', 'player'),
                 'expectedPropGame')

fixtDF = GetFixtureGoal(resultDF, fixtDF)

# who's got a kind and tricky schedule to come:
fixtDF %>%
  filter(gameweek <= min(gameweek) + 9) %>%
  group_by(team) %>%
  summarise(sumEScored = sum(gwweight * escored),
            sumEConceded = sum(gwweight * econceded)) %>%
  arrange(desc(sumEScored - sumEConceded))

gbgdf = ProcessDeserved(gbgdf)
summaryDF = ProcessDeserved(summaryDF)

playerDF = ffModel:::CalculateLatestGoalAssistRate(playerDF, gbgdf, summaryDF, resultDF)

# might want to do this:
# source(paste0(USERPATH, 'data fetching/strip_ffprice.r')); StripFFPrice()
playerDF = ffDataJoining:::MatchFFPlayerData(playerDF)

playerFixtDF = GetPlayerFixture(fixtDF, playerDF, gbgdf)
playerFixtDF = GetFixtureExpectedPoint(playerFixtDF)
playerDF = GetPlayerValue(playerDF, playerFixtDF)

currentTeamDF = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))
forcedInclusionExclusion = read.csv(paste0(DATAPATH, 'forced-inclusion-exclusion.csv'))

## hmm, it seems by adding heavy hitters, you don't actually lose anything like the number of points you might expect
# first issue is to sort out the minutes though
inTheBank = 0.1
source('get-optimal-team.r')
