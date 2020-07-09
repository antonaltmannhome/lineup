### file that runs through the standard procedure for each week
source('c:/research/lineup/ff_startup.r')

### firstly, get the latest player stats
source(paste(USERPATH,'data code/strip_whoscored.r',sep=''))

### then get hold of overall results
source(paste(USERPATH,'strip_fixture_result.r',sep=''))

resultdf = getresultdf()
makegbgdf(resultdf)

### next we want odds for upcoming fixtures
#oddscheckerBrowser = 'Firefox'

StripFFPrice()

source('soccerway/get-appearance-data.r')
# now we want to get everything using the same player names. soccerway has ids which is fantastic
source('data code/sort-player-name.r')
source('data code/match-ff-price.r')

# now we can compile all of the players for this season along with their prices


### update list of active players

source('model_startup.r')
getstartingplayer(gbgdf, summarydf, playerdf)
source(paste(USERPATH,'manual_strip_spreadex.r',sep=''))

#######################################################################
####### or just do this if you want the latest player abilities #######
#######################################################################

source('c:/research/lineup/ff_startup.r')
source('model_startup.r')

source('get_player_estimate.r')

### then update all the players if you have time

### and then we're good to get the expected player points and values
playervalue=getplayervalue(fixtdf, playerdf, summarydf, gbgdf)
playerfixtdf = getfixtureexpectedpoint(fixtdf, playerdf, summarydf, gbgdf)
currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))

forcedInclusionExclusion = read.csv(paste0(DATAPATH, 'forced-inclusion-exclusion.csv'))

currentmoney = 100.1
source('get-optimal-team.r')

getcurrentexpectedpoint(playerfixtdf, currentteam)
