### let's have a separate file to do the data update
source('c:/git/lineup/ff_startup.r')

seasoninfo = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''))
currentseason = 1920

### firstly, get the latest player stats
source(paste(USERPATH, 'data fetching/strip_whoscored.r',sep=''))

### then get hold of overall results
source(paste(USERPATH, 'data fetching/strip_fixture_result.r',sep=''))

### then get hold of overall results
source(paste(USERPATH, 'data fetching/strip-soccerway.r',sep=''))

## then the latest ff prices

source(paste0(USERPATH, 'data fetching/strip_ffprice.r'))
StripFFPrice()

source(paste0(USERPATH, 'data fetching/strip-current-team-price.r'))
NavigateAndSaveTransferPage()
GetCleanCurrentTeamPriceDF()

## finally fill in the odds
source(paste0(USERPATH, 'data fetching/manual_strip_spreadex.r'))

### ok, we've got all the raw data. now got to join the buggers all up

## step 1, convert the cumulative whoscored data into match by match:
