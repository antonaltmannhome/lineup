### right, the new model startup should be cleaner than the old one

### combine the team spreadsheets into one nice dataframe
source('c:/research/general_startup.r')
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(lubridate))
suppressWarnings(library(readr))
suppressWarnings(library('Rglpk'))

USERPATH='c:/git/lineup/'
DATAPATH='d:/whoscored_data/'
setwd(USERPATH)
source('admin_funct.r')

options(warn=2, dplyr.print_max = 1e9)
source(paste0(USERPATH, 'team-funct.r'))
source(paste0(USERPATH, 'player-funct.r'))

seasonInfoDF = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''))
seasonInfoDF$seasonNumber = match(seasonInfoDF$season, with(seasonInfoDF, sort(season)))
##### aaarghhh, want to define seasonNumber at this point but it starts with 1516, whereas appearanceDF starts with 1617, and uses 1 for 1617.
### the solution: appearanceDF should use seasonInfoDF as its guide for season numbers.
### update, seasonInfoDF now starts with 1617, hope that doesn't cause problems
### if it does, fix when needed, don't have seasonInfoDF annoying in every other situation
currentseason = 2021
currentseasonnumber = with(seasonInfoDF, seasonNumber[which(season == currentseason)])

resultDF = ffDataLoading:::GetResultDF()
resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')
fixtDF = ffDataLoading:::GetFixtDF(resultDF)

resultDF = ffDataLoading:::AlignGameweekAndSeasonWithResultDF(resultDF)

dum = ffDataLoading:::AlignOddsWithResultsAndFixtures(resultDF, fixtDF)
resultDF = dum$resultDF
fixtDF = dum$fixtDF

resultDF = ffDataLoading:::CreateDaynum(resultDF)

gbgdf = ffDataLoading::ReadGbgDF()
dum = ffDataLoading::BolsterGbgDF(gbgdf, resultDF)
resultDF = dum$resultDF
gbgdf = dum$gbgdf
# i think the chances are, we will not be wanting the players who are unidentified any time soon
gbgdf = gbgdf %>%
        filter(!is.na(player))

summaryDF = ffDataLoading::GetSummaryDF(gbgdf)
playerDF = ffDataLoading:::ReadCurrentSeasonPlayerDF() %>%
              rename(player = whoscoredPlayer)
# no no that cocks up when a team hasn't played at the start of the season. it also misses out players who haven't yet played. iut should really use the ffplayer database as its reference
# but that'#s way too big a job


doubledUpPlayer = playerDF %>% count(team, playerid) %>% filter(n>1)
if (nrow(doubledUpPlayer) > 0) {
  print(doubledUpPlayer)
  stop('aaargh, some players appear twice in playerDF')
}
