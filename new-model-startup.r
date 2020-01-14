### right, the new model startup should be cleaner than the old one

### combine the team spreadsheets into one nice dataframe
source('c:/research/general_startup.r')
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(lubridate))
suppressWarnings(library('Rglpk'))

USERPATH='c:/git/lineup/'
DATAPATH='d:/whoscored_data/'
setwd(USERPATH)
source('admin_funct.r')

options(warn=2, dplyr.print_max = 1e9)
if (FALSE) {
ffDataLoadingPath = paste0(USERPATH, 'ffDataLoading')
# usethis::create_package(ffDataLoadingPath)
#devtools::load_all(ffDataLoadingPath)
devtools::install(ffDataLoadingPath)
}

seasoninfo = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''))
##### aaarghhh, want to define seasonNumber at this point but it starts with 1516, whereas appearanceDF starts with 1617, and uses 1 for 1617.
### the solution: appearanceDF should use seasoninfo as its guide for season numbers. 
currentseason = 1920

resultdf = ffDataLoading:::GetResultDF()
fixtdf = ffDataLoading:::GetFixtDF()

resultdf = ffDataLoading:::AlignGameweekAndSeasonWithResultDF(resultdf)

dum = ffDataLoading:::AlignOddsWithResultsAndFixtures(resultdf, fixtdf)
resultdf = dum$resultDF
fixtdf = dum$fixtDF

resultdf = ffDataLoading:::CreateDaynum(resultdf)

gbgdf = ffDataLoading::ReadGbgDF()
gbgdf = ffDataLoading::BolsterGbgDF(gbgdf, resultdf)
# i think the chances are, we will not be wanting the players who are unidentified any time soon
gbgdf = gbgdf %>%
        filter(!is.na(player))

summarydf = ffDataLoading::GetSummaryDF(gbgdf)
playerDF = ffDataLoading:::ReadCurrentSeasonPlayerDF() %>%
              rename(player = whoscoredPlayer)

doubledUpPlayer = playerDF %>% count(team, playerid) %>% filter(n>1)
if (nrow(doubledUpPlayer) > 0) {
  print(doubledUpPlayer)
  stop('aaargh, some players appear twice in playerDF')
}
