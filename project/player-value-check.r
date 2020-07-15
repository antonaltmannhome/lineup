## let's see if i can be guided as to how much a transfer would hit my team value

# so first step is to align the data with my current team


# the idea is it lets you try various combinations of which week to WC, which week to free hit
source('c:/git/lineup/new-model-startup.r')

playerDF = ffDataJoining:::MatchFFPlayerData(playerDF)

# so here is curren team
currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))

nextGameWeek = min(fixtDF$gameweek)
currentteam = lazy_left_join(currentteam,
                             fixtDF %>% filter(gameweek == nextGameWeek),
                             'team',
                             'oppteam') %>%
  lazy_left_join(playerDF, c('team', 'player'), 'ffPrice')

# and here is the relevant file that has info about prices
b = scan('c:/temp/ff-transfer-list-view.txt', sep = '\n', what = '', quiet = TRUE)

# find bit that refers to the prices
startIndex = grep(' \tGoalkeepers\tCP\tSP\tPP\tF\tTP\tFix', b)
priceLine = b[startIndex: (startIndex + 18)]
playerPriceLine = priceLine[c(2:3, 5:9, 11:15, 17:19)]

PlayerPriceLineToDF = function(x) {
  strsplit(x, split = '\t')[[1]]
}

playerPriceMatrix = t(sapply(playerPriceLine, PlayerPriceLineToDF))
rownames(playerPriceMatrix) = NULL
colnames(playerPriceMatrix)  = c('currentPrice', 'sellingPrice', 'purchasePrice', 'form', 'totalPoint', 'nextFixtureAbbn')

playerPriceDF = as_tibble(playerPriceMatrix)
playerPriceDF$currentPrice = as.numeric(playerPriceDF$currentPrice)
playerPriceDF$sellingPrice = as.numeric(playerPriceDF$sellingPrice)
playerPriceDF$purchasePrice = as.numeric(playerPriceDF$purchasePrice)
# ok, but now we have to figure out who is who
# to do that, we start with current team

teamAbbn = read_csv(paste0(DATAPATH, 'team_abbn.csv'), col_types = list(
  source = col_character(),
  wrongname = col_character(),
  correctname = col_character()
)) %>%
  filter(source == 'fftransfer')

playerPriceDF$nextFixture = teamAbbn$correctname[
  match(tolower(gsub(' .+$', '', playerPriceDF$nextFixtureAbbn)),
        teamAbbn$wrongname)]
playerPriceDF$ffPosition = rep(c('g', 'd', 'm', 'f'), c(2, 5, 5, 3))

currentteam[,c('sellingPrice', 'currentPrice', 'purchasePrice')] = NA
for (j in 1:nrow(currentteam)) {
  candidatePriceIndex = which(playerPriceDF$currentPrice == currentteam$ffPrice[j] &
                                playerPriceDF$ffPosition == currentteam$ffposition[j] &
                                playerPriceDF$nextFixture == currentteam$oppteam[j])
  if (length(candidatePriceIndex) == 1) {
    currentteam[j, c('sellingPrice', 'currentPrice', 'purchasePrice')] =
      playerPriceDF[candidatePriceIndex, c('sellingPrice', 'currentPrice', 'purchasePrice')]
  }
  if (length(candidatePriceIndex) != 1) {
    stop()
  }
}

# ok, if i get the data up to date then this will actually work i think

## so this will be pasted into join-all-data once it's working

source('c:/git/lineup/ffDataJoining/data-joining-startup.r')
resultDF = ffDataLoading:::GetResultDF()
fixtDF = ffDataLoading:::GetFixtDF(resultDF)
playerDF = ffDataLoading:::ReadCurrentSeasonPlayerDF() %>%
  rename(player = whoscoredPlayer)
playerDF = ffDataJoining:::MatchFFPlayerData(playerDF)

currentteam = read_csv(paste(DATAPATH, 'currentteam.csv', sep = ''), col_types = list(
  team = col_character(),
  player = col_character(),
  ffposition = col_character()
))

nextGameWeek = min(fixtDF$gameweek)
currentteam = lazy_left_join(currentteam,
                             fixtDF %>% filter(gameweek == nextGameWeek),
                             'team',
                             'oppteam') %>%
  lazy_left_join(playerDF, c('team', 'player'), 'ffPrice')

playerPriceDF = read_csv(paste0(DATAPATH, 'unmatched-player-price.csv'), col_types = list(
  currentPrice = col_double(),
  sellingPrice = col_double(),
  purchasePrice = col_double(),
  form = col_double(),
  totalPoint = col_double(),
  nextFixtureAbbn = col_character(),
  nextFixture = col_character(),
  ffPosition = col_character()
))

currentteam[,c('sellingPrice', 'currentPrice', 'purchasePrice')] = NA_real_
for (j in 1:nrow(currentteam)) {
  candidatePriceIndex = which(playerPriceDF$currentPrice == currentteam$ffPrice[j] &
                                playerPriceDF$ffPosition == currentteam$ffposition[j] &
                                playerPriceDF$nextFixture == currentteam$oppteam[j])
  if (length(candidatePriceIndex) == 1) {
    currentteam[j, c('sellingPrice', 'currentPrice', 'purchasePrice')] =
      playerPriceDF[candidatePriceIndex, c('sellingPrice', 'currentPrice', 'purchasePrice')]
  }
  if (length(candidatePriceIndex) != 1) {
    stop()
  }
}

write_csv(file = paste0(DATAPATH, 'current-team-plus-value.csv'),
          currentteam)
# insert that into join-all-data on saturday i guess
