### so just while we're testing ideas out, we'll see if we can do something intelligent with regards to fatigue. we've got chelsea's predictions on a game by game basis, can we
# (1) prove that fatigue does lead to unexpected rests in a predictable way
# (2) adjust our LikFunct so that fatigued players don't get downgraded after a rest (but possibly do in the build up to a rest)

source('project/appearance/biased urn approach/biased-urn-game-by-game-set-up-data.r')

# so step 1, load in chelsea's estimates for one of our files

myTeam = 'chelsea'
timeDownWeightCoef = 0.1
priorStrength = 10
fileIn = paste0(DATAPATH, 'active_player/biased_urn_output/playermle_', myTeam, '_timedownweight_', timeDownWeightCoef, '_priorstrength_', priorStrength, '.csv')
myTeamEstimateDF = read_csv(fileIn, col_types = list(
  team = col_character(),
  alltimetgn = col_integer(),
  mainpos2 = col_character(),
  player = col_character(),
  appearanceMle = col_double(),
  appearanceOdds = col_double()
))

myTeamMainposTgn = unTeamMainposTgn %>%
  filter(team == myTeam)
myTeamTgn = unTeamTgn %>%
  filter(team == myTeam)

myList = list('vector', nrow(myTeamTgn))
for (tbi in 1:nrow(myTeamTgn)) {
  myList[[tbi]] = with(myTeamTgn[tbi,],
                       GetFormationProbabilityByTeam(team, alltimetgn, 38, historicFormationDF))
}
myTeamTgnFormationDF = bind_rows(myList)

myList = list('vector', nrow(myTeamTgn))
# actually, instead of predicting just hte next game, maybe THIS should still predict the next block of games
# to get rid of the resting issue
for (tbi in 1:nrow(myTeamTgn)) {
  myList[[tbi]] =  with(myTeamTgn[tbi,],
                        GetExpectedPropGameByTeam(team, alltimetgn,
                                                  myTeamEstimateDF, myTeamTgnFormationDF))
}
myTeamExpectedPropGameDF = bind_rows(myList)

subGbgDF = gbgdf2 %>%
  filter(team == myTeam) %>%
  left_join(myTeamExpectedPropGameDF, c('player', 'team', 'mainpos2', 'alltimetgn')) %>%
  lazy_left_join(resultDF, c('team', 'alltimetgn'), 'season')

calibplot(subGbgDF$expectedPropGame, subGbgDF$minute/94)
# looks pretty solid to me

# but we need a horizontal diagnosis tool

ViewHorizDiag = function(mySeason, lhTgn, rhTgn) {
  horizDiag = gbgdf %>%
    filter(team == myTeam) %>%
    lazy_left_join(myTeamExpectedPropGameDF, c('team', 'player', 'alltimetgn')) %>%
    mutate(predObs = paste(round(expectedPropGame, 2), minute, as.integer(available), sep = '/')) %>%
    filter(season == mySeason & between(teamgamenumber, lhTgn, rhTgn) & !is.na(mainpos2)) %>%
    select(player, mainpos2, teamgamenumber, predObs) %>%
    spread(key = teamgamenumber, value = predObs) %>%
    arrange(match(mainpos2, c('def', 'mid', 'att')))
  return(horizDiag)
}

# gah reece james switches from def to mid, which boosts defs probs of playing, decreases midfielders. can we force players to have same position throughout the season maybe? Not a big deal to rerun an entire team's ests for a season if you have to

# now, what about a fatigue measure?

# we've got the champs league stuff to load in too somewhere
euroFixtureDF = read_csv(paste0(DATAPATH, 'euro-fixtures-historic.csv'), col_types = list(
  tournament = col_character(),
  date = col_integer(),
  team = col_character()
)) %>%
  filter(date > 20160800)

seasonInfoDF$ymdStart = ymd(seasonInfoDF$start)
seasonInfoDF$ymdEnd = ymd(seasonInfoDF$end)
euroFixtureDF$ymdDate = ymd(euroFixtureDF$date)

expandedSeasonInfoDateVec = c(seasonInfoDF$ymdStart[-1], tail(seasonInfoDF$ymdEnd, 1))
expandedSeasonInfoSeasonVec = seasonInfoDF$season[-1]
euroFixtureDF$season = expandedSeasonInfoSeasonVec[findInterval(euroFixtureDF$ymdDate, expandedSeasonInfoDateVec)]

# so let's take a regular and get his list of days available and played

myPlayer = 'eden hazard'
mySeason = 1718
miniGbgDF = subGbgDF %>%
  filter(season == mySeason & player == myPlayer)  %>%
  left_join(myTeamExpectedPropGameDF, c('player', 'team', 'mainpos2', 'alltimetgn')) %>%
  left_join(resultDF, c('team', 'alltimetgn'), 'date') %>%
  mutate(ymdDate = ymd(as.integer(date)))

# but we want to insert the euro fixtures too. assume he played in them too
myDateVec = miniGbgDF %>%
  select(ymdDate, minute) %>%
  bind_rows(euroFixtureDF %>%
              filter(team == myTeam & season == mySeason) %>%
              select(ymdDate) %>%
              mutate(minute = 94)) %>%
  arrange(ymdDate)

# so for each game we want to know its intensity, number of games both before and after within various time intervals
# don't know if we want that matrix actually. think you handle fatigue and intensity more separately than that
# nah, too complicated that. let's do the matrix thing, it's good enough for what we want to do i think

dayDiffMat = abs(matrix(rep(myDateVec$ymdDate, nrow(myDateVec)), nrow = nrow(myDateVec), byrow = FALSE) -
      matrix(rep(myDateVec$ymdDate, nrow(myDateVec)), nrow = nrow(myDateVec), byrow = TRUE))
# so the stuff to the left is surely more important than stuff to the right. so calculate fatigue and fixture business separately and handle separately in the formula that we eventually make

