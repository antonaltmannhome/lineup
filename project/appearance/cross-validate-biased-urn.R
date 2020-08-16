### aa-biased-urn-model was good.
### now we need to be able to run it, with a window, rather than for a whole season at a time.
### and have a time downweight in it

source('c:/git/lineup/new-model-startup.r')
source('project/appearance/biased-urn-funct.R')
library(doParallel)

fakeMaxTime = 1000L

# need to sort out seasonNumber. why do we need 1516 in seasonInfoDF?
seasonInfoDF$seasonNumber = match(seasonInfoDF$season, with(seasonInfoDF, sort(season[havegbg])))
resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')

numBlockWithinSeason = 4
resultDF$inBlock = with(resultDF, (seasonNumber - 1) * numBlockWithinSeason +
                          cut(teamgamenumber, br = seq(0.5, 38.5, le = 5), labels = FALSE))

gbgdf = lazy_left_join(gbgdf,
                        resultDF,
                        c('season', 'team', 'teamgamenumber'),
                       'inBlock')

gbgdf2 = gbgdf %>%
  filter(!grepl('(injury|suspension)', startTime)) %>%
  mutate(startTime2 = ifelse(!startTime %in% c('U', 'UU'), startTime, as.character(fakeMaxTime)),
         endTime2 = ifelse(!startTime %in% c('U', 'UU'), endTime, as.character(fakeMaxTime))) %>%
  mutate(startTime2 = as.integer(startTime2),
         endTime2 = as.integer(endTime2))
# except we've got the problem that players sometimes officially start the match at eg 97 minutes.
# let's ceiling the endTime to be the greater of 94 and the biggest startTime of the match

gbgdf2$mainpos2 = with(gbgdf2, case_when(
  mainpos %in% c('D', 'DMC') ~ 'def',
  mainpos == 'M' ~ 'mid',
  mainpos %in% c('AM', 'FW') ~ 'att',
  TRUE ~ 'other'
))

# now, we will want to run it e.g 4 times a season, on every occasion predicting the next quarter of the season
# i think that's the level we would want it at

# but in fact, predicting isn't trivial, that's a job unto itself
# because you need to provide estimate of how many att/mids etc will play
# so let's not do the downweight just yet, let's try to predict minutes for a team for a block of the season

unTeamMainposBlock = gbgdf2 %>%
  distinct(team, mainpos2, inBlock)


GetPlayerAppearanceMle = function(myTeam, myBlock, myMainpos, blockWindowSize) {
  
  subGbgdf2 = gbgdf2 %>%
    filter(team == myTeam & mainpos2 == myMainpos & inBlock < myBlock & inBlock >= myBlock - blockWindowSize)
  splitTeamDF = subGbgdf2 %>%
    group_by(season, team, teamgamenumber) %>%
    do(SplitGame(.$player, .$startTime2, .$endTime2))
  
  unMatchSection = splitTeamDF %>%
    group_by(teamgamenumber, matchSection, minDiff) %>%
    summarise(numWhoPlayed = sum(played)) %>%
    ungroup() %>%
    mutate(logLik = NA)
  
  # but this leaves us with some match sections where no players play
  # which we don't want, so get rid
  unMatchSection = unMatchSection %>%
    filter(numWhoPlayed > 0)
  splitTeamDF = splitTeamDF %>%
    semi_join(unMatchSection, c('teamgamenumber', 'matchSection'))
  
  myUnPlayer = unique(splitTeamDF$player)
  # who has made the median number of appearances, they should have value zero
  myTotalMinutePlayed = splitTeamDF %>%
    group_by(player) %>%
    summarise(totalMinutePlayed = sum(endTime - startTime)) %>%
    arrange(totalMinutePlayed)
  referencePlayer = myTotalMinutePlayed$player[floor( (length(myUnPlayer)+1) / 2)]
  myUnPlayer = c(referencePlayer, setdiff(myUnPlayer, referencePlayer))
  splitTeamDF$playerNumber = match(splitTeamDF$player, myUnPlayer)
  
  allPermutationDF = splitTeamDF %>%
    group_by(teamgamenumber, matchSection) %>%
    summarise(allPermutationByPlayerNumber = CalculateAllPermutation(played, playerNumber),
              activePlayerList = list(playerNumber),
              minDiff = minDiff[1])
  
  theta0 = rep(0, length(myUnPlayer) - 1)
  
  maxInfo = nlm(CalculateAllMatchSectionLogLik, p = theta0, allPermutationDF = allPermutationDF, stepmax = 5)
  dum = c(0, maxInfo$est)
  myAppearanceMle = data.frame(team = myTeam, inBlock = myBlock, mainpos = myMainpos,
                               player = myUnPlayer, appearanceMle = dum, appearanceOdds = exp(dum) / sum(exp(dum)))
  
  return(myAppearanceMle)
}

## right, let's try to predict how many minutes man city will play in block 12 eg assuming we know who will be available when
# so firstly get hold of the player mles
# what formation do they tend to play?
# this will surely become a function of its own in time

GetPositionDataForTeamBlock = function(myTeam, myBlock) {
  numGameInBlock = with(resultDF, sum(team == myTeam & inBlock == myBlock))
  totalByPositionDF = gbgdf2 %>%
    lazy_left_join(resultDF,
                   c('season', 'team', 'teamgamenumber'),
                   'maxEndTime') %>%
    filter(team == myTeam & inBlock == myBlock) %>%
    group_by(mainpos2) %>%
    summarise(numPlayerInPos = sum( (endTime2 - startTime2) / maxEndTime) / numGameInBlock)
  
  return(totalByPositionDF)
}


myTeam = 'mancity'
myBlock = 12
GetPositionDataForTeamBlock = function(myTeam, myBlock)
# so what I would like to do is then have a list of combos that average out to that
# come back to that
# let's make a guess for that and predict for it
numPlayerInPosComboDF = tibble(mainpos2 = rep(c('def', 'mid', 'att'), 2),
                               numPlayerInPos = c(2, 5, 3, 3, 4, 3))

# loop for now of course
myPlayerMle = GetPlayerAppearanceMle(myTeam, myBlock, myMainpos = 'def', blockWindowSize = 4)
BiasedUrn::meanMWNCHypergeo(rep(1, nrow(myPlayerMle)), 3, myPlayerMle$appearanceOdds)
# ok so that is nice - but we'd need to loop over every match i think and adjust first and last arguments according to who is available

# which is pretty easy actually
for (j in which(resultDF$team == myTeam & resultDF$inBlock == myBlock)) {
  
}