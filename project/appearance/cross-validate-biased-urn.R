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

gbgdf2 = gbgdf2 %>%
  select(season, seasonNumber, teamgamenumber, team, player, startTime2, endTime2, minute, played, available, mainpos2, inBlock)

# now, we will want to run it e.g 4 times a season, on every occasion predicting the next quarter of the season
# i think that's the level we would want it at

# but in fact, predicting isn't trivial, that's a job unto itself
# because you need to provide estimate of how many att/mids etc will play
# so let's not do the downweight just yet, let's try to predict minutes for a team for a block of the season

unTeamMainposBlock = gbgdf2 %>%
  distinct(team, mainpos2, inBlock)


GetPlayerAppearanceMle = function(pastGbgDF, myMainpos) {
  
  splitTeamDF = pastGbgDF %>%
    filter(mainpos2 == myMainpos) %>%
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
  
  # who has made the median number of appearances, they should have value zero
  myTotalMinutePlayed = splitTeamDF %>%
    group_by(player) %>%
    summarise(totalMinutePlayed = sum(endTime - startTime)) %>%
    arrange(totalMinutePlayed)

  referencePlayer = myTotalMinutePlayed$player[floor(0.5 * nrow(myTotalMinutePlayed)+1)]
  myUnPlayer = c(referencePlayer, setdiff(myTotalMinutePlayed$player, referencePlayer))
  splitTeamDF$playerNumber = match(splitTeamDF$player, myUnPlayer)
  
  allPermutationDF = splitTeamDF %>%
    group_by(teamgamenumber, matchSection) %>%
    summarise(allPermutationByPlayerNumber = CalculateAllPermutation(played, playerNumber),
              activePlayerList = list(playerNumber),
              minDiff = minDiff[1])
  
  theta0 = rep(0, length(myUnPlayer) - 1)
  
  maxInfo = nlm(CalculateAllMatchSectionLogLik, p = theta0, allPermutationDF = allPermutationDF, stepmax = 5)
  dum = c(0, maxInfo$est)
  myAppearanceMle = data.frame(player = myUnPlayer, appearanceMle = dum, appearanceOdds = exp(dum) / sum(exp(dum)))
  
  return(myAppearanceMle)
}

## right, let's try to predict how many minutes man city will play in block 12 eg assuming we know who will be available when
# so firstly get hold of the player mles
# what formation do they tend to play?
# this will surely become a function of its own in time

GetPastFormationForTeamBlock = function(pastGbgDF) {
  numGameInBlock = nrow(pastGbgDF %>% distinct(season, teamgamenumber))
  pastFormationDF = pastGbgDF %>%
    lazy_left_join(resultDF,
                   c('season', 'team', 'teamgamenumber'),
                   'maxEndTime') %>%
    group_by(mainpos2) %>%
    summarise(numPlayerInPos = sum( (endTime2 - startTime2) / maxEndTime) / numGameInBlock) %>%
    mutate(floorNP = floor(numPlayerInPos),
           ceilingNP = ceiling(numPlayerInPos))
  # get rid of gk and sort the normal way
  pastFormationDF = pastFormationDF %>%
    filter(mainpos2 %in% c('def', 'mid', 'att')) %>%
    arrange(match(mainpos2, c('def', 'mid', 'att')))
  
  return(pastFormationDF)
}

FormationLikFunct = function(theta, pastFormationDF, validFormationMatrix) {
  wgtVec = c(1, exp(theta))
  calcFormation = colSums(validFormationMatrix * wgtVec)/sum(wgtVec)
  sqDiff = sum( (pastFormationDF$numPlayerInPos - calcFormation) ^ 2)
  return(sqDiff)
}

GetFormationWeight = function(pastFormationDF) {
  
  expandedPastFormationDF = with(pastFormationDF,
                  expand.grid(def = c(floorNP[mainpos2 == 'def'], ceilingNP[mainpos2 == 'def']),
                              mid = c(floorNP[mainpos2 == 'mid'], ceilingNP[mainpos2 == 'mid']),
                              att = c(floorNP[mainpos2 == 'att'], ceilingNP[mainpos2 == 'att'])))
  validFormationMatrix = expandedPastFormationDF[which(rowSums(expandedPastFormationDF) == 10),]
  
  # but then we need to weighted combo that sums to the average
  maxInfo = nlm(FormationLikFunct, p = c(0, 0),
                pastFormationDF = pastFormationDF,
                validFormationMatrix = validFormationMatrix,
                stepmax = 3)
  formationWeightDF = tibble(formationIndex = 1:nrow(validFormationMatrix),
                             formationWgt = c(1, exp(maxInfo$est)) / sum(c(1, exp(maxInfo$est))))
  
  formationDF = as_tibble(validFormationMatrix) %>%
    mutate(formationIndex = 1:n()) %>%
    select(formationIndex, everything()) %>%
    left_join(formationWeightDF, 'formationIndex')
  
  return(formationDF)
}

MakeMatchFormationDF = function(myTeam, myBlock, formationDF) {
  
  # now we want a list of matches-formnation combos, but only the ones that are actually possible
  matchFormationDF = merge(resultDF %>%
                             filter(team == myTeam & inBlock == myBlock) %>%
                             select(season, teamgamenumber),
                           formationDF) %>%
    left_join(gbgdf2 %>%
                filter(team == myTeam & inBlock == myBlock) %>%
                group_by(season, teamgamenumber) %>%
                summarise(numDef = sum(mainpos2 == 'def' & available),
                          numMid = sum(mainpos2 == 'mid' & available),
                          numAtt = sum(mainpos2 == 'att' & available)),
              c('season', 'teamgamenumber')) %>%
    mutate(isPossible = (numDef >= def &
                           numMid >= mid &
                           numAtt >= att))
  # the impossible ones need to have formationWgt set to 0 and the remaining ones summed up accordingly
  scaleUp = matchFormationDF %>%
    group_by(season, teamgamenumber) %>%
    summarise(sumPossible = sum(formationWgt[isPossible]))
  matchFormationDF = matchFormationDF %>%
    left_join(scaleUp, c('season', 'teamgamenumber')) %>%
    mutate(formationWgt = ifelse(isPossible, formationWgt / sumPossible, 0))

  return(matchFormationDF)  
}

GetExpectedNumGameByTeamBlock = function(myTeam, myBlock, blockWindowSize) {
  
  # filter out players who officially were here but never really played
  pastGbgDF = gbgdf2 %>%
    filter(team == myTeam & inBlock < myBlock & inBlock >= myBlock - blockWindowSize)
  pastGbgDF = semi_join(pastGbgDF,
                        pastGbgDF %>%
                          group_by(player) %>%
                          summarise(sumMinute = sum(minute)) %>%
                          filter(sumMinute > 10),
                        'player')
  
  pastFormationDF = GetPastFormationForTeamBlock(pastGbgDF)
  formationDF = GetFormationWeight(pastFormationDF)
  #numFormation = nrow(formationDF)
  matchFormationDF = MakeMatchFormationDF(myTeam, myBlock, formationDF)
  
  # so what I would like to do is then have a list of combos that average out to that
  # that went swimmingly for that example, nice
  
  # so for each match, we need to calculate the prob of each available player playing for each plausible formation
  # loop for now of course
  myList = vector('list', 3)
  for (i in 1:3) {
    myMainpos = c('def', 'mid', 'att')[i]
    myPlayerMle = GetPlayerAppearanceMle(pastGbgDF, myMainpos = myMainpos) %>%
      mutate(team = myTeam, block = myBlock, mainpos2 = myMainpos)
    
    subGbgDF = gbgdf2 %>%
      filter(team == myTeam & inBlock == myBlock & available & mainpos2 == myMainpos) %>%
      lazy_left_join(myPlayerMle, 'player', 'appearanceOdds') %>%
      replace_na(list(appearanceOdds = 0.0001))
    
    subMatchFormationDF = matchFormationDF %>%
      filter(formationWgt > 0) %>%
      select(season, teamgamenumber, formationIndex, myMainpos, formationWgt) %>%
      rename(numPlayerToSelect = myMainpos)
    
    repSubGbgDF = full_join(subGbgDF,
                            subMatchFormationDF,
                            c('season', 'teamgamenumber'))
    
    if (FALSE) {
    repSubGbgDF = subGbgDF %>%
      lazy_left_join(myPlayerMle, 'player', 'appearanceOdds') %>%
      replace_na(list(appearanceOdds = 0.0001)) %>%
      slice(rep(1:n(), each = numFormation)) %>%
      mutate(formationIndex = rep(1:numFormation, n()/numFormation)) %>%
      left_join(formationDF %>%
                  gather(mainpos2, numPlayerInPos, -c(formationIndex, formationWgt)),
                c('formationIndex', 'mainpos2')) %>%
      left_join(subGbgDF %>%
                  group_by(season, teamgamenumber) %>%
                  summarise(sumAvailable = n()),
                c('season', 'teamgamenumber')) %>%
      mutate(isPossible = sumAvailable >= numPlayerInPos)
    # but are there any impossible formations? eg 5 defender slots but only 4 defenders available?
    # if so, need to set its formationWgt to zero and compensate with the remaining ones
    scaleUpQuant = repSubGbgDF %>%
      group_by(season, teamgamenumber) %>%
      summarise(toScaleup = 1/sum(formationWgt[isPossible]))
    }
    
    repSubGbgDF = repSubGbgDF %>%
      group_by(teamgamenumber, formationIndex) %>%
      mutate(probPlay = BiasedUrn::meanMWNCHypergeo(rep(1, n()), numPlayerToSelect[1], appearanceOdds)) %>%
      ungroup()
    
    myList[[i]] = repSubGbgDF %>%
      group_by(player) %>%
      summarise(expectedNumGame = sum(probPlay * formationWgt)) %>%
      mutate(team = myTeam, mainpos2 = myMainpos, inBlock = myBlock)
  }
  
  # ok so that all works, i believe we need to loop by team/block
  expectedNumGameDF = bind_rows(myList)
  
  return(expectedNumGameDF)
}

# ok, now we need to get it looping, need to add these functions into biased-urn-funct
# need to rearrange the final function a bit, need to separate out the estimation of players and calculating the expected number of games played
