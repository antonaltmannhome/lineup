
source('c:/git/lineup/new-model-startup.r')

fakeMaxTime = 1000L

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
# argh, need to split the game up as subs get made
SplitGame = function(myPlayer, myStartTime, myEndTime) {
  # for debugging:
  # subGbgdf = gbgdf2 %>% filter(team == 'mancity' & season == 1920 & teamgamenumber == 2 & mainpos %in% c('D', 'DMC')); myStartTime = subGbgdf %>% pull(startTime2); myEndTime = subGbgdf %>% pull(endTime2); myPlayer = subGbgdf %>% pull(player)
  unSubTime = sort(unique(c(myStartTime[myStartTime != fakeMaxTime],
                            myEndTime[!is.na(myEndTime) & myEndTime != fakeMaxTime])))
  if (length(unSubTime) == 0) {
    myRepeatedPlayedDF = data.frame(player = character(),
                                    matchSection = integer(),
                                    minDiff = integer(),
                                    startTime = integer(),
                                    endTime = integer(),
                                    lhTimeCutoff = integer(),
                                    rhTimeCutoff = integer(),
                                    played = integer())
  }
  if (length(unSubTime) > 0) {
    minDiff = diff(unSubTime)
    numInterval = length(unSubTime) - 1
    myRepeatedPlayedDF = data.frame(player = rep(myPlayer, numInterval),
                                  matchSection = rep(1:numInterval, rep(length(myStartTime), numInterval)),
                                  minDiff = rep(minDiff, rep(length(myStartTime), numInterval)),
                                  startTime = rep(myStartTime, numInterval),
                                  endTime = rep(myEndTime, numInterval),
                                  lhTimeCutoff = rep(head(unSubTime, -1), rep(length(myStartTime), numInterval)),
                                  rhTimeCutoff = rep(tail(unSubTime, -1), rep(length(myStartTime), numInterval)))
    myRepeatedPlayedDF$played = with(myRepeatedPlayedDF, as.integer(startTime <= lhTimeCutoff & endTime >= rhTimeCutoff))
  }
    
  return(myRepeatedPlayedDF)
}

# this function makes al permutations - but that Recall thing does funny stuff, so need a wrapper function to actually call it
InternalCalculateAllPermutation = function(x) {
  if (length(x) == 1) {
    allPermutationMatrix = matrix(x, nrow = 1, ncol = 1)
  }
  else {
    allPermutationMatrix = matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      allPermutationMatrix <- rbind(allPermutationMatrix, cbind(x[i], Recall(x[-i])))
    }
  }
  return(allPermutationMatrix)
}

CalculateAllPermutation = function(playedIndicator, playerNumber) {
  x = which(playedIndicator == 1)
  allPermutationByIndex = InternalCalculateAllPermutation(x)
  # but that references just 1:(number of players), we need our actual players referenced
  allPermutationByPlayerNumber = matrix(playerNumber[allPermutationByIndex],
                                        nrow = nrow(allPermutationByIndex),
                                        ncol = ncol(allPermutationByIndex))
  myList = list(allPermutationByPlayerNumber = allPermutationByPlayerNumber)
  return(myList)
}

# so firstly we need the permutations for all matches in advance

AABiasedUrnProb = function(allPermMatrix, activePlayerNumber, weight) {
  bottomLineMatrix = matrix(sum(weight[activePlayerNumber]), ncol = 1, nrow = nrow(allPermMatrix))
  if (ncol(allPermMatrix) > 1) {
    for (j in 2:ncol(allPermMatrix)) {
      bottomLineMatrix = cbind(bottomLineMatrix, bottomLineMatrix[,j-1] - weight[allPermMatrix[,j]])
    }
  }
  topLineMatrix = matrix(weight[allPermMatrix], nrow = nrow(allPermMatrix), ncol = ncol(allPermMatrix))
  prob = sum(apply(topLineMatrix / bottomLineMatrix, 1, prod))
  return(prob)
}

CalculateAllMatchSectionLogLik = function(theta0, allPermutationDF) {
  theta = c(1, exp(theta0))
  allPermutationDF = allPermutationDF %>%
    rowwise() %>%
    mutate(byMatchSectionProb = AABiasedUrnProb(allPermutationByPlayerNumber, activePlayerList, theta),
           byMatchSectionLogLik = minDiff * log(byMatchSectionProb))
  totalLogLik = sum(allPermutationDF$byMatchSectionLogLik)
  if (FALSE) {
    print(theta)
    print(totalLogLik)
  }
  return(-totalLogLik)
}
GradAllMatchSection = function(theta0, allPermutationDF) {
  theta = c(1, exp(theta0))
  allPermutationDF = allPermutationDF %>%
    rowwise() %>%
    mutate(byMatchSectionProb = AABiasedUrnProb(allPermutationByPlayerNumber, activePlayerList, theta),
           byMatchSectionLogLik = minDiff * log(byMatchSectionProb))
  totalLogLik = sum(allPermutationDF$byMatchSectionLogLik)
  if (FALSE) {
    print(theta)
    print(totalLogLik)
  }
  return(-totalLogLik)
}

GetPlayerAppearanceMle = function(myTeam, mySeason, myMainpos) {
  
  subGbgdf2 = gbgdf2 %>%
    filter(season == mySeason & team == myTeam & mainpos2 == myMainpos)
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
  myAppearanceMle = data.frame(team = myTeam, season = mySeason, player = myUnPlayer, appearanceMle = dum, appearanceOdds = exp(dum) / sum(exp(dum)))
  
  return(myAppearanceMle)
}

# so how long does it take to do the entire lot?
unTeamSeasonPosition = gbgdf2 %>%
  filter(mainpos2 != 'other') %>%
  distinct(season, team, mainpos2)
# still takes a while sadly. think we should press on with 1st deriv
# no, do not do that. it's just too complicated. better to look into situations where it's slow, maybe we can weed out obviously nonsense players
# and there's one example of all arsenal players not playing, that is rubbish, we should filter things like that out


appearanceMleList = vector('list', nrow(unTeamSeasonPosition))
timestamp = rep(NA, length(unTeamSeasonPosition))
for (tspi in 1:nrow(unTeamSeasonPosition)) {
  # mancity attack 1920 is tspi = 212
  # tspi = 214; myTeam = unTeamSeasonPosition$team[tspi]; mySeason = unTeamSeasonPosition$season[tspi]; myMainpos = unTeamSeasonPosition$mainpos2[tspi]
  # 214 is a problem
  appearanceMleList[[tspi]] = with(unTeamSeasonPosition[tspi,],
                                   GetPlayerAppearanceMle(team, season, mainpos2))
  with(unTeamSeasonPosition[tspi,],
       message('Have processed ', season, ' / ', team, ' / ', mainpos2))
  timestamp[tspi] = date()
}

# takes 15 mins or so. so not massively practical for optimising a full run but not atrocious either. there's also that multi node thing to try
saveRDS(file = 'project/appearance/appearanceMleList.rds', object = appearanceMleList)
