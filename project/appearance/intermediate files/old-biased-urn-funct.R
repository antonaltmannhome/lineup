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
  theta = exp(theta0)
  allPermutationDF = allPermutationDF %>%
    rowwise() %>%
    mutate(byMatchSectionProb = AABiasedUrnProb(allPermutationByPlayerNumber, activePlayerList, theta),
           byMatchSectionLogLik = minDiff * log(byMatchSectionProb))
  dataLogLik = sum(allPermutationDF$byMatchSectionLogLik)
  
  penaltyLik = 10 * sum(theta0^2)
  totalLogLik = dataLogLik - penaltyLik
  if (FALSE) {
    print(theta)
    print(totalLogLik)
  }
  return(-totalLogLik)
}

