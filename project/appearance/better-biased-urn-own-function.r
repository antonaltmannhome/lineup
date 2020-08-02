# some very silly things happening in make-own-biased-urn, let's unsilly it


### combine the team spreadsheets into one nice dataframe
source('c:/research/general_startup.r')
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(lubridate))
suppressWarnings(library(readr))
suppressWarnings(library('Rglpk'))

# this function makes al permutations
CalculateAllPermutation = function(x) {
  if (length(x) == 1) {
    allPermutationMatrix = x
  }
  else {
    allPermutationMatrix = matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      allPermutationMatrix <- rbind(allPermutationMatrix, cbind(x[i], Recall(x[-i])))
    }
  }
  return(allPermutationMatrix)
}

CallCalculateAllPermutation = function(playedIndicator) {
  x = which(playedIndicator == 1)
  dum = CalculateAllPermutation(x)
  return(dum)
}

AaDHyperGeo = function(success, weight) {
  successIndex = which(success == 1)
  
  
  allSuccessPermutation = CalculateAllPermutation(successIndex)
  bottomLineMatrixList = vector('list', sum(success))
  bottomLineInit = matrix(rep(1:length(success), nrow(allSuccessPermutation)),
                          byrow = TRUE,
                          nrow = nrow(allSuccessPermutation))
  
  bottomLineMatrixList[[1]] = bottomLineInit
  if (sum(success) > 1) {
    for (j in 2:sum(success)) {
      bottomLineMatrixList[[j]] = t(apply(cbind(bottomLineMatrixList[[j - 1]], allSuccessPermutation[, j - 1]), 1, function(x) setdiff(head(x, -1), tail(x, 1))))
    }
  }
  
  # and then, we calculate the probabilities
  permutationProb = rep(1, nrow(allSuccessPermutation))
  for (j in 1:sum(success)) {
    permutationProb = permutationProb * weight[allSuccessPermutation[,j]] / apply(matrix(weight[bottomLineMatrixList[[j]]], nrow = nrow(allSuccessPermutation)), 1, sum)
  }
  
  overallProb = sum(permutationProb)
  return(overallProb)
}

GetBiasedUrnPermutationInfo = function(success) {
  successIndex = which(success == 1)
  allSuccessPermutation = CalculateAllPermutation(successIndex)
  bottomLineMatrixList = vector('list', sum(success))
  bottomLineInit = matrix(rep(1:length(success), nrow(allSuccessPermutation)),
                          byrow = TRUE,
                          nrow = nrow(allSuccessPermutation))
  
  bottomLineMatrixList[[1]] = bottomLineInit
  if (sum(success) > 1) {
    for (j in 2:sum(success)) {
      bottomLineMatrixList[[j]] = t(apply(cbind(bottomLineMatrixList[[j - 1]], allSuccessPermutation[, j - 1]), 1, function(x) setdiff(head(x, -1), tail(x, 1))))
    }
  }
  return(lst(allSuccessPermutation, bottomLineMatrixList))
}

AaDHyperGeoJustProbabilityCalculation = function(success, weight, permutationInfo) {
  # and then, we calculate the probabilities
  topLine = t(apply(permutationInfo$allSuccessPermutation, 1, function(x) weight[x]))
  bottomLine = sapply(permutationInfo$bottomLineMatrixList, function(y) apply(y, 1, function(x) sum(weight[x])))
  overallProd = sum(apply(topLine / bottomLine,1,prod))
}


NewAA = function(success, weight) {
  allPermMatrix = CalculateAllPermutation(which(success == 1))
  bottomLineMatrix = matrix(sum(weight), ncol = 1, nrow = nrow(allPermMatrix))
  for (j in 2:ncol(allPermMatrix)) {
    bottomLineMatrix = cbind(bottomLineMatrix, bottomLineMatrix[,j-1] - weight[allPermMatrix[,j]])
  }
  topLineMatrix = matrix(weight[allPermMatrix], nrow = nrow(allPermMatrix), ncol = ncol(allPermMatrix))
  prob = sum(apply(topLineMatrix / bottomLineMatrix, 1, prod))
  return(prob)
}
NewAAJustProb = function(allPermMatrix, weight) {
  bottomLineMatrix = matrix(sum(weight), ncol = 1, nrow = nrow(allPermMatrix))
  for (j in 2:ncol(allPermMatrix)) {
    bottomLineMatrix = cbind(bottomLineMatrix, bottomLineMatrix[,j-1] - weight[allPermMatrix[,j]])
  }
  topLineMatrix = matrix(weight[allPermMatrix], nrow = nrow(allPermMatrix), ncol = ncol(allPermMatrix))
  prob = sum(apply(topLineMatrix / bottomLineMatrix, 1, prod))
  return(prob)
}
# ok it's only slightly slower now - or is it? maybe it's just the permutation bit

  
CompareSpeed = function(success, weight) {
  # check they agree first
  BiasedUrnAnswer = dMWNCHypergeo(success, rep(1, length(success)), sum(success), weight)
  allPermMatrix = CalculateAllPermutation(which(success == 1))
  AAAnswer = NewAAJustProbweight, allPermMatrix)
  message('BiasedUrnAnser: ', BiasedUrnAnswer, ', AA answer: ', AAAnswer)
    
  print(system.time(for (i in 1:1000) dMWNCHypergeo(success, rep(1, length(success)), sum(success), weight)))
  print(system.time(for (i in 1:1000) NewAAJustProb(success, weight, allPermMatrix)))
}
  
# so actually AA is faster, but you have to add the permutation calculations, but only once for each subset.
# for each run, that'll be nteam * 4 * 3 = 240 * 0.25 sec = 1 minute.
# although we could just hard code them once surely and store as a list
# but what about the derivs

# let's not worry about derivs just yet: i think converting this new quick way to c++ is the next step.
# think jumping through hoops to do it in R is not the way forward

# so let's get hold of eg man city data for 1920 season
mancity1920att = dget('project/appearance/man-city-attack.dat')
splitTeamDF = mancity1920att$splitTeamDF
myUnPlayer = mancity1920att$myUnPlayer
unMatchSection = mancity1920att$unMatchSection

# so firstly we need the permutations for all matches in advance

allPermutationList = tapply(splitTeamDF$played,
                            paste(splitTeamDF$teamgamenumber, splitTeamDF$matchSection),
                            CallCalculateAllPermutation)

# so let's do in R firstly
CalculateAllMatchSectionLogLik = function(theta0, allPermutationList) {
  theta = c(1, exp(theta0))
  byTeamMatchSectionProb = sapply(allPermutationList, NewAAJustProb, weight = theta)
  totalLogLik = mean(log(byTeamMatchSectionProb))
  print(theta)
  print(totalLogLik)
  return(-totalLogLik)
}

## well it's fast but looks wrong sadly
# i think firstly, i've not mulitplied by the number of minutes but also i've not accounted for who was actually available, and assumed everybody always was
theta0 = rep(0, length(myUnPlayer) - 1)
# bit fiddly to sort that one, will come back to that
