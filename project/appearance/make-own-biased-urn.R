## biasedurn is nice but i'm sure we can make our own much faster version

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

CompareSpeed = function(success, weight) {
  # check they agree first
  BiasedUrnAnswer = dMWNCHypergeo(success, rep(1, length(success)), sum(success), weight)
  permutationInfo = GetBiasedUrnPermutationInfo(success)
  AAAnswer = AaDHyperGeoJustProbabilityCalculation(success, weight, permutationInfo)
  message('BiasedUrnAnser: ', BiasedUrnAnswer, ', AA answer: ', AAAnswer)

  print(system.time(for (i in 1:1000) dMWNCHypergeo(success, rep(1, length(success)), sum(success), weight)))
  print(system.time(for (i in 1:1000) AaDHyperGeoJustProbabilityCalculation(success, weight, permutationInfo)))
}
  # hmm, it's still slow, how is hyper so fast? shame it occasionally breaks
# although i can speed up AA in fact - lots of repeated calculations - we could work out how many unique topLine/BottomLines we actually hve to do and only do those
