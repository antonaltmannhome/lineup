# try 1st deriv on simple example to start with, then try to roll it out to proper thing. build it up slowly

# so simplifications we'll do are:
## not exping the params
## not having a default
## relevants players are number 1:n


### combine the team spreadsheets into one nice dataframe
source('c:/research/general_startup.r')
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(lubridate))
suppressWarnings(library(readr))
suppressWarnings(library('Rglpk'))

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
SimpleLikFunct = function(theta, allPermMatrix, activePlayerNumber) {
  AABiasedUrnProb(allPermMatrix, activePlayerNumber, theta)
}
SimpleGrad = function(theta, allPermMatrix, activePlayerNumber) {
  gradvec = rep(NA, length(theta))
  for (j in 1:length(theta)) {
    gradvec[j] = 1/sum(theta) - theta[j] / sum(theta)^2
  }
}

numPlayer = 3
playedIndicator = rep(0, numPlayer)
playedIndicator[resample(1:numPlayer, ceiling(numPlayer / 2))] = 1
allPermMatrix = CalculateAllPermutation(playedIndicator, 1:length(playedIndicator))$allPermutationByPlayerNumber
# so firstly we need the permutations for all matches in advance

weight = seq(0.2, 1, le = numPlayer)

SimpleLikFunct(weight, allPermMatrix, 1:numPlayer)
numDeriv::grad(SimpleLikFunct, weight, allPermMatrix = allPermMatrix, activePlayerNumber = 1:numPlayer)

# ugh it's actually this
# deriv(~log(exp(x)/(exp(x) + exp(y) + exp(z)) * exp(y)/(exp(y)+exp(z)) + exp(y)/(exp(x) + exp(y) + exp(z)) * exp(x)/(exp(x)+exp(z))), 'x')
# fancy that? surely not

