## try to combine the three files that suggest what changes you should make to your team
# ie get-optimal-team, suggested-transfer and the current prices of each player
# get rid of that column saying what the value rank is, it's completely useless
# what is better is, how much do you gain/lose by replacing this player with one or 2 others eg

source('knapsack-funct.r')

CalculatePointGainFromTransfer = function(forcedInclusionExclusion) {
  idealteam = RunKnapsack(playerDF, forcedInclusionExclusion, currentmoney)
  idealteamgameweekexpectedpoint = GetCurrentExpectedPoint(playerFixtDF, idealteam)
  
  idealteamfullinfo = CalculateExpectedPoint(playerFixtDF, idealteam, warnAboutMissingPlayer = FALSE)
  
  pointGainFromTransfer = idealteamfullinfo$totalexpectedpoint - currentteamfullinfo$totalexpectedpoint
  
  return(lst(idealteam, pointGainFromTransfer))
}

CalculateExtraPointForOneTransfer = function(currentteam) {
  totalNumPossible = 15
  transferOutcomeDF = tibble(pointGain1 = rep(NA_real_, totalNumPossible))
  transferOutcomeDF$replacement1 = NA_character_
  for (j in 1:totalNumPossible) {
    adjCurrentTeam = currentteam %>%
      mutate(includeOrExclude = 'include')
    adjCurrentTeam$includeOrExclude[j] = 'exclude'
    alternativeTeam = CalculatePointGainFromTransfer(adjCurrentTeam)
    transferOutcomeDF$pointGain1[j] = alternativeTeam$pointGainFromTransfer
    transferOutcomeDF$replacement1[j] = setdiff(alternativeTeam$idealteam$player, currentteam$player)
  }
  return(transferOutcomeDF)
}


CalculateExtraPointForTwoTransfers = function(currentTeam) {
  
  totalNumPossible = choose(15, 2)
  allPossibleTransfer = combn(15, 2)
  transferOutcomeDF = tibble(pointGain = rep(NA, totalNumPossible))
  transferOutcomeDF$drop1 = NA_character_
  transferOutcomeDF$drop2 = NA_character_
  transferOutcomeDF$replacement1 = NA_character_
  transferOutcomeDF$replacement2 = NA_character_
  for (j in 1:totalNumPossible) {
    adjCurrentTeam = currentteam %>%
      mutate(includeOrExclude = 'include')
    adjCurrentTeam$includeOrExclude[allPossibleTransfer[,j]] = 'exclude'
    alternativeTeam = CalculatePointGainFromTransfer(adjCurrentTeam)
    transferOutcomeDF$pointGain[j] = alternativeTeam$pointGainFromTransfer
    droppedPlayer = currentteam$player[allPossibleTransfer[,j]]
    transferOutcomeDF$drop1[j] = droppedPlayer[1]
    transferOutcomeDF$drop2[j] = droppedPlayer[2]
    replacementPlayer = setdiff(alternativeTeam$idealteam$player, currentteam$player)
    transferOutcomeDF$replacement1[j] = replacementPlayer[1]
    transferOutcomeDF$replacement2[j] = replacementPlayer[2]
    if ( (j %% 10) == 0) {
      message('Have calculated gain from ', j, ' pairs so far')
    }
  }

  return(transferOutcomeDF)
}

CalculateBestTwoTransfersByPlayer = function(currentteam) {
  extraPointForTwoTransfers = CalculateExtraPointForTwoTransfers(currentTeam)
  bestPointGainDF = tibble(pointGain2 = rep(NA_real_, 15))
  bestPointGainDF$otherDroppedPlayer = NA_character_
  bestPointGainDF$replacement2ab = NA_character_
  bestPointGainDF$replacement2 = NA_character_
  for (j in 1:15) {
    currentPlayerIndex = with(extraPointForTwoTransfers,
                              which(drop1 == currentteam$player[j] | drop2 == currentteam$player[j]))
    currentBestTransfersIndex = currentPlayerIndex[which.max(extraPointForTwoTransfers$pointGain[currentPlayerIndex])]
    bestPointGainDF[j,c('pointGain2', 'replacement2a', 'replacement2b')] =
      extraPointForTwoTransfers[currentBestTransfersIndex,c('pointGain', 'replacement1', 'replacement2')]
    bestPointGainDF$otherDroppedPlayer[j] = setdiff(as.character(extraPointForTwoTransfers[currentBestTransfersIndex,c('drop1', 'drop2')]), currentteam$player[j])
  }
}

idealteam = RunKnapsack(playerDF, forcedInclusionExclusion, currentmoney)
idealteamfullinfo = CalculateExpectedPoint(playerFixtDF, idealteam)

currentteamfullinfo = CalculateExpectedPoint(playerFixtDF, currentteam)

currentteamgameweekexpectedpoint = GetCurrentExpectedPoint(playerFixtDF, currentteam)

# then, what happens if you forcibly exclude each player, how much better/worse is the total points?
# i think the question is, what do you gain by forcibly including each suggested player

# start again. want to know:
# total points with ideal team (replacing price of players you already have)
# total points you get by excluding each player on their own or with another player

# not sure how to do the thing with replacing your players and estimating curren team value, will come back to that

dum = cbind(currentteam,
            CalculateExtraPointForOneTransfer(currentteam),
            CalculateBestTwoTransfersByPlayer(currentteam))
