# try to suggest transfers to me, or at least indicate which transfers are apparently suggested by the ideal team, but are actually basically pointless when it comes to expected gains

# so let's firstly pick two random players to exclude

forcedInclusionExclusion = currentteam %>%
  mutate(includeOrExclude = 'include') %>%
  mutate(toExclude = (1:n()) %in% resample(1:n(), 2)) %>%
  mutate_cond(toExclude,
              includeOrExclude = 'exclude')


CalculatePointGainFromTransfer = function(forcedInclusionExclusion) {
  idealteam = RunKnapsack(playerDF, forcedInclusionExclusion, currentmoney)
  idealteamgameweekexpectedpoint = getcurrentexpectedpoint(playerfixtdf, idealteam)

  idealteamfullinfo = calculateexpectedpoint(playerfixtdf, idealteam, warnAboutMissingPlayer = FALSE)

  pointGainFromTransfer = idealteamfullinfo$totalexpectedpoint - currentteamfullinfo$totalexpectedpoint

  return(lst(idealteam, pointGainFromTransfer))
}

# can we unloop this?
fiList = NULL
fiList[[1]] = forcedInclusionExclusion = currentteam %>%
  mutate(includeOrExclude = 'include') %>%
  mutate(toExclude = (1:n()) %in% resample(1:n(), 2)) %>%
  mutate_cond(toExclude,
              includeOrExclude = 'exclude')

fiList[[2]] = forcedInclusionExclusion = currentteam %>%
  mutate(includeOrExclude = 'include') %>%
  mutate(toExclude = (1:n()) %in% resample(1:n(), 2)) %>%
  mutate_cond(toExclude,
              includeOrExclude = 'exclude')

lapply(fiList, CalculatePointGainFromTransfer)

# you can unloop it. let's do that

totalNumPossible = choose(15, 2)
allPossibleTransfer = combn(15, 2)
allPossiblePointGain = rep(NA, totalNumPossible)
allPossibleDrop = array(NA, dim = c(totalNumPossible, 2))
allPossibleNewPlayer = array(NA, dim = c(totalNumPossible, 2))
for (j in 1:totalNumPossible) {
  adjCurrentTeam = currentteam %>%
    mutate(includeOrExclude = 'include')
  adjCurrentTeam$includeOrExclude[allPossibleTransfer[,j]] = 'exclude'
  alternativeTeam = CalculatePointGainFromTransfer(adjCurrentTeam)
  allPossiblePointGain[j] = alternativeTeam$pointGainFromTransfer
  allPossibleDrop[j,] = currentteam$player[allPossibleTransfer[,j]]
  allPossibleNewPlayer[j,] = setdiff(alternativeTeam$idealteam$player, currentteam$player)
  if ( (j %% 10) == 0) {
    message('Have calculated gain from ', j, ' pairs so far')
  }
}

print('Top 5 pairs to transfer:')
print(cbind(allPossibleDrop, allPossiblePointGain, allPossibleNewPlayer)[order(-allPossiblePointGain),][1:10,])
# worth knowing what points gain is in immediate week though

# but also worth knowing what gains area available with just 1 transfer

totalNumPossible = 15
allPossiblePointGain = rep(NA, totalNumPossible)
allPossibleNewPlayer = rep(NA, totalNumPossible)
for (j in 1:totalNumPossible) {
  adjCurrentTeam = currentteam %>%
    mutate(includeOrExclude = 'include')
  adjCurrentTeam$includeOrExclude[j] = 'exclude'
  alternativeTeam = CalculatePointGainFromTransfer(adjCurrentTeam)
  allPossiblePointGain[j] = alternativeTeam$pointGainFromTransfer
  allPossibleNewPlayer[j] = setdiff(alternativeTeam$idealteam$player, currentteam$player)
}

print(cbind(currentteam$player, allPossiblePointGain, allPossibleNewPlayer)[order(-allPossiblePointGain),][1:10,])
