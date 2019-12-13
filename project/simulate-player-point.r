### just thinking about the idea of simulating the teams instead, see how different the choices would be

source('run-model.r')
currentPlayerFixtDF = semi_join(playerfixtdf, currentteam, c('team', 'player')) %>%
                        select(player, team, isHome, oppteam, gameweek, ffPosition,
                               probStart, probOffBench, eMinStart, eMinBench,
                              egoal, eassist, eteamconceded, egksave)

### argh, it's more difficult than i thought to get retro-player expected minutes. want to know distro of minutes given all the expected minute values
### don't think you need to do anything that complicated.
### just simulate 1/0 do they start or not, then assume they play the expected minutes if they start. that's got to be close to correct.
### the expected minutes will be too high for ever-presents of course, but that's a model issue to be dealt with at model level

numSim = 10

SimPointFunct = function(numSim) {
  simStart = t(sapply(currentPlayerFixtDF$probStart, function(x) rbinom(numSim, 1, x)))
  simBench = (simStart == 0) * t(sapply(currentPlayerFixtDF$probOffBench, function(x) rbinom(numSim, 1, x)))
  simMinFromStart = currentPlayerFixtDF$eMinStart * simStart
  simMinFromBench = currentPlayerFixtDF$eMinBench * simBench
  # what? this is completely wrong
  simMinute = simMinFromStart + simMinFromBench
  simEGoal = currentPlayerFixtDF$egoal * simMinute / 94
  simEAssist = currentPlayerFixtDF$eassist * simMinute / 94
  simETeamConceded = currentPlayerFixtDF$eteamconceded * simMinute / 94
  simEGKSave = currentPlayerFixtDF$egksave * simMinute / 94
  
  simGoal = t(apply(simEGoal, 1, function(x) rpois(length(x), x)))
  simAssist = t(apply(simEAssist, 1, function(x) rpois(length(x), x)))
  simTeamConceded = t(apply(simETeamConceded, 1, function(x) rpois(length(x), x)))
  simGKSave = t(apply(simEGKSave, 1, function(x) rpois(length(x), x)))
  
  MinPointFunct = function(x) {
    minPoint = rep(0, length(x))
    minPoint[which(x > 60)] = 2
    minPoint[which(x > 0 & x < 60)] = 1
    return(minPoint)
  }
  simMinPoint = t(apply(simMinute, 1, MinPointFunct))
  currentPlayerFixtDF$pointForGoal = c(4, 5, 6, 6)[match(currentPlayerFixtDF$ffPosition, c('f', 'm', 'd', 'g'))]
  currentPlayerFixtDF$pointForCS = c(0, 1, 4, 4)[match(currentPlayerFixtDF$ffPosition, c('f', 'm', 'd', 'g'))]
  simGoalPoint = currentPlayerFixtDF$pointForGoal * simGoal
  simAssistPoint = 3 * simAssist
  simCSPoint = currentPlayerFixtDF$pointForCS * (simTeamConceded == 0) * (simMinute > 60) 
  simGKPoint = (currentPlayerFixtDF$ffPosition == 'g') * floor(simGKSave / 3)
  
  simPoint = simMinPoint + simGoalPoint + simAssistPoint + simCSPoint + simGKPoint
  
  # not what we're going to use, but anyway:
  currentPlayerFixtDF$meanPoint = rowSums(simPoint)
  
  print(currentPlayerFixtDF %>%
          group_by(team, player) %>%
          summarise(sumEPoint = sum(meanPoint)) %>%
          arrange(desc(sumEPoint)))
  
  return(lst(simPoint, simMinute))
}
  

# this is really hard to browse at the moment, that's the first problem
BrowsePlayerGW = function(playerString, myGW) {
  playerGWIndex = with(currentPlayerFixtDF, which(grepl(playerString, player) & gameweek == myGW))
  print(currentPlayerFixtDF[playerGWIndex,])
  print('sim minute:')
  print(simMinute[playerGWIndex,])
  print('sim expected goal:')
  print(simEGoal[playerGWIndex,])
  print('sim expected assists:')
  print(simEAssist[playerGWIndex,])
  print('sim team conceded:')
  print(simETeamConceded[playerGWIndex,])
}

# ok but how do we actually predict number of points scored? think we use what we already have in run-knapsack, still go by expected points. but do the subbing in thing, because it's important
# but do it later

## so let's start selecting our captain and our team for each gameweek
# ah but you might want to captain a player who isn't gertain to start, if there Epoint is v high

dum = SimPointFunct(1000)
currentPlayerFixtDF$ePoint = rowSums(dum$simPoint)
currentPlayerFixtDF$ePoint