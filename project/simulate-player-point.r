### just thinking about the idea of simulating the teams instead, see how different the choices would be

currentPlayerFixtDF = semi_join(playerfixtdf, currentteam, c('team', 'player')) %>%
                        select(player, team, isHome, oppteam, gameweek, ffPosition,
                               probStart, probOffBench, eMinStart, eMinBench,
                              egoal, eassist, eteamconceded, egksave)

### argh, it's more difficult than i thought to get retro-player expected minutes. want to know distro of minutes given all the expected minute values
### don't think you need to do anything that complicated.
### just simulate 1/0 do they start or not, then assume they play the expected minutes if they start. that's got to be close to correct.
### the expected minutes will be too high for ever-presents of course, but that's a model issue to be dealt with at model level

numSim = 10

simStart = t(sapply(currentPlayerFixtDF$probStart, function(x) rbinom(numSim, 1, x)))
simStartPlusEMin = cbind(currentPlayerFixtDF[,c('eMinStart', 'eMinBench')], simStart)
# what? this is completely wrong
simMinute = t(apply(simStartPlusEMin, 1, function(x) ifelse(x[3:length(x)] == 1, x[1], x[2])))
simEGoal = currentPlayerFixtDF$egoal * simMinute / 94
simEAssist = currentPlayerFixtDF$eassist * simMinute / 94
simETeamConceded = currentPlayerFixtDF$eteamconceded * simMinute / 94

simGoal = t(apply(simEGoal, 1, function(x) rpois(length(x), x)))
simAssist = t(apply(simEAssist, 1, function(x) rpois(length(x), x)))
simTeamConceded = t(apply(simETeamConceded, 1, function(x) rpois(length(x), x)))

MinPointFunct = function(x) {
  minPoint = rep(0, length(x))
  minPoint[which(x > 60)] = 2
  minPoint[which(x > 0 & x < 60)] = 1
  return(minPoint)
}
simMinPoint = t(apply(simMinute, 1, MinPointFunct))
currentPlayerFixtDF$pointForGoal = c(4, 5, 6, 6)[match(currentPlayerFixtDF$ffPosition, c('f', 'm', 'd', 'd'))]
currentPlayerFixtDF$pointForCS = c(0, 1, 4, 4)[match(currentPlayerFixtDF$ffPosition, c('f', 'm', 'd', 'd'))]
simGoalPoint = currentPlayerFixtDF$pointForGoal * simGoal
simAssistPoint = 3 * simAssist
simCSPoint = currentPlayerFixtDF$pointForCS * (simTeamConceded == 0) * (simMinute > 60) 

simPoint = simGoalPoint + simAssistPoint + simCSPoint

# but then you have to add goalie stuff, then the clean sheet stuff

# not what we're going to use, but anyway:
currentPlayerFixtDF$meanPoint = rowSums(simPoint)
currentPlayerFixtDF %>% group_by(team, player) %>% summarise(sumEPoint = sum(meanPoint)) %>% arrange(desc(sumEPoint))

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
