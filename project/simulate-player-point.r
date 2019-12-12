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
simMinute = t(apply(simStartPlusEMin, 1, function(x) ifelse(x[3:length(x)] == 1, x[1], x[2])))
simEGoal = currentPlayerFixtDF$egoal * simMinute / 94
simEAssist = currentPlayerFixtDF$eassist * simMinute / 94
simETeamConceded = currentPlayerFixtDF$eteamconceded * simMinute / 94

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
