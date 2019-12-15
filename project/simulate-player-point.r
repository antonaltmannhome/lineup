### just thinking about the idea of simulating the teams instead, see how different the choices would be

source('c:/git/lineup/new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

# might be worth updating the prices? not completely convneient to do that right now though, so do manually

playerDF = ffModel:::CalculateUpToDatePlayerSmooth(gbgdf)
# NB this is a bit slow but you can get the game by game calculations this way:
# gbgdf = CalculateHistoricExpectedMinute(gbgdf)

fixtdf = getfixturegoal(resultdf, fixtdf)

# who's got a kind and tricky schedule to come:
fixtdf %>%
  filter(gameweek <= min(gameweek) + 9) %>%
  group_by(team) %>%
  summarise(sumEScored = sum(gwweight * escored),
            sumEConceded = sum(gwweight * econceded)) %>%
  arrange(desc(sumEScored - sumEConceded))

gbgdf = processdeserved(gbgdf)
summarydf=processdeserved(summarydf)

playerDF = ffModel:::CalculateLatestGoalAssistRate(playerDF, gbgdf, summarydf, resultDF)

# might want to do this:
# source(paste0(USERPATH, 'data fetching/strip_ffprice.r')); StripFFPrice()
playerDF = ffDataJoining:::MatchFFPlayerData(playerDF)

playerfixtdf = getplayerfixture(fixtdf, playerDF, gbgdf)
playerfixtdf = getfixtureexpectedpoint(playerfixtdf)
playerDF = getplayervalue(playerDF, playerfixtdf)

currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))
# test it handles rarely playing players well
currentteam[which(currentteam$ffposition == 'm')[1],] = c('mancity', 'riyad mahrez', 'm')

currentPlayerFixtDF = semi_join(playerfixtdf, currentteam, c('team', 'player')) %>%
                        select(player, team, isHome, oppteam, gameweek, ffPosition,
                               probStart, probOffBench, eMinStart, eMinBench, eMin,
                              egoal, eassist, eteamconceded, egksave)

### argh, it's more difficult than i thought to get retro-player expected minutes. want to know distro of minutes given all the expected minute values
### don't think you need to do anything that complicated.
### just simulate 1/0 do they start or not, then assume they play the expected minutes if they start. that's got to be close to correct.
### the expected minutes will be too high for ever-presents of course, but that's a model issue to be dealt with at model level

# we need two things, the newly calculated expected points (which does minutes played properly) which we use to select the team. then we use the sims to calcualte actual points

currentPlayerFixtDF$pointForGoal = c(4, 5, 6, 6)[match(currentPlayerFixtDF$ffPosition, c('f', 'm', 'd', 'g'))]
currentPlayerFixtDF$pointForCS = c(0, 1, 4, 4)[match(currentPlayerFixtDF$ffPosition, c('f', 'm', 'd', 'g'))]
currentPlayerFixtDF$eMinPoint = with(currentPlayerFixtDF, 2 * probStart * (eMinStart > 60) +
                                                          (1 - probStart) * probOffBench)
currentPlayerFixtDF = currentPlayerFixtDF %>%
  mutate(pointForGoal = c(4, 5, 6, 6)[match(ffPosition, c('f', 'm', 'd', 'g'))],
         pointForCS = c(0, 1, 4, 4)[match(ffPosition, c('f', 'm', 'd', 'g'))],
         pointForGKSave = c(0, 0, 0, 1)[match(ffPosition, c('f', 'm', 'd', 'g'))],
         eMinPoint = 2 * probStart * (eMinStart > 60) + (1 - probStart) * probOffBench,
         eGoalPoint = pointForGoal * egoal * eMin / 94,
         eAssistPoint = 3 * eassist * eMin / 94,
         eCSPoint = pointForCS * dpois(0, eteamconceded) * probStart * (eMinStart > 60)) %>%
          rowwise() %>%
         mutate(eGKSavePoint = pointForGKSave * sum(floor( (0:10)/3) * dpois(0:10, egksave * eMin / 94)),
         ePoint = eMinPoint + eGoalPoint +  eAssistPoint + eCSPoint + eGKSavePoint) %>%
  ungroup()

# diagnosis line
# currentPlayerFixtDF %>% filter(gameweek == 17) %>% select(player, isHome, oppteam, eMin, egoal, eassist, egksave, eMinPoint, eGoalPoint, eAssistPoint, eCSPoint, eGKSavePoint, ePoint)    

SelectBestTeam = function(subPlayerDF) {
  obj=subPlayerDF$ePoint
  myNumPlayer = nrow(subPlayerDF)
  var.types <- c(rep("B", myNumPlayer),'I','I','I','C')
  mat10=matrix(0,nrow=myNumPlayer,ncol=myNumPlayer)
  diag(mat10)=1
  conmat=rbind(
    mat10,
    as.numeric(subPlayerDF$ffPosition == 'g'),
    as.numeric(subPlayerDF$ffPosition == 'd'),
    as.numeric(subPlayerDF$ffPosition == 'f'),
    rep(1,myNumPlayer)
  )
  direction=c(rep('<=',myNumPlayer),'==',rep('>=',2),'==')
  rhs=c(rep(1,myNumPlayer),1,3,1,11)
  sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)
  subPlayerDF$selected = sol$solution == 1
  subPlayerDF$captain = subPlayerDF$ePoint == max(subPlayerDF$ePoint)
  subPlayerDF$viceCaptain = rank(-subPlayerDF$ePoint) == 2
  # ok, so that can give us the chosen squad for each week
  subPlayerDF = subPlayerDF %>%
    mutate(subOrder = 99) %>%
    mutate_cond(!selected & ffPosition != 'g', subOrder = rank(-ePoint))

  return(subPlayerDF)
}

currentPlayerFixtDF = currentPlayerFixtDF %>%
  group_by(gameweek) %>%
  do(SelectBestTeam(.))

simEGoal = currentPlayerFixtDF$egoal * simMinute / 94
simEAssist = currentPlayerFixtDF$eassist * simMinute / 94
simETeamConceded = currentPlayerFixtDF$eteamconceded * simMinute / 94
simEGKSave = currentPlayerFixtDF$egksave * simMinute / 94

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
simPoint = dum$simPoint
simMinute = dum$simMinute

# then once team is selected, need function that brings in the subs/vice captain thing
## right, now we can start on that bit# actually i say sod that, just sub the next best player in, it's close enough surely

ReviseSubOrder1 = function(x) {
  x[which(x == 1)] = 99
  x[which(x == 2)] = 1
  x[which(x == 3)] = 2
  return(x)
}
ReviseSubOrder2 = function(x) {
  x[which(x == 2)] = 99
  x[which(x == 3)] = 2
  return(x)
}
RevisedSelected = function()

RecalculateSelected = function(myGameweek, numSim) {
  cgwIndex = which(currentPlayerFixtDF$gameweek == myGameweek)
  cgwSimPlayed = !near(simPoint[cgwIndex,], 0)
  cgwInfo = currentPlayerFixtDF[cgwIndex,c('selected', 'subOrder', 'captain', 'viceCaptain', 'ffPosition')]
  
  updateSelectedMat = matrix(FALSE, nrow = length(cgwIndex), ncol = numSim)
  # sub the gk if needed
  mainGKIndex = which(cgwInfo$ffPosition == 'g' & cgwInfo$selected)
  subGKIndex = which(cgwInfo$ffPosition == 'g' & !cgwInfo$selected)
  selectedGKPlayed = cgwSimPlayed[which(cgwInfo$ffPosition == 'g' & cgwInfo$selected)]
  updateSelectedMat[mainGKIndex, which(selectedGKPlayed)] = TRUE
  updateSelectedMat[subGKIndex, which(!selectedGKPlayed)] = TRUE

  # then the rest
  revisedSubOrder = matrix(rep(cgwInfo$subOrder, numSim), ncol = numSim)
  # first step, subs who didn't play, subsequent subs get promoted
  # but need to do this twice in case both 1st and 2nd sub don't play
  sub1NotPlayed = which(colSums(revisedSubOrder == 1 & !cgwSimPlayed) == 1)
  for (i in 1:2) {
    revisedSubOrder[,sub1NotPlayed] = apply(revisedSubOrder[,sub1NotPlayed], 2, ReviseSubOrder1)
    sub1NotPlayed = which(colSums(revisedSubOrder == 1 & !cgwSimPlayed) == 1)
  }
  sub2NotPlayed = which(colSums(revisedSubOrder == 2 & !cgwSimPlayed) == 1)
  revisedSubOrder[,sub2NotPlayed] = apply(revisedSubOrder[,sub2NotPlayed], 2, ReviseSubOrder2)
  
  sumSelectedAndPlayed = colSums(cgwSimPlayed & cgwInfo$selected)
  numReqSub = 11 - sumSelectedAndPlayed
  
  ### arrghh too tired, but we're getting there
}