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

## this will go into a library eventually

MakeTeamFixtDF = function(myTeam) {
  
  teamFixtDF = semi_join(playerfixtdf %>%
                           filter(gameweek <= min(gameweek) + 9),  # i think we should truncate to next ten weeks
                         myTeam, c('team', 'player')) %>%
    select(player, team, isHome, oppteam, gameweek, ffPosition,
           probStart, probOffBench, eMinStart, eMinBench, eMin,
           egoal, eassist, eteamconceded, egksave) %>%
    arrange(gameweek, match(ffPosition, c('g', 'd', 'm', 'f')))
  
  ### argh, it's more difficult than i thought to get retro-player expected minutes. want to know distro of minutes given all the expected minute values
  ### don't think you need to do anything that complicated.
  ### just simulate 1/0 do they start or not, then assume they play the expected minutes if they start. that's got to be close to correct.
  ### the expected minutes will be too high for ever-presents of course, but that's a model issue to be dealt with at model level
  
  # we need two things, the newly calculated expected points (which does minutes played properly) which we use to select the team. then we use the sims to calcualte actual points
  
  teamFixtDF$pointForGoal = c(4, 5, 6, 6)[match(teamFixtDF$ffPosition, c('f', 'm', 'd', 'g'))]
  teamFixtDF$pointForCS = c(0, 1, 4, 4)[match(teamFixtDF$ffPosition, c('f', 'm', 'd', 'g'))]
  teamFixtDF$eMinPoint = with(teamFixtDF, 2 * probStart * (eMinStart > 60) +
                                (1 - probStart) * probOffBench)
  teamFixtDF = teamFixtDF %>%
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

  return(teamFixtDF)  
}


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
  # you might not have enough eligible players to field of course
  maxEligiblePlayer = pmin(10, sum(subPlayerDF$ffPosition != 'g')) + pmin(1, sum(subPlayerDF$ffPosition == 'g'))
  rhs=c(rep(1,myNumPlayer),1,3,1,maxEligiblePlayer)
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

SimPointFunct = function(numSim, teamFixtDF) {
  simStart = t(sapply(teamFixtDF$probStart, function(x) rbinom(numSim, 1, x)))
  simBench = (simStart == 0) * t(sapply(teamFixtDF$probOffBench, function(x) rbinom(numSim, 1, x)))
  simMinFromStart = teamFixtDF$eMinStart * simStart
  simMinFromBench = teamFixtDF$eMinBench * simBench
  # what? this is completely wrong
  simMinute = simMinFromStart + simMinFromBench
  simEGoal = teamFixtDF$egoal * simMinute / 94
  simEAssist = teamFixtDF$eassist * simMinute / 94
  simETeamConceded = teamFixtDF$eteamconceded * simMinute / 94
  simEGKSave = teamFixtDF$egksave * simMinute / 94
  
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
  simGoalPoint = teamFixtDF$pointForGoal * simGoal
  simAssistPoint = 3 * simAssist
  simCSPoint = teamFixtDF$pointForCS * (simTeamConceded == 0) * (simMinute > 60) 
  simGKPoint = (teamFixtDF$ffPosition == 'g') * floor(simGKSave / 3)
  
  simPoint = simMinPoint + simGoalPoint + simAssistPoint + simCSPoint + simGKPoint
  
  # not what we're going to use, but anyway:
  teamFixtDF$meanPoint = rowSums(simPoint)
  
  print(teamFixtDF %>%
          group_by(team, player) %>%
          summarise(sumEPoint = sum(meanPoint)) %>%
          arrange(desc(sumEPoint)))
  
  return(lst(simPoint, simMinute))
}


# this is really hard to browse at the moment, that's the first problem
BrowsePlayerGW = function(playerString, myGW) {
  playerGWIndex = with(teamFixtDF, which(grepl(playerString, player) & gameweek == myGW))
  print(teamFixtDF[playerGWIndex,])
  print('sim minute:')
  print(simMinute[playerGWIndex,])
  print('sim expected goal:')
  print(simEGoal[playerGWIndex,])
  print('sim expected assists:')
  print(simEAssist[playerGWIndex,])
  print('sim team conceded:')
  print(simETeamConceded[playerGWIndex,])
}

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
RevisedSelectedOutfield = function(bigVec) {
  numOutfieldPlayer = (length(bigVec) - 1) / 3
  myPlayedStatus = bigVec[1:numOutfieldPlayer]
  myInitSelectedStatus = bigVec[(numOutfieldPlayer + 1) : (2 * numOutfieldPlayer)]
  mySubOrder = bigVec[(2 * numOutfieldPlayer + 1): (3 * numOutfieldPlayer)]
  myNumReqSub = tail(bigVec, 1)
  
  myRevisedSelectedStatus = myInitSelectedStatus
  myRevisedSelectedStatus[which(myPlayedStatus == 0 & myInitSelectedStatus == 1)] = FALSE
  myRevisedSelectedStatus[which(mySubOrder <= myNumReqSub)] = TRUE
  
  return(myRevisedSelectedStatus)
}

RecalculateSelected = function(myGameweek, numSim, simMinute, teamFixtDF) {
  cgwGKIndex = with(teamFixtDF, which(gameweek == myGameweek & ffPosition == 'g'))
  cgwOFIndex = with(teamFixtDF, which(gameweek == myGameweek & ffPosition != 'g'))
  cgwGKSimPlayed = !near(simMinute[cgwGKIndex,], 0)
  cgwOFSimPlayed = !near(simMinute[cgwOFIndex,], 0)
  cgwGKInfo = teamFixtDF[cgwGKIndex,c('selected', 'subOrder', 'captain', 'viceCaptain')]
  cgwOFInfo = teamFixtDF[cgwOFIndex,c('selected', 'subOrder', 'captain', 'viceCaptain')]
  
  myNumGK = length(cgwGKIndex)
  revisedGKSelectedMat = matrix(FALSE, nrow = myNumGK, ncol = numSim)
  # sub the gk if needed
  mainGKIndex = which(cgwGKInfo$selected)
  subGKIndex = which(!cgwGKInfo$selected)
  mainGKPlayed = cgwGKSimPlayed[mainGKIndex,]
  subGKPlayed = cgwGKSimPlayed[subGKIndex,]
  revisedGKSelectedMat[mainGKIndex, which(mainGKPlayed)] = TRUE
  revisedGKSelectedMat[subGKIndex, which(!mainGKPlayed & subGKPlayed)] = TRUE
  
  # then the rest
  revisedSubOrder = matrix(rep(cgwOFInfo$subOrder, numSim), ncol = numSim)
  # first step, subs who didn't play, subsequent subs get promoted
  # but need to do this twice in case both 1st and 2nd sub don't play
  sub1NotPlayed = which(colSums(revisedSubOrder == 1 & !cgwOFSimPlayed) == 1)
  if (length(sub1NotPlayed) > 0) {
    for (i in 1:2) {
      revisedSubOrder[,sub1NotPlayed] = apply(revisedSubOrder[,sub1NotPlayed, drop = FALSE], 2, ReviseSubOrder1)
      sub1NotPlayed = which(colSums(revisedSubOrder == 1 & !cgwOFSimPlayed) == 1)
    }
  }
  sub2NotPlayed = which(colSums(revisedSubOrder == 2 & !cgwOFSimPlayed) == 1)
  if (length(sub2NotPlayed) > 0) {
    revisedSubOrder[,sub2NotPlayed] = apply(revisedSubOrder[,sub2NotPlayed, drop = FALSE], 2, ReviseSubOrder2)
  }
    
  sumSelectedAndPlayed = colSums(cgwOFSimPlayed & cgwOFInfo$selected)
  numReqSub = pmin(3, 10 - sumSelectedAndPlayed)
  
  ### now label the non-playing selecteds to enable us to loop
  # no, but you need to bring in the selected info, hmm, how to do that
  # making a big array seems the only way i can think of
  bigArr = rbind(cgwOFSimPlayed,
                 matrix(rep(cgwOFInfo$selected, numSim), ncol = numSim),
                 revisedSubOrder,
                 numReqSub)
  revisedOFSelectedMat = apply(bigArr, 2, RevisedSelectedOutfield)
  
  revisedSelectedMat = rbind(revisedGKSelectedMat, revisedOFSelectedMat)
  
  # then reallocate the captain if necessary
  
  
  revisedSelectedDF = as.data.frame(revisedSelectedMat)
  
  return(revisedSelectedDF)
}


RecalculateCaptain = function(myGameweek, numSim, simMinute, teamFixtDF) {
  cgwIndex = with(teamFixtDF, which(gameweek == myGameweek))
  cgwSimPlayed = !near(simMinute[cgwIndex,], 0)
  revisedCaptainMat = matrix(FALSE, nrow = length(cgwIndex), ncol = numSim)
  isCaptainIndex = which(teamFixtDF$captain[cgwIndex])
  isViceCaptainIndex = which(teamFixtDF$viceCaptain[cgwIndex])
  captainPlayedIndex = which(cgwSimPlayed[isCaptainIndex,])
  captainNotPlayedIndex = which(!cgwSimPlayed[isCaptainIndex,])
  revisedCaptainMat[isCaptainIndex, captainPlayedIndex] = TRUE
  revisedCaptainMat[isViceCaptainIndex, captainNotPlayedIndex] = TRUE
  
  revisedCaptainDF = as.data.frame(revisedCaptainMat)
  
  return(revisedCaptainDF)
}

ViewSingleSim = function(myGameweek, mySimNo) {
  myIndex = which(teamFixtDF$gameweek == myGameweek)
  View(cbind(teamFixtDF[myIndex, c('player', 'ePoint', 'selected', 'subOrder')], simMinute[myIndex, mySimNo], revisedSelected[myIndex, mySimNo]))
}

CalculateSimulatedPoint = function(myTeam, numSim) {
  
  teamFixtDF = MakeTeamFixtDF(myTeam)
  # diagnosis line
  # teamFixtDF %>% filter(gameweek == 17) %>% select(player, isHome, oppteam, eMin, egoal, eassist, egksave, eMinPoint, eGoalPoint, eAssistPoint, eCSPoint, eGKSavePoint, ePoint)    
  
  teamFixtDF = teamFixtDF %>%
    group_by(gameweek) %>%
    do(SelectBestTeam(.))
  
  # ok but how do we actually predict number of points scored? think we use what we already have in run-knapsack, still go by expected points. but do the subbing in thing, because it's important
  # but do it later
  
  ## so let's start selecting our captain and our team for each gameweek
  # ah but you might want to captain a player who isn't gertain to start, if there Epoint is v high
  
  dum = SimPointFunct(numSim, teamFixtDF)
  simPoint = dum$simPoint
  simMinute = dum$simMinute
  
  # then once team is selected, need function that brings in the subs/vice captain thing
  ## right, now we can start on that bit# actually i say sod that, just sub the next best player in, it's close enough surely
  revisedSelected = purrr::map_df(unique(teamFixtDF$gameweek), RecalculateSelected, numSim = 1000, simMinute = simMinute, teamFixtDF = teamFixtDF)
  
  revisedCaptain = purrr::map_df(unique(teamFixtDF$gameweek), RecalculateCaptain, numSim = 1000, simMinute = simMinute, teamFixtDF = teamFixtDF)
  
  ### yeehah, done all the tedious stuff. now just a case of adding up the expected points, and get summed points for a chosen team, the fun bit
  simPointAdjForPlaying = simPoint * revisedSelected + simPoint * revisedCaptain
  
  totalPointBySim = colSums(simPointAdjForPlaying)
  meanTotalPoint = mean(totalPointBySim)
  
  return(meanTotalPoint)
}

# let's experiment with it a bit
numSim = 1000
currentteam = read.csv(paste(DATAPATH, 'currentteam.csv', sep = ''))
# test it handles rarely playing players well
currentteam[which(currentteam$ffposition == 'd')[1],] = c('liverpool', 'trent alexander arnold', 'd')
currentteam[which(currentteam$ffposition == 'd')[2],] = c('mancity', 'kyle walker', 'd')
currentteam[which(currentteam$ffposition == 'm')[1],] = c('liverpool', 'sadio mane', 'm')
currentteam[which(currentteam$ffposition == 'm')[2],] = c('mancity', 'raheem sterling', 'm')
currentteam[which(currentteam$ffposition == 'm')[3],] = c('mancity', 'kevin de bruyne', 'm')
currentteam[which(currentteam$ffposition == 'f')[3],] = c('tottenham', 'harry kane', 'm')

CalculateSimulatedPoint(currentTeam, 1000)

# 1000 sims seems enough. doing sensible things
# does crash if you do something ludicrous like player 5 liverpool players when they don't have a fixture one week, probably not an issue

# have got ideal team from get-optimal-team and..

forcedInclusionExclusion = tibble(team = 'mancity', player = 'aymeric laporte', includeOrExclude = 'exclude')
idealteam = RunKnapsack(playerDF, forcedInclusionExclusion, currentmoney)
CalculateSimulatedPoint(idealteam, 1000)
# 559

forcedInclusionExclusion = tibble(team = 'mancity', player = 'kevin de bruyne', includeOrExclude = 'include')
idealteam = RunKnapsack(playerDF, forcedInclusionExclusion, currentmoney)
CalculateSimulatedPoint(idealteam, 1000)
# 551

forcedInclusionExclusion = tibble(team = 'liverpool', player = 'sadio mane', includeOrExclude = 'include')
idealteam = RunKnapsack(playerDF, forcedInclusionExclusion, currentmoney)
CalculateSimulatedPoint(idealteam, 1000)
# 544

forcedInclusionExclusion = tibble(team = c('mancity', 'liverpool'), player = c('raheem sterling', 'sadio mane'), includeOrExclude = rep('include', 2))
idealteam = RunKnapsack(playerDF, forcedInclusionExclusion, currentmoney)
CalculateSimulatedPoint(idealteam, 1000)
# 551

# interesting, we should ignore the captain thing, just doing what we're doing seems fine.
# we've not tried forcibly including a cheap but ever present defender though, will try that next
# but this doesn't seem like a game changer. let's focus on the expected minutes and time downweight in scoring instead
