

source('c:/research/lineup/new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

### want to rule out situations where there was a weirdly small number of players in each position
gbgdf$tgId = with(gbgdf, paste(season, teamgamenumber, team))

# let's see what a normal number actually is though
numPos = gbgdf %>%
  filter(!is.na(mainpos)) %>%
  group_by(tgId) %>%
  summarise(numGk = sum( (startTime == '0') & (mainpos == 'GK')),
            numD = sum( (startTime == '0') & (mainpos == 'D')),
            numM = sum( (startTime == '0') & (mainpos %in% c('DMC', 'M'))),
            numF = sum( (startTime == '0') & (mainpos %in% c('AM', 'FW'))),
            numPosGK = sum(mainpos == 'GK'),
            numPosD = sum(mainpos == 'D'),
            numPosM = sum(mainpos  %in% c('DMC', 'M')),
            numPosF = sum(mainpos %in% c('AM', 'FW')))

mcnumpos = numPos %>% filter(grepl('1920.+mancity', tgId))# %>% gather(position, numplayer, -tgId)

# can we use BiasedUIrn to predict prob of each formation?
LikFunct = function(theta) {
  posWgt = c(1, exp(theta))
  mcnumpos$logProb = rep(NA, nrow(mcnumpos))
  for (j in 1:nrow(mcnumpos)) {
    mcnumpos$logProb[j] = with(mcnumpos[j,],
                       log(BiasedUrn::dMFNCHypergeo(x = c(numGk, numD, numM, numF),
                                                m = c(numPosGK, numPosD, numPosM, numPosF),
                                                n = 11, odds = posWgt)))
  }
  sumLogProb = sum(mcnumpos$logProb)
  
  return(-sumLogProb)
}

maxInfo = nlm(LikFunct, p = c(0,0,0))
optWgtVec = c(1, exp(maxInfo$est))
# so now we can surely predict prob of various likely formations
possibleForm = rbind(c(1, 2, 4, 4), c(1, 3, 4, 3), c(1, 3, 3, 4), c(1, 4, 4, 2))
totalAvail = matrix(rep(c(2, 5, 10, 6), 4), byrow = TRUE, nrow = 4)
apply(cbind(possibleForm, totalAvail), 1,
      function(x) BiasedUrn::dMFNCHypergeo(x[1:4], x[5:8], 11, optWgtVec))

# ok. that seems to be working fine
# the next question is, how likely is ech player to be picked?

CheckByTeamPos = function(mySeason, myTeam, myPos) {
  # let's take midfielders
  # this feels like a special case of this. because you can only pick each element a maximum of once.
  # so that's surely something else
  # let's just do it anyway though
  mcmfAvailable = gbgdf %>%
    filter(season == mySeason & mainpos %in% myPos & team == myTeam) %>%
    select(tgId, player, available) %>%
    mutate(available = as.numeric(available)) %>%
    spread(key = player, available)
  mcmfIsStart = gbgdf %>%
    filter(season == mySeason & mainpos %in% myPos & team == myTeam) %>%
    select(tgId, player, isStart) %>%
    mutate(isStart = as.numeric(isStart)) %>%
    spread(key = player, isStart)
  
  mcmfAvailable[which(is.na(mcmfAvailable), arr = T)] = FALSE
  mcmfIsStart[which(is.na(mcmfIsStart), arr = T)] = FALSE
  
  currentPlayer = names(mcmfIsStart)[-1]
  availableMat = as.matrix(mcmfAvailable[,-1])
  isStartMat = as.matrix(mcmfIsStart[,-1])
  mcmfTotal = rowSums(mcmfIsStart[,2:ncol(mcmfIsStart)])
  
  LikFunct = function(theta, mcmfTotal, availableMat, isStartMat) {
    playerProb = c(1, exp(theta))
    logProb = rep(NA, length(mcmfTotal))
    for (j in 1:length(mcmfTotal)) {
      availableIndex = which(availableMat[j,] == 1)
      logProb[j] = log(BiasedUrn::dMFNCHypergeo(x = isStartMat[j,availableIndex],
                                                m = availableMat[j,availableIndex],
                                                n = mcmfTotal[j],
                                                odds = playerProb[availableIndex]))
    }
    sumLogProb = sum(logProb)
    
    return(-sumLogProb)
  }
  maxInfo = nlm(LikFunct, p = rep(0, length(currentPlayer) - 1),
                mcmfTotal = mcmfTotal,
                availableMat = availableMat,
                isStartMat = isStartMat)
  optWgtVec = c(1, exp(maxInfo$est))
  cbind(currentPlayer, optWgtVec)[order(optWgtVec),]
  
  # surprising variation among the players,let's see how that translates probability wise
  # actually, i want expected prob for each player, can i get that?
  print(cbind(currentPlayer, BiasedUrn::meanMFNCHypergeo(rep(1, length(currentPlayer)), 4, optWgtVec, precision = 0.1)))
  # that looks pretty reasonable to be fair
  # compared to the raw data:
  print(gbgdf %>% filter(season == mySeason & team == myTeam & mainpos %in% myPos) %>% group_by(player) %>% summarise(sum(available), sum(isStart)))
  # gundogan starts 0.75 of the time, but on some occasions, other players haven't been available. we're assuming that everyone's available in that calculation
  
  # but of course we've got downweighting to worry about, plus tiredness, recovery from injury....ugh
}
# no there is something badly wrong here, try liverpool, AM,FW
# two problems: (1) need some sort of penalty, not sure what (2) don't force a player who hardly ever plays to be 1
