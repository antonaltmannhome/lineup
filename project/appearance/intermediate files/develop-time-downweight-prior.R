# let's develop the time downweight and prior strength features here

source('c:/git/lineup/new-model-startup.r')
source('project/appearance/biased-urn-funct.R')
library(doParallel)
registerDoParallel()

fakeMaxTime = 1000L

# need to sort out seasonNumber. why do we need 1516 in seasonInfoDF?
seasonInfoDF$seasonNumber = match(seasonInfoDF$season, with(seasonInfoDF, sort(season[havegbg])))
resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')
resultDF = resultDF %>%
  mutate(alltimetgn = (seasonNumber - 1) * 38 + teamgamenumber)

numBlockWithinSeason = 4
resultDF$inBlock = as.integer(with(resultDF, (seasonNumber - 1) * numBlockWithinSeason +
                                     cut(teamgamenumber, br = seq(0.5, 38.5, le = 5), labels = FALSE)))

gbgdf = lazy_left_join(gbgdf,
                       resultDF,
                       c('season', 'team', 'teamgamenumber'),
                       c('inBlock', 'alltimetgn'))

gbgdf2 = gbgdf %>%
  filter(!grepl('(injury|suspension)', startTime)) %>%
  mutate(startTime2 = ifelse(!startTime %in% c('U', 'UU'), startTime, as.character(fakeMaxTime)),
         endTime2 = ifelse(!startTime %in% c('U', 'UU'), endTime, as.character(fakeMaxTime))) %>%
  mutate(startTime2 = as.integer(startTime2),
         endTime2 = as.integer(endTime2))
# except we've got the problem that players sometimes officially start the match at eg 97 minutes.
# let's ceiling the endTime to be the greater of 94 and the biggest startTime of the match

gbgdf2$mainpos2 = with(gbgdf2, case_when(
  mainpos %in% c('D', 'DMC') ~ 'def',
  mainpos == 'M' ~ 'mid',
  mainpos %in% c('AM', 'FW') ~ 'att',
  TRUE ~ 'other'
))

gbgdf2 = gbgdf2 %>%
  select(alltimetgn, team, player, startTime2, endTime2, minute, played, available, mainpos2, inBlock)

GetPastResultGbgDF = function(myTeam, myBlock, blockWindowSize, timeDownWeightCoef) {
  
  pastTeamResultDF = resultDF %>%
    filter(team == myTeam & inBlock < myBlock & inBlock >= myBlock - blockWindowSize)

  maxAllTimeTgn = max(pastTeamResultDF$alltimetgn)
  pastTeamResultDF = pastTeamResultDF %>%
    mutate(timeDownWeight = exp(-timeDownWeightCoef * (maxAllTimeTgn - alltimetgn)))
  
  pastTeamGbgDF = gbgdf2 %>%
    semi_join(pastTeamResultDF, c('team', 'alltimetgn'))

  pastTeamGbgDF = semi_join(pastTeamGbgDF,
                        pastTeamGbgDF %>%
                          group_by(player) %>%
                          summarise(sumMinute = sum(minute)) %>%
                          filter(sumMinute > 10),
                        'player')

  return(lst(pastTeamResultDF, pastTeamGbgDF))
}

CalculateAllMatchSectionLogLik = function(theta0, allPermutationDF, priorStrength) {
  theta = exp(theta0)
  allPermutationDF = allPermutationDF %>%
    rowwise() %>%
    mutate(byMatchSectionProb = AABiasedUrnProb(allPermutationByPlayerNumber, activePlayerList, theta),
           byMatchSectionLogLik = minDiff * log(byMatchSectionProb))
  dataLogLik = with(allPermutationDF, sum(timeDownWeight * byMatchSectionLogLik))
  
  penaltyLik = priorStrength * sum(theta0^2)
  totalLogLik = dataLogLik - penaltyLik
  if (FALSE) {
    print(theta)
    print(totalLogLik)
  }
  return(-totalLogLik)
}

GetPlayerAppearanceMleByBlock = function(myTeam, myMainpos, myBlock, blockWindowSize, priorStrength, timeDownWeightCoef) {
  
  dum = GetPastResultGbgDF(myTeam, myBlock, blockWindowSize, timeDownWeightCoef)
  pastTeamResultDF = dum$pastTeamResultDF
  pastTeamGbgDF = dum$pastTeamGbgDF
  
  splitTeamDF = pastTeamGbgDF %>%
    filter(mainpos2 == myMainpos) %>%
    group_by(team, alltimetgn) %>%
    do(SplitGame(.$player, .$startTime2, .$endTime2))
  
  unMatchSection = splitTeamDF %>%
    group_by(alltimetgn, matchSection, minDiff) %>%
    summarise(numWhoPlayed = sum(played)) %>%
    ungroup() %>%
    mutate(logLik = NA)
  
  # but this leaves us with some match sections where no players play
  # which we don't want, so get rid
  unMatchSection = unMatchSection %>%
    filter(numWhoPlayed > 0)
  splitTeamDF = splitTeamDF %>%
    semi_join(unMatchSection, c('alltimetgn', 'matchSection'))
  
  # who has made the median number of appearances, they should have value zero
  myTotalMinutePlayed = splitTeamDF %>%
    group_by(player) %>%
    summarise(totalMinutePlayed = sum(endTime - startTime)) %>%
    arrange(totalMinutePlayed)
  
  referencePlayer = myTotalMinutePlayed$player[floor(0.5 * nrow(myTotalMinutePlayed)+1)]
  myUnPlayer = c(referencePlayer, setdiff(myTotalMinutePlayed$player, referencePlayer))
  splitTeamDF$playerNumber = match(splitTeamDF$player, myUnPlayer)
  
  allPermutationDF = splitTeamDF %>%
    group_by(alltimetgn, matchSection) %>%
    summarise(allPermutationByPlayerNumber = CalculateAllPermutation(played, playerNumber),
              activePlayerList = list(playerNumber),
              minDiff = minDiff[1]) %>%
    lazy_left_join(pastTeamResultDF, 'alltimetgn', 'timeDownWeight')
  
  theta0 = rep(0, length(myUnPlayer))
  
  maxInfo = nlm(CalculateAllMatchSectionLogLik, p = theta0,
                allPermutationDF = allPermutationDF,
                priorStrength = priorStrength,
                stepmax = 5)
  
  myAppearanceMle = data.frame(team = myTeam, inBlock = myBlock, mainpos2 = myMainpos, player = myUnPlayer,
                               appearanceMle = maxInfo$estimate,
                               appearanceOdds = exp(maxInfo$estimate) / sum(exp(maxInfo$estimate)))
  
  return(myAppearanceMle)
}
