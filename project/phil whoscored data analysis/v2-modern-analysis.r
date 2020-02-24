### went down a few blind alleys with previous approach, let's start again with another idea

source('c:/git/lineup/new-model-startup.r')

# and also, if we have enough data for a player throughout a season, do we really have to make it all oos? yes, because we want to knwo the value of a goal at predicting goals
gbgdf$nonpengoal = with(gbgdf, goal - penaltyscored)
gbgdf$ontargetbutmiss = with(gbgdf, ont - goal)
gbgdf$offtnotbar = with(gbgdf, offt - bar)

# let's please have camelcase vars
gbgdf$nonPenaltyGoal = with(gbgdf, goal - penaltyscored)
gbgdf$onTargetNotGoal = with(gbgdf, ont - goal)
gbgdf$offTargetNotBar = with(gbgdf, offt - bar)
gbgdf$shotInBox = gbgdf$shotib
gbgdf$shotOutBox = gbgdf$shotoob
gbgdf$shot6Yard = gbgdf$shot6yd

# this shows we've got pretty much every goal correctly accounted for:
with(gbgdf, table(shotOutBox + shotInBox + shot6Yard - (offTargetNotBar + onTargetNotGoal + block + bar+ nonPenaltyGoal + penaltyscored)))

seasonTotalDF = gbgdf %>%
  group_by(season, team, player) %>%
  summarise(sumMinute = sum(minute),
            sumNonPenGoal = sum(nonPenaltyGoal),
            sumShotInBox = sum(shotInBox),
            sumShotOutBox = sum(shotOutBox),
            sumShot6Yard = sum(shot6Yard),
            sumOffTargetNotBar = sum(offTargetNotBar),
            sumOnTargetNotGoal = sum(onTargetNotGoal),
            sumBar = sum(bar),
            sumBlock = sum(block),
            sumNonPenaltyGoal = sum(nonPenaltyGoal))

oosDF = gbgdf %>%
  left_join(seasonTotalDF, c('season', 'team', 'player')) %>%
  group_by(season, team, player) %>%
  mutate(oosMinute = sumMinute - minute,
         oosNonPenGoal = sumNonPenaltyGoal - nonPenaltyGoal,
         oosShotInBox = sumShotInBox - shotInBox,
         oosShotOutBox = sumShotOutBox - shotOutBox,
         oosShot6Yard = sumShot6Yard - shot6Yard,
         oosOffTargetNotBar = sumOffTargetNotBar - offTargetNotBar,
         oosOnTargetNotGoal = sumOnTargetNotGoal - onTargetNotGoal,
         oosBar = sumBar - bar,
         oosBlock = sumBlock - block,
         oosNonPenaltyGoal = sumNonPenaltyGoal - nonPenaltyGoal)

# restrict to players who've played at least 10 games in the season, and get rid of gks
subOosDF = oosDF %>%
  filter(oosMinute > 900 &
           mainpos %in% c('AM', 'D', 'DMC', 'FW', 'M')) %>%
  mutate(gameProp = minute / 94,
         oosSumGameProp = oosMinute / 94)

## so the idea now is to map each quantity onto the goals scale, then allow it to be mixed in


AccuracyLikFunct1 = function(theta) {
  overallMeanRate = exp(theta[1])
  overallPriorStrength = exp(theta[2])
  nonPenaltyGoalWeight = exp(theta[3])
  subOosDF$postTopLine = with(subOosDF, overallMeanRate * overallPriorStrength + nonPenaltyGoalWeight * oosNonPenaltyGoal)
  subOosDF$postBottomLine = with(subOosDF, overallPriorStrength + oosSumGameProp * nonPenaltyGoalWeight)
  subOosDF$eGoal = with(subOosDF, gameProp * postTopLine / postBottomLine)
  subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
  sumLogLik = sum(subOosDF$logLik)
  
  return(-sumLogLik)
}

maxInfo1 = nlm(AccuracyLikFunct1, p = c(log(0.05), log(10), 0), stepmax = 2)

# that's our benchmark. now let's add, i dunno, shots in the box

mod = lm(nonPenaltyGoal ~ oosShotInBox, data = subOosDF)
# no, that makes negative fits, can't have that
subOosDF$goalScaleShotInBox = pmax(fitted(mod), 0)


AccuracyLikFunct2 = function(theta) {
  overallMeanRate = exp(theta[1])
  overallPriorStrength = exp(theta[2])
  nonPenaltyGoalWeight = exp(theta[3])
  shotInBoxWeight = exp(theta[4])
  subOosDF$postTopLine = with(subOosDF, overallMeanRate * overallPriorStrength + nonPenaltyGoalWeight * oosNonPenaltyGoal + shotInBoxWeight * goalScaleShotInBox)
  subOosDF$postBottomLine = with(subOosDF, overallPriorStrength + oosSumGameProp * (nonPenaltyGoalWeight + shotInBoxWeight))
  subOosDF$eGoal = with(subOosDF, gameProp * postTopLine / postBottomLine)
  subOosDF$logLik = with(subOosDF, log(dpois(nonPenaltyGoal, eGoal)))
  sumLogLik = sum(subOosDF$logLik)
  
  return(-sumLogLik)
}

maxInfo2 = nlm(AccuracyLikFunct2, p = c(log(0.05), log(10), 0, 0), stepmax = 2)
# done nothign useful at all and made goal coef look weird, this is a fail
