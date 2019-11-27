# let's investigate some expected time biases, got the framework to do so, let's just look into the long term thing first
source('c:/research/lineup/project/appearance/set-up-30-wide-df.r')

ViewPlayer = function(mySeason, myPlayer, myGNum) {
  grepExpression = paste0(mySeason,' [a-z]+ ',myGNum,' .*',myPlayer,'.*')
  return(rgbgdf %>% filter(grepl(grepExpression, key)))
}

overallMean = with(gbgdf30, mean(isStart[available]))

LikFunct = function(theta, runMode) {
  dwSpeed = exp(theta[1])
  priorStr = exp(theta[2])
  message('dwSpeed: ', dwSpeed, ', priorStr: ', priorStr)
  rgbgdf2 = rgbgdf
  rgbgdf2$wgt = exp(-dwSpeed * (30 - rgbgdf2$rgnum))
  rgbgdf2$wgt[!rgbgdf2$available] = 0

  smStartDF = rgbgdf2 %>% group_by(key) %>% summarise(smStart = (priorStr * overallMean + sum(wgt * isStart)) / (priorStr + sum(wgt)))
  gbgdf30Plus = left_join(gbgdf30, smStartDF, 'key')

  if (runMode == 'max') {
    gbgdf30Plus = gbgdf30Plus %>%
      mutate(logLik = ifelse(available, log(dbinom(isStart, 1, smStart)), 0))
    meanLogLik = mean(gbgdf30Plus$logLik)

    message('meanLogLik: ', meanLogLik)

    toReturn = -meanLogLik
  }

  if (runMode == 'fit') {
    toReturn = gbgdf30Plus
  }

  return(toReturn)
}

maxInfo = nlm(LikFunct, p = c(log(0.5), log(0.5)), stepmax = 0.5, runMode = 'max')

# awesome, this is the maximum:
theta = c(-1.0997049, -0.5568053)
gbgdf30 = LikFunct(theta, runMode = 'fit')

# next step, look for biases using that.seems like such a fast downweight, surely it's missing the ones who've jsut been resting recently

longTermMean = rgbgdf %>%
  group_by(key) %>%
  summarise(longTermMean = mean(isStart[available]))

gbgdf2 = left_join(gbgdf30, longTermMean, 'key')
# but we need previous start or not
previousIsStart = rgbgdf %>%
  group_by(key) %>%
  summarise(previousIsStart = isStart[which(rgnum == 30)],
            previous2IsStart = isStart[which(rgnum == 30)] & isStart[which(rgnum == 29)],
            previousInjured = grepl('injury', startTime[which(rgnum == 30)]),
            previousSuspended = grepl('suspension', startTime[which(rgnum == 30)]),
            previous2Injured = grepl('injury', startTime[which(rgnum == 30)]) &
                                grepl('injury', startTime[which(rgnum == 29)]))

gbgdf2 = left_join(gbgdf2, previousIsStart, 'key')

with(gbgdf2[gbgdf2$available,], calibplot(smStart, isStart))
with(gbgdf2[gbgdf2$available,], calibplot(longTermMean, isStart))

# what about, if player was injured in previous match but is now not, are they less likely to start
ViewBias = function(myIndex) {
  message('# Mean starting: ', with(gbgdf2[myIndex,], mean(isStart)))
  message('# Mean long term average: ', with(gbgdf2[myIndex,], mean(longTermMean)))
}

ViewBias(with(gbgdf2, which(available & previousInjured)))
# Mean starting: 0.408088235294118
# Mean long term average: 0.63214436314793
# huge bias

ViewBias(with(gbgdf2, which(available & previousSuspended)))
# Mean starting: 0.62962962962963
# Mean long term average: 0.760445001476912
# not quite so huge but worth noting

ViewBias(with(gbgdf2, which(available & previous2Injured)))
# Mean starting: 0.333333333333333
# Mean long term average: 0.608914547364528
# even huger

ViewBias(with(gbgdf2, which(available & previousIsStart)))
# Mean starting: 0.841462553822263
# Mean long term average: 0.761381353505749

ViewBias(with(gbgdf2, which(available & previous2IsStart)))
# Mean starting: 0.890004403346543
# Mean long term average: 0.802393415035029

# obviously we need to have more weight on recent events then.
# however i contend that by adjusting for injury recovery, downweight speed won't be so fast

# one thing that would certainly help is having a 'coming back from unavailable, esp injury' factor. need to think how that would work though
# but let's bring in the european data
