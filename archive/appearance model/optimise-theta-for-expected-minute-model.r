### the expected-minute-model code is great. let's optimise the parameters of it
## so load the code in, and then...

ProbStartLogLik = function(theta) {
  gbgdfPlusProbStart = CalculateQuantity(theta, quantityChoice = 'probStart', runMode = 'fit')
  gbgdfPlusProbStart$logLik = with(gbgdfPlusProbStart, log(dbinom(isStart, 1, probStart)))
  sumLogLik = with(gbgdfPlusProbStart, sum(logLik[available]))

  gameDownweightCoef = exp(theta[1])
  seasonDownweightCoef = exp(theta[2])
  priorStrength = exp(theta[3])
  
  message('gameDownweightCoef: ', gameDownweightCoef)
  message('seasonDownweightCoef: ', seasonDownweightCoef)
  message('priorStrength: ', priorStrength)
  message('mean sumLogLik: ', sumLogLik)
  
  return(-sumLogLik)
}

theta = c(-0.9439482, -0.1502391, -0.8147686)
maxInfo = nlm(ProbStartLogLik, p = theta, stepmax = 1)
#$estimate
#[1] -1.031  0.785 -0.658

ProbOffBenchLogLik = function(theta) {
  gbgdfPlusProbOffBench = CalculateQuantity(theta, quantityChoice = 'probOffBench', runMode = 'fit')
  gbgdfPlusProbOffBench$logLik = with(gbgdfPlusProbOffBench, log(dbinom(!isStart & played, 1, probOffBench)))
  sumLogLik = with(gbgdfPlusProbOffBench, sum(logLik[available]))
  
  gameDownweightCoef = exp(theta[1])
  seasonDownweightCoef = exp(theta[2])
  priorStrength = exp(theta[3])
  
  message('gameDownweightCoef: ', gameDownweightCoef)
  message('seasonDownweightCoef: ', seasonDownweightCoef)
  message('priorStrength: ', priorStrength)
  message('mean sumLogLik: ', sumLogLik)
  
  return(-sumLogLik)
}

theta = c(-0.9439482, -0.1502391, -0.8147686)
maxInfo = nlm(ProbOffBenchLogLik, p = theta, stepmax = 1)
# [1] -2.2414  0.0788  0.9960

EMinStartSqDiff = function(theta) {
  gbgdfPlusEMinStart = CalculateQuantity(theta, quantityChoice = 'minuteStart', runMode = 'fit')
  gbgdfPlusEMinStart$sqDiff = with(gbgdfPlusEMinStart, (minute - eMinStart)^2)
  sumSqDiff = with(gbgdfPlusEMinStart, sum(sqDiff[available & isStart]))
  
  gameDownweightCoef = exp(theta[1])
  seasonDownweightCoef = exp(theta[2])
  priorStrength = exp(theta[3])
  
  message('gameDownweightCoef: ', gameDownweightCoef)
  message('seasonDownweightCoef: ', seasonDownweightCoef)
  message('priorStrength: ', priorStrength)
  message('sumSqDiff: ', sumSqDiff)
  
  return(sumSqDiff)
}

theta = c(-0.9439482, -0.1502391, -0.8147686)
maxInfo = nlm(EMinStartSqDiff, p = theta, stepmax = 1)
#$estimate
#[1] -2.7508 -0.0219  0.8938

EMinBenchSqDiff = function(theta) {
  gbgdfPlusEMinBench = CalculateQuantity(theta, quantityChoice = 'minuteBench', runMode = 'fit')
  gbgdfPlusEMinBench$sqDiff = with(gbgdfPlusEMinBench, (minute - eMinBench)^2)
  sumSqDiff = with(gbgdfPlusEMinBench, sum(sqDiff[available & !isStart & played]))
  
  gameDownweightCoef = exp(theta[1])
  seasonDownweightCoef = exp(theta[2])
  priorStrength = exp(theta[3])
  
  message('gameDownweightCoef: ', gameDownweightCoef)
  message('seasonDownweightCoef: ', seasonDownweightCoef)
  message('priorStrength: ', priorStrength)
  message('sumSqDiff: ', sumSqDiff)
  
  return(sumSqDiff)
}

theta = c(-0.9439482, -0.1502391, -0.8147686)
maxInfo = nlm(EMinBenchSqDiff, p = theta, stepmax = 1)
#$estimate
#[1] -3.941  0.483  2.073
