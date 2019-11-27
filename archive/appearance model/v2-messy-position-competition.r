### messy-postion-...etc has become unusable, start again

library(data.table)
source('c:/research/lineup/appearance model/appearance-model-startup.r')

foLikFunct = function(theta, x, y, priorStr) {
	dataLik = log(dbinom(x, 1, invlogit(theta[1] + theta[2] * y)))
	priorLik = priorStr * (theta[2] ^ 2)
	
	totalLik = sum(dataLik) - priorLik
	
	return(-totalLik)
}

RunGlm3 = function(x, y, priorStr) {

	maxInfo = nlm(foLikFunct, p = c(0,0), x = x, y = y, priorStr = priorStr, stepmax = 1)
	
	return(maxInfo$est)
}

MakeCompetitionDF = function(myTeam) {
	foDF = MakeFocusDF(myTeam)

	numApp = foDF %>%
				group_by(focusPlayer, otherPlayer) %>%
				summarise(numApp = n())
	foDF = left_join(foDF, numApp)

	goodFoDT = foDF %>%
					filter(focusPlayer != otherPlayer &
							numApp > 5) %>%
					data.table
	setkeyv(goodFoDT, c('focusPlayer', 'otherPlayer'))

	coefTab = goodFoDT[focusAvailable == TRUE,
					RunGlm3(focusStarted, otherStarted, priorStr),
					.(focusPlayer, otherPlayer)]
	## but that puts both numer into two columns, so need to transpose slightly
	coefDF = coefTab %>%
				group_by(focusPlayer, otherPlayer) %>%
				summarise(intercept = V1[1], compcoef=V1[2])

	return(coefDF)
}

### a lot look really dodgy, pretty sure trying to infer where every player plays from the stats would be better
### standard errors shoiuld be easy enough to get hold of and useful
