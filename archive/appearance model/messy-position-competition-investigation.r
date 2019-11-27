source('c:/research/lineup/appearance-model-startup.r')

# try to make the model matrix

myDF = appearanceDF %>%
		filter(team == 'mancity' & date == 20180812) %>%
		filter(!startTime %in% c('injury', 'suspension')) %>%
		mutate(started = (startTime == 0)) %>%
		select(player, started)

# think we want this:
myModelDF = with(myDF,
				tibble(focusPlayer = rep(player, nrow(myDF)),
						focusStarted = rep(started, nrow(myDF)),
						otherPlayer = rep(player, rep(nrow(myDF), nrow(myDF))),
						otherStarted = rep(started, rep(nrow(myDF), nrow(myDF)))))

# no. just no, not at all. surely you've got to have all the other players in columns but then how do you deal with players being unavailable for some games, you can't have an NA in the model matrix

## can't we just look at the raw data, not worrying about the model matrix etc for now?

matrix(rep(myDF$started, rep(nrow(myDF), nrow(myDF))), nrow = nrow(myDF))

### actually i think that focus/other thing is what we want, lets do that for all man city matches

matchDF = appearanceDF %>%
			distinct(date, team)

MakeFocusDF = function(myMatchRow) {
	myDF = inner_join(appearanceDF,
						myMatchRow,
						c('team', 'date')) %>%
						mutate(started = (startTime == 0)) %>%
						select(player, started)

	# think we want this:
	myModelDF = with(myDF,
					tibble(focusPlayer = rep(player, nrow(myDF)),
							focusStarted = rep(started, nrow(myDF)),
							otherPlayer = rep(player, rep(nrow(myDF), nrow(myDF))),
							otherStarted = rep(started, rep(nrow(myDF), nrow(myDF)))))

	return(myModelDF)
}

myList = NULL
mancityMatchDF = matchDF %>%
				filter(team == 'mancity')
for (j in 1:nrow(mancityMatchDF)) {
	myList[[j]] = MakeFocusDF(mancityMatchDF[j,])
}

mancityAppDF = bind_rows(myList)

gjVsa = mancityAppDF %>% filter(grepl('gabriel.+jesus', focusPlayer) & grepl('aguero', otherPlayer))
gjVds = mancityAppDF %>% filter(grepl('gabriel.+jesus', focusPlayer) & grepl('david.+silva', otherPlayer))

# should show higher correlation with one than the other i think
with(gjVsa, cor(focusStarted, otherStarted))
with(gjVds, cor(focusStarted, otherStarted))
# we do indeed

# get number of combinations of each
numApp = mancityAppDF %>%
			group_by(focusPlayer, otherPlayer) %>%
			summarise(numApp = n())

mancityAppDF = left_join(mancityAppDF, numApp)
mancityAppDF = mancityAppDF %>%
				filter(!focusPlayer %in%)

CheckPlayer= function(playerString) {
	mancityAppDF %>%
		filter(grepl(playerString, focusPlayer)) %>%
		filter(numApp > 5) %>%
		group_by(otherPlayer) %>%
		summarise(interCor = cor(focusStarted, otherStarted),
					numCombo = numApp[1])
}

CheckPlayer2 = function(myTeam, playerString) {
	myList = NULL
	mancityMatchDF = matchDF %>%
					filter(team == myTeam)
	for (j in 1:nrow(mancityMatchDF)) {
		myList[[j]] = MakeFocusDF(mancityMatchDF[j,])
	}

	mancityAppDF = bind_rows(myList)
	# get number of combinations of each
	numApp = mancityAppDF %>%
				group_by(focusPlayer, otherPlayer) %>%
				summarise(numApp = n())
	mancityAppDF = left_join(mancityAppDF, numApp)

	corDF = mancityAppDF %>%
		filter(grepl(playerString, focusPlayer)) %>%
		filter(numApp > 5) %>%
		group_by(otherPlayer) %>%
		summarise(interCor = cor(focusStarted, otherStarted),
					numCombo = numApp[1])
					
	return(corDF)
}

## let's try another thing:

RunGlm = function(myFocusPlayer, myOtherPlayer) {
	gjsaDF = mancityAppDF %>%
			filter(grepl(focusString, focusPlayer) &
					grepl(otherString, otherPlayer))
	mod = glm(focusStarted ~ otherStarted, data = gjsaDF, family = binomial)
	
	dum = as.numeric(summary(mod)$coef[2,c(1,4)])
	
	interestingCoefDF = data.frame(coef = dum[1], pval = dum[2])
	return(interestingCoefDF)
}

RunGlm2 = function(x, y, myFocusPlayer, myOtherPlayer) {

	mod = glm(x ~ y, family = binomial)
	
	modCoef = summary(mod)$coef
	if (nrow(modCoef)!=2) {
		stop(paste(myFocusPlayer[1], myOtherPlayer[1]))
	}
	
	dum = as.numeric(modCoef[2,c(1,4)])
	
	interestingCoefDF = data.frame(coef = dum[1], pval = dum[2])
	return(interestingCoefDF)
}

goodMCAppDT = mancityAppDF %>%
				filter(numApp > 50) %>%
				data.table
setkeyv(goodMCAppDT, c('focusPlayer', 'otherPlayer'))

countInfo = goodMCAppDT[,.N, by = .(focusPlayer, otherPlayer)]

coefTab = goodMCAppDT[,
				RunGlm2(focusStarted, otherStarted, focusPlayer, otherPlayer),
				.(focusPlayer, otherPlayer)]

### ah bugger it crashes if a player has never started or never not started. fair enough but a pain in the arse. e.g ederson has started every time silva has started, need to filter out those situations aaarghh

