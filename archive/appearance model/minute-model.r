### let's try to model minutes

### step 1, see how good i am currently. let's make a df of all my guesses thus far


source('c:/research/lineup/ff_startup.r')
source('model_startup.r')

alldate = getalldate()

### scan them all in
gbgdf$active = NA

for (di in 1:length(alldate)) {
	activeplayerfile=paste(DATAPATH,alldate[di],'/activeplayer.csv',sep='')
	if (file.exists(activeplayerfile)) {
		activedf=read.csv(activeplayerfile)
		# pain in the arse to determine which games to join to unfortunately
		gwtomatch = with(resultdf, min(gameweek[date > alldate[di]]))
		seasontomatch = with(resultdf, min(season[date > alldate[di]]))
		
	}
}
combineddata=do.call(rbind,tempbigdf)


### we'll come back to that. let's start on the modelling for now
### let's just start with a single team e.g man city.

mcgbg = gbgdf %>% filter(team == 'mancity')

rhminute = mcgbg %>% filter(grepl('sterling', player)) %>% pull(minute)
rawavg = cumsum(rhminute) / cumsum(rep(90, length(rhminute)))
naivepred = lag(rawavg, 1)

MakeNaivePred = function(myMinute) {
	rawAverageMinute = cumsum(myMinute) / cumsum(rep(90, length(myMinute)))
	naiveProb = lag(rawAverageMinute, 1)
	return(naiveProb)
}

gbgdf2 = gbgdf %>%
		group_by(team, player) %>%
		arrange(season, teamgamenumber) %>%
		mutate(naiveProb = MakeNaivePred(minute),
				logLik = dbinom(minute> 60, 1, naiveProb)) %>%
		ungroup()

meanLogLik = mean(gbgdf2$logLik, na.rm = TRUE)

# the lack of a downweight is the most blatantly ridiculous thing here

# let's try using the roll_mean function
dwgtbygame = 0.9
windowsize = 10
wgtvec = dwgtbygame ^ ((windowsize - 1):0)
rhminute = as.matrix(mcgbg %>%
					filter(grepl('sterling', player)) %>%
					select(minute))
roll_mean(rhminute, windowsize, wgtvec)

## seems to be doing something right. let's make a function for each player using that

getweightedminute = function(myplayer, windowsize, dwgtbygame) {
	rhminute = as.matrix(gbgdf %>%
					filter(player == myplayer) %>%
					select(minute))
	wgtvec = dwgtbygame ^ ((windowsize - 1):0)
	wgtminute = roll_mean(rhminute, windowsize, wgtvec)
	extendedwgtminute = c(rep(NA, windowsize - 1), wgtminute)
	
	plot(1:nrow(rhminute), rhminute)
	lines(1:nrow(rhminute), extendedwgtminute)
	#return(cbind(rhminute, wgtminute))
}

### the massive autocorrelation of missing games is a problem. we can try to eliminate these ones or could we instead include the autocorrelation as part of our model?
### nice idea but might take ages to implement

## to do that we should firstly reduce our data to binary, played > 60 minutes or not. the chance of being substituted is second order

gbgdf$appeared = with(gbgdf, minute > 60)
getweightedappearance = function(myplayer, windowsize, dwgtbygame) {
	myappearance = as.matrix(gbgdf %>%
					filter(player == myplayer) %>%
					select(appeared))
	wgtvec = dwgtbygame ^ ((windowsize - 1):0)
	wgtappearance = roll_mean(myappearance, windowsize, wgtvec)
	extendedwgtappearance = c(rep(NA, windowsize - 1), wgtappearance)
	
	plot(1:nrow(myappearance), myappearance)
	lines(1:nrow(myappearance), extendedwgtappearance)
	#return(cbind(rhminute, wgtminute))
}

### so i think we should do something like:

### get a shortlist of players who have played regularly within a season
### produce a probability that they play according to this and get a likelihood stat. optimise weight of this.
### find biases within this. the most obvious one is non-independence of points.
## that's got really hard really quickly. don't think i can get much further with this. but it's an interesting and important problem.
