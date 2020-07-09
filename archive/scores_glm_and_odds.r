### let's try to combine the results glm with the odds to weed out wrong odds firstly, and also get the best team strength predictions
### no more southampton being 7th bets team hopefully


source('c:/research/lineup/ff_startup.r')
source('model_startup.r')

gameplayedinfo = resultdf %>%
					group_by(team) %>%
					arrange(season, gameweek) %>%
					mutate(playedsofar = 1:n()) %>%
					group_by(team, season, gameweek) %>%
					summarise(playedsofar = playedsofar[1])

resultdf = lazy_left_join(resultdf,
							gameplayedinfo,
							c('season', 'team', 'gameweek'),
							'playedsofar')
resultdf = lazy_left_join(resultdf,
							gameplayedinfo %>%
								select(season, team, gameweek, playedsofar) %>%
								dplyr::rename(oppteam = team,
											oppplayedsofar = playedsofar),
							c('season', 'oppteam', 'gameweek'),
							'oppplayedsofar')

resultdf = resultdf %>%
			mutate(isgameplayedvalid = (playedsofar > 5) &
										(oppplayedsofar > 5))

oddslikfunct=function(theta, truncresultdf, unteam) {
	offtheta=theta[1:length(unteam)]
	deftheta=theta[(length(unteam)+1):(2*length(unteam))]
	homeeffect=theta[2*length(unteam)+1]
	
	if (any(offtheta > 5) | any(deftheta > 5)) return(10E6)
	
	#truncresultdf = truncresultdf %>%
	#					mutate(ehgoal = exp(offtheta[hmap] + deftheta[amap] + homeeffect),
	#							eagoal = exp(offtheta[amap] + deftheta[hmap]),
	#							sqdiff = (escored-ehgoal)^2 + (econceded-eagoal)^2)
	
	truncresultdf$ehgoal = with(truncresultdf, exp(offtheta[hmap] + deftheta[amap] + homeeffect))
	truncresultdf$eagoal = with(truncresultdf, exp(offtheta[amap] + deftheta[hmap]))
	truncresultdf$sqdiff = with(truncresultdf, (oddsescored-ehgoal)^2 + (oddseconceded-eagoal)^2)
	
	sumSqDiff = with(truncresultdf, sum(timeDownWgt * sqdiff))
	
	priorpen=0.01*(mean(offtheta))^2

	toReturn = sumSqDiff + priorpen

	#print(theta)
	#print(toReturn)
	
	return(toReturn)
}

getdateforgameweek = function(myseason, mygameweek) {
	dateforgameweek = with(resultdf, date[season == myseason & gameweek == mygameweek])[1]
	return(dateforgameweek)
}

getoddsparam = function(mySeasonGameweekIndex, downWeight, allseasongameweek, runmode) {

	myseason = with(allseasongameweek, season[rowIndex == mySeasonGameweekIndex])
	mygameweek = with(allseasongameweek, gameweek[rowIndex == mySeasonGameweekIndex])

	truncresultdf = resultdf %>%
					filter(date < getdateforgameweek(myseason, mygameweek))
	truncresultdf = makedwvec(truncresultdf, downWeight)
	truncresultdf = truncresultdf %>%
					filter(isHome &
							!is.na(oddsescored) &
							!is.na(oddseconceded))

	unteam=unique(c(truncresultdf$team, truncresultdf$oppteam))
	truncresultdf$hmap=match(truncresultdf$team,unteam)
	truncresultdf$amap=match(truncresultdf$oppteam,unteam)
	truncresultdf$hprop=with(truncresultdf,oddsescored/(oddsescored+oddseconceded))

	thetainit=rep(0,2*length(unteam)+1)
	maxinfo=nlm2(oddslikfunct, p=thetainit, truncresultdf = truncresultdf, unteam = unteam)

	attcoefdf = tibble(team = unteam, attcoef = maxinfo$est[1:length(unteam)])
	defcoefdf = tibble(team = unteam, defcoef = maxinfo$est[(length(unteam)+1):(2*length(unteam))])
	homecoef = maxinfo$est[2*length(unteam)+1]

	if (runmode == 'max') {
		sumLogLik = makecurrentgwprediction(myseason, mygameweek, attcoefdf, defcoefdf, homecoef, 'max')
		return(sumLogLik)
	}
	if (runmode == 'fit') {
		currentgwresultdf = makecurrentgwprediction(myseason, mygameweek, attcoefdf, defcoefdf, homecoef, 'fit')
		return(currentgwresultdf)
	}
}

makedwvec = function(truncresultdf, downWeight) {
	truncresultdf = truncresultdf %>%
					mutate(ymdDate = ymd(date),
							maxDate = max(ymdDate),
							daysGap = as.duration(ymdDate %--% maxDate) / ddays(1),
							timeDownWgt = exp(-downWeight * daysGap)) %>%
					select(-c(ymdDate, maxDate, daysGap))

	return(truncresultdf)
}

makecurrentgwprediction = function(myseason, mygameweek, attcoefdf, defcoefdf, homecoef, runmode) {
	
	# i think it's best to now just predict with that
	currentgwresultdf = resultdf %>%
						filter(season == myseason &
								gameweek == mygameweek)
	currentgwresultdf = left_join(currentgwresultdf, attcoefdf, 'team')
	currentgwresultdf = left_join(currentgwresultdf,
									defcoefdf %>%
									dplyr::rename(oppteam = team),
									'oppteam')
	currentgwresultdf = currentgwresultdf %>%
							mutate(homeeffect = ifelse(isHome, homecoef, 0))

	currentgwresultdf = currentgwresultdf %>%
						mutate(expectedScored = exp(attcoef + defcoef + homeeffect))
	
	if (runmode == 'max') {
		currentgwresultdf$logLik = with(currentgwresultdf, log(dpois(scored, expectedScored)))
		sumLogLik = with(currentgwresultdf, sum(logLik[isgameplayedvalid]))
		return(sumLogLik)
	}
	if (runmode == 'fit') {
		return(currentgwresultdf %>%
				select(date, team, expectedScored))
	}
}

getscoresparam = function(mySeasonGameweekIndex, downWeight, allseasongameweek) {

	myseason = with(allseasongameweek, season[rowIndex == mySeasonGameweekIndex])
	mygameweek = with(allseasongameweek, gameweek[rowIndex == mySeasonGameweekIndex])

	truncresultdf = resultdf %>%
					filter(date < getdateforgameweek(myseason, mygameweek))
	truncresultdf = makedwvec(truncresultdf, downWeight)
					
	mod = glm(scored ~ factor(team) + factor(oppteam) + isHome - 1,
				family = poisson, data = truncresultdf, weight = timeDownWgt)

	attcoef = coef(mod)[grep('factor\\(team\\)', names(coef(mod)))]
	attcoefdf = tibble(team = gsub('factor\\(team\\)', '', names(attcoef)),
						attcoef = attcoef)
	defcoef = coef(mod)[grep('factor\\(oppteam\\)', names(coef(mod)))]
	names(defcoef) = gsub('factor\\(oppteam\\)', '', names(defcoef))
	# but we're missing the first team's defensive coef, so add that in
	missingdefteam = setdiff(mod$xlevels[[2]], names(defcoef))
	defcoef = c(0, defcoef)
	names(defcoef)[1] = missingdefteam
	defcoefdf = tibble(team = names(defcoef),
						defcoef = defcoef)
	
	homecoef = coef(mod)[['isHomeTRUE']]
	
	sumLogLik = makecurrentgwprediction(myseason, mygameweek, attcoefdf, defcoefdf, homecoef)
	
	return(sumLogLik)
}
					
allseason = c(1617, 1718)
allgameweek = 1:38
allseasongameweek = expand.grid(season = allseason,
								gameweek = allgameweek) %>%
					arrange(season, gameweek) %>%
					mutate(rowIndex = 1:n())

allseasongameweek$isafterburnin = 
	with(allseasongameweek, (season != allseason[1]) | season == allseason[1] & gameweek > 18)
allseasongameweek$hashappened = with(allseasongameweek, paste(season, gameweek)) %in%
								with(resultdf, paste(season, gameweek))
allseasongameweek$isvalid = with(allseasongameweek, isafterburnin & hashappened)

TryScoreDownweight = function(downWeight) {
	scoresloglikbyweek = with(allseasongameweek %>% filter(isvalid),
						purrr::map_dbl(rowIndex,
									getscoresparam,
									downWeight = downWeight,
									allseasongameweek = allseasongameweek,
									runmode = 'max'))
	sumsummedloglik = sum(scoresloglikbyweek)
	return(sumsummedloglik)
}

# amazing, wants to look all the way back

TryOddsDownweight = function(downWeight) {
	oddsloglikbyweek = with(allseasongameweek %>% filter(isvalid),
						purrr::map_dbl(rowIndex,
										getoddsparam,
										downWeight = downWeight,
										allseasongameweek = allseasongameweek,
										runmode = 'max'))
	return(sum(oddsloglikbyweek))
}
# 0.015	seems to be best

# try to pick up seemingly strange odds

alloddsfit = with(allseasongameweek %>% filter(isvalid),
						purrr::map_df(rowIndex,
										getoddsparam,
										downWeight = 0.015,
										allseasongameweek = allseasongameweek,
										runmode = 'fit',
										.id = 'gameweek'))

resultdf = left_join(resultdf,
					alloddsfit %>%
					select(date, team, expectedScored),
					by = c('date', 'team'))
			
offEyeCheck = resultdf %>%
				filter(season == 1718 & !is.na(oddsescored) & !is.na(expectedScored)) %>%
				group_by(team) %>%
				summarise(sumscored = sum(scored),
							sumoddsscored = sum(oddsescored),
							sumestscored = sum(expectedScored))			
			
defEyeCheck = resultdf %>%
				filter(season == 1718 & !is.na(oddsescored) & !is.na(expectedScored)) %>%
				group_by(oppteam) %>%
				summarise(sumconceded = sum(scored),
							sumoddsconceded = sum(oddsescored),
							sumestconceded = sum(expectedScored))			

combinedEyeCheck = full_join(offEyeCheck,
							defEyeCheck %>%
							dplyr::rename(team = oppteam),
							'team') %>%
					mutate(offDelta = sumscored - sumoddsscored,
							defDelta = sumconceded - sumoddsconceded,
							delta = offDelta - defDelta) %>%
					arrange(delta)

### southampton hugely overrated, burnley underrated
library(ggplot2)
ggplot(resultdf %>%
		filter(season == 1718 & !is.na(expectedScored + oddsescored))) +
	geom_point(aes(x = expectedScored, y = oddsescored, col = team, size = 3))
	
							
# ok so that's kind of working
# next steps:
# accept downweight as an argument
# produce predictions, optimise them
# detect faulty odds

# odds massively better than scores, just ignore scores for now
# let's make predictions based on these
# ok some odd things happening - sometimes escored != econceded of matching line in resultdf
# resultdf[with(resultdf, which(abs(expectedScored - escored)>0.5)),]

# but got to do something else, this is dragging on horribly
