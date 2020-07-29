
source('c:/git/lineup/new-model-startup.r')
library(BiasedUrn)

# let's fill in the gaps in mainpos

# i think if a player it literally injured or unused all season, we can ditch them altogether
# otherwise, use whatever position they're logged as for the majority of other matches

# so let's get each players stats on their position each year
playerPosStat = gbgdf %>%
  filter(!is.na(mainpos)) %>%
  group_by(season, team, player) %>%
  count(mainpos) %>%
  ungroup()
# but i want to know situation where there are more than one answer
numPosBySTP = playerPosStat %>%
  group_by(season, team, player) %>%
  summarise(numPos = n()) %>%
  ungroup()
# ok so there are genuine examples of players playing in differnt positions. just go with the majority
playerMainPos = playerPosStat %>%
  group_by(season, team, player) %>%
  summarise(mostFreqPos = mainpos[which.max(n)])
gbgdf = subset_join(gbgdf,
                    playerMainPos %>%
                      rename(mainpos = mostFreqPos),
                    c('season', 'team', 'player'),
                    is.na(mainpos))
# the ones that are NA, i say ditch from gbgdf, they're not active players
# but when? it might cause problms for player matching, so i say: fill in in the data joining, but remove before modelling


### want to rule out situations where there was a weirdly small number of players in each position
gbgdf$tgId = with(gbgdf, paste(season, teamgamenumber, team))

# let's see what a normal number actually is though
numPosByTG = gbgdf %>%
  filter(!is.na(mainpos)) %>%
  group_by(tgId) %>%
  summarise(numGk = sum(minute * (mainpos == 'GK'))/94,
            numD = sum(minute * (mainpos == 'D'))/94,
            numM = sum(minute * (mainpos %in% c('DMC', 'M')))/94,
            numF = sum(minute * (mainpos %in% c('AM', 'FW')))/94)

# want to know the number of times a team has played within previous year
# want to use roll library for this ie no loops
# and want to do similar thing for players eventually
# so let's try to get it right
earliestDate = min(ymd(resultDF$date))
latestDate = max(ymd(resultDF$date))
daysDiff = as.integer(difftime(latestDate, earliestDate, units = 'days'))
fullDateRange = .POSIXct(character(daysDiff + 1))
fullDateRange[1:length(fullDateRange)] = earliestDate + days(0:(length(fullDateRange) - 1))
fullDateRange = as.integer(gsub('-', '', gsub(' .+$', '', as.character(fullDateRange))))

teamDayDF = expand_grid(date = fullDateRange, team = unique(resultDF$team))
teamDayDF = indicate_overlapping_combination(teamDayDF, resultDF, c('team', 'date'), 'teamPlayed')

teamDayDF = teamDayDF %>%
  group_by(team) %>%
  arrange(date) %>%
  mutate(rollingSumGame = roll::roll_sum(teamPlayed, 365, min_obs = 1)) %>%
  ungroup()

resultDF = left_join(resultDF, teamDayDF, c('team', 'date'), 'rollingSumGame')

# next we need to count the number of goalies/defs/mids/fws selected, to see it was a normal looking squad
# or do we? tricky with the new more refined position data, let's see how we get on if we omit that, can't be too much of a problem surely
sumPositionByMatch = gbgdf %>%
  group_by(season, team, teamgamenumber, mainpos) %>%
  summarise(sumMinute = sum(minute) / 94)

# but we do need to hve condition that player has played more than once
gbgdf = gbgdf %>%
  lazy_left_join(resultDF, c('season', 'team', 'teamgamenumber'), 'rollingSumGame') %>%
  mutate(isPredValid = !grepl('(injury|suspension)', startTime) & rollingSumGame >= 5 & gameForTeamNumber > 1)

### impossible to rerun what was there before! But let's try out doing the outfield players as a dicol from scratch

# let's take man city's last 10 games eg

gbgdf2 = gbgdf %>%
  filter(!grepl('(injury|suspension)', startTime)) %>%
  mutate(startTime2 = ifelse(!startTime %in% c('U', 'UU'), startTime, '95'),
         endTime2 = ifelse(!startTime %in% c('U', 'UU'), endTime, '95')) %>%
  mutate(startTime2 = as.integer(startTime2),
         endTime2 = as.integer(endTime2))

# argh, need to split the game up as subs get made
SplitGame = function(myPlayer, myStartTime, myEndTime) {
  # for debugging:
  # subGbgdf = gbgdf2 %>% filter(team == 'mancity' & season == 1920 & teamgamenumber == 2 & mainpos %in% c('D', 'DMC')); myStartTime = subGbgdf %>% pull(startTime2); myEndTime = subGbgdf %>% pull(endTime2); myPlayer = subGbgdf %>% pull(player)
  unSubTime = sort(unique(c(myStartTime[myStartTime != 95L], myEndTime[!is.na(myEndTime) & myEndTime != 95L])))
  minDiff = diff(unSubTime)
  numInterval = length(unSubTime) - 1
  myRepeatedPlayedDF = data.frame(player = rep(myPlayer, numInterval),
                                  matchSection = rep(1:numInterval, rep(length(myStartTime), numInterval)),
                                  minDiff = rep(minDiff, rep(length(myStartTime), numInterval)),
                                  startTime = rep(myStartTime, numInterval),
                                  endTime = rep(myEndTime, numInterval),
                                  lhTimeCutoff = rep(head(unSubTime, -1), rep(length(myStartTime), numInterval)),
                                  rhTimeCutoff = rep(tail(unSubTime, -1), rep(length(myStartTime), numInterval)))
  myRepeatedPlayedDF$played = with(myRepeatedPlayedDF, as.integer(startTime <= lhTimeCutoff & endTime >= rhTimeCutoff))
  
  return(myRepeatedPlayedDF)
}

# can we get do to do this nicely

# right, now we can do a little dicol on that i'd have thought
# ah i've just understood what the previous thing did, it made up all the pairs of players for each section
# which is a major code overhaul, let's not do that now

# let's see how we get on with hypergeo these days though

rlikfunct=function(theta0, splitTeamDF, unMatchSection) {
  theta = c(0, theta0)
  #print(theta)
  splitTeamDF$theta = theta[splitTeamDF$playerNumber]
  
  # ugh, seems we have to loop over each teamgamenumber-match
  splitTeamDF$one = 1
  for (j in 1:nrow(unMatchSection)) {
    sax = with(splitTeamDF, which(teamgamenumber == unMatchSection$teamgamenumber[j] &
                                    matchSection == unMatchSection$matchSection[j]))
    unMatchSection$logLik[j] = unMatchSection$minDiff[j] *
      with(splitTeamDF[sax, ], log(dMWNCHypergeo(x=played,
                                                 m=one,
                                                 n=unMatchSection$numWhoPlayed[j],
                                                 odds=exp(theta))))
  }
  
  sumLogLik = sum(unMatchSection$logLik)
  #print(sumLogLik)
  return(-sumLogLik)
}

GetPlayerAppearanceMle = function(myTeam, mySeason, myMainpos) {
  
  subGbgdf2 = gbgdf2 %>%
    filter(season == mySeason & team == myTeam & mainpos2 == myMainpos)
  splitTeamDF = subGbgdf2 %>%
    group_by(season, team, teamgamenumber) %>%
    do(SplitGame(.$player, .$startTime2, .$endTime2))
  splitTeamDF = splitTeamDF %>%
    left_join(splitTeamDF %>%
                group_by(teamgamenumber, matchSection) %>%
                summarise(numActivePlayer = sum(played)),
              c('teamgamenumber', 'matchSection'))
  unMatchSection = splitTeamDF %>%
    group_by(teamgamenumber, matchSection, minDiff) %>%
    summarise(numWhoPlayed = sum(played)) %>%
    ungroup() %>%
    mutate(logLik = NA)

  myUnPlayer = unique(splitTeamDF$player)
  # who has made the median number of appearances, they should have value zero
  myTotalMinutePlayed = splitTeamDF %>%
    group_by(player) %>%
    summarise(totalMinutePlayed = sum(endTime - startTime)) %>%
    arrange(totalMinutePlayed)
  referencePlayer = myTotalMinutePlayed$player[floor( (length(myUnPlayer)+1) / 2)]
  myUnPlayer = c(referencePlayer, setdiff(myUnPlayer, referencePlayer))
  splitTeamDF$playerNumber = match(splitTeamDF$player, myUnPlayer)
  theta0 = rep(0, length(myUnPlayer) - 1)
  
  maxInfo = nlm(rlikfunct, p = theta0, splitTeamDF = splitTeamDF, unMatchSection = unMatchSection, stepmax = 5)
  dum = c(0, maxInfo$est)
  myAppearanceMle = data.frame(team = myTeam, season = mySeason, player = myUnPlayer, appearanceMle = dum, appearanceOdds = exp(dum) / sum(exp(dum)))
  
  return(myAppearanceMle)
}

# seems roughly ok, a bit slow but not devastating
# need to do the rolling thing, and with a time downweight. but in fact, if we want to check out the fatigue/schedule thing, we can probably get away with doing it once for the entire season
# although i suppose players go in and out of the team, but we're really only interested in the ones who play all the time anyway

# so how about we run it for each team each season to build up our array
gbgdf2$mainpos2 = with(gbgdf2, case_when(
  mainpos %in% c('D', 'DMC') ~ 'def',
  mainpos == 'M' ~ 'mid',
  mainpos %in% c('AM', 'FW') ~ 'att',
  TRUE ~ 'other'
))
unTeamSeasonPosition = gbgdf2 %>%
  filter(mainpos2 != 'other') %>%
  distinct(season, team, mainpos2)


appearanceMleList = vector('list', nrow(unTeamSeasonPosition))
for (tspi in 1:nrow(unTeamSeasonPosition)) {
  appearanceMleList[[tspi]] = with(unTeamSeasonPosition[tspi,],
                                   GetPlayerAppearanceMle(team, season, mainpos2))
  with(unTeamSeasonPosition[tspi,],
       message('Have processed ', season, ' / ', team, ' / ', mainpos2))
}
