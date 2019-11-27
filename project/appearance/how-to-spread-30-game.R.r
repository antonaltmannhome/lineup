#### if we want to investigate biases with current expected minutes model, you can load in the existing calcuations like this:
gbgdf = CalculateHistoricExpectedMinute(gbgdf)

# so i reckon it underestimates starting prob when a player didn't start previous match but did have high start proprtion while fit in previous 30 games
#] let's try to test that

# set up dummy tibble to cevelop the method

myTB = tibble(player = c(rep('walker', 3), rep('kbd', 4)),
              gnum = c(1:3, 1:4),
              start = c(T, F, T, F, T, T, T))

# ok, first job, how to do we mutate in such a way that we turn a scalar into a vector?
# maybe do works

GetPrev2TB = function(x) {
  if (length(x) == 1) prev2TB = tibble(prev2 = rep(NA, 2))
  if (length(x) > 1) prev2TB = tibble(prev2 = tail(lag(x), 2))
  return(prev2TB)
}
myTB %>%
  group_by(player) %>%
  arrange(gnum) %>%
  do(GetPrev2(.$start))
# no that doesn't work, don't know why


myTB = tibble(player = c(rep('walker', 3), rep('kbd', 4)),
              gnum = c(1:3, 1:4),
              start = c(T, F, T, F, T, T, T))

GetPrev2List = function(x) {
  if (length(x) == 1) prev2List = list(rep(NA, 2))
  if (length(x) > 1) prev2List = list(tail(lag(x), 2))
  return(prev2List)
}
myTB = myTB %>%
  group_by(player) %>%
  arrange(gnum) %>%
  mutate(prev2 = GetPrev2List(start))
# no that is wrong

# now have to push that list into a wide data frame
myTBLong = myTB %>%
  unnest(prev2) %>%
  group_by(player, gnum, start) %>%
  mutate(pgnum = 1:n()) %>%
  ungroup()
myTBWide = myTBLong %>%
  spread(value = prev2, key = pgnum)

complicated.wizardry <- function(a,b){
  a+b
}

cumlist <- function(sofar, remaining, myfn){
  if(length(remaining)==1)return(c(sofar, myfn(sofar[length(sofar)],remaining[1])))
  return (  cumlist( c(sofar, myfn(sofar[length(sofar)],remaining[1])),remaining[2:length(remaining)],myfn))
}

cumlist(0,1:10,complicated.wizardry)

# let's try to make that a little simpler
cumlist2 = function(sofar, remaining){
  if (length(remaining) == 1) {
    val1 = sofar
    val2a = sofar[length(sofar)]
    val2b = remaining[1]
    toReturn = c(val1, val2a + val2b)
  }
  if (length(remaining) > 1) {
    val1 = sofar
    val2a = sofar[length(sofar)]
    val2b = remaining[1]
    newSoFar = c(sofar, val2a + val2b)
    newRemaining = remaining[2:length(remaining)]
    toReturn = cumlist2(newSoFar, newRemaining)
  }
  return (toReturn)
}

cumlist2(0,1:10)

# ok that works. we're on the way. now need to generalise. although the compwiz bit wa a silly way of doing that

myAdd = function(x, y) x+y
myPaste = function(x, y) paste(c(x, y), collapse = ',')

cumlist3 = function(sofar, remaining, myFunct){
  if (length(remaining) == 1) {
    val1 = sofar
    val2a = sofar[length(sofar)]
    val2b = remaining[1]
    toReturn = c(val1, myFunct(val2a, val2b))
  }
  if (length(remaining) > 1) {
    val1 = sofar
    val2a = sofar[length(sofar)]
    val2b = remaining[1]
    newSoFar = c(sofar, myFunct(val2a, val2b))
    newRemaining = remaining[2:length(remaining)]
    toReturn = cumlist3(newSoFar, newRemaining, myFunct)
  }
  return (toReturn)
}

cumPaste = function(x) {
  pastex = rep(NA, length(x))
  pastex[1] = x[1]
  for (i in 1:length(x)) {
    pastex[i] = paste(c(pastex[i-1], i), collapse = ',')
  }
  return(pastex)
}

# ok that was actually very easy although it will be slow for long ones surely?
# let's try cumList
CumList = function(x) {
  myList = vector('list', length(x))
  myList[[1]] = x[1]
  for (i in 2:length(x)) {
    myList[[i]] = c(myList[[i-1]], x[i])
  }
  return(myList)
}
# getting there, now we want it to lag and we want it to have NAs to make length regular
SubInNA = function(y, n) c(rep(NA, n-length(y)), y)

cumList = function(x) {
  myList = vector('list', length(x))
  myList[[1]] = x[1]
  for (i in 2:length(x)) {
    myList[[i]] = c(myList[[i-1]], x[i])
  }
  myList = sapply(myList, SubInNA, length(x))
  return(myList)
}

# now need the lag
cumList = function(x) {
  myList = vector('list', length(x))
  myList[[1]] = NULL
  myList[[2]] = x[1]
  for (i in 3:length(x)) {
    myList[[i]] = c(myList[[i-1]], x[i-1])
  }
  myList = sapply(myList, SubInNA, length(x))
  return(myList)
}

# sorted. now cap at length k

cumList = function(x, k, idDF) {
  myList = vector('list', length(x))
  myList[[1]] = NULL
  myList[[2]] = x[1]
  for (i in 3:length(x)) {
    myList[[i]] = c(myList[[i-1]], x[i-1])
  }
  myMatrix = sapply(myList,
                function(y, n) {
                  c(rep(NA, n-length(y)), y)
                },
                length(x))
  myTruncMatrix = t(apply(myMatrix, 1, tail, k))
  
  myDF = as.data.frame(myTruncMatrix)
  # now put the id columns back on
  myDF = cbind(idDF, myDF)
  return(myDF)
}

# it's there, standlone version
# it's probably slow too, but can you have lists in rcpp? probably not

# so, we now have to apply it in a dplyr way. let's try on some actual data
littlegbgdf = gbgdf %>%
  filter(season == 1920 & grepl('(raheem|sadio)', player)) %>%
  select(team, teamgamenumber, player, startTime, goal) # actually want availability there, not goal, but bear with me

startHistory = littlegbgdf %>%
  group_by(player) %>%
  arrange(teamgamenumber) %>%
  do(cumList(.$startTime, 5, data.frame(.$team, .$teamgamenumber))) %>%
  rename(team = "..team", teamgamenumber = "..teamgamenumber")

lgbgplus = left_join(littlegbgdf, startHistory, c('team', 'teamgamenumber', 'player'))
# it is there. just need to rename the columns to 'startTime1' etc i suppose.

# right, let's actually set this thing up
