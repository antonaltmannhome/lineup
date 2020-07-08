
source('new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

# only care about players who've played at least 30 games for their club for now
numGameForClub = gbgdf %>%
  group_by(team, playerid) %>%
  summarise(sumGame = n())

gbgdf = left_join(gbgdf, numGameForClub, c('team', 'playerid'))

gbgdf30 = gbgdf %>% filter(sumGame >= 30)
gbgdf30$key = with(gbgdf30, paste(season, team, teamgamenumber, player))

# right, let's see how long the loop takes to run then

MakeWideHistory = function(x, k, idDF) {
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

keyHistory = gbgdf30 %>%
  group_by(team, player) %>%
  arrange(season, teamgamenumber) %>%
  do(MakeWideHistory(.$key, 30, data.frame(.$season, .$teamgamenumber))) %>%
  rename(teamgamenumber = "..teamgamenumber", season = "..season") %>%
  ungroup()

### then we make that verticle again, it'll be a big old df
vertKeyHistory = gather(keyHistory, rgnum, rkey, -c(team, player, season, teamgamenumber))
# awesome, now we join in the columns of interest
vertKeyHistory$key = with(vertKeyHistory, paste(season, team, teamgamenumber, player))
vertKeyHistory$rgnum = as.integer(gsub('V', '', vertKeyHistory$rgnum))
# but we only want ones where they've got all 30 games
have30 = vertKeyHistory %>%
  group_by(key) %>%
  summarise(all30 = all(!is.na(rkey)))
vertKeyHistory = inner_join(vertKeyHistory, have30 %>% filter(all30), 'key')
rgbgdf = lazy_left_join(vertKeyHistory,
                        gbgdf30 %>%
                        unite('seasGNum', c('season', 'teamgamenumber')),
                        by = c('rkey' = 'key'),
                        c('isStart', 'available', 'goal', 'startTime', 'seasGNum'))

# but we don't want ones where the player was unavailable for the whole time, pointless and irritating
atLeastOnceAvailable = rgbgdf %>%
  group_by(key) %>%
  summarise(atLeastOnceAvailable = any(available))
rgbgdf = semi_join(rgbgdf, atLeastOnceAvailable %>% filter(atLeastOnceAvailable), 'key')

# but we'll be wanting to join that back to the one-row-for-each-key df, so need to filter out ones we can't predict for there too
gbgdf30 = inner_join(gbgdf30, have30 %>% filter(all30), 'key')

# might seem rash to get rid of rkey now, but should remake lgbgdf if more columns are needed
rgbgdf = within(rgbgdf, rm(team, player, rkey))
# what we don't have is the current match stat, so let's get that
# no, you don't do that. what you do is do a summary within the model, then join it to gbgdf30
if (FALSE) {
  lgbgdf = lazy_left_join(lgbgdf, gbgdf30, 'key', c('isStart', 'available'))
  lgbgdf = lgbgdf %>% select(key, isStart, available, everything())
}

# shall we try using the front end to data table to do this? seems ideal

# let's do the dplyr version first, check that table is the same but faster

# right, let's do some analysis then
# so let's firstly set up our simple downweight model
