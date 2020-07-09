.MakeInitialGbgDF = function(myseason, seasonInfoDF) {
  alldate = getalldate()
  alldateseason = seasonInfoDF$season[cut(alldate, c(seasonInfoDF$start, tail(seasonInfoDF$end, 1)), label = FALSE) + 1]

  myalldate = alldate[alldateseason == myseason]

  resultDF = ffDataLoading:::GetResultDF()

  myResultDF = resultDF %>% filter(season == myseason)

	### scan them all in
	tempbigdf=NULL
	for (di in 1:length(myalldate)) {
		combinedteamfile=paste0(DATAPATH,'summarised_whoscored_data/combined_data_', myalldate[di],'.csv')
		tempbigdf[[di]]=read.csv(combinedteamfile)
		tempbigdf[[di]]$filedate = myalldate[di]
	}
	combineddata=do.call(rbind,tempbigdf)

	combineddata$matchdate=NA
	teamlist=unique(combineddata$team)
	for (ti in 1:length(teamlist)) {
		sax=which(combineddata$team==teamlist[ti])
		currentteamfiledate=unique(combineddata$filedate[sax])
		currentteamresultdate=with(myResultDF,sort(date[team==teamlist[ti]]))

		if (length(currentteamfiledate) != length(currentteamresultdate)) {
			message('For ', teamlist[ti],', I have ', length(currentteamresultdate), ' matches in resultDF but ', length(currentteamfiledate), ' matches in the whoscored data')
			message('Cannot match date to matches, check what is happening in makegbgdf, in player_funct.r')
			stop()
		}

		combineddata$matchdate[sax]=currentteamresultdate[match(combineddata$filedate[sax],currentteamfiledate)]
	}

  combineddata$season = myseason

	### then, each quantity is just current minus previous
	matchstatcol = setdiff(names(combineddata), c('team','player','mainpos','filedate','matchdate'))

	gbgdf = combineddata %>%
            select(-filedate) %>%
            rename(date = matchdate) %>%
            lazy_left_join(resultDF, c('date', 'team'), 'teamgamenumber')

	# now fix broken names
	gbgdf = ffDataJoining:::.FixWhoscoredPlayerName(gbgdf)

	gbgdf = gbgdf %>%
					group_by(season,team,player) %>%
					arrange(date) %>%
					mutate(minute = c(minute[1], diff(minute)),
						goal = c(goal[1], diff(goal)),
						assist = c(assist[1], diff(assist)),
						shotoob = c(shotoob[1], diff(shotoob)),
						shot6yd = c(shot6yd[1], diff(shot6yd)),
						shotib = c(shotib[1], diff(shotib)),
						openplay = c(openplay[1], diff(openplay)),
						counter = c(counter[1], diff(counter)),
						setpiece = c(setpiece[1], diff(setpiece)),
						penaltytaken = c(penaltytaken[1], diff(penaltytaken)),
						offt = c(offt[1], diff(offt)),
						bar = c(bar[1], diff(bar)),
						ont = c(ont[1], diff(ont)),
						block = c(block[1], diff(block)),
						penaltyscored = c(penaltyscored[1], diff(penaltyscored)),
						longkp = c(longkp[1], diff(longkp)),
						shortkp = c(shortkp[1], diff(shortkp))) %>%
	        ungroup()

  return(gbgdf)
}

.FixWhoscoredPlayerName = function(gbgdf) {
	namecorrectiondf = read.csv(paste(DATAPATH,'fixplayername.csv',sep=''))
	sax = which(with(gbgdf, paste(season, team, player)) %in% with(namecorrectiondf, paste(season, team, oldname)))
	mdum = match(with(gbgdf, paste(season, team, player))[sax], with(namecorrectiondf, paste(season, team, oldname)))
	gbgdf[sax,'player'] = namecorrectiondf[mdum,'newname']
	return(gbgdf)
}

# we next have a function that loads in the initial gbgdf and takes out players who have obviously left (ie moved to another club). it will leave in players who have left, but not obviously, however we only really care about those for the current season

.RemoveObviousLeavingPlayer = function(initialgbgdf) {

  # this would be very annoying if we did it in the actual anlysis, but it's useful to do it while we're trying to sort out player ids:

  initialgbgdf = initialgbgdf %>% rename(whoscoredplayer = player)

  playerteamdf = initialgbgdf %>%
                      group_by(whoscoredplayer, team) %>%
                      summarise(joindate = min(date),
                                leavedate = max(date)) %>%
                      arrange(joindate) %>%
    mutate(nextjoindate = lead(joindate, 1),
           overlap = !is.na(nextjoindate) & nextjoindate < leavedate) %>%
    ungroup()

  # so we look out for players who appear to be at two clubs at once. they have 0 minutes in gbgdf for their old club while they're actually playing for their new club


  ### so, let's look out for
  # (1) players having only 0 minutes sequences while having a sequence at another club
  # (2) don't think we can distinguish between a player leaving mid season for a non-prem club, and a player being at a club but never being picked. but it doesn't matter for the player model and riku we have systems in place to help with that

  # so it's only 1 that we need to be clever with, so let's have a go at that

  initialgbgdf$playerhasleft = FALSE
  possiblemidseasonleaverdf = playerteamdf %>%
                              filter(overlap) %>%
                              mutate(hasleft = FALSE)
  if (nrow(possiblemidseasonleaverdf) == 0) {
    gbgdf = initialgbgdf
  }

  if (nrow(possiblemidseasonleaverdf) > 0) {
    for (oi in 1:nrow(possiblemidseasonleaverdf)) {
      overlapinfo = possiblemidseasonleaverdf %>% slice(oi)
      ### so the question is, the games of 'team' where date is greater than nextjoindate, are they all 0 minutes?
      currentteamminuteafternextjoindate = semi_join(initialgbgdf, overlapinfo, c('whoscoredplayer', 'team')) %>%
                                            filter(date >= overlapinfo$nextjoindate) %>%
                                            select(teamgamenumber, team, minute, whoscoredplayer)
      playerhasostensiblyleft = all(currentteamminuteafternextjoindate$minute == 0)
      if (playerhasostensiblyleft) {
        currentteamminuteafternextjoindate$playerhasleft = TRUE
        possiblemidseasonleaverdf$hasleft[oi] = TRUE
        initialgbgdf = join_on_overlap(initialgbgdf,
                                   currentteamminuteafternextjoindate,
                                            c('teamgamenumber', 'team', 'whoscoredplayer'))
      }
      if (!playerhasostensiblyleft) {
        message('eek, we have an exmaple of a player who seems to be at two clubs at once')
        print(overlapinfo)
        stop()
      }
    }

    gbgdf = initialgbgdf %>%
      filter(!playerhasleft) %>%
      select(-c(isOverlapping, playerhasleft))

    # but in some situations we also want to have the details of who has left (namely, when we're getting info for the current season ) so we return that info as well
    playerteamdf = lazy_left_join(playerteamdf,
                                  possiblemidseasonleaverdf,
                                  c('team', 'whoscoredplayer'),
                                  'hasleft')

  }

  playerteamdf = playerteamdf %>% select(-overlap)

  return(list(gbgdf = gbgdf,
              playerteamdf = playerteamdf))
}

.ArchivePreviousSeason = function(currentseason) {
  seasonInfoDF = read.csv(paste(DATAPATH,'seasonInfoDF.csv',sep=''))
  seasonInfoDF$toarchive = with(seasonInfoDF, season != currentseason & havegbg)

  for (si in which(seasonInfoDF$toarchive)) {
    ffDataJoining:::CombineWhoscoredGameByGameDataForSeason(seasonInfoDF$season[si])
    message('Have archived all game by game data for ', seasonInfoDF$season[si])
  }
}

CombineWhoscoredGameByGameDataForSeason = function(myseason) {
  initialgbgdf = ffDataJoining:::.MakeInitialGbgDF(myseason, seasonInfoDF)
  gbgdf = ffDataJoining:::.RemoveObviousLeavingPlayer(initialgbgdf)$gbgdf
  # don't think we need playerteamdf there, but can always change if it turns out we do

  fileout = paste0(DATAPATH, 'gamebygame/gamebygame', myseason, '.csv')
  write.csv(file = fileout, gbgdf, row.names = FALSE)
}

LoadGbgWithoutAppearanceDF = function() {
  gbgdflist = vector('list', nrow(seasonInfoDF))
  for (si in which(seasonInfoDF$havegbg)) {
    filein = paste0(DATAPATH, 'gamebygame/gamebygame', seasonInfoDF$season[si], '.csv')
    gbgdflist[[si]] = readr::read_csv(filein, col_types = list(
                                                  team = readr::col_character(),
                                                  whoscoredplayer = readr::col_character(),
                                                  mainpos = readr::col_character(),
                                                  minute = readr::col_integer(),
                                                  goal = readr::col_integer(),
                                                  assist = readr::col_integer(),
                                                  shotoob = readr::col_integer(),
                                                  shot6yd = readr::col_integer(),
                                                  shotib = readr::col_integer(),
                                                  openplay = readr::col_integer(),
                                                  counter = readr::col_integer(),
                                                  setpiece = readr::col_integer(),
                                                  penaltytaken = readr::col_integer(),
                                                  offt = readr::col_integer(),
                                                  bar = readr::col_integer(),
                                                  ont = readr::col_integer(),
                                                  block = readr::col_integer(),
                                                  penaltyscored = readr::col_integer(),
                                                  longkp = readr::col_integer(),
                                                  shortkp = readr::col_integer(),
                                                  date = readr::col_integer(),
                                                  season = readr::col_integer(),
                                                  teamgamenumber = readr::col_integer()
                                                ))
    gbgdflist[[si]]$season = seasonInfoDF$season[si]
  }
  gbgdf = bind_rows(gbgdflist)

  return(gbgdf)
}

MakeCurrentPlayerFile = function(currentseason) {
  seasonInfoDF = read.csv(paste(DATAPATH,'seasonInfoDF.csv',sep=''))
  initialgbgdf = ffDataJoining:::.MakeInitialGbgDF(currentseason, seasonInfoDF)
  dum = ffDataJoining:::.RemoveObviousLeavingPlayer(initialgbgdf)
  gbgdf = dum$gbgdf
  playerteamdf = dum$playerteamdf

  ffplayerpricedf = readr::read_csv(paste0(DATAPATH, 'ff-price.csv'),
                                    col_types = list(player = readr::col_character(),
                                                     team = readr::col_character(),
                                                     ffposition = readr::col_character(),
                                                     price = readr::col_double()))
  ffplayerpricedf$teamsurname=with(ffplayerpricedf,paste(team,player))

  playerteamdf = playerteamdf %>%
    mutate(surname = case_when(
      adjustedwhoscoredname == 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',whoscoredname),
      #adjustedwhoscoredname != 'none' & !ffuseswholename ~ gsub('^[^ ]+ ','',adjustedwhoscoredname),
      #adjustedwhoscoredname != 'none' & ffuseswholename ~ adjustedwhoscoredname,
      adjustedwhoscoredname == 'none' & ffuseswholename ~ whoscoredname,
      adjustedwhoscoredname != 'none' ~ adjustedwhoscoredname))
  playerdf$teamsurname=with(playerdf,paste(probablelatestteam,surname))

}
