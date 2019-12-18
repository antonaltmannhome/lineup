
LoadAllSoccerwayWhoscoredPlayerMap = function(appearanceDF) {
  unTeamSeason = appearanceDF %>%
                  distinct(season, team)
  playerMatchList = vector('list', nrow(unTeamSeason))
  for (ti in 1:nrow(unTeamSeason)) {
    storedFile = with(unTeamSeason[ti,],
                  paste0(DATAPATH, 'soccerway_saved/team_season_player_match/', team, '_', season, '.csv'))
    playerMatchList[[ti]] = read.csv(storedFile, as.is = TRUE)
    playerMatchList[[ti]]$team = unTeamSeason$team[ti]
  }
  playerMatch = bind_rows(playerMatchList)
  message('Annoying problem, some extra ruubbish columns have been added to the player matching files, remove when next handy')
  return(playerMatch)
}

UpdateSoccerwayWhoscoredPlayerMap = function(appearanceDF) {
  unTeam = appearanceDF %>%
            filter(season == currentseason) %>%
            distinct(team)
  for (ti in 1:nrow(unTeam)) {
    ffDataJoining:::.MatchSoccerwayPlayerToWhoscoredByTeam(unTeam$team[ti], currentseason)
  }
}

.GetPlayerSummaryFromGame = function(appearanceDF, gbgdf, myTeam, mySeason) {

    mySoccerwayPlayer = appearanceDF %>%
  						filter(team == myTeam & season == mySeason) %>%
  						rename(rikuName = player) %>%
  						group_by(season, playerid, rikuName) %>%
  						summarise(rikuNumGame = sum(!is.na(endTime) & endTime != 'U')) %>%
  						filter(rikuNumGame > 0) %>%
  						ungroup %>%
  						mutate(rowIndex = 1:n())

  	myWhoScoredPlayer = gbgdf %>%
  						filter(team == myTeam & season == mySeason) %>%
  						rename(WSName = whoscoredplayer) %>%
  						group_by(season, WSName) %>%
  						summarise(WSNumGame = sum(minute > 0)) %>%
  						ungroup()

    return(list(mySoccerwayPlayer = mySoccerwayPlayer,
                myWhoScoredPlayer = myWhoScoredPlayer))
}

.DoObviousMatch = function(mySoccerwayPlayer, myWhoScoredPlayer) {

  mySoccerwayPlayer$nameMatched = '*'
  myWhoScoredPlayer$alreadyMatched = FALSE
  # so firstly, match by name if we can
  mapByName = match(with(mySoccerwayPlayer, paste(season, rikuName, rikuNumGame)),
            with(myWhoScoredPlayer, paste(season, WSName, WSNumGame)))
  mySoccerwayPlayer[,c('WSName', 'WSNumGame')] =
    myWhoScoredPlayer[mapByName, c('WSName', 'WSNumGame')]
  mySoccerwayPlayer$nameMatched[!is.na(mapByName)] = ' '
  myWhoScoredPlayer$alreadyMatched[mapByName] = TRUE

  return(list(mySoccerwayPlayer = mySoccerwayPlayer,
              myWhoScoredPlayer = myWhoScoredPlayer))
}

.MatchByNumberOfGame = function(mySoccerwayPlayer, myWhoScoredPlayer) {
  # now the other ones, match by number of games if possible
  missingIndex = mySoccerwayPlayer %>% filter(is.na(WSName)) %>% pull(rowIndex)

  mySoccerwayPlayer$likelyMatchIndex = NA
  for (mi in missingIndex) {
    dum = mySoccerwayPlayer %>% filter(rowIndex == mi)
    likelyMatch = with(myWhoScoredPlayer,
              which(season == dum$season &
                  WSNumGame == dum$rikuNumGame &
                  !alreadyMatched))
    if (length(likelyMatch) == 1) {
      mySoccerwayPlayer$WSName[mi] = myWhoScoredPlayer$WSName[likelyMatch]
      mySoccerwayPlayer$likelyMatchIndex[mi] = likelyMatch
    }
  }

  attemptSummary = mySoccerwayPlayer %>%
      filter(rowIndex %in% missingIndex & !is.na(WSName)) %>%
      select(rowIndex, season, rikuName, WSName)
  # now ask me if i agree with all the matches
  if (nrow(attemptSummary) > 0) {
    print(attemptSummary)
    print('Enter all the ones that you think are wrong, separated by ENTER (blank when done)')
    finished = FALSE
    wrongIndex = NULL
    while(!finished) {
      myWrong = askcond(T, T)
      if (!is.null(myWrong)) {
        wrongIndex = c(wrongIndex, myWrong)
      }
      if (is.null(myWrong)) {
        finished = TRUE
      }
    }

    if (length(wrongIndex) > 0) {
      mySoccerwayPlayer$WSName[mySoccerwayPlayer$rowIndex %in% wrongIndex] = NA
      mySoccerwayPlayer$likelyMatchIndex[mySoccerwayPlayer$rowIndex %in% wrongIndex] = NA
    }
  }

  # we can now say that the likelyMatch ones in myWhoScoredPlayer are alreadyMatched
  myWhoScoredPlayer$alreadyMatched[unique(mySoccerwayPlayer$likelyMatchIndex)]= TRUE

  return(list(mySoccerwayPlayer = mySoccerwayPlayer,
              myWhoScoredPlayer = myWhoScoredPlayer))
}

.ManuallyMatch = function(mySoccerwayPlayer, myWhoScoredPlayer) {

  # now loop through the unmatched ones, ask user to identify them
  if (any(is.na(mySoccerwayPlayer$WSName))) {
	  mySoccerwayMissing = mySoccerwayPlayer %>%
  								      filter(is.na(WSName))
    myWhoScoredUnmatched = myWhoScoredPlayer %>%
  											filter(!alreadyMatched) %>%
  											mutate(rowIndex = 1:n()) %>%
  											select(rowIndex, WSName, WSNumGame)
		for (j in 1:nrow(mySoccerwayMissing)) {
			cat('\n\n')
			print(as.data.frame(mySoccerwayMissing[j,c('season', 'rikuName', 'rikuNumGame')]))
			print('Which player is this (enter for none)?')
			print(as.data.frame(myWhoScoredUnmatched))
			myRowIndex = askcond(T, T)
			if (!is.null(myRowIndex)) {
				fullSoccerwayPlayerIndex = which(mySoccerwayPlayer$rowIndex == mySoccerwayMissing$rowIndex[j])
				mySoccerwayPlayer$WSName[fullSoccerwayPlayerIndex] = myWhoScoredUnmatched$WSName[myRowIndex]
			}
		}
	}

  return(mySoccerwayPlayer)
}

.UpdateForMissingPlayer = function(missingPlayerIndex, mySoccerwayPlayer, myWhoScoredPlayer) {
  satis = FALSE
  while(!satis) {
    for (mi in missingPlayerIndex){
      print('This is the information from soccerway with no match from whoscored:')
      print(mySoccerwayPlayer[mi,])
      print('This is the list of unmatched whoscored players:')
      unmatchedWhoscoredIndex = which(!myWhoScoredPlayer$WSName %in% mySoccerwayPlayer$whoscoredPlayer)
      if (length(unmatchedWhoscoredIndex) == 0) {
        message('There is no match, happy to ignore?')
        userEntry = askcond(F, F)
        if (userEntry == 'y') {
          mySoccerwayPlayer$whoscoredPlayer[mi] = 'IGNORE'
          satis = TRUE
        }
        if (userEntry == 'n') {
          stop('Ok, you need to write some more code!')
        }
      }
      if (length(unmatchedWhoscoredIndex) > 0) {
          satis = FALSE
          while(!satis) {
            print('Which of these players is the missing one? (press 0 for no matches)')
            dum = cbind(1:length(unmatchedWhoscoredIndex), myWhoScoredPlayer[unmatchedWhoscoredIndex,])
            names(dum)[1] = 'index'
            print(dum)
            userEntry = askcond(TRUE, FALSE)
            if (userEntry == 0) {
              stop('Write some code!')
            }
            if (userEntry %in% c(1:length(unmatchedWhoscoredIndex))) {
              mySoccerwayPlayer$whoscoredPlayer[mi] = myWhoScoredPlayer$WSName[unmatchedWhoscoredIndex[userEntry]]
              satis = TRUE
            }
          }
        }
    }
  }
  return(mySoccerwayPlayer)
}

# let's do it team by team - want facility to update of course
.MatchSoccerwayPlayerToWhoscoredByTeam = function(myTeam, mySeason) {
  # load in what's already been done, if anything
  storedFile = paste0(DATAPATH, 'soccerway_saved/team_season_player_match/', myTeam, '_', mySeason, '.csv')
  # we might be at the start of a season, so this file might not exists
  alreadyDone = file.exists(storedFile)

  if (alreadyDone) {
    storedInfo = read.csv(storedFile, as.is = TRUE)
  }

  dum = ffDataJoining:::.GetPlayerSummaryFromGame(appearanceDF, gbgdf, myTeam, mySeason)
  mySoccerwayPlayer = dum$mySoccerwayPlayer
  myWhoScoredPlayer = dum$myWhoScoredPlayer

  if (alreadyDone) {
    mySoccerwayPlayer = lazy_left_join(mySoccerwayPlayer, storedInfo, 'playerid', 'whoscoredPlayer')
    # anyone missing?
    missingPlayerIndex = which(is.na(mySoccerwayPlayer$whoscoredPlayer))
    if (length(missingPlayerIndex) > 0) {
      message('myTeam = \'', myTeam, '\'; mySeason =', mySeason)
      mySoccerwayPlayer = ffDataJoining:::.UpdateForMissingPlayer(missingPlayerIndex, mySoccerwayPlayer, myWhoScoredPlayer)
      # we can now update the stored file with this new info
      write.csv(file = storedFile, mySoccerwayPlayer,	row.names = FALSE)
    }
  }

  if (!alreadyDone) {
    dum = ffDataJoining:::.DoObviousMatch(mySoccerwayPlayer, myWhoScoredPlayer)
    mySoccerwayPlayer = dum$mySoccerwayPlayer
    myWhoScoredPlayer = dum$myWhoScoredPlayer

    dum = ffDataJoining:::.MatchByNumberOfGame(mySoccerwayPlayer, myWhoScoredPlayer)
    mySoccerwayPlayer = dum$mySoccerwayPlayer
    myWhoScoredPlayer = dum$myWhoScoredPlayer

    mySoccerwayPlayer = ffDataJoining:::.ManuallyMatch(mySoccerwayPlayer, myWhoScoredPlayer)

    # lots of intermediate columns potentially, reduce them down
    mySoccerwayPlayer = mySoccerwayPlayer  %>%
                        select(season, playerid, rikuName, WSName) %>%
                        rename(soccerwayPlayer = rikuName,
                            whoscoredPlayer = WSName)

  	write.csv(file = storedFile, mySoccerwayPlayer,	row.names = FALSE)
  }

  return(NULL)
}

CheckDoubleNamePlayer = function(playerMatchDF) {
  doubleNamePlayer = playerMatchDF %>%
    distinct(playerid, whoscoredplayer) %>%
    count(playerid) %>%
    filter(n > 1)
  doubleNameInfo = semi_join(playerMatchDF, doubleNamePlayer, 'playerid')
  if (nrow(doubleNameInfo) > 0) {
    print(doubleNameInfo)
    stop('aaarghh, we do not want players having two names within playerMatchDF or gfgdf')
  }
}

CheckNonTrivialMissing = function(appearanceDF) {
  stop('this function is deprecated')
  appearanceDF$nonTrivialMissing = with(appearanceDF, is.na(player) & !startTime %in% c('U', 'UU', 'injury', 'suspension'))
  if (sum(appearanceDF$nonTrivialMissing) > 0) {
    message('These are the non-trivial examples of datas being in soccerway data but not in whoscored:')
    print(appearanceDF %>%
            filter(nonTrivialMissing) %>%
            select(date, team, soccerwayPlayer, startTime, endTime))
  }
}
