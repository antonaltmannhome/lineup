### our position data is so bad. let's try to nous it from the data, via cluster analysis maybe?

## but we're limited by data - we've not been collecting match stats concerning clearances etc
but maybe that's ok. we can classify players into attacking or not. so three pots, goalie, non-attack ,attack

### let's just do a bit of exploratin firstly

gbgdf$minPlay = NA
gbgdf$minPlay[which(gbgdf$endTime == 'U' | is.na(gbgdf$endTime))] = 0
gbgdf$minPlay[which(gbgdf$endTime != 'U' & !is.na(gbgdf$endTime))] = as.numeric(gbgdf$endTime[which(gbgdf$endTime != 'U')]) -
                                              as.numeric(gbgdf$startTime[which(gbgdf$endTime != 'U')])

DisplayStatByTeam = function(myTeam, mySeason) {
  mySummary = gbgdf %>%
    filter(team == myTeam & season == mySeason) %>%
    group_by(player) %>%
    summarise(sumMin = sum(minPlay),
              meanShotIb = 94 * sum(shotib) / sumMin,
              meanShotOob = 94 * sum(shotoob) / sumMin,
              meanShot6yd = 94 * sum(shot6yd) / sumMin,
              meanShotOpen = 94 * sum(openplay) / sumMin,
              meanShortKp = 94 * sum(shortkp) / sumMin,
              meanLongKp = 94 * sum(longkp) / sumMin)

  return(mySummary)
}

# no that is rubbish, it needs to be proportion of total shots, adjusted by the number of minute they've played

# whatam i doing, we have nearly complete positional data already
# just fill in the gaps for that surely
# only seems to happen when a player misses games at the start of the season

# can't be bothered right now though
