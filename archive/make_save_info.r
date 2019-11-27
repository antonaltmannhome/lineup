### let's look into choosing goalkeepers as well
### need to merge code in neatly somewhere at some point

shotinfo = gbgdf %>%
			group_by(season, gameweek, team) %>%
			summarise(shot = sum(ont))

resultdf = left_join(resultdf,
						shotinfo %>%
						dplyr::rename(ht = team, hshotont = shot),
						by = c('season', 'gameweek', 'ht'))
resultdf = left_join(resultdf,
						shotinfo %>%
						dplyr::rename(at = team, ashotont = shot),
						by = c('season', 'gameweek', 'at'))

resultdf = resultdf %>%
			mutate(hsave = ashotont - asc,
					asave = hshotont - hsc)

gbgdf$gksave = 0
gbgdf$oppegoal = NA
gbgtohomeresultmap = match(
						with(gbgdf,
							paste(season, team, gameweek)),
						with(resultdf,
							paste(season, ht, gameweek)))
gbgtoawayresultmap = match(
						with(gbgdf,
							paste(season, team, gameweek)),
						with(resultdf,
							paste(season, at, gameweek)))
ishomegbgindex = !is.na(gbgtohomeresultmap)
isawaygbgindex = !is.na(gbgtoawayresultmap)
isPlayingKeeper = with(gbgdf, mainpos == 'GK' & minute > 0)
gbgdf$gksave[isPlayingKeeper & ishomegbgindex] = resultdf$hsave[gbgtohomeresultmap[isPlayingKeeper & ishomegbgindex]]
gbgdf$gksave[isPlayingKeeper & isawaygbgindex] = resultdf$asave[gbgtoawayresultmap[isPlayingKeeper & isawaygbgindex]]

gbgdf$oppegoal[ishomegbgindex] = resultdf$aescore[gbgtohomeresultmap[ishomegbgindex]]
gbgdf$oppegoal[isawaygbgindex] = resultdf$hescore[gbgtoawayresultmap[isawaygbgindex]]

gbgdf$gksave[which(gbgdf$gksave < 0)] = 0

mod = with(gbgdf[sax,], glm(gksave ~ oppegoal, family='poisson'))

### then got to join it up to fixtures etc
