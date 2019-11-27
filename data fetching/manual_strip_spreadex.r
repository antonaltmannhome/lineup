### ok, so we can't paste in the odds from any of the spread betting sites, just have to manually enter them

## firstly scan in what's already been done (at start of season, this won't exist so you have to make it)

resultdf = ffDataLoading:::GetResultDF()
fixtdf = ffDataLoading:::GetFixtDF()
spreadexfile = paste0(DATAPATH, 'spreadex_',currentseason,'.csv')

if (FALSE) {
horizResultDF = resultdf %>%
				filter(season==1819 & gameweek<=2 & isHome) %>%
				select(date, team, oppteam) %>%
				dplyr::rename(ht = team, at = oppteam)

horizFixtDF = fixtdf %>%
				filter(isHome) %>%
				select(date, team, oppteam) %>%
				dplyr::rename(ht = team, at = oppteam)

horizFixtDF = bind_rows(horizResultDF, horizFixtDF) %>%
				mutate(supspread = NA, totspread = NA)

write.csv(file = spreadexfile, horizFixtDF, row.names = FALSE)

write.csv(file = 'd:/whoscored_data/spreadex_1617.csv',
			resultdf %>%
				filter(season==1617 & isHome) %>%
				select(date, team, oppteam, oddsescored, oddseconceded) %>%
				dplyr::rename(ht = team, at = oppteam) %>%
				mutate(supspread = oddsescored - oddseconceded,
						totspread = oddsescored + oddseconceded) %>%
				select(-c(oddsescored, oddseconceded)),
			row.names = FALSE)

write.csv(file = 'd:/whoscored_data/spreadex_1718.csv',
			resultdf %>%
				filter(season==1718 & isHome) %>%
				select(date, team, oppteam, oddsescored, oddseconceded) %>%
				dplyr::rename(ht = team, at = oppteam) %>%
				mutate(supspread = oddsescored - oddseconceded,
						totspread = oddsescored + oddseconceded) %>%
				select(-c(oddsescored, oddseconceded)),
			row.names = FALSE)
}

existingodds = read.csv(spreadexfile, as.is = TRUE)
# fixtures might have changed since it was made, so join in new stuff

horizResultDF = resultdf %>%
				filter(season== currentseason & isHome) %>%
				select(date, team, oppteam) %>%
				dplyr::rename(ht = team, at = oppteam)

horizFixtDF = fixtdf %>%
				filter(isHome) %>%
				select(date, team, oppteam) %>%
				dplyr::rename(ht = team, at = oppteam)

horizDF = bind_rows(horizResultDF, horizFixtDF)
				
horizDF = left_join(horizDF, existingodds, c('date', 'ht', 'at'))

write.csv(file = spreadexfile, horizDF, row.names = FALSE)

message('Have updated the fixture list in ', spreadexfile,', you can add spread data now')
