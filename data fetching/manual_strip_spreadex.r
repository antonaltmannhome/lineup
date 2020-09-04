### ok, so we can't paste in the odds from any of the spread betting sites, just have to manually enter them

## firstly scan in what's already been done (at start of season, this won't exist so you have to make it)

spreadexfile = paste0(DATAPATH, 'spreadex_',currentseason,'.csv')
if (aboutToStartSeason) {
  fixtDF = ffDataLoading:::GetFixtDF()
  spreadexDF = fixtDF %>%
    filter(isHome) %>%
    select(date, ht = team, at = oppteam) %>%
    mutate(supspread = NA,
           totspread = NA)
  write_csv(x = spreadexDF, path = spreadexfile)
}
if (!aboutToStartSeason) {
  resultDF = ffDataLoading:::GetResultDF()
  fixtDF = ffDataLoading:::GetFixtDF(resultDF)
  
  existingodds = read.csv(spreadexfile, as.is = TRUE)
  # fixtures might have changed since it was made, so join in new stuff
  
  horizResultDF = resultDF %>%
    filter(season== currentseason & isHome) %>%
    select(date, team, oppteam) %>%
    dplyr::rename(ht = team, at = oppteam)
  
  horizFixtDF = fixtDF %>%
    filter(isHome) %>%
    select(date, team, oppteam) %>%
    dplyr::rename(ht = team, at = oppteam)
  
  horizDF = bind_rows(horizResultDF, horizFixtDF)
  
  horizDF = left_join(horizDF, existingodds, c('date', 'ht', 'at'))
  
  write.csv(file = spreadexfile, horizDF, row.names = FALSE)
  
  message('Have updated the fixture list in ', spreadexfile,', you can add spread data now')
}
