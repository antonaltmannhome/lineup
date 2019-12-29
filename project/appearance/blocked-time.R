### let's try predicting total minutes played in next ten games, not just next game, that seems too prone to inter-game correlation
source('c:/git/lineup/new-model-startup.r')

gbgdf = gbgdf %>%
  group_by(team, player) %>%
  arrange(seasonNumber, teamgamenumber) %>%
  mutate(gameForTeamNumber = 1:n()) %>%
  ungroup()

distinctSeasonTeamGameNumber = resultdf %>%
  distinct(season, teamgamenumber) %>%
  mutate(seasonNumber = match(season, unique(season))) %>%
  group_by(seasonNumber) %>%
  mutate(inBlock = (seasonNumber - 1) * 4 + cut(teamgamenumber, c(0, 6, 14, 22, 30, 39), labels = FALSE)) %>%
  ungroup()

predictAtDF = distinctSeasonTeamGameNumber %>%
  group_by(inBlock) %>%
  slice(n())

gbgdf = gbgdf %>%
  lazy_left_join(distinctSeasonTeamGameNumber,
                 c('season', 'teamgamenumber'),
                 'inBlock')

gbgdf = indicate_overlapping_combination(gbgdf, predictAtDF, c('season', 'inBlock', 'teamgamenumber'), 'predictAt')

# right, now we add up the number of minutes a player was available for, and how many minutes of those they actually played
blockedAppearanceDF = gbgdf %>%
  filter(available) %>%
  group_by(player, inBlock) %>%
  summarise(sumAvailable = n() * 94,
            sumPlayed = sum(minute))
# not sure about this. have we got enough data points to make sensible estimates? we've got quite a lot really, it hink it's fine. could see how sensitive it is to changing the block divisions

### now, the idea is, we run the loop as before, but with the same downweight for everything i guess, and compare predicted minutes with actual, just at the blocked time points

theta = c(log(0.35), log(2), log(0.5))
CalculateSqDiff = function(theta) {
  quantityChoiceVector = c('probStart', 'probOffBench', 'eMinStart', 'eMinBench')
  for (qi in 1:length(quantityChoiceVector)) {
    
    gbgdfPlusQuantity = ffModel:::CalculateHistoricSingleQuantity(theta, quantityChoice = quantityChoiceVector[qi], gbgdf)
    gbgdf = lazy_left_join(gbgdf,
                           gbgdfPlusQuantity,
                           c('seasonNumber', 'team', 'player', 'teamgamenumber'),
                           quantityChoiceVector[qi])
  }
  
  gbgdf$eMin = with(gbgdf, probStart * eMinStart + (1 - probStart) * probOffBench * eMinBench)
  
  predictDF = gbgdf %>%
    filter(predictAt) %>%
    select(season, player, inBlock, eMin)
  
  # now careful, predictDF for eg block 8 is a prediction for block 9
  predictDF$blockToPredict = predictDF$inBlock + 1
  blockedAppearanceDF = blockedAppearanceDF %>%
    left_join(predictDF %>%
                select(player, blockToPredict, eMin) %>%
                rename(inBlock = blockToPredict),
              c('player', 'inBlock'))
  
  # then turn eMin into actual prediction, adjusting for available minutes
  blockedAppearanceDF$eSumPlayed = with(blockedAppearanceDF, eMin / 94 * sumAvailable)
  
  blockedAppearanceDF$sqDiff = with(blockedAppearanceDF, (eSumPlayed - sumPlayed)^2)
  
  meanSqDiff = mean(blockedAppearanceDF$sqDiff, na.rm = TRUE)
  
  return(meanSqDiff)
}

## season downweight is such an arse though, slows down optimisation so much, for basically nothing
