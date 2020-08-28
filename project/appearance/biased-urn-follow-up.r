# so it feels like we've cracked the main biased urn bit. the next bit is, where are its biases? can we adjust for fatigue, scoring in previous match etc?


source('c:/git/lineup/new-model-startup.r')
source('project/appearance/biased-urn-funct.R')
#library(doParallel)
#registerDoParallel()

fakeMaxTime = 1000L

# need to sort out seasonNumber. why do we need 1516 in seasonInfoDF?
seasonInfoDF$seasonNumber = match(seasonInfoDF$season, with(seasonInfoDF, sort(season[havegbg])))
resultDF = lazy_left_join(resultDF, seasonInfoDF, 'season', 'seasonNumber')
resultDF = resultDF %>%
  mutate(alltimetgn = (seasonNumber - 1) * 38 + teamgamenumber)

uniqueSeasonTeamGameNumber = resultDF %>%
  distinct(seasonNumber, teamgamenumber) %>%
  arrange(seasonNumber, teamgamenumber)
uniqueSeasonTeamGameNumber$inBlock = NA
for (si in 1:length(unique(uniqueSeasonTeamGameNumber$seasonNumber))) {
  # first four games are in previous season's block, or no block if it's the first season
  currentSeasonIndex = with(uniqueSeasonTeamGameNumber, which(seasonNumber == si))
  initialBlock = cut(uniqueSeasonTeamGameNumber$teamgamenumber[currentSeasonIndex],
                     br = c(-0.5, seq(4.5, 34.5, 6), 38.5),
                     labels = FALSE) - 1
  adjustedBlock = initialBlock + (si - 1) * 6
  uniqueSeasonTeamGameNumber$inBlock[currentSeasonIndex] = adjustedBlock
}
resultDF = resultDF %>%
  left_join(uniqueSeasonTeamGameNumber,
            c('seasonNumber', 'teamgamenumber'))

gbgdf = lazy_left_join(gbgdf,
                       resultDF,
                       c('season', 'team', 'teamgamenumber'),
                       c('inBlock', 'alltimetgn'))

gbgdf2 = gbgdf %>%
  filter(!grepl('(injury|suspension)', startTime)) %>%
  mutate(startTime2 = ifelse(!startTime %in% c('U', 'UU'), startTime, as.character(fakeMaxTime)),
         endTime2 = ifelse(!startTime %in% c('U', 'UU'), endTime, as.character(fakeMaxTime))) %>%
  mutate(startTime2 = as.integer(startTime2),
         endTime2 = as.integer(endTime2))
# except we've got the problem that players sometimes officially start the match at eg 97 minutes.
# let's ceiling the endTime to be the greater of 94 and the biggest startTime of the match

gbgdf2$mainpos2 = with(gbgdf2, case_when(
  mainpos %in% c('D', 'DMC') ~ 'def',
  mainpos == 'M' ~ 'mid',
  mainpos %in% c('AM', 'FW') ~ 'att',
  TRUE ~ 'other'
))


unTeamMainposBlock = gbgdf2 %>%
  distinct(team, mainpos2, inBlock) %>%
  group_by(team, mainpos2) %>%
  arrange(inBlock) %>%
  mutate(blockDelta = inBlock - lag(inBlock, 1),
         isValid = !is.na(blockDelta) & blockDelta == 1) %>%
  filter(mainpos2 %in% c('def', 'mid', 'att') & isValid) %>%
  select(team, mainpos2, inBlock) %>%
  ungroup()
unTeamBlock = unTeamMainposBlock %>%
  distinct(team, inBlock)

biasedUrnModelOutput = readRDS(file = paste0(DATAPATH, 'active_player/biased_urn_output/output_timedownweight_0.1_priorstrength_33.rds'))
allTeamSeasonMainposEstimateDF = biasedUrnModelOutput$allTeamSeasonMainposEstimateDF
allTeamBlockFormationDF = biasedUrnModelOutput$allTeamBlockFormationDF
allTeamBlockExpectedNumGame = biasedUrnModelOutput$allTeamBlockExpectedNumGame
# so we now align the estimated number of games up with the block of matches it was intended to predict
# this is a bit trickier than i thought, because the current method predicts for the entire block, but we want to predict one game at a time


GetExpectedNumGameByPlayerGame = function(myTeam, myBlock, allTeamSeasonMainposEstimateDF, allTeamBlockFormationDF) {
  # so for each match, we need to calculate the prob of each available player playing for each plausible formation
  matchFormationDF = allTeamBlockFormationDF %>%
    filter(team == myTeam & inBlock == myBlock)
  # loop for now of course
  myList = vector('list', 3)
  for (i in 1:3) {
    myMainpos = c('def', 'mid', 'att')[i]
    myPlayerMle = allTeamSeasonMainposEstimateDF %>%
      filter(team == myTeam & inBlock == myBlock & mainpos2 == myMainpos)
    
    subGbgDF = gbgdf2 %>%
      filter(team == myTeam & inBlock == myBlock & available & mainpos2 == myMainpos) %>%
      lazy_left_join(myPlayerMle, 'player', 'appearanceOdds') %>%
      replace_na(list(appearanceOdds = 0.0001))
    
    subMatchFormationDF = matchFormationDF %>%
      select(alltimetgn, formationIndex, myMainpos, formationWgt) %>%
      rename(numPlayerToSelect = myMainpos)
    
    repSubGbgDF = full_join(subGbgDF,
                            subMatchFormationDF,
                            'alltimetgn')
    
    repSubGbgDF = repSubGbgDF %>%
      group_by(alltimetgn, formationIndex) %>%
      mutate(probPlay = BiasedUrn::meanMWNCHypergeo(rep(1, n()), numPlayerToSelect[1], appearanceOdds)) %>%
      ungroup()
    
    myList[[i]] = repSubGbgDF %>%
      group_by(alltimetgn, player) %>%
      summarise(expectedMinute = 94 * sum(probPlay * formationWgt)) %>%
      mutate(team = myTeam, mainpos2 = myMainpos)
  }
  
  # ok so that all works, i believe we need to loop by team/block
  expectedNumGameDF = bind_rows(myList)
  
  return(expectedNumGameDF)
}

# cool, how do i run the bastard without a loop
# yes, like this, only takes 1 minute to run
date()
allGameExpectedMinute = purrr::map2_dfr(.x = unTeamBlock$team, .y = unTeamBlock$inBlock, .f = GetExpectedNumGameByPlayerGame, allTeamSeasonMainposEstimateDF = allTeamSeasonMainposEstimateDF, allTeamBlockFormationDF = allTeamBlockFormationDF)
date()

# ok, we're in business. so let's see if plyers play more often if they scored in the previous match

gbgdf2 = lazy_left_join(gbgdf2,
                   allGameExpectedMinute,
                   c('alltimetgn', 'player'),
                   'expectedMinute')
# ah but hang on, there's that bias for the higher predicted minutes, they play quite a lot less than the model expects
# is it the players who we expect to get 0 minutes whatever happens that are the problem here?
# yes, that's really rubbish what it does at the extreme left.
# but the 0s would obviously be solved by updating more than once a block...
# but that's not the problem we're interested in right now, and doesn't affect us. it's fatigue that we want to look at
# no, these still look pretty rubbish, e.g bruno fernandes takes ages to be recognised as a starter
# so in his case the prior looks way too strong
# but when a player is a regular, the prior looks too weak because it overpredicts
# do we need a different prior for unknown players compared to known ones? not sure that makes any sense
# but there are two effects we're trying to counteract with the prior
# (1) new players aren't infinitely likely to play 90 minutes if they play 90 minutes in their first game
# (2) no matter how often you've played the entire game, there's always the chance you will be benched
# maybe a prior isn't the right way to address (2)
# maybe we need an equivalent of an intercept, that no matter how good you are there's always a chance you will be benched

# we've got the champs league stuff to load in too somewhere
euroFixtureDF = read_csv(paste0(DATAPATH, 'euro-fixtures-historic.csv'), col_types = list(
  tournament = col_character(),
  date = col_integer(),
  team = col_character()
))
               
# cool, now just need to blend it in with the actual data and we're on our way

# note that this only really affects the players who are regular starters, so we need to filter down to them

