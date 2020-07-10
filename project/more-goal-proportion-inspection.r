# trying to get away from using player's official position so much.
# so here we look at prop deserved goals scored.
# and the question is, does it think a defender who scores in their first game is a goal machine?

source('new-model-startup.r')
gbgdf = ProcessDeserved(gbgdf)
summaryDF = ProcessDeserved(summaryDF)

InspectTeamPlayer = function(myTeam, myPlayer, mySeason = 1920) {
  
  mugbgdf = gbgdf %>% filter(team == myTeam & season == mySeason)
  
  bygamedeserved = mugbgdf %>%
    group_by(teamgamenumber) %>%
    summarise(teamdeservedgoal = sum(deservedgoal),
              teamdeservedassist = sum(deservedassist))
  mugbgdf = left_join(mugbgdf, bygamedeserved, 'teamgamenumber')
  
  mugbgdf = mugbgdf %>%
    group_by(player) %>%
    arrange(teamgamenumber) %>%
    mutate(csdeservedgoal = cumsum(deservedgoal),
           csdeservedassist = cumsum(deservedassist),
           csteamdeservedgoal = cumsum(teamdeservedgoal * minute / 94),
           csteamdeservedassist = cumsum(teamdeservedassist * minute / 94)) %>%
    ungroup()
  
  mugbgdf = mugbgdf %>%
    mutate(cspropdeservedgoal = csdeservedgoal / csteamdeservedgoal,
           cspropdeservedassist = csdeservedassist / csteamdeservedassist)
  
  print(mugbgdf %>%
          filter(grepl(myPlayer, player)) %>%
          arrange(teamgamenumber) %>%
          select(minute, player, deservedgoal, deservedassist, cspropdeservedgoal, cspropdeservedassist))
}

# so for van dijk who eg scored in the first game of the season, yes, but by the third game it had reached the correct point
# always seems to settled down after about 3 games
# think i like this

# let's see how long it take for the overall prior to get squashed

# so, let's use no prior, and predict goals scored

# but we need cumulative proportin until now, which we don't quite sem to have done in previous code

resultDFPlus = resultDF %>%
  left_join(gbgdf %>%
              group_by(season, team, teamgamenumber) %>%
              summarise(teamDeservedGoal = sum(deservedgoal)),
            c('season', 'team', 'teamgamenumber'))

gbgdfPlus = gbgdf %>%
  lazy_left_join(resultDFPlus, c('season', 'team', 'teamgamenumber'), 'teamDeservedGoal')

downWeight = 0.95
windowWidth = 38
chcumteamdeserved = gbgdfPlus %>%
  group_by(player) %>%
  arrange(season, teamgamenumber) %>%
  mutate(rsTeamDeservedGoal = roll_sum(teamDeservedGoal * minute / 94,
                                       windowWidth,
                                       weights = downWeight^(windowWidth:1)),
         rsPlayerDeservedGoal = roll_sum(deservedgoal,
                                         windowWidth,
                                         weights = downWeight^(windowWidth:1)),
         playerGoalProp = rsPlayerDeservedGoal / rsTeamDeservedGoal) %>%
  ungroup()

chcumteamdeserved %>% filter(team == 'manutd' & grepl('rashford', player) & season == 1920) %>% select(minute, teamgamenumber, teamDeservedGoal, rsTeamDeservedGoal, deservedgoal, rsPlayerDeservedGoal, playerGoalProp) %>% View
# almost exactly what the InspectPlayer says, why is it slightly different
# still, looks pretty good, no horrible loop, hooray
# but we won't be able to downweight extra between seasons, hopefully won't matter too much

# next step is to predict opgoal.
# although sum deserved goal is much bigger than sum goal, oh shit
