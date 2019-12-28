### actually, how different are strikers goal proportion rates, and do they really vary much from season to season?

source('new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

gbgdf = processdeserved(gbgdf)

regularSeasonStriker = gbgdf %>%
  group_by(season, player) %>%
  summarise(sumMinute = sum(minute),
            sumGoalRate = 94 * sum(opgoal) / sum(minute)) %>%
  filter(sumMinute > 18 * 94 & sumGoalRate > 0.3)

teamGoal = gbgdf %>%
  group_by(season, teamgamenumber, team) %>%
  summarise(teamOpGoal = sum(opgoal))

teamSeasonGoal = teamGoal %>%
  group_by(season, team) %>%
  summarise(teamSumMinute = 94 * n(),
            teamSeasonOpGoal = sum(teamOpGoal))

regularStrikerSeasonGoal = gbgdf %>%
  group_by(season, player, team) %>%
  summarise(playerSumMinute = sum(minute),
            playerSeasonOpGoal = sum(opgoal)) %>%
  left_join(teamSeasonGoal, c('season', 'team')) %>%
  mutate(adjTeamSeasonopGoal = playerSumMinute / teamSumMinute * teamSeasonOpGoal,
          goalProp = playerSeasonOpGoal / adjTeamSeasonopGoal) %>%
  filter(playerSumMinute > teamSumMinute / 2 & goalProp > 0.1)

# interesting stuff. so a top striker eg lukaku/kane in an amazing season can get 0.4 of team goals. But is that predictable?
# also, you need to know what players who aren't quite strikers, eg hazard, alli, get. it bobs about between seasons, but do the shots bob about as much?
# anyway, it's pretty trivial to classify players into attacking or not based on this. could re-optimise the shot values but only include attacking players

