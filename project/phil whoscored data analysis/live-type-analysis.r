# what about the live2 type idea? we've got expected goals for every game, just do regression of it against all the shots. once for accuracy, once for location
# although we'll have to sum up shots in the match, then allocate to players, which sounds ok

### went down a few blind alleys with previous approach, let's start again with another idea

source('c:/git/lineup/new-model-startup.r')

# and also, if we have enough data for a player throughout a season, do we really have to make it all oos? yes, because we want to knwo the value of a goal at predicting goals
gbgdf$nonpengoal = with(gbgdf, goal - penaltyscored)
gbgdf$ontargetbutmiss = with(gbgdf, ont - goal)
gbgdf$offtnotbar = with(gbgdf, offt - bar)

# let's please have camelcase vars
gbgdf$nonPenaltyGoal = with(gbgdf, goal - penaltyscored)
gbgdf$onTargetNotGoal = with(gbgdf, ont - goal)
gbgdf$offTargetNotBar = with(gbgdf, offt - bar)
gbgdf$shotInBox = gbgdf$shotib
gbgdf$shotOutBox = gbgdf$shotoob
gbgdf$shot6Yard = gbgdf$shot6yd

# this shows we've got pretty much every goal correctly accounted for:
with(gbgdf, table(shotOutBox + shotInBox + shot6Yard - (offTargetNotBar + onTargetNotGoal + block + bar+ nonPenaltyGoal + penaltyscored)))

# now sum by game
totalBySeasonTeamGame = gbgdf %>%
  group_by(season, team, teamgamenumber) %>%
  summarise(gameOnTarget = sum(onTargetNotGoal),
            gameOffTarget = sum(offTargetNotBar),
            gameBar = sum(bar),
            gameBlock = sum(block),
            gameNonPenaltyGoal = sum(nonPenaltyGoal))

resultPlusDF = left_join(resultdf, totalBySeasonTeamGame, c('season', 'team', 'teamgamenumber'))

mod = lm(oddsescored ~ gameOnTarget + gameOffTarget + gameBar + gameBlock + gameNonPenaltyGoal,
         data = resultPlusDF)
# hm, they all come out roughly the same, i don't believe that
