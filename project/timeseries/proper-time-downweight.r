### let's take our existing goal/assists models and try to make them have proper time downweight

source('new-model-startup.r')
gbgdf = ffModel:::CalculateDeservedGoalAssist(gbgdf)
summarydf = ffModel::CalculateDeservedGoalAssist(summarydf)

playerGameDF = gbgdf %>%
  filter(played)

dum = ffModel:::CalculateSeasonDeservedGoalAssist(summarydf, playerGameDF)
seasondeservedsummary = dum$seasondeservedsummary
playerGameDF = dum$gbgdf

playerGameDF = ffModel:::ImputeMissingMatchOdds(playerGameDF, resultDF)
playerGameDF = ffModel:::CalculatePositionBasedExpectedGoalAssist(playerGameDF)

# this is what current model does. We want to change it so that it's not constant through an entire season, and doesn't treat the previous season as one solid block
if (FALSE) {
  playerGameDF = playerGameDF %>%
    mutate_cond(is.na(previoustotalgame),
                previoustotalgame = 0,
                previousdeservedgoal = 0,
                previousteamgoal = -999,
                previousdeservedassist = 0,
                previousteamassist = -999
    )
  playerGameDF = playerGameDF %>%
    mutate(adjpreviousdeservedgoal = previousdeservedgoal / (previousteamgoal/38),
           adjpreviousdeservedassist = previousdeservedassist / (previousteamassist/38))
  
  playerGameDF = playerGameDF %>%
    group_by(season,team,player) %>%
    arrange(teamgamenumber) %>%
    mutate(chcumgame = cumsum(minute/94),
           chcumadjdeservedgoal = cumsum(deservedgoal/teamoddsescored),
           chcumadjdeservedassist = cumsum(deservedassist/teamoddsescored),
           legalcumgame = lag(chcumgame, default = 0),
           legalcumadjdeservedgoal = lag(chcumadjdeservedgoal, default = 0),
           legalcumadjdeservedassist = lag(chcumadjdeservedassist, default = 0)) %>%
    ungroup()
}

### so features we want in this:
## prior to the position
## and prior to proportion of previous season to when they joined their team/data
## and prior to proportion of previous team if they change teams, if available
## no that's a silly idea: just downweight a bit more
## maybe downweiht abit more between seasons? also allow different time downweights for D/M/F
### although determining likely position by stats would of course be better, but sounds tricky for now

# i think we want to loop over all unique game-for-team numbers as in the playing or not code
# let's nab that

## is this the wrong code to have set up? maybe we should start with the expected minute stuff instead


