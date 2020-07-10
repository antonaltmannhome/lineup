# trying to get away from using player's official position so much.
# so here we look at prop deserved goals scored.
# and the question is, does it think a defender who scores in their first game is a goal machine?

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
