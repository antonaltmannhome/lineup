# do the start of scoring streaks look any different to false dawns?
# need to identify both of course.

source('c:/git/lineup/new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

gbgdf = ffModel:::CalculateDeservedGoalAssist(gbgdf)

# streaks i can think of: vardy 19/20, ings 19/20, richarlison 17/18
# let's come up with a simple def: >= 1goal/game over at least 8 weeks
# do we have to be so binary though?
# no, let's do some plotting first. 


PlotAttackingCont = function(myPlayerString) {
  subGbgDF = gbgdf %>%
    filter(grepl(myPlayerString, player))
  vertGbgDF = subGbgDF %>%
    select(season, teamgamenumber, goal, deservedgoal, ont, offt, block, shotoob, shotib) %>%
    arrange(season, teamgamenumber) %>%
    mutate(totalGameNumber = 1:n()) %>%
    select(-c(season, teamgamenumber)) %>%
    gather(stat, total, -totalGameNumber)
  
  fillVector = c('goal' = 'black',
                 'deservedgoal' = 'white',
                 'ont' = 'green',
                 'offt' = 'blue',
                 'block' = 'cyan',
                 'shotoob' = 'brown',
                 'shotib' = 'grey')
  sizeVector = c('goal' = 3,
                 'deservedgoal' = '3',
                 'ont' = 1,
                 'offt' = 1,
                 'block' = 1,
                 'shotoob' = 1,
                 'shotib' = 1)
  
  ggplot(vertGbgDF) +
    geom_point(aes(x = totalGameNumber, y = total, fill = stat),
               shape = 22, col = 'black') +
    scale_fill_manual(values = fillVector) +
    scale_size_manual(sizeVector)
  
  # totally useless
}



PlotAttackingCont = function(myPlayerString) {
  subGbgDF = gbgdf %>%
    filter(grepl(myPlayerString, player))
  vertGbgDF = subGbgDF %>%
    select(season, teamgamenumber, goal, deservedgoal) %>%
    arrange(season, teamgamenumber) %>%
    mutate(totalGameNumber = 1:n())

  
  ggplot(vertGbgDF) +
    geom_point(aes(x = totalGameNumber, y = goal)) +
    geom_line(aes(x = totalGameNumber, y = deservedgoal))
}

# also pointless. the fact that goals are part of deserved goal is a bit concerning
