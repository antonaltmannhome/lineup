## can we quickly fit time series to players?

FitTSToPlayer = function(playerString) {

  jvdg = gbgdf %>%
    filter(grepl(playerString, player) & startTime == '0') %>%
    pull(deservedgoal) %>%
    ts()
  
  plot.ts(jvdg)
  dum = HoltWinters(jvdg, beta = FALSE, gamma = FALSE)
  lines(dum$fitted[,'xhat'], col = 'red')
  print(dum$alpha)
}

# interesting but ulitmately not flexible enough: our outcome measure should be goal, not deserved goal. and also we want the positional prior or their previous # of goals. and of course, the opponent.
# in short, this is rubbish, need our own function. but crucially, the more rare a player's goals, the slower the ts has to be

# also, all previous research done on anything ever suggests that downweighting is fine.. so just tweak what we've done

