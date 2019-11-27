
source('c:/research/lineup/new-model-startup.r')
source(paste0(USERPATH, 'team_funct.r'))
source(paste0(USERPATH, 'player_funct.r'))

# let's fill in the gaps in mainpos

# i think if a player it literally injured or unused all season, we can ditch them altogether
# otherwise, use whatever position they're logged as for the majority of other matches

# so let's get each players stats on their position each year
playerPosStat = gbgdf %>%
  filter(!is.na(mainpos)) %>%
  group_by(season, team, player) %>%
  count(mainpos) %>%
  ungroup()
# but i want to know situation where there are more than one answer
numPosBySTP = playerPosStat %>%
              group_by(season, team, player) %>%
              summarise(numPos = n()) %>%
              ungroup()
# ok so there are genuine examples of players playing in differnt positions. just go with the majority
playerMainPos = playerPosStat %>%
  group_by(season, team, player) %>%
  summarise(mostFreqPos = mainpos[which.max(n)])
gbgdf = subset_join(gbgdf,
          playerMainPos %>%
          rename(mainpos = mostFreqPos),
          c('season', 'team', 'player'),
          is.na(mainpos))
# the ones that are NA, i say ditch from gbgdf, they're not active players
# but when? it might cause problms for player matching, so i say: fill in in the data joining, but remove before modelling


### want to rule out situations where there was a weirdly small number of players in each position
gbgdf$tgId = with(gbgdf, paste(season, teamgamenumber, team))

# let's see what a normal number actually is though
numPosByTG = gbgdf %>%
  filter(!is.na(mainpos)) %>%
  group_by(tgId) %>%
  summarise(numGk = sum(minute * (mainpos == 'GK'))/94,
            numD = sum(minute * (mainpos == 'D'))/94,
            numM = sum(minute * (mainpos %in% c('DMC', 'M')))/94,
            numF = sum(minute * (mainpos %in% c('AM', 'FW')))/94)


ludat$predyn=rep(0,dim(ludat)[1])
# home team
sax=which(gamedat$hg365[rr]>=5 & team==gamedat$ht[rr] & gamedat$hngk[rr]>=1 & ludat$npgamclub>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='G')
ludat$predyn[sax]=1
sax=which(gamedat$hg365[rr]>=5 & team==gamedat$ht[rr] & gamedat$hndef[rr]>=4 & ludat$npgamclub>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='D')
ludat$predyn[sax]=1
sax=which(gamedat$hg365[rr]>=5 & team==gamedat$ht[rr] & gamedat$hnmf[rr]>=6 & ludat$npgamclub>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='MF')
ludat$predyn[sax]=1
# away team
sax=which(gamedat$ag365[rr]>=5 & team==gamedat$at[rr] & gamedat$angk[rr]>=1 & ludat$npgamclub>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='G')
ludat$predyn[sax]=1
sax=which(gamedat$ag365[rr]>=5 & team==gamedat$at[rr] & gamedat$andef[rr]>=4 & ludat$npgam>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='D')
ludat$predyn[sax]=1
sax=which(gamedat$ag365[rr]>=5 & team==gamedat$at[rr] & gamedat$anmf[rr]>=6 & ludat$npgam>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='MF')
ludat$predyn[sax]=1
