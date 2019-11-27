### want to produce a file with soccerway playerid, soccerway name, soccerway current team, whoscored name, ff name, nametouse
## the idea is you can then use combo of name and team to uniquely identify every palyer. then when doing any analysis, it's the nametouse that you use
## for example, if there are two players called john smith, one is referred to in any anlysis is john smith1 the other john smith2, to avoid any confusion

# source('c:/research/lineup/ff_startup.r')

source('c:/research/lineup/appearance model/appearance-model-funct.r')
source('c:/research/lineup/data code/make_gbgdf.r')
source('c:/research/lineup/data code/sort-player-name-funct.r')

appearanceDF = getappearancedf()

gbgdf = LoadGbgDF()

# let's load in what we've already matched up
unTeamSeason = LogAlreadyMatched(appearanceDF)

# step through each team this season, update if necessary
for (ti in which(unTeamSeason$season == currentseason)) {
  dum = MatchSoccerwayPlayerToWhoscoredByTeam(unTeamSeason$team[ti], unTeamSeason$season[ti])
}
