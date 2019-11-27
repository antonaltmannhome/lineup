### just thinking about the idea of simulating the teams instead, see how different the choices would be

currentPlayerFixtDF = semi_join(playerfixtdf, currentteam, c('team', 'player')) %>%
                        select(player, team, ffPosition, probStart, probOffBench, eMinStart, eMinBench,
                              egoal, eassist, eteamconceded, egksave)

### argh, it's more difficult than i thought to get retro-player expected minutes. want to know distro of minutes given all the expected minute values
currentPlayerFixtDF
