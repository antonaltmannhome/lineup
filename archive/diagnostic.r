gbgdf$expectedgoal = with(gbgdf, minute*teamegoal*normgoalrate3/90)
gbgdf$expectedassist = with(gbgdf, minute*teamegoal*normassistrate3/90)
total = gbgdf %>%
		filter(season==1718) %>%
		group_by(team, player) %>%
		summarise(totmin=sum(minute),
					totegoal=sum(expectedgoal),
					totdeservedgoal=sum(deservedgoal),
					totgoal=sum(goal-penaltyscored),
					toteassist = sum(expectedassist),
					totdeservedassist=sum(deservedassist),
					totassist=sum(assist))

					
with(total, plot(totegoal, totgoal))
abline(0,1,col='red')
with(total[total$totegoal>3,], text(totegoal, totgoal, player,cex=0.7))

with(total, plot(totdeservedgoal, totgoal))
abline(0,1,col='red')
with(total[total$totdeservedgoal>3,], text(totdeservedgoal, totgoal, player,cex=0.7))
