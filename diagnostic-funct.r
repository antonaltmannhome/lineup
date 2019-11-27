
viewteam=function(myteam, mysummarydf=summarydf) {
  
  info=mysummarydf %>% filter(team==myteam)
  ### want to get approximate proportion of goals and assists each player gets, so...
  totaldeserved=mysummarydf %>% filter(team==myteam) %>% summarise(totaldeservedgoal=sum(deservedgoal),totaldeservedassist=sum(deservedassist))
  info = info %>% mutate(deservedgoalproportion=deservedgoal/totaldeserved[['totaldeservedgoal']], deservedassistproportion=deservedassist/totaldeserved[['totaldeservedassist']])
  ### but then rescale according to number of minutes played
  totalminute=round(sum(info$minute)/11)
  info = info %>% mutate(deservedgoalproportion = round(deservedgoalproportion*totalminute/minute,2), deservedassistproportion = round(deservedassistproportion*totalminute/minute,2))
  info = info %>% arrange(-deservedpointpg) %>% select(player,minute,ffposition,deservedgoalpg,deservedassistpg,deservedpointpg,deservedgoalproportion,deservedassistproportion)
  ### but let's add another column indicating expected points if expected goals is 1
  return(info)
}

viewplayer=function(playername, mygbgdf=gbgdf) {
  ### firstly select the bits we want
  minigamebygame = mygbgdf %>%
    filter(grepl(playername,tolower(gbgdf$player)) & season == currentseason)
  if (nrow(minigamebygame)==0) stop('Can\'t find that player, exiting\n')
  if (length(unique(minigamebygame[,'player']))>1) stop('not unique player\n')
  ### paste the minutes/goals/assists into a single cell
  ### or just plot it?
  plot(minigamebygame$teamgamenumber, minigamebygame$minute,ylim=c(0,90),pch=16,xlab='teamgamenumber',ylab='minutes',main=minigamebygame[1,'player'])
  points(minigamebygame$teamgamenumber,minigamebygame$deservedgoal*90/1.5,pch=15,col='blue')
  points(minigamebygame$teamgamenumber,minigamebygame$deservedassist*90/1.5,pch=15,col='green')
  ### overlay average, weighted by minutes played
  meangoal=with(minigamebygame,weighted.mean(90/minute * deservedgoal,minute))
  meanassist=with(minigamebygame,weighted.mean(90/minute * deservedassist,minute))
  abline(h=meangoal*90/1.5,col='blue')
  abline(h=meanassist*90/1.5,col='green')
  text(max(minigamebygame$teamgamenumber),meangoal*90/1.5,round(meangoal,2),col='blue',pos=3,cex=0.8)
  text(max(minigamebygame$teamgamenumber),meanassist*90/1.5,round(meanassist,2),col='green',pos=3,cex=0.8)
  legend(min(minigamebygame$teamgamenumber),90,c('minutes','goals','assists'),pch=c(16,15,15),col=c('black','blue','green'))
}

viewgeneral=function(pos=NULL,mincutoff=NULL,pointcutoff=NULL,teamlist=NULL,todisplay=20, mysummarydf=summarydf) {
  minimodel=mysummarydf
  if (!is.null(pos)) minimodel = minimodel %>% filter(mainpos==pos)
  if (!is.null(mincutoff)) minimodel = minimodel %>% filter(minute>max(minute)*mincutoff)
  if (!is.null(pointcutoff)) minimodel = minimodel %>% filter(deservedpointpg>pointcutoff)
  if (!is.null(teamlist)) minimodel = minimodel %>% filter(team %in% teamlist)
  minimodel = minimodel %>% arrange(-deservedpointpg) %>% select(team,player,minute,deservedgoalpg,deservedassistpg,deservedpointpg)
  print(minimodel[1:todisplay,])
}
