### let's develop our player selector thing


source('c:/research/lineup/ff_startup.r')
suppressWarnings(library('Rglpk'))
playervalue=getplayervalue()
playerfixtdf=getfixtureexpectedpoint()

### this is the naive way of doing it:
dsax=with(playervalue,which(ffposition=='d'))
msax=with(playervalue,which(ffposition=='m'))
fsax=with(playervalue,which(ffposition=='f'))

subplayervalue=rbind(playervalue[dsax,],playervalue[msax,],playervalue[fsax,])
nsubplayer=nrow(subplayervalue)

#var.types <- c(rep("B", nsubplayer),'C')
obj=subplayervalue$epoint
var.types <- c(rep("B", nsubplayer),'I','I','I','C')
mat10=matrix(0,nrow=nsubplayer,ncol=nsubplayer)
diag(mat10)=1
conmat=rbind(
mat10,
c(rep(1,length(dsax)),rep(0,length(msax)),rep(0,length(fsax))),
c(rep(0,length(dsax)),rep(1,length(msax)),rep(0,length(fsax))),
c(rep(0,length(dsax)),rep(0,length(msax)),rep(1,length(fsax))),
subplayervalue$price
)
direction=c(rep('<=',nsubplayer),rep('==',3),'<=')
rhs=c(rep(1,nsubplayer),5,5,3,90)
sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)

optplayer=subplayervalue[which(sol$solution==1),]

### however, what would be better is to find out how many points that actually scores with captain, subs etc

getactualpoint=function(optplayer) {
	subplayerfixtdf=playerfixtdf %>% filter(paste(team,player) %in% paste(optplayer$team,optplayer$player))
	### now choose the best 10 players week by week
	ungw=unique(subplayerfixtdf$gameweek)
	weekbyweekdf=NULL
	for (gi in 1:length(ungw)) {
		sax=with(subplayerfixtdf,which(gameweek == ungw[gi]))
		minidf=subplayerfixtdf[sax,]
		### have to put in order of position
		minidf = minidf %>% arrange(match(ffposition,c('d','m','f')))
		obj=minidf$expectedpoint
		nsubplayer=nrow(minidf)
		nplayertype=table(minidf$ffposition)
		nplayertype=nplayertype[match(names(nplayertype),c('d','m','f'))]
		var.types <- c(rep("B", nsubplayer),'I','I','C')
		mat10=matrix(0,nrow=nsubplayer,ncol=nsubplayer)
		diag(mat10)=1
		conmat=rbind(
		mat10,
		c(rep(1,nplayertype[1]),rep(0,nplayertype[2]),rep(0,nplayertype[3])),
		c(rep(0,nplayertype[1]),rep(0,nplayertype[2]),rep(1,nplayertype[3])),
		rep(1,nsubplayer)
		)
		direction=c(rep('<=',nsubplayer),c('>=','>='),'<=')
		rhs=c(rep(1,nsubplayer),3,1,10)
		sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)
		minidf$selected=sol$solution
		### then appoint our captain
		minidf$iscaptain=as.numeric(minidf$expectedpoint==max(minidf$expectedpoint))
		minidf$expectedpoint[which(minidf$selected==0)]=0
		minidf$expectedpoint[which(minidf$iscaptain==1)]=2*minidf$expectedpoint[which(minidf$iscaptain==1)]
		weekbyweekdf[[gi]]=minidf %>% select(player,selected,iscaptain,expectedpoint)
	}
	weekbyweekdf=do.call(rbind,weekbyweekdf)
	### what we really care about is total points, so let's calculate that
	totalpoint=sum(weekbyweekdf$expectedpoint)
	return(totalpoint)
}

### right, now can we beat this total by removing certain players?
