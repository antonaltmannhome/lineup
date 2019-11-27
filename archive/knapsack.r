### let's try the knapsack approach to ff

source('c:/research/lineup/whoscored/whoscored_startup.r')
suppressWarnings(library('Rglpk'))
playervalue=getplayervalue()

### so let's say we've got e.g 30 million to spend on defenders, let's get best set
sax=with(playervalue,which(ffposition=='d'))

subplayervalue=playervalue[sax,]
nsubplayer=nrow(subplayervalue)

obj=subplayervalue$epoint
#var.types <- c(rep("B", nsubplayer),'C')
mat10=matrix(0,nrow=nsubplayer,ncol=nsubplayer)
diag(mat10)=1
conmat=rbind(
	rep(1,nsubplayer),
	subplayervalue$price
	)
direction=c('<=','<=')
rhs=c(5,30)
Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,max=T)

#var.types <- c(rep("B", nsubplayer),'C')
obj=subplayervalue$epoint
var.types <- c(rep("B", nsubplayer),'I','C')
mat10=matrix(0,nrow=nsubplayer,ncol=nsubplayer)
diag(mat10)=1
conmat=rbind(
	mat10,
	rep(1,nsubplayer),
	subplayervalue$price
	)
direction=c(rep('<=',nsubplayer),'==','<=')
rhs=c(rep(1,nsubplayer),5,30)
sol=Rglpk_solve_LP(obj=obj,mat=conmat,dir=direction,rhs=rhs,types=var.types,max=T)

### ok, let's try doing defenders and midfielders together

### so let's say we've got e.g 30 million to spend on defenders, let's get best set
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

### so as expected, picks out injured or unselected players...
### but it's a start - intesting results
### i think we should restrict to 80 mill, and only take 4 def/mids, assume you can always drop one

subplayervalue[which(sol$solution==1),]

### so now work out who you would actually field, who you would captain etc, then start shuffling players out based on that
