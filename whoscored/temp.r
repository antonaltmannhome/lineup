
processpage=function(myteam,filedir,ishistorical=F) {
	cat('About to strip',myteam,'data...\n')
	filename=paste('whoscored/data/',filedir,'/',myteam,'_',allfiletype,'.txt',sep='')
	mywsteam=teamdf[which(teamdf$team==myteam),'wsteam']
	for (j in 1:length(filename)) {
		b=scan(paste(USERPATH,filename[j],sep=''),'',sep='\n',quiet=T,encoding='UTF-8')
		### have we got the correct team firstly?
		if (!ishistorical) pageteam=tolower(gsub(' Top Players','',b[grep('Top Players',b)]))
		if (ishistorical) pageteam=tolower(gsub(' Squad Archive','',b[grep('Squad Archive',b)]))
		if (pageteam!=mywsteam) {
			cat('Have picked up data for',pageteam,'when we want data for',mywsteam,', recording error\n')
		}
		if (pageteam==mywsteam) {
			if (ishistorical) {
				### for some reason is puts the team name by the players stats, get rid
				sax=grep('^[A-z ]+, [0-9]{2}, [A-Z\\(\\)]+',b)
				b[sax]=gsub('^[A-z ]+, ','',b[sax])
			}
			sax=grep('^[0-9]{2}, [A-Z\\(\\)]+',b)
			tabdat1=t(sapply(b[sax],function(x) gsub(' ','',strsplit(x,split='\t')[[1]])))
			rownames(tabdat1)=b[sax-1]
			### let's get the first listed position and use that
			dum=as.character(gsub('(^.+, *)([^\\(]+)(.*$)','\\2',tabdat1[,1]))
			### what do we want to keep? depends on the file
			if (allfiletype[j]=='summary') {
				tabdat=cbind(rep(myteam,dim(tabdat1)[1]),rownames(tabdat1),dum)
				colnames(tabdat)=c('team','player','mainpos')
			}
			if (allfiletype[j]=='summary') {
				keep=c(5,6,7)
				colname=c('minute','goal','assist')
			}
			if (allfiletype[j]=='shotzone') {
				keep=7:9
				colname=c('shotoob','shot6yd','shotib')
			}
			if (allfiletype[j]=='shotsit') {
				keep=7:10
				colname=c('openplay','counter','setpiece','penaltytaken')
			}
			if (allfiletype[j]=='shotacc') {
				keep=7:10
				colname=c('offt','bar','ont','block')
			}
			if (allfiletype[j]=='goalsit') {
				keep=10
				colname=c('penaltyscored')
			}
			if (allfiletype[j]=='keypass') {
				keep=c(7,8)
				colname=c('longkp','shortkp')
			}
			tabdat2=tabdat1[,keep,drop=F]
			colnames(tabdat2)=colname
			tabdat2[tabdat2=='-']=0
			if (!all(apply(tabdat2,2,function(x) all(round(as.numeric(x))==as.numeric(x))))) {
				stop('Seem to have average, not totals for',allfiletype[j],'\n')
			}
			tabdat=cbind(tabdat,tabdat2)
		}
	}
	tabdat=as.data.frame(tabdat)
	### make numeric columns numeric
	numcol=setdiff(names(tabdat),c('team','player','mainpos'))
	tabdat[,numcol]=apply(tabdat[,numcol],2,function(x) as.numeric(x))
	rownames(tabdat)=NULL
	return(as.data.frame(tabdat))
}
