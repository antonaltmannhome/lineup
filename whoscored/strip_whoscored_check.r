### combine the team spreadsheets into one nice dataframe
source('c:/research/general_startup.r')
source('c:/research/ahkfunct.r')

SQLPATH='c:/research/sqlscript/'
USERPATH='c:/research/lineup/'

teamdf=read.csv(paste(USERPATH,'whoscored/data/teampage.csv',sep=''))

mycountry='spain'

filetype=c('summary','shotzone','shotacc','keypass')

### get hold of lionks locations too
linkdf=read.csv(paste(USERPATH,'whoscored/location.csv',sep=''))
mydeselectpoint=linkdf[linkdf$type=='deselect-all',c('xloc','yloc')] # don't want to type that too often
iserror=NULL


allpage=expand.grid(teamdf$team[teamdf$country==mycountry],filetype,stringsAsFactors=F)
names(allpage)=c('team','type')
allpage=allpage[order(allpage[,'team']),]

if (is.null(iserror)) allpage$getit=1
if (!is.null(iserror)) allpage$getit=iserror

ahkfile=paste(SQLPATH,'temp.ahk',sep='')
if (file.exists(ahkfile)) file.remove(ahkfile)
insertselect()

myteamtoget=unique(allpage$team[allpage$getit==1])
for (ti in 1:length(myteamtoget)) {

	myfiletype=with(allpage,type[team==myteamtoget[ti] & getit==1])
	myfilename=paste('whoscored/data/',myteamtoget[ti],'_',myfiletype,'_',numericdate(),'.txt',sep='')
	for (k in 1:length(myfilename)) {
		dum=paste(USERPATH,myfilename[k],sep='')
		if (file.exists(dum)) file.remove(dum)
	}
	
	### summary:
	mypage=paste('http://www.whoscored.com/Teams/',with(teamdf[teamdf$country==mycountry,],pageno[match(myteamtoget[ti],team)]),sep='')
	gotowebadd(mypage,waittime=20)
	if (any(myfiletype=='summary')) {
		### now save to notepad++
		selectalltonotepad(myfilename[myfiletype=='summary'], deselectpoint=mydeselectpoint)
	}
		
	### good, now get hold of the shots in detail
	comm=NULL
	comm[1]='sleep 500'
	comm[2]='WinSelect("Chrome")'
	comm[3]='sleep 500'
	comm[4]=paste('click',with(linkdf[linkdf$type=='detailed',],paste(xloc,yloc,sep=',')))
	comm[5]='sleep 8000'
	comm[6]=paste('click ',with(linkdf[linkdf$type=='detailed-accum',],paste(xloc,yloc,sep=',')))
	comm[7]='sleep 1000'
	comm[8]=paste('click ',with(linkdf[linkdf$type=='detailed-accum-total',],paste(xloc,yloc,sep=',')))
	comm[9]='sleep 2000'
	write(file=ahkfile,comm,append=T)
	if (any(myfiletype=='shotzone')) {
		selectalltonotepad(myfilename[myfiletype=='shotzone'], deselectpoint=mydeselectpoint)
	}
	
	### now go from there to shot accuracy
	comm=NULL
	comm[1]='sleep 500'
	comm[2]='WinSelect("Chrome")'
	comm[3]='sleep 1000'
	comm[4]=paste('click ',with(linkdf[linkdf$type=='detailed-subcat',],paste(xloc,yloc,sep=',')))
	comm[5]='sleep 2000'
	comm[6]=paste('click ',with(linkdf[linkdf$type=='detailed-subcat-accuracy',],paste(xloc,yloc,sep=',')))
	comm[7]='sleep 2000'
	write(file=ahkfile,comm,append=T)
	if (any(myfiletype=='shotacc')) {
		selectalltonotepad(myfilename[myfiletype=='shotacc'], deselectpoint=mydeselectpoint)
	}
	### passes
	comm=NULL
	comm[1]='sleep 500'
	comm[2]='WinSelect("Chrome")'
	comm[3]='sleep 500'
	comm[4]=paste('click ',with(linkdf[linkdf$type=='detailed-category',],paste(xloc,yloc,sep=',')))
	comm[5]='sleep 1000'
	comm[6]=paste('click ',with(linkdf[linkdf$type=='detailed-category-keypass',],paste(xloc,yloc,sep=',')))
	comm[7]='sleep 1000'
	write(file=ahkfile,comm,append=T)
	if (any(myfiletype=='keypass')) {
		selectalltonotepad(myfilename[myfiletype=='keypass'], deselectpoint=mydeselectpoint)
	}
}

insertabort()
runscript()
Sys.sleep(1)

checkpage=function(myteam, myfiletype, mycolhead) {
	if (myfiletype=='summary') {
		truecolhead="R\t\tName\tCM\tKG\tApps\tMins\tGoals\tAssists\tYel\tRed\tSpG\tPS%\tAerialsWon\tMotM\tRating"
	}
	if (myfiletype=='shotzone') {
		truecolhead="R\t\tName\tCM\tKG\tApps\tMins\tTotal\tOutOfBox\tSixYardBox\tPenaltyArea\tRating"
	}
	if (myfiletype=='shotacc') {
		truecolhead="R\t\tName\tCM\tKG\tApps\tMins\tTotal\tOffTarget\tOnPost\tOnTarget\tBlocked\tRating"
	}
	if (myfiletype=='keypass') {
		truecolhead="R\t\tName\tCM\tKG\tApps\tMins\tTotal\tLong\tShort\tRating"
	}
	if (mycolhead==truecolhead) return(0)
	if (!mycolhead==truecolhead) {
		cat('Have an error with',myteam,', file:',myfiletype,'\n')
		return(1)
	}
}

teamlist=teamdf$team[teamdf$country==mycountry]
### now combine into a single spreadsheet
fulldf=NULL
iserror=rep(0,length(teamlist)*length(filetype))

for (ti in 1:length(teamlist)) {
	cat('About to strip',teamlist[ti],'data...\n')
	filename=paste('whoscored/data/',teamlist[ti],'_',filetype,'_',numericdate(),'.txt',sep='')
	for (j in 1:length(filename)) {
		b=scan(paste(USERPATH,filename[j],sep=''),'',sep='\n',quiet=T)
		sax=grep('^[0-9]{2}, [A-Z\\(\\)]+',b)
		### first check we've got the correct file - it can go wrong at times:
		iserror[which(paste(allpage$team,allpage$type)==paste(teamlist[ti],filetype[j]))]=checkpage(teamlist[ti],filetype[j],b[sax[1]-3])
		tabdat1=t(sapply(b[sax],function(x) strsplit(x,split='\t')[[1]]))
		rownames(tabdat1)=b[sax-1]
		### let's get the first listed position and use that
		dum=as.character(gsub('(^.+, )([^\\(]+)(.*$)','\\2',tabdat1[,1]))
		### what do we want to keep? depends on the file
		if (filetype[j]=='summary') {
			tabdat=cbind(rep(teamlist[ti],dim(tabdat1)[1]),rownames(tabdat1),dum)
			colnames(tabdat)=c('team','player','mainpos')
		}
		if (filetype[j]=='summary') {
			keep=c(5,6,7)
			colname=c('minute','goal','assist')
		}
		if (filetype[j]=='shotzone') {
			keep=7:9
			colname=c('shotoob','shot6yd','shotib')
		}
		if (filetype[j]=='shotacc') {
			keep=7:10
			colname=c('offt','bar','ont','block')
		}
		if (filetype[j]=='keypass') {
			keep=c(7,8)
			colname=c('longkp','shortkp')
		}
		tabdat2=tabdat1[,keep]
		colnames(tabdat2)=colname
		tabdat=cbind(tabdat,tabdat2)
	}
	tabdat[tabdat=='-']=0
	fulldf[[ti]]=tabdat
}

fulldf=do.call(rbind,fulldf)
write.csv(file=paste(USERPATH,'whoscored/data/combined_',numericdate(),'.csv',sep=''),fulldf,row.names=F)
