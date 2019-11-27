### combine the team spreadsheets into one nice dataframe
source('c:/research/general_startup.r')
source('c:/research/ahkfunct.r')

SQLPATH='c:/research/sqlscript/'
USERPATH='c:/research/lineup/'
setwd(USERPATH)
source('whoscored/strip_whoscored_funct.r')

teamdf=read.csv(paste(USERPATH,'whoscored/data/teampage.csv',sep=''))

browserToUse='Firefox'

mycountry='england'
datetouse=numericdate()
#datetouse=20160423

allfiletype=c('summary','shotzone','shotsit','shotacc','goalsit','keypass')

### and get hold of nay one off adjustmetns to y axis that might be necssary this week
# yaxisadjustment=read.csv(paste(USERPATH,'whoscored/data/team_yaxis_adjustment.csv',sep=''))

### we often have net drop outs, so good to be able to resume in this case

allpage=expand.grid(teamdf$team[teamdf$country==mycountry],allfiletype,stringsAsFactors=F)
names(allpage)=c('team','type')
allpage=allpage[order(allpage[,'team']),]

DIRTOUSE=paste('whoscored/data/',datetouse,sep='')
if (!file.exists(DIRTOUSE)) dir.create(DIRTOUSE)
allpage$filename=with(allpage,paste(DIRTOUSE,'/',team,'_',type,'_',datetouse,'.txt',sep=''))
### so which ones have we already got?
allpage$got=rep(0,nrow(allpage))
### is there anything useful about doing this? commenting out for now
#for (j in 1:nrow(allpage)) allpage$got[j]=file.exists(paste(USERPATH,allpage$filename[j],sep=''))

teamwaittime=6 # how long it takes (in seconds) to load initial page for team
subcatwaittime=3000 # how long it waits (in milliseconds) for data to refresh on page when we click subcategories

ahkfile=paste(SQLPATH,'temp.ahk',sep='')
initfile=function() {
	if (file.exists(ahkfile)) file.remove(ahkfile)
	insertselect()
}

### now let user decide which teams they want

myteamtoget=select.list(unique(allpage$team),multiple=T,title='select teams to update')

for (ti in 1:length(myteamtoget)) {
	
	myfiletype=with(allpage,type[team==myteamtoget[ti] & got==0])
	myfilename=with(allpage,filename[team==myteamtoget[ti] & got==0])
	### if you already have any of the files, it is almost certain you want to replace them so let's get rid of them
	for (j in 1:length(myfilename)) if (file.exists(paste(USERPATH,myfilename[j],sep=''))) file.remove(paste(USERPATH,myfilename[j],sep=''))

	### get hold of links locations  - they need reseting each time
	linkdf=read.csv(paste(USERPATH,'whoscored/location_',browserToUse,'.csv',sep=''))

	initfile()
	### firstly navigate to main page of team
	mypage=paste('http://www.whoscored.com/Teams/',with(teamdf[teamdf$country==mycountry,],pageno[match(myteamtoget[ti],team)]),sep='')
	gotowebadd(mypage,waittime=teamwaittime,browserchoice=browserToUse)
		
	mydeselectpoint=linkdf[linkdf$type=='deselect-all',c('xloc','ylocdown')] # don't want to type that too often

	### now save to notepad++
	selectalltonotepad(myfilename[myfiletype=='summary'], deselectpoint=mydeselectpoint)

	insertabort()
	runscript()

	### now scan that in and check how many fixtures lines there are
	b=scan(file=paste(USERPATH,myfilename[myfiletype=='summary'],sep=''),what='',quiet=T,sep='\n')
	
	curwsteam=teamdf$wsteam[teamdf$team==myteamtoget[ti]]
	fixtline=grep(paste(curwsteam,'fixtures'),tolower(b))
	squadline=grep(paste(curwsteam,'squad'),tolower(b))
	
	combline=paste(b[(fixtline+1):(squadline-1)],collapse='')
	combline=gsub('\\\t',' ',combline)
	### number of dates in that is a good guide to how many fixtures there are
	dateinfo=gregexpr('[0-9]{2}-[0-9]{2}-[0-9]{4}',combline)
	numgam=length(dateinfo[[1]])
	### now adjust depending on number of games

	if (!numgam %in% c(3:7)) stop('need to add more lines for numgam\n')
	linkdf$yloc=linkdf$ylocdown

	### however, the 'category' submenu will appear up or down depending on numgam
	if (numgam>3) linkdf$yloc[linkdf$type=='detailed-category-keypass']=linkdf$ylocup[linkdf$type=='detailed-category-keypass']

	numgamlist=c(3,4,5,6,7)
	adjlist=c(0,23,46,64,88)

	linkdf$yloc=linkdf$yloc + adjlist[which(numgamlist==numgam)]
	
	initfile()

	### good, now get hold of the shots in detail
	if (any(myfiletype=='shotzone')) {
		comm=NULL
		comm[1]='sleep 500'
		comm[2]=paste('WinSelect("',browserToUse,'")',sep='')
		comm[3]='sleep 500'
		comm[4]=paste('click',with(linkdf[linkdf$type=='detailed',],paste(xloc,yloc,sep=',')),';click on detailed')
		comm[5]=paste('sleep',subcatwaittime)
		comm[6]=paste('click ',with(linkdf[linkdf$type=='detailed-accum',],paste(xloc,yloc,sep=',')),';click on detailed/accumulation')
		comm[7]='sleep 1000'
		comm[8]=paste('click ',with(linkdf[linkdf$type=='detailed-accum-total',],paste(xloc,yloc,sep=',')),';click on detailed/accumulation/total')
		comm[9]=paste('sleep',subcatwaittime)
		write(file=ahkfile,comm,append=T)
		if (any(myfiletype=='shotzone')) {
			selectalltonotepad(myfilename[myfiletype=='shotzone'], deselectpoint=mydeselectpoint)
		}
	}
		
	if (any(myfiletype=='shotsit')) {
		### now go from there to shot situations
		comm=NULL
		comm[1]='sleep 500'
		comm[2]=paste('WinSelect("',browserToUse,'")',sep='')
		comm[3]='sleep 1000'
		comm[4]=paste('click ',with(linkdf[linkdf$type=='detailed-subcat',],paste(xloc,yloc,sep=',')),';click on detailed/subcategory')
		comm[5]='sleep 1000'
		comm[6]=paste('click ',with(linkdf[linkdf$type=='detailed-subcat-situations',],paste(xloc,yloc,sep=',')),';click on detailed/subcategory/situations')
		comm[7]=paste('sleep',subcatwaittime)
		write(file=ahkfile,comm,append=T)
		if (any(myfiletype=='shotsit')) {
			selectalltonotepad(myfilename[myfiletype=='shotsit'], deselectpoint=mydeselectpoint)
		}
	}

	if (any(myfiletype=='shotzone')) {
		### now go from there to shot accuracy
		comm=NULL
		comm[1]='sleep 500'
		comm[2]=paste('WinSelect("',browserToUse,'")',sep='')
		comm[3]='sleep 1000'
		comm[4]=paste('click ',with(linkdf[linkdf$type=='detailed-subcat',],paste(xloc,yloc,sep=',')),';click on detailed/subcategory')
		comm[5]='sleep 1000'
		comm[6]=paste('click ',with(linkdf[linkdf$type=='detailed-subcat-accuracy',],paste(xloc,yloc,sep=',')),';click on detailed/subcategory/accuracy')
		comm[7]=paste('sleep',subcatwaittime)
		write(file=ahkfile,comm,append=T)
		if (any(myfiletype=='shotacc')) {
			selectalltonotepad(myfilename[myfiletype=='shotacc'], deselectpoint=mydeselectpoint)
		}
	}
	
	if (any(myfiletype=='keypass')) {
		### passes
		comm=NULL
		comm[1]='sleep 500'
		comm[2]=paste('WinSelect("',browserToUse,'")',sep='')
		comm[3]='sleep 500'
		comm[4]=paste('click ',with(linkdf[linkdf$type=='detailed-category',],paste(xloc,yloc,sep=',')),';click on detailed/category')
		comm[5]='sleep 1000'
		comm[6]=paste('click ',with(linkdf[linkdf$type=='detailed-category-keypass',],paste(xloc,yloc,sep=',')),';click on detailed/category/keypass')
		comm[7]=paste('sleep',subcatwaittime)
		write(file=ahkfile,comm,append=T)
		if (any(myfiletype=='keypass')) {
			selectalltonotepad(myfilename[myfiletype=='keypass'], deselectpoint=mydeselectpoint)
		}
	}
	
	insertabort()
	runscript()
}

Sys.sleep(1)


wsteamtoget=teamdf$wsteam[match(myteamtoget,teamdf$team)]
### now combine into a single spreadsheet
fulldf=NULL
iserror=rep(0,length(myteamtoget)*length(filetype))

for (ti in 1:length(myteamtoget)) {
	cat('About to strip',myteamtoget[ti],'data...\n')
	filename=paste('whoscored/data/',datetouse,'/',myteamtoget[ti],'_',filetype,'_',datetouse,'.txt',sep='')
	for (j in 1:length(filename)) {
		apmatch=which(paste(allpage$team,allpage$type)==paste(myteamtoget[ti],filetype[j]))
		b=scan(paste(USERPATH,filename[j],sep=''),'',sep='\n',quiet=T,encoding='UTF-8')
		### have we got the correct team firstly?
		pageteam=tolower(gsub(' Top Players','',b[grep('Top Players',b)]))
		if (pageteam!=wsteamtoget[ti]) {
			cat('Have picked up data for',pageteam,'when we want data for',wsteamtoget[ti],', recording error\n')
			iserror[apmatch]=1
		}
		if (pageteam==wsteamtoget[ti]) {
			sax=grep('^[0-9]{2}, [A-Z\\(\\)]+',b)
			### first check we've got the correct file - it can go wrong at times:
			iserror[apmatch]=checkpage(myteamtoget[ti],filetype[j],gsub(' ','',b[sax[1]-3]))
			tabdat1=t(sapply(b[sax],function(x) gsub(' ','',strsplit(x,split='\t')[[1]])))
			rownames(tabdat1)=b[sax-1]
			### let's get the first listed position and use that
			dum=as.character(gsub('(^.+, )([^\\(]+)(.*$)','\\2',tabdat1[,1]))
			### what do we want to keep? depends on the file
			if (filetype[j]=='summary') {
				tabdat=cbind(rep(myteamtoget[ti],dim(tabdat1)[1]),rownames(tabdat1),dum)
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
			if (filetype[j]=='shotsit') {
				keep=7:10
				colname=c('openplay','counter','setpiece','penaltytaken')
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
			tabdat2[tabdat2=='-']=0
			if (!all(apply(tabdat2,2,function(x) all(round(as.numeric(x))==as.numeric(x))))) {
				cat('Seem to have average, not totals for',filetype[j],'\n')
				iserror[apmatch]=1
			}
			tabdat=cbind(tabdat,tabdat2)
		}
	}
	
	fulldf[[ti]]=tabdat
}

fulldf=do.call(rbind,fulldf)
fulldf=as.data.frame(fulldf)
### horrible but necessary
fulldf$player=iconv(iconv(fulldf[,'player'],fr='UTF-8',to='latin1'),fr='latin1',to='ASCII//TRANSLIT')

write.csv(file=paste(USERPATH,'whoscored/data/',datetouse,'/combined_',numericdate(),'.csv',sep=''),fulldf,row.names=F)
