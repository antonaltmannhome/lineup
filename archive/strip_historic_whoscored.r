### want to have ability to cycle through a set of pages and obtain summary stats for the players on them

source(paste(USERPATH,'whoscored_funct.r',sep=''))
browserToUse='Firefox'
allfiletype=c('summary','shotzone','shotsit','shotacc','goalsit','keypass')
teamdf=read.csv(paste(DATAPATH,'/teampage.csv',sep=''))

myseason=1516
ahkfile=paste(AHKPATH,'temp.ahk',sep='')
teamwaittime=6 # how long it takes (in seconds) to load initial page for team
subcatwaittime=3000 # how long it waits (in milliseconds) for data to refresh on page when we click subcategories
locationinfo=NULL # fingers crossed, it's not needed for historic

pagedf=read.csv(paste(DATAPATH,myseason,'/summary_coverpage.csv',sep=''),sep='~',as.is=T)
for (ti in 1:nrow(pagedf)) {
	### have we already got it?
	myallfile=paste(DATAPATH,myseason'/',pagedf[ti,'team'],'_',allfiletype,'.txt',sep='')
	if (!all(file.exists(myallfile))) {
		### firstly navigate to the page of interest
		initfile()
		gotowebadd(pagedf[ti,'page'],browserchoice=browserToUse)
	
		insertabort()
		runscript()
	
		stripcurrentpage(pagedf[ti,'team'], pagedf[ti,'season'], locationinfo, ahkfile, ishistoric=T)
	}	
}

### then run the checks and balances
iserrorarr=array(NA,dim=c(nrow(pagedf),length(allfiletype)))
for (ti in 1:nrow(pagedf)) {
	iserrorarr[ti,]=checkteam(pagedf[ti,'team'],myseason,ishistoric=T)
}
print('Here is the array of errors:')
print(iserrorarr)

### now combine into a single spreadsheet
modeldf=NULL

for (ti in 1:nrow(pagedf)) {
	summarydf=processpage(pagedf[ti,'team'],myseason,ishistoric=T)
	modeldf[[ti]]=processdeserved(summarydf)
}

modeldf=do.call(rbind,modeldf)
modeldf=as.data.frame(modeldf)

write.csv(file=paste(DATAPATH,myseason,'/model.csv',sep=''),modeldf,row.names=F)
