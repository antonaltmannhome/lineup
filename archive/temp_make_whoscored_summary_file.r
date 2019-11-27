
### now combine into a single spreadsheet
#modeldf=NULL

for (ti in 1:length(myteamtoget)) {
	combineddf=processpage(myteamtoget[ti],datetouse)
	fileout=paste(DIRTOUSE,'/',myteamtoget[ti],'_combined.csv',sep='')
	write.csv(file=fileout,combineddf,row.names=FALSE)
}


### so how about this for a plan: get all the 

### get every single file, put it all into one huge data frame

DATAPATH = 'd:/whoscored_data/'
USERPATH='c:/research/lineup/'
currentseason=1617

source(paste(USERPATH,'ff_funct.r',sep=''))
alldate = getalldate()

### scan them all in
tempbigdf=NULL
for (di in 1:length(alldate)) {
	combinedteamfile=paste(DATAPATH,alldate[di],'/combined_data.csv',sep='')
	tempbigdf[[di]]=read.csv(combinedteamfile)
	tempbigdf[[di]]$filedate=alldate[di]
}
combineddata=do.call(rbind,tempbigdf)

### now we want to align date of actual matches with the filedate column
result=read.csv(paste(DATAPATH,'fixture_result/all_result.csv',sep=''))

combineddata$matchdate=NA
teamlist=unique(combineddata$team)
for (ti in 1:length(teamlist)) {
	sax=which(combineddata$team==teamlist[ti])
	currentteamfiledate=unique(combineddata$filedate[sax])
	resultdate=with(result,sort(date[ht==teamlist[ti] | at==teamlist[ti]]))
	filedateresultdate=resultdate[findInterval(currentteamfiledate,resultdate)]
	combineddata$matchdate[sax]=filedateresultdate[match(combineddata$filedate[sax],currentteamfiledate)]
}

### then, each quantity is just current minus previous
matchstatcol = setdiff(names(combineddata), c('team','player','mainpos','filedate','matchdate'))

gamebygame = combineddata
gamebygame = gamebygame %>%
				group_by(team,player) %>%
				arrange(matchdate) %>%
				mutate(minute = c(minute[1], diff(minute)),
						goal = c(goal[1], diff(goal)),
						assist = c(assist[1], diff(assist)),
						shotoob = c(shotoob[1], diff(shotoob)),
						shot6yd = c(shot6yd[1], diff(shot6yd)),
						shotib = c(shotib[1], diff(shotib)),
						openplay = c(openplay[1], diff(openplay)),
						counter = c(counter[1], diff(counter)),
						setpiece = c(setpiece[1], diff(setpiece)),
						penaltytaken = c(penaltytaken[1], diff(penaltytaken)),
						offt = c(offt[1], diff(offt)),
						bar = c(bar[1], diff(bar)),
						ont = c(ont[1], diff(ont)),
						block = c(block[1], diff(block)),
						penaltyscored = c(penaltyscored[1], diff(penaltyscored)),
						longkp = c(longkp[1], diff(longkp)),
						shortkp = c(shortkp[1], diff(shortkp)))

write.csv(file=paste(DATAPATH,'gamebygame_data.csv',sep=''),gamebygame,row.names=F)

						
	

### first find all unique teams in the set
	currentdir=paste(DATAPATH,alldate[di],sep='')
	currentfile=list.files(currentdir)
	currentsummaryfile=currentfile[grep('^[a-z]+_combined.csv',currentfile)]
	currentsummarylist=NULL
	for (k in 1:length(currentsummaryfile)) {
		currentsummarylist[[k]]=read.csv(paste(currentdir,currentsummaryfile[k],sep='/'),as.is=T)
	}
	currentsummary=do.call(rbind,currentsummarylist)
	
	tempbigdf[[di]] = read.csv(paste(DATAPATH,alldate[di],'/summarydf.csv',sep=''),as.is=T)
	### store the date
	tempbigdf[[di]] = alldate[di]
}

for (di in 1:length(alldate)) {
	currentdir=paste(DATAPATH,alldate[di],sep='')
	currentfile=list.files(currentdir)
	currentcombinedfile=currentfile[grep('^[a-z]+_combined.csv',currentfile)]
	if (length(currentcombinedfile)>0) for (j in 1:length(currentcombinedfile)) file.remove(paste(DATAPATH,alldate[di],'/',currentcombinedfile[j],sep=''))
}
