### want to turn whoscored summaries into match by match

### first step is to set up data frame with line for each player and date intervals
source('c:/research/general_startup.r')
source('c:/research/ahkfunct.r')

SQLPATH='c:/research/sqlscript/'
USERPATH='c:/research/lineup/'

DATAPATH='d:/whoscored_data/'

### get hold of current prem teams
teamdf=read.csv(paste(DATAPATH,'teampage_england.csv',sep=''))

myteam=unique(teamdf$team)

### so which files are relevant?

dum=list.files(DATAPATH)

isvalid=grep('combined_[0-9]{8}',dum)
### don't need any of the older stuff though so eliminate
isvaliddate=as.numeric(gsub('(^[^_]+_)([0-9]{8})(.csv)','\\2',dum[isvalid]))

keep=which(isvaliddate>20160427)
isvalid=isvalid[keep]
isvaliddate=isvaliddate[keep]

dumdf=NULL
for (j in 1:length(isvalid)) {
	dumdf[[j]]=read.csv(paste(DATAPATH,dum[isvalid[j]],sep=''))
	dumdf[[j]]$date=isvaliddate[j]
}

bigdf=do.call(rbind,dumdf)

bigdf$player=tolower(iconv(bigdf$player,fr='UTF-8',to='ASCII//TRANSLIT'))
