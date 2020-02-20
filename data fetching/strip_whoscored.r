source(paste(USERPATH,'data fetching/whoscored_funct.r',sep=''))

teamdf=read.csv(paste(DATAPATH,'/teampage.csv',sep=''))
allfiletype=c('summary','shotzone','shotsit','shotacc','goalsit','keypass')

browserToUse='Firefox'
datetouse=numericdate()

### we often have net drop outs, so good to be able to resume in this case

DIRTOUSE=paste(TEMPPATH,datetouse,sep='')
if (!file.exists(DIRTOUSE)) dir.create(DIRTOUSE)
ahkfile=paste(TEMPPATH,'temp.ahk',sep='')

teamwaittime=6 # how long it takes (in seconds) to load initial page for team
subcatwaittime=3000 # how long it waits (in milliseconds) for data to refresh on page when we click subcategories
defaultnumgam = 8
defaultnumtournament = 4

### now let user decide which teams they want

beforeseason=FALSE
allteam=teamdf$team[teamdf$active==1]

myteamtoget=select.list(allteam,multiple=T,title='select teams to update')

for (ti in 1:length(myteamtoget)) {
	Sys.sleep(sample(1:4,1))
	gotocurrentpage(myteamtoget[ti], thiscomputer = thiscomputer)
	stripcurrentpage(myteamtoget[ti], DIRTOUSE, ahkfile, ishistoric=F,beforeseason)
}

Sys.sleep(1)

### then run the checks and balances
iserrorarr=array(NA,dim=c(length(myteamtoget),length(allfiletype)))
for (ti in 1:length(myteamtoget)) {
	iserrorarr[ti,]=checkteam(myteamtoget[ti],DIRTOUSE,ishistoric=F,beforeseason)
}

problemteam = myteamtoget[apply(iserrorarr,1,function(x) any(x==1))]

if (length(problemteam) > 0) {
	print('You need to need to rerun for the following teams:')
	problemindex = match(problemteam, myteamtoget)
	print(paste(problemteam, problemindex))
}

if (length(problemteam) == 0) {
	print('data all looks good!')
	message('If you are happy for me to proceed, type \'y\' and I will produce the summary file and zip the raw html')
	dum = askcond(F, T)
	if (dum == 'y') {
		combineteamfile(DIRTOUSE, datetouse)

		# final task, zip the whoscored files into current year directory, then delete
		setwd(TEMPPATH)
		currentYear = substr(datetouse, 1, 4)
		targetZipFile = paste0('d:/whoscored_data/whoscored', currentYear, '.7z')
		myCommand = paste0('7z a ', targetZipFile, ' ', datetouse, '\\*.txt')
		system(myCommand)

		setwd(USERPATH)
	}
}
