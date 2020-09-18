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
defaultnumgam = 5
defaultnumtournament = 2

### now let user decide which teams they want

beforeseason=FALSE
allteam=teamdf$team[teamdf$active==1]

myteamtoget=select.list(allteam,multiple=T,title='select teams to update')

for (ti in 1:length(myteamtoget)) {
	Sys.sleep(sample(1:4,1))
  isFirstTeam = (ti == 1) # atom cocks up the very first save, this should help
	gotocurrentpage(myteamtoget[ti], thiscomputer = thiscomputer)
	stripcurrentpage(myteamtoget[ti], DIRTOUSE, ahkfile, ishistoric=F,beforeseason,isFirstTeam = isFirstTeam)
}

Sys.sleep(1)

### then run the checks and balances
iserrorarr=array(NA,dim=c(length(myteamtoget),length(allfiletype)))
teamStatList = vector('list', length(myteamtoget))
for (ti in 1:length(myteamtoget)) {
  dum = ProcessPage(myteamtoget[ti], DIRTOUSE)
  teamStatList[[ti]] = dum$myTeamPlayerDF
	iserrorarr[ti,]=dum$iserror
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
	  combinedteamsummary=do.call(rbind, teamStatList)
	  fileout=paste0(DATAPATH, 'summarised_whoscored_data/combined_data_', datetouse, '.csv')
	  write.csv(file=fileout, combinedteamsummary, row.names=FALSE)
	  cat('Have created',fileout,'...\n')
	  
		# final task, zip the whoscored files into current year directory, then delete
		setwd(TEMPPATH)
		currentYear = substr(datetouse, 1, 4)
		targetZipFile = paste0('d:/whoscored_data/whoscored', currentYear, '.7z')
		myCommand = paste0('7z a ', targetZipFile, ' ', datetouse, '\\*.txt')
		system(myCommand)

		setwd(USERPATH)
	}
}
