
getalldate=function(startseason='1617') {
	### often want to get all match dates for current season
	allfile=list.files(DATAPATH)
	alldateever=as.numeric(allfile[grep('^[0-9]{8}$',allfile)])
	startcutoff=as.numeric(paste('20',substr(startseason,1,2),'0801',sep=''))
	sax=which(alldateever>startcutoff)
	alldate=alldateever[sax]
	return(alldate)
}

getpreviousdate=function(datetouse) {
	### sometimes useful to know what the 
	alldate=getalldate()
	keep=which(alldate<datetouse)
	if (length(keep)==0) previousdate=NULL
	if (length(keep)>0) previousdate=max(alldate[keep])
	return(previousdate)
}

getmostrecentdir=function(mysource,stepback=0) {
	if (mysource=='whoscored') {
		dum=list.files(DATAPATH)
		dum=dum[grep('^[0-9]{8}$',dum)]
		toreturn=as.numeric(max(dum))
	}
	if (mysource=='spreadex') {
		dum=list.files(paste(DATAPATH,'spreadex_saved',sep=''))
		dum=dum[grep('^spreadex',dum)]
		datedum=as.numeric(gsub('(spreadex_)([0-9]{8})(.dat)','\\2',dum))
		toreturn=dum[which.max(datedum)]
	}
	if (mysource=='teamability') {
		dum=list.files(paste(DATAPATH,'spreadex_saved',sep=''))
		teamabilitydum=dum[grep('^teamability',dum)]
		homeeffectdum=dum[grep('^homeeffect',dum)]
		datedum=as.numeric(gsub('(teamability_)([0-9]{8})(.csv)','\\2',teamabilitydum))
		toreturn=list(teamability=teamabilitydum[which.max(datedum)],homeeffect=homeeffectdum[which.max(datedum)])
	}
	return(toreturn)
}

cleanteam=function(myteam,mysource) {
	abbndf=read.csv(paste(DATAPATH,'team_abbn.csv',sep=''))
	abbndf=abbndf[abbndf$source==mysource,]
	sax=which(myteam %in% abbndf$wrongname)
	correctteam=myteam
	correctteam[sax]=abbndf$correctname[match(myteam[sax],abbndf$wrongname)]
	return(correctteam)
}

matchplayer=function(myplayer,mysource) {
	abbndf=read.csv(paste(DATAPATH,'player_abbn.csv',sep=''))
	abbndf=abbndf[abbndf$source==mysource,]
	sax=which(myplayer %in% abbndf$wrongname)
	correctplayer=myplayer
	correctplayer[sax]=abbndf$correctname[match(myplayer[sax],abbndf$wrongname)]
	return(correctplayer)
}
