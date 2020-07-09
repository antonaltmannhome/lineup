### several massive functions that will be used when combining all the different data sources together, shove em all in here

if (F) {
	ignorematch=function(datehteam.tm) {
		# sometimes data is dodgy for a game, just ignore for now
		sqlUpdate('game','donotuse','datehteam',1,datehteam.tm)
		print('This is what the line for that game now looks like:')
		a=sqlQuery(odbcConnection,paste('select * from game where datehteam="',datehteam.tm,'"',sep=''),as.is=T)
		print(a)
	}
}

makedaylist=function(seastodo) {
	### function that creates full list of days, useful for checking which days we should look at for possible matches
	dmonth1=c(31,30,31,30,31)
	dmonth2=c(31,28,31,30,31,30,31)
	dum=1:dmonth1[1]
	for (j in 2:5) dum=c(dum,1:dmonth1[j])
	dum[nchar(as.character(dum))==1]=paste('0',as.character(dum[nchar(as.character(dum))==1]),sep='')
	fulldaylist1=paste(paste('20',substr(as.character(seastodo),1,2),sep=''),rep(c('08','09','10','11','12'),dmonth1),dum,sep='/')
	dum=1:dmonth2[1]
	for (j in 2:7) dum=c(dum,1:dmonth2[j])
	dum[nchar(as.character(dum))==1]=paste('0',as.character(dum[nchar(as.character(dum))==1]),sep='')
	fulldaylist2=paste(paste('20',substr(as.character(seastodo),3,4),sep=''),rep(c('01','02','03','04','05','06','07'),dmonth2),dum,sep='/')
	fulldaylist=c(fulldaylist1,fulldaylist2)
	fulldatelist=gsub('/','',fulldaylist)
	## but of course we've no need for anything after today
	keep=which(fulldatelist<numericdate())
	fulldatelist=fulldatelist[keep]
	fulldaylist=fulldaylist[keep]
	return(list(fulldatelist=fulldatelist,fulldaylist=fulldaylist))
}

if (FALSE) {
cleanteam=function(tname) {
	teamabbn=read.csv(paste(USERPATH,'team_abbn.dat',sep=''),sep=':',head=F,as.is=T)
	while(!all(tname %in% teamabbn$V1)) {
		sax=which(!tname %in% teamabbn$V1)
		cat('Do not have an entry for these teams in team_abbn.dat, please add them then press return\n')
		print(tname[sax])
		dum=askcond(F,T)
		teamabbn=read.csv(paste(USERPATH,'team_abbn.dat',sep=''),sep=':',head=F,as.is=T)
	}
	return(teamabbn$V2[match(tname,teamabbn$V1)])
}	
}

processnewplayer=function(playername,playerid) {
	
	cat('About to get player info for',playername,'\n')
	fileout=paste(USERPATH,'data/playerhtml/',playername,'-',playerid,'.html',sep='')
	if (!file.exists(fileout)) {
		webadd=paste('http://uk.soccerway.com/players',playername,playerid,sep='/')
		b1=scanrobust(webadd,'',sep='\n',quiet=T,encoding='UTF-8')
		### save to disk in case we need to overwrite the database once more
		write(file=fileout,b1)
	}
	
	b1=scan(fileout,'',sep='\n',quiet=T,encoding='latin1')
	b1=iconv(b1,from='latin1',to='ASCII//TRANSLIT')
	
	playernat=playerdob=playerpos=playerheight=playerweight=playerfoot=NA
	
	if (length(grep('<dt>Nationality</dt>',b1))==1) playernat=gsub('[^[:alpha:]]','',tolower(gsub('^(.+>)([^<]+)(<.+$)','\\2',b1[grep('<dt>Nationality</dt>',b1)+1])))
	if (length(grep('<dt>Date of birth</dt>',b1))==1) {
		dum=gsub('^(.+>)([^<]+)(<.+$)','\\2',b1[grep('<dt>Date of birth</dt>',b1)+1])
		ddum=gsub(' .+$','',dum)
		mdum=match(gsub('(^.+ )([^ ]+)( .+$)','\\2',dum),month.name)
		ydum=gsub('^.+ ','',dum)
		ddum=ifelse(nchar(ddum)==1,paste('0',ddum,sep=''),ddum)
		mdum=ifelse(nchar(mdum)==1,paste('0',mdum,sep=''),mdum)
		playerdob=paste(ydum,mdum,ddum,sep='')
	}
	
	### get his short name
	sax=grep('<title>.+Profile with new',b1)
	if (length(sax)==0) {stop('Error, something unusual about player profile page for',playername,'\n')}
	playerabb=gsub('[^[[:alpha:] ]','',tolower(gsub('(^.+<title>.+ - )(.+)( - Profil.+$)','\\2',b1[sax])))
	if (length(grep('<dt>Position</dt>',b1))==1) playerpos=tolower(gsub('^(.+>)([^<]+)(<.+$)','\\2',b1[grep('<dt>Position</dt>',b1)+1]))
	playerpos=c('G','D','M','F')[which(c('goalkeeper','defender','midfielder','attacker')==playerpos)]
	if (length(grep('<dt>Height</dt>',b1))==1) playerheight=gsub(' cm','',gsub('^(.+>)([^<]+)(<.+$)','\\2',b1[grep('<dt>Height</dt>',b1)+1]))
	if (length(grep('<dt>Weight</dt>',b1))==1) playerweight=gsub(' kg','',gsub('^(.+>)([^<]+)(<.+$)','\\2',b1[grep('<dt>Weight</dt>',b1)+1]))
	if (length(grep('<dt>Foot</dt>',b1))==1) playerfoot=tolower(gsub('^(.+>)([^<]+)(<.+$)','\\2',b1[grep('<dt>Foot</dt>',b1)+1]))
	tabdat=data.frame(swayid=playerid,swayname=playername,swayabb=playerabb,dob=playerdob,height=playerheight,weight=playerweight,foot=playerfoot,position=playerpos,nationality=playernat)
	tabdat[is.na(tabdat)]=-99
	### now write all of that to the database
	sqlInsert('player',names(tabdat),tabdat)
}

coachproblem=function(myprobpaste) {
	fileout=paste(USERPATH,'data/playercoachnightmare.dat',sep='')
	if (file.exists(fileout)) {
		gotprobpaste=scan(fileout,'',sep='\n',quiet=T)
		toadd=which(!myprobpaste %in% gotprobpaste)
	}
	if (!file.exists(fileout)) {
		toadd=1:length(myprobpaste)
	}
	if (length(toadd)>0) {
		write(file=fileout,myprobpaste[toadd],append=T)
	}
}


getswmatchinfo=function(webadd, mykey) {
	
	### firstly check to see if we've already got the filename
	filen=paste(USERPATH,'data/gamehtml/',mykey,'.html',sep='')
	
	if (!file.exists(filen)) {
		b1=scanrobust(webadd,'',sep='\n',quiet=T,encoding='UTF-8')
		write(file=filen,b1)
	}
	
	b1=scan(filen,'',sep='\n',quiet=T,encoding='latin1')
	b1=iconv(b1,from='latin1',to='ASCII//TRANSLIT')
	
	### get hold of match details
	mdate=paste(strsplit(webadd,split='/')[[1]][5:7],collapse='')
	gameline=grep('<title>.+ vs. .+</title>',b1)
	hometeam=tolower(gsub('^.+>','',strsplit(b1[gameline],split=' vs. ')[[1]][1]))
	awayteam=tolower(gsub('(^.+ vs\\. )([^\\-]+)( .+$)','\\2',b1[gameline]))
	hometeam=cleanteam(hometeam)
	awayteam=cleanteam(awayteam)
	
	cat('About to process',mykey,'...\n')
	### break down info into three sections
	
	### right, we have a problem with player links changing to coach links - need to override when this happens
	sax=grep('/coaches/',b1)
	probsax=sax[!grepl('Coach:',b1[sax-1])]
	if (length(probsax)>0) {
		b1[probsax]=gsub('/coaches/','/players/',b1[probsax])
		### also record it where it happens
		probplayer=gsub('(^.+players/)([^/]+)(/.+$)','\\2',b1[probsax])
		probplayerid=gsub('(^.+players/[^/]+.)([0-9]+)(/.+$)','\\2',b1[probsax])
		probpaste=paste(probplayerid,probplayer,sep='~')
		coachproblem(probpaste)
	}
	
	linb=b1[(grep('Lineups',b1)+1):(grep('Substitutes',b1)-1)]
	teamb=grep('a href="/teams',linb)
	startline=grep('<a href="/players/.+/[0-9]',linb)
	### occasioanlly that picks up a coach, get rid of them
	dum=grep('Coach:',linb[startline-1])
	if (length(dum)>0) startline=startline[-dum]
	startplayer=gsub('(^.+players/)([^/]+)(/.+$)','\\2',linb[startline])
	startplayerid=gsub('(^.+players/[^/]+.)([0-9]+)(/.+$)','\\2',linb[startline])
	startplayerabb=gsub('(^.+>)([^<]+)(</a>$)','\\2',linb[startline])
	hastartplayer=rep(1:2,rep(11,2))
	
	subb=b1[(grep('Substitutes',b1)+1):(grep('Additional info',b1)-1)]
	startline=grep('<a href="/players/.+/[0-9]',subb)
	### but separate into those who were subs to start with, and those who were subbed off
	substatus=rep(3,length(startline))
	substatus[grep('Substituted',subb[startline+1])]=1
	substatus[grep('substitute substitute-out',subb[startline])]=2
	subtime=rep(NA,length(startline))
	subtime[substatus==1]=gsub("(^.+> )([0-9\\+]+)('</p>)","\\2",subb[startline[substatus==2]])
	subtime[substatus==2]=gsub("(^.+> )([0-9\\+]+)('</p>)","\\2",subb[startline[substatus==2]])
	subplayer=gsub('(^.+players/)([^/]+)(/.+$)','\\2',subb[startline])
	subplayerid=gsub('(^.+players/[^/]+.)([0-9]+)(/.+$)','\\2',subb[startline])
	subplayerabb=gsub('(^.+>)([^<]+)(</a>.*$)','\\2',subb[startline])
	hasubplayer=rep(NA,length(subplayer))
	# but which side did they play for? this should be right hopefully
	dum=grep('<div class=\"container right\">  ',subb)
	hasubplayer[startline<dum]=1
	hasubplayer[startline>dum]=2
	
	## however, we don't want the doubling up of players, so let's convert things to minute on and minute off
	unplayerid=unique(c(startplayerid,subplayerid))
	startix=which(unplayerid %in% startplayerid)
	mstart=match(unplayerid[startix],startplayerid)
	subonix=which(unplayerid %in% subplayerid[substatus==1] & !unplayerid %in% subplayerid[substatus==2])
	msubon=match(unplayerid[subonix],subplayerid)
	suboffix=which(unplayerid %in% subplayerid[substatus==2])
	msuboff=match(unplayerid[suboffix],subplayerid)
	subonoffix=which(unplayerid %in% subplayerid[substatus==1] & unplayerid %in% subplayerid[substatus==2])
	msubonoffon=which(substatus==1)[match(unplayerid[subonoffix],subplayerid[substatus==1])]
	msubonoffoff=which(substatus==2)[match(unplayerid[subonoffix],subplayerid[substatus==2])]
	
	starttime=endtime=hastatus=rep('U',length(unplayerid))
	starttime[startix]=0
	starttime[subonix]=subtime[msubon]
	endtime[subonix]=94
	endtime[which(unplayerid %in% startplayerid & !unplayerid %in% subplayerid)]=94
	endtime[suboffix]=subtime[msuboff]
	starttime[subonoffix]=subtime[msubonoffon]
	endtime[subonoffix]=subtime[msubonoffoff]
	hastatus=c(hastartplayer,hasubplayer)[match(unplayerid,c(startplayerid,subplayerid))]
	
	### now it can be for certain leagues that there is no info about sidelined players, so we have to be flexible to that
	
	if (length(grep('Sidelined',b1))==0) {
		sideplayerid=NULL
		sideplayerabb=NULL
		sideplayer=NULL
		hasideplayer=NULL
		sidestarttime=NULL
		sideendtime=NULL
	}
	
	if (length(grep('Sidelined',b1))>0) {
		sideb=b1[(grep('Sidelined',b1)+1):(max(grep('a href="/players/[^/]+/[0-9]+',b1))+5)]
		startline=grep('<a href="/players/.+/[0-9]',sideb)
		teamb=grep('a href="/teams',sideb)
		sideteam=cleanteam(tolower(gsub('(^.+a href.+>)([^<]+)(<.+$)','\\2',sideb[teamb])))
		sideplayer=gsub('(^.+players/)([^/]+)(/.+$)','\\2',sideb[startline])
		sideplayerid=gsub('(^.+players/[^/]+.)([0-9]+)(/.+$)','\\2',sideb[startline])
		sideplayerabb=gsub('(^.+>)([^<]+)(</a>.*$)','\\2',sideb[startline])
		sideplayerlongreason=gsub('(^.+icon )([^"]+)(".+$)','\\2',sideb[startline+2])
		sideplayerreason=c('I','S')[match(sideplayerlongreason,c('injury','suspension'))]
		# this bit is an arse, won't always be two teams listed
		if (length(teamb)==1) {
			hasideplayer=rep(match(sideteam,c(hometeam,awayteam)),length(sideplayer))
		}
		if (length(teamb)==2) {
			hasideplayer=rep(NA,length(sideplayer))
			hasideplayer[startline<teamb[2]]=match(sideteam[1],c(hometeam,awayteam))
			hasideplayer[startline>teamb[2]]=match(sideteam[2],c(hometeam,awayteam))
		}
		sidestarttime=sideendtime=sideplayerreason
	}
	
	allplayerid=c(unplayerid,sideplayerid)
	allplayerabb=c(startplayerabb,subplayerabb,sideplayerabb)[match(allplayerid,c(startplayerid,subplayerid,sideplayerid))]
	allplayer=c(startplayer,subplayer,sideplayer)[match(allplayerid,c(startplayerid,subplayerid,sideplayerid))]
	allplayerabb=iconv(allplayerabb,from='UTF-8',to='ASCII//TRANSLIT')
	### can still make that a bit nicer
	allplayerabb=gsub('[^[:alpha:] ]','',tolower(allplayerabb))
	allha=c(hastatus,hasideplayer)
	allstarttime=c(starttime,sidestarttime)
	allendtime=c(endtime,sideendtime)
	allteam=c(hometeam,awayteam)[allha]
	
	### however we have a bit of a problem with time listed as things like '90+2' sometimes, so let's remedy them
	dum=grep('\\+',allstarttime)
	if (length(dum)>0) {
		allstarttime[dum]=as.numeric(gsub('\\+.+$','',allstarttime[dum])) + as.numeric(gsub('^.+\\+','',allstarttime[dum]))
	}
	dum=grep('\\+',allendtime)
	if (length(dum)>0) {
		allendtime[dum]=as.numeric(gsub('\\+.+$','',allendtime[dum])) + as.numeric(gsub('^.+\\+','',allendtime[dum]))
	}
	
	tabdat=data.frame(mykey,allteam,allplayerid,allplayer,allplayerabb,allstarttime,allendtime,stringsAsFactors=F)
	names(tabdat)=c('mkey','team','swayid','swayname','swayabb','starttime','endtime')
	
	## occasional problem of player being inserted twice, get rid of this
	ptabdat=apply(tabdat,1,paste,collapse='~')
	mdum=match(unique(ptabdat),ptabdat)
	tabdat=tabdat[mdum,]
	
	return(tabdat)
}

