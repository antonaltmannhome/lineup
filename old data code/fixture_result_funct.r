savePageInfo=function(webPage, fileName, thiscomputer) {
	initfile()
	gotowebadd(webPage, thiscomputer = thiscomputer, browserchoice=browserToUse)
	insertabort()
	runscript()
	
	print('Press enter when all results have loaded')
	dum=scan(what='',nmax=1,quiet=T)
	
	### you should be on summary page for team in question - now save to notepad++
	initfile()
	insertselectwindow(browserToUse)
	fileOut=paste(DATAPATH,'fixture_result/',fileName,sep='')
	if (file.exists(fileOut)) file.remove(fileOut)
	selectalltonotepad(fileOut)
	
	insertabort()
	runscript()
}

convertdate=function(datedata) {
	dumday=gsub('(^[a-z]+ )([0-9]{1,2})( .+$)','\\2',datedata)
	dumwordmonth=gsub('(^[a-z]+ [0-9]{1,2} )([a-z]+)( .+$)','\\2',datedata)
	dummonth=match(dumwordmonth,tolower(month.name))
	dumyear=gsub('(^[a-z]+ [0-9]{1,2} [a-z]+ )(.+$)','\\2',datedata)
	dumday=ifelse(nchar(dumday)==1,paste('0',dumday,sep=''),dumday)
	dummonth=ifelse(nchar(dummonth)==1,paste('0',dummonth,sep=''),dummonth)
	dumdate=paste(dumyear,dummonth,dumday,sep='')
	return(dumdate)
}

stripawayteam=function(atdata) {
	
	firsttry=gsub(' .+$','',atdata)
	firstmatch=which(firsttry %in% fixtureteam)
	stillleft=setdiff(1:length(atdata),firstmatch)
	secondtry=gsub('(^[a-z]+ [a-z]+)( .+$)','\\1',atdata[stillleft])
	secondmatch=stillleft[which(secondtry %in% fixtureteam)]
	stillleft=setdiff(1:length(atdata),c(firstmatch,secondmatch))
	if (length(stillleft)>0) stop('Got some weird issue with away team names, investigate\n')

	ateam=rep(NA,length(atdata))
	ateam[firstmatch]=firsttry[firstmatch]
	ateam[secondmatch]=secondtry
	return(ateam)
}
