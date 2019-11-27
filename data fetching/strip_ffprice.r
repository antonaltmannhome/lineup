StripFFPrice = function() {

	ScrapeContentFromWebsite()
	StripContentFromWebScrape()
}

ScrapeContentFromWebsite = function() {

	if (file.exists('c:/temp/temp-ff-price.dat')) {
		file.remove('c:/temp/temp-ff-price.dat')
	}

	initfile()
	### firstly navigate to main page of team
	mypage=paste('https://fantasy.premierleague.com/player-list/')
	gotowebadd(mypage,browserchoice='Firefox',thiscomputer = thiscomputer)

	insertabort()
	runscript()

  initfile()
  insertselectwindow('Firefox')
	selectalltonotepad('c:/temp/temp-ff-price.dat', deselectpoint=c(100, 500))

	insertabort()
	runscript()
}

StripContentFromWebScrape = function() {
	rawdata = tolower(scan('c:/temp/temp-ff-price.dat', sep = '\n', what = '', quiet = TRUE, encoding = 'UTF-8'))
	rawdata = iconv(rawdata,from='UTF-8',to='ascii//translit')

	# pascal gross is an arsehole, need to manually replace him
	rawdata = gsub('gro\\?\tbrighton', 'gross\tbrighton', rawdata)
	rawdata = gsub('\'', '', rawdata)

	positionstring = c('goalkeepers', 'defenders', 'midfielders', 'forwards')
	positionabb = c('g','d','m','f')
	positionline = rep(NA, length(positionstring))
	for (j in 1:length(positionstring)) {
	  positionline[j] = grep(positionstring[j], rawdata)
	}

	playerline = grep('^[a-z \\-]+\t[a-z ]+\t[0-9\\-]+\t.+[0-9\\.]+$',rawdata)
	ffplayer = gsub('\t.+$','',rawdata[playerline])
	ffplayer = gsub('\\-', ' ', ffplayer)
	ffplayer = gsub('\'', ' ', ffplayer)
	ffteam = gsub('(.+\t)([a-z ]+)(\t.+$)','\\2',rawdata[playerline])
	ffteam = gsub(' ', '', ffteam)
	ffteam = cleanteam(ffteam, 'premierleague')
	ffprice = as.numeric(gsub('(^.+\\?)([0-9\\.]+$)','\\2',rawdata[playerline]))
	ffposition = positionabb[findInterval(playerline, c(positionline, nrow(rawdata)))]

	ffpricedf = tibble(player = ffplayer, team = ffteam, ffposition = ffposition, price = ffprice)

	# then save the bastard somewhere
	# although of course we have the name matching to do
	fileout = paste0(DATAPATH, 'ff-price.csv')
	write.csv(file = fileout, ffpricedf, row.names = FALSE)
}
