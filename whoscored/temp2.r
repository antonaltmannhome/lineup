### firstly get what we will be pasting in

b=scan('whoscored/data/20160829/sunderland_goalsit.txt','',sep='\n')
headerline=b[grep('Player.+CM.+KG',b)]
defoeline=gsub('[0-9]+, [A-Z\\(\\),]+ \t[0-9]{3} \t[0-9]+ \t[0-9]+\t[0-9]+','',b[grep('Defoe',b)[1]+1])
genericline=gsub('[0-9]','-',defoeline)

### now scan in file we want to clean
myteam='arsenal'
mydatetouse=20160825
correctfile=function(myteam,mydatetouse) {
	filein=paste('whoscored/data/',mydatetouse,'/',myteam,'_goalsit.txt',sep='')
	b=scan(filein,'',sep='\n')
	
	oldheaderline=grep('Nationali',b)+1
	oldfooterline=grep('Mins: Minutes played',b)
	chunk1=b[1:oldheaderline]
	chunk2=b[(oldheaderline+1):(oldfooterline-1)]
	chunk3=b[oldfooterline:length(b)]
	
	### but we need the list of players from a non-screwed file
	filein=paste('whoscored/data/',mydatetouse,'/',myteam,'_shotsit.txt',sep='')
	b=scan(filein,'',sep='\n')
	helpheaderline=grep('Nationali',b)+1
	helpfooterline=grep('Mins: Minutes played',b)
	helpchunk2=b[(helpheaderline+1):(helpfooterline-1)]
	
	### now pick out the lines we want to overwrite
	helpstatline=grep('^[0-9]+, [A-Za-z\\(\\)]+',helpchunk2)
	helphalf1=gsub('(^[0-9]+, [A-Za-z\\(\\),]+ \t[0-9]+ \t[0-9]+ \t[0-9]+\t[0-9]+)(.+$)','\\1',helpchunk2[helpstatline])
	helpnewstatline=paste(helphalf1,genericline)
	helpchunk2[helpstatline]=helpnewstatline
	
	newinfo=c(chunk1,helpchunk2,chunk3)
	fileout=paste('whoscored/data/',mydatetouse,'/',myteam,'_goalsit.txt',sep='')
	write(file=fileout,newinfo)
}

### and the same for shotsit for first week

b=scan('whoscored/data/20160829/sunderland_shotsit.txt','',sep='\n')
headerline=b[grep('Player.+CM.+KG',b)]
defoeline=gsub('[0-9]+, [A-Z\\(\\),]+ \t[0-9]{3} \t[0-9]+ \t[0-9]+\t[0-9]+','',b[grep('Defoe',b)[1]+1])
genericline=gsub('[0-9]','-',defoeline)

### now scan in file we want to clean
myteam='arsenal'
mydatetouse=20160818
correctfile=function(myteam,mydatetouse) {
	filein=paste('whoscored/data/',mydatetouse,'/',myteam,'_shotsit.txt',sep='')
	b=scan(filein,'',sep='\n')
	
	oldheaderline=grep('Nationali',b)+1
	oldfooterline=grep('Mins: Minutes played',b)
	chunk1=b[1:oldheaderline]
	chunk2=b[(oldheaderline+1):(oldfooterline-1)]
	chunk3=b[oldfooterline:length(b)]
	
	### but we need the list of players from a non-screwed file
	filein=paste('whoscored/data/',mydatetouse,'/',myteam,'_shotacc.txt',sep='')
	b=scan(filein,'',sep='\n')
	helpheaderline=grep('Nationali',b)+1
	helpfooterline=grep('Mins: Minutes played',b)
	helpchunk2=b[(helpheaderline+1):(helpfooterline-1)]
	
	### now pick out the lines we want to overwrite
	helpstatline=grep('^[0-9]+, [A-Za-z\\(\\)]+',helpchunk2)
	helphalf1=gsub('(^[0-9]+, [A-Za-z\\(\\),]+ \t[0-9]{3} \t[0-9]+ \t[0-9]+\t[0-9]+)(.+$)','\\1',helpchunk2[helpstatline])
	helpnewstatline=paste(helphalf1,genericline)
	helpchunk2[helpstatline]=helpnewstatline
	
	newinfo=c(chunk1,helpchunk2,chunk3)
	fileout=paste('whoscored/data/',mydatetouse,'/',myteam,'_shotsit.txt',sep='')
	write(file=fileout,newinfo)
}
