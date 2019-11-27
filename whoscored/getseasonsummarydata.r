### source this to get the latest stats on whoscored

source('c:/research/general_funct.r')
source('c:/research/ahkfunct.r')
USERPATH='c:/research/lineup/whoscored/'
setwd(USERPATH)
options(warn=2)
season=1516
alltype=c('offense','shot','target','pass')
cat('What data do you want to get?\n(1) offense\n(2) shot zones\n(3) shot accuracy\n(4) key passes\n')
dum=scan(quiet=T,nmax=1)
datatype=alltype[dum] # can be 'offense','shot','target','pass'
AHKFILE=paste(USERPATH,'script/runme.ahk',sep='')
SQLPATH=paste(USERPATH,'script/',sep='')

cat('For ALL actions, restart openoffice Calc if it is already open\n')
if (datatype=='offense') cat('To start with:\n(1) With Edge, go to the \'Detailed\' section, then \'Offensive\'\nThen click on \'All players\' rather than \'Minimum apps\n')
if (datatype=='shot') cat('To start with:\n(1) With Edge, go to the \'Detailed\' section, \nthen \'Detailed\' and then \'Shots\' and \nthen in Accumulation, select \'Total\'\n')
if (datatype=='target') cat('To start with:\n(1) With Edge, go to the \'Detailed\' section, \nthen \'Detailed\' and then \'Shots\', then pick \'Accuracy\' from \'Zone\'\nand then in Accumulation, select \'Total\'\n')
if (datatype=='pass') cat('To start with:\n(1) With Edge, go to the \'Detailed\' section, \nthen \'Detailed\' and then \'Key passes\', \nthen in Accumulation, select \'Total\'\n')
cat('(2) Then scroll the page so that the \'next\' tab is visible\n(3) Then open Openoffice Calc\n')
cat('Press enter when done\n')
dum=scan(what='',quiet=T,nmax=1)
cat('Then open autohotkeys spy and find the location of the \'next\'button.\nEnter the x-location:\n')
xloc=scan(nmax=1,quiet=T)
cat('Enter y-location:\n')
yloc=scan(nmax=1,quiet=T)
cat('Enter the total number of pages:\n')
npage=scan(nmax=1,quiet=T)

### firstly write the head to the file
b=scan(paste(USERPATH,'script/header.txt',sep=''),'',sep='\n',quiet=T)
write(file=AHKFILE,b)
for (i in 1:npage) {
	### scan in the template for our script
	b=scan(paste(USERPATH,'script/template.txt',sep=''),'',sep='\n',quiet=T)
	### sub in the bits that are relevant for now
	fileout=paste(USERPATH,'data/',datatype,'_',season,'_',i,'_',numericdate(),'.csv',sep='')
	if (file.exists(fileout)) file.remove(fileout)
	b=gsub('FILENAME',gsub('/','\\\\\\\\',fileout),b)
	if (i==1) {
		##calc is often slow for this, allow more time for pasting and saving
		dum=grep('Calc',b)
		b[dum+3]=gsub('8000','12000',b[dum+3])
		b[dum+5]=gsub('2000','5000',b[dum+3])
	}
	### now substitute in the 'next' click
	if (i<npage) {
		b=gsub('XLOC',xloc,b)
		b=gsub('YLOC',yloc,b)
	}
	if (i>1) {
		## get rid of bit where you select csv format
		dum=grep('Down 13',b)
		b[(dum-3):(dum+2)]='sleep 5'
		### also get rid of the bit with the formatting dialogue
		dum=grep('450,50',b)
		b[dum:(dum+1)]='sleep 5'
	}
	### but if we're on the last page, don't bother clicking for 'next' at the end
	if (i==npage) {
		b=b[-grep('XLOC',b)]
	}
	write(file=AHKFILE,b,append=T)
}
b=scan(paste(USERPATH,'script/footer.txt',sep=''),'',sep='\n',quiet=T)
write(file=AHKFILE,b,append=T)

runscript(AHKFILE)
print('Do CTRL-ALT-B to start it off')
