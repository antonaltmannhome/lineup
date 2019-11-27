#### just want something that quickl yconverts fantasy price source into a niceish list
### modify accordingly:
library(plyr)

mydate=20160513
b=scan(paste('c:/research/lineup/prices_source/',mydate,'.htm',sep=''),'',sep='\n',encoding='UTF-8')
#b=iconv(b,fr='UTF-8',to='ASCII//TRANSLIT')

### obviously tis won't work forever, buyt..

dum=b[grep('Ighalo',b)]
dum2=unlist(strsplit(dum,split=']'))
sax=grep('^, \\[[0-9]+, [^,]+, [0-9]+, .+$',dum2)
dum3=dum2[sax]
dum4=llply(dum3,function(x) strsplit(x,split=',')[[1]])
cleanit=function(x) gsub(' |\\"','',x)
dum5=llply(dum4,cleanit)

player=sapply(dum5,function(x) paste(x[5],x[6]))
price=sapply(dum5,function(x) x[12])

mydf=data.frame(player=player,price=price)
