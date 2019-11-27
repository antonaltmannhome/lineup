### now that you've got the data, scan it in

source('c:/research/general_funct.r')
source('c:/research/ahkfunct.r')
USERPATH='c:/research/lineup/whoscored/'
setwd(USERPATH)
library(numDeriv)
options(warn=2)
season=1415

alltype=c('offense','pass','shot','target')

allstat=NULL

for (datatype in alltype) {
	dum=list.files(paste(USERPATH,'data',sep=''))
	myfile=dum[grep(datatype,dum)]
	### easier to trace problems if we put it in order
	myrank=as.numeric(gsub('(^.+[0-9]{4}_)([0-9]+)(_[0-9]{8}.+$)','\\2',myfile))
	myfile=myfile[order(myrank)]
	statlist=NULL
	for (k in 1:length(myfile)) {
		b=scan(paste(USERPATH,'data/',myfile[k],sep=''),'',sep='\n',quiet=T)
		dum=grep('^R,,Name',b)
		sax=(dum+1):(dum+10)
		b2=b[sax]
		write(file='temp.csv',b2)
		bcsv=read.csv('temp.csv',sep=',',header=F,as.is=T)
		names(bcsv)=unlist(strsplit(b[dum],split=','))
		### there might be less than 10 entries on last page, must deal with this
		sax=which(bcsv[,3]!='')
		statlist[[k]]=bcsv[sax,]
	}
	combstat=do.call(rbind,statlist)
	if (datatype==alltype[1]) {
		keepcol=setdiff(names(combstat),c('R','','Apps','Total','Rating'))
		allstat=combstat[,keepcol]
	}
	if (datatype!=alltype[1]) {
		keepcol=setdiff(names(combstat),c('R','','Apps','Mins','Total','Rating'))
		allstat=merge(allstat,combstat[,keepcol],by='Name')
	}
	cat('Have got',datatype,'...\n')
}

### right, now we can process some columns
desccol=c('Name','Mins')
varcol=setdiff(names(allstat),desccol)
for (k in varcol) {
	dum=allstat[,k]
	dum[grep('-',dum)]='0'
	dum=as.numeric(dum)
	allstat[,k]=dum
}

poislik=function(theta) {
	mu=theta[1]
	for (j in 1:length(coltouse)) mu=mu+theta[j+1]*allstat[,coltouse[j]]
	emu=exp(mu)*allstat$Mins/90
	loglik=mean(log(dpois(allstat$Goals,emu)))
	return(-loglik)
}
coltouse=c('OutOfBox','SixYardBox','PenaltyArea')
maxinfo=nlm2(poislik,p=rep(0,length(coltouse)+1))
modsum=data.frame(coefname=c('Intercept',coltouse),Est=maxinfo$est, StdErr=0.5*sqrt(diag(solve(hessian(poislik,maxinfo$est)))))
modsum$zval=modsum$Est/modsum$StdErr
modsum$pval=2*pnorm(-abs(modsum$zval),0,1)
