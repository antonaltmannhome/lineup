### want a file that just produces the estimates for the current data and fitness files



source('c:/research/general_startup.r')
source('c:/research/lineup/lineup_startup.r')
source('c:/research/lineup/lineup_readindata.r')
library(BiasedUrn)
library(Rcpp)  ## for cxxfunction()
library(inline)  ## for cxxfunction()

ludat$predyn=rep(0,dim(ludat)[1])
# home team
sax=which(gamedat$hg365[rr]>=5 & team==gamedat$ht[rr] & gamedat$hngk[rr]>=1 & ludat$npgamclub>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='G')
ludat$predyn[sax]=1
sax=which(gamedat$hg365[rr]>=5 & team==gamedat$ht[rr] & gamedat$hndef[rr]>=4 & ludat$npgamclub>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='D')
ludat$predyn[sax]=1
sax=which(gamedat$hg365[rr]>=5 & team==gamedat$ht[rr] & gamedat$hnmf[rr]>=6 & ludat$npgamclub>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='MF')
ludat$predyn[sax]=1
# away team
sax=which(gamedat$ag365[rr]>=5 & team==gamedat$at[rr] & gamedat$angk[rr]>=1 & ludat$npgamclub>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='G')
ludat$predyn[sax]=1
sax=which(gamedat$ag365[rr]>=5 & team==gamedat$at[rr] & gamedat$andef[rr]>=4 & ludat$npgam>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='D')
ludat$predyn[sax]=1
sax=which(gamedat$ag365[rr]>=5 & team==gamedat$at[rr] & gamedat$anmf[rr]>=6 & ludat$npgam>1 & !ludat$starttime %in% c('S','I') & ludat$pos=='MF')
ludat$predyn[sax]=1

### various useful functions

splitgame=function(mi) {
	## mi is the game number
	myunteam=unique(team[rr==mi])
	donesofar=0
	
	for (ti in 1:2) {
		sax=which(rr==mi & team==myunteam[ti])
		myuntime=sort(unique(c(nstmin[sax],nendmin[sax])))
		ldum=NULL
		for (ui in 1:(length(myuntime)-1)) {
			donesofar=donesofar+1
			dum=swayid[sax[which(!is.na(nstmin[sax]) & nstmin[sax]<=myuntime[ui] & nendmin[sax]>=myuntime[ui+1])]]
			ldum[[ui]]=cbind(donesofar,myunteam[ti],myuntime[ui+1]-myuntime[ui],dum,1)
			dum=swayid[sax[which( (!is.na(nstmin[sax]) & (nstmin[sax]>=myuntime[ui+1] | nendmin[sax]<=myuntime[ui])) | stmin[sax] %in% c('B','U'))]]
			ldum[[ui]]=rbind(ldum[[ui]],cbind(donesofar,myunteam[ti],myuntime[ui+1]-myuntime[ui],dum,0))
		}
		if (ti==1) hdum=do.call(rbind,ldum)
		if (ti==2) adum=do.call(rbind,ldum)
		### now put that into an array
	}
	return(rbind(hdum,adum))
}

### right, let's make the arrays now...
cat('About to make the huge availability array, this will take about',round(dim(gamedat)[1]*system.time(splitgame(1))[3]),'seconds...\n')
dum=apply(matrix(1:dim(gamedat)[1],ncol=1),1,splitgame)
ndum=sapply(dum,function(x) dim(x)[1])
dum=do.call(rbind,dum)
### need to get the numeric columns to be numeric
sdf=data.frame(rr=rep(1:dim(gamedat)[1],ndum),srr=as.numeric(dum[,1]),team=dum[,2],mindiff=as.numeric(dum[,3]),swayid=as.numeric(dum[,4]),yn=as.numeric(dum[,5]),stringsAsFactors=F)
### we want to make srr unique, so must increment by match
sdf$srr=match(paste(sdf$srr,sdf$rr),unique(paste(sdf$srr,sdf$rr)))
sdf$pos=ludat$pos[match(paste(sdf$rr,sdf$swayid),paste(ludat$rr,ludat$swayid))]
### that is a little cumbersome, would be nice to have one that uniquely references these mini matches, unnecessary repetition of mindiff at the moment eg
mdum=with(sdf,match(unique(paste(srr,team)),paste(srr,team)))
msdf=sdf[mdum,c('rr','srr','team','mindiff')]
# v important df for if we want to do players dicol eg
rlikfunct=function(theta, dicoldf) {
	#print(round(theta,5),collapse=',')
	# loglik=log(exp(theta[bigarr[,1]])/(exp(theta[bigarr[,1]])+exp(theta[bigarr[,2]])))
	loglik=dicoldf$mindiff*dicoldf$dwgt*(theta[dicoldf[,'y']] -log(exp(theta[dicoldf[,'y']])+exp(theta[dicoldf[,'n']])))
	
	priorlik=priorstr*sum(theta^2)
	
	totallik=mean(loglik)-priorlik
	
	return(-totallik)
}

drlikfunct=function(theta, dicoldf) {
	#print(round(theta,5),collapse=',')
	# loglik=log(exp(theta[bigarr[,1]])/(exp(theta[bigarr[,1]])+exp(theta[bigarr[,2]])))
	dloglik=rep(0,length(theta))
	for (j in 1:length(theta)) {
		sax=which(dicoldf[,'y']==j)
		if (length(sax)>0) dloglik[j]=sum(dicoldf[sax,'mindiff']*dicoldf$dwgt[sax]*(1-exp(theta[dicoldf[sax,'y']])/(exp(theta[dicoldf[sax,'y']]) + exp(theta[dicoldf[sax,'n']]))))
		sax=which(dicoldf[,'n']==j)
		if (length(sax)>0) dloglik[j]=dloglik[j]+sum(dicoldf[sax,'mindiff']*dicoldf$dwgt[sax]*(-exp(theta[dicoldf[sax,'n']])/(exp(theta[dicoldf[sax,'y']]) + exp(theta[dicoldf[sax,'n']]))))
	}
	
	dloglik=dloglik/dim(dicoldf)[1]-2*priorstr*theta
	
	return(-dloglik)
}

cppcode='
double clikfunct(NumericVector theta, NumericVector mindiff, NumericVector dwgt, IntegerVector yvec, IntegerVector nvec, double priorstr) {

int nplayer=theta.size();
int npoint=yvec.size();

//		IntegerVector yvec=as<IntegerVector>(dfval["yvec"]);
//		IntegerVector nvec=dfval["nvec"];
//		NumericVector minplay=dfval["minplay"];
//		NumericVector dwgt=dfval["dwgt"];
//		double priorstr=dfval["priorstr"];

// Rcpp::Rcout << "priorstr is" << priorstr << std::endl;

double datalik;
datalik=0.0;
for (int i=0;i<npoint;i++) {
datalik+=mindiff[i]*dwgt[i]*(theta[yvec[i]-1]-log(exp(theta[yvec[i]-1])+exp(theta[nvec[i]-1])));
}
datalik/=npoint;
double priorlik;
priorlik=0.0;
for (int i=0;i<nplayer;i++) {
priorlik+=pow(theta[i],2.0);
}
priorlik*=priorstr;
double loglik;
loglik=datalik-priorlik;
return(-loglik);
}'
cppFunction(cppcode)

cppcode='
NumericVector cdlikfunct(NumericVector theta, NumericVector mindiff, NumericVector dwgt, IntegerVector yvec, IntegerVector nvec, double priorstr) {

int nplayer=theta.size();
int npoint=yvec.size();

NumericVector dloglik(nplayer);
for (int i=0;i<nplayer;i++) {
dloglik[i]=0.0;
}
for (int i=0;i<npoint;i++) {
dloglik[yvec[i]-1]+=mindiff[i]*dwgt[i]*(1.0-exp(theta[yvec[i]-1])/(exp(theta[yvec[i]-1])+exp(theta[nvec[i]-1])));
dloglik[nvec[i]-1]+=mindiff[i]*dwgt[i]*(-exp(theta[nvec[i]-1])/(exp(theta[yvec[i]-1])+exp(theta[nvec[i]-1])));
}
for (int i=0;i<nplayer;i++) {
dloglik[i]/=npoint;
}
for (int i=0;i<nplayer;i++) {
dloglik[i]-=2.0*priorstr*theta[i];
}
return(-dloglik);
}'
cppFunction(cppcode)

sdftoyn=function(subtsax) {
	
	saxy=subtsax[sdf$yn[subtsax]==1]
	saxn=subtsax[sdf$yn[subtsax]==0]
	subdf=expand.grid(sdf$swayid[saxy],sdf$swayid[saxn])
	return(subdf)
}


getteamstat=function(datetouse,teamtouse) {
	
	daynotouse=mkdayno(datetouse)
	validgame=getpastix(datetouse,teamtouse)
	
	sax=which(team==teamtouse & rr %in% validgame)
	allposunswayid=with(ludat,unique(swayid[sax]))
	### will want first appearance by each player within validgame
	alllposfirstdayno=ludat$dayno[sax[match(allposunswayid,ludat$swayid[sax])]]
	allposplayerest=rep(NA,length(allposunswayid))
	
	for (posi in 1:length(unpos)) {
		
		mtsax=which(msdf$team==teamtouse & msdf$rr %in% validgame)
		tsax=which(sdf$srr %in% msdf$srr[mtsax] & sdf$pos==unpos[posi])
		dumdwgt=exp(-dwgt*(gamedat$dayno[msdf$rr[tail(mtsax,1)]]-gamedat$dayno[msdf$rr[mtsax]]))
		myunswayid=unique(sdf$swayid[tsax])
		### we want to know first appearance for each player
		myfirstdayno=alllposfirstdayno[match(myunswayid,allposunswayid)]
		### now make the dicol pairings
		dum=tapply(tsax,sdf$srr[tsax],sdftoyn)
		ndum=sapply(dum,function(x) dim(x)[1])
		dumrr=rep(1:length(mtsax),ndum)
		dumsrr=rep(msdf$srr[mtsax],ndum)
		## but if we want it to reference
		dum=do.call(rbind,dum)
		dicoldf=data.frame(dicolrr=dumrr,srr=dumsrr,y=match(dum[,1],myunswayid),n=match(dum[,2],myunswayid))
		dicoldf$mindiff=msdf[dicoldf$srr,'mindiff']
		dicoldf$dwgt=dumdwgt[dicoldf$dicolrr]
		# mdicoldf=data.frame(srr=dumsrr,mindiff=msdf$mindiff[mtsax][nrr],dwgt=dumdwgt[nrr],y=match(dum[,1],myunswayid),n=match(dum[,2],myunswayid),fullrr=splitdf[tsax,'rr'][nrr])
		
		thetainit=rep(0,length(myunswayid))
		
		# maxinfonlm=nlm(likfunctbin,p=theta)
		
		### or
		## maxinfooptim=optim(par=thetainit,f=likfunctbin,gr=dlikfunctbin,method='BFGS', dicoldf=dicoldf)
		maxinfooptim=optim(par=thetainit,f=clikfunct,gr=cdlikfunct,method='BFGS',mindiff=dicoldf[,'mindiff'],dwgt=dicoldf[,'dwgt'],yvec=as.integer(dicoldf[,'y']),nvec=as.integer(dicoldf[,'n']),priorstr=priorstr)
		
		allposplayerest[match(myunswayid,allposunswayid)]=maxinfooptim$par
	}
	tabdat=cbind(allposunswayid,getpname(allposunswayid),allposplayerest)
	colnames(tabdat)=c('swayid','swayname','est')
	return(tabdat)
}

priorstr=0.001
dwgt=0.02

library(xlsx)
### so find most recent squad list
dum=list.files(paste(USERPATH,'data/fitness',sep=''))
### restrict to excel files
dum=dum[grep('xls$',dum)]
cursquadfile=paste(USERPATH,'data/fitness/',dum[which.max(as.numeric(gsub('(^.+_)([0-9]+)(.+$)','\\2',dum)))],sep='')
curpremteam=names(getSheets(loadWorkbook(cursquadfile)))

cursquadinfo=NULL
for (ci in 1:length(curpremteam)) {
	## get current squad info
	cursquadinfo[[ci]]=read.xlsx(cursquadfile,sheetName=curpremteam[ci],stringsAsFactors=F)
	### rename m/f as mf
	cursquadinfo[[ci]][cursquadinfo[[ci]]$pos %in% c('M','A'),'pos']='MF'
}

### right, now got to produce a nice file with all the estimates in it

estlist=NULL
for (ci in 1:length(curpremteam)) {
	estlist[[ci]]=getteamstat(numericdate(),curpremteam[ci])
	cat('Have got estimates for',curpremteam[ci],'\n')
}

getemin=function(estinfo,fitinfo) {
	
	nplayer.tm=dim(fitinfo)[1]
	### need to produce multiple possible outcomes of the squad
	saxdoubt=which(fitinfo$avail>0 & fitinfo$avail<1)
	nfitcomb=2^length(saxdoubt)
	if (length(saxdoubt)==0) {
		outprob=1
		fitarr=array(fitinfo[,"avail"],dim=c(nplayer.tm,1))
	}
	if (length(saxdoubt)>0) {
		dvec=rep(c(),length(saxdoubt))
		for (k in 1:length(saxdoubt)) dvec[[k]]=paste(rep(fitinfo$swayid[saxdoubt[k]],2),c(0,1))
		possgrid=expand.grid(dvec,stringsAsFactors=F)
		# then assign a probability to each
		outprob=rep(NA,nfitcomb)
		for (k in 1:nfitcomb) {
			playid=gsub(' .+','',possgrid[k,])
			playprob=fitinfo$avail[match(playid,fitinfo$swayid)]
			playoutcome=as.numeric(gsub('.+ ','',possgrid[k,]))
			dumprob=ifelse(playoutcome==1,playprob,1-playprob)
			outprob[k]=prod(dumprob)
		}
		### now put together the entire team sheet based on these
		fitarr=array(NA,dim=c(nplayer.tm,nfitcomb))
		sax=which(fitinfo$avail==0 | fitinfo$avail==1)
		fitarr[sax,]=array(rep(fitinfo$avail[sax],nfitcomb),dim=c(length(sax),nfitcomb))
		for (k in 1:nfitcomb) fitarr[saxdoubt,k]=as.numeric(gsub('.+ ','',possgrid[k,]))
	}
	
	### now just assume formation is 1-4-6
	
	### get current ests matched up
	curest=exp(as.numeric(estinfo[match(fitinfo[,"swayid"],estinfo[,"swayid"]),'est']))
	# assume any players with no rating are rubbish
	curest[is.na(curest)]=0.001
	
	emintab=array(0,dim=c(nfitcomb,nplayer.tm))
	probtab=rep(NA,nfitcomb)
	emin=rep(0,nplayer.tm)
	for (fiti in 1:nfitcomb) {
		cat('About to process combination',fiti,'of',nfitcomb,'\n')
		### have to weed out 'impossible' formations
		dum=tapply(fitarr[,fiti],fitinfo[,"pos"],sum)
		maxpos=as.numeric(dum)[match(unpos,names(dum))]
		for (posi in 1:length(unpos)) {
			sax=which(fitinfo[,"pos"]==unpos[posi] & fitarr[,fiti]==1)
			emintab[fiti,sax]=90*meanMWNCHypergeo(rep(1,length(sax)),formation[posi],curest[sax],preci=0.01)
		}
		probtab[fiti]=outprob[fiti]
		emin=emin+probtab[fiti]*emintab[fiti,]
	}
	
	
	# but what we'd also like is, what would the numbers be given total fitness?
	
	cat('About to process expected minutes for fully fit squad...\n')
	eminff=rep(0,dim=c(nplayer.tm))
	for (posi in 1:length(unpos)) {
		sax=which(fitinfo[,"pos"]==unpos[posi])
		eminff[sax]=90*meanMWNCHypergeo(rep(1,length(sax)),formation[posi],curest[sax],preci=0.01)
	}
	
	allinfo=cbind(fitinfo,round(emin,2),round(eminff,2))
	colnames(allinfo)=c(colnames(fitinfo),'emin','eminff')
	
	return(allinfo)
}

## paste it all together
nicetab=list(NULL)
for (ci in 1:length(curpremteam)) {
	cat('About to process expected minutes for',curpremteam[ci],'\n')
	nicetab[[ci]]=getemin(estlist[[ci]],cursquadinfo[[ci]])
}
names(nicetab)=curpremteam

### now write that to an excel file
fileout=paste(USERPATH,'data/weeklyest/expected_minutes_',numericdate(),'.xls',sep='')
if (file.exists(fileout)) file.remove(fileout)
for (ci in 1:length(curpremteam)) {
	### reorder to that you get goalie/def/midfield-striker in order of expected minutes
	tempdf=nicetab[[ci]]
	tempdf=tempdf[order(match(tempdf$pos,c('G','D','MF')),-tempdf$emin),]
	write.xlsx(tempdf,file=fileout,sheetName=curpremteam[ci],row.names=F,append=T)
}
