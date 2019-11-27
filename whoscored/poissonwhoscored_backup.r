### if i get whoscored data, want to do poisson, but with different number of minutes played. let's see if we can just use glm with a few tweaks

library(numDeriv)

matchlen=c(90,90,45,20)
ngoal=c(3,1,1,0)
nshot=c(10,7,5,3)
npass=c(2,0,4,5)

poislik=function(theta) {
	mu=exp(theta[1] + theta[2]*nshot)
	loglik=mean(log(dpois(ngoal,mu)))
	return(-loglik)
}
maxinfo=nlm(poislik,p=c(0,0))

### try to reproduce glm output
modsum=data.frame(Est=maxinfo$est, StdErr=0.5*sqrt(diag(solve(hessian(poislik,maxinfo$est)))))
modsum$zval=modsum$Est/modsum$StdErr
modsum$pval=2*pnorm(-abs(modsum$zval),0,1)

mod=glm(ngoal~nshot,family=poisson)

### ok that's the rubbish version, now let's tweak


poislik=function(theta) {
	mu=exp(theta[1] + theta[2]*nshot)*matchlen/90
	loglik=mean(log(dpois(ngoal,mu)))
	return(-loglik)
}
maxinfo=nlm(poislik,p=c(0,0))

nshot2=nshot*90/matchlen
ngoal2=ngoal*90/matchlen

mod=glm(ngoal2~nshot2,family=poisson)
