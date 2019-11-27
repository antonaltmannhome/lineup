### no, the non-poisson bit must be a problem, the total goals predictions are quite a way out, and that's a problem for what we're doing

message('go to https://www.oddschecker.com/football/english/premier-league, then press enter')
dum=scan(what='',quiet=T,nmax=1)
fileout=paste(TEMPPATH,'oddschecker.txt',sep='')
if (file.exists(fileout)) file.remove(fileout)
initfile()
insertselectwindow(oddscheckerBrowser)
selectalltonotepad(file=fileout)
insertselectwindow('R')
insertabort()
runscript()

b = tolower(scan(fileout, '', sep='\n', quiet = TRUE))
alloddsindex = grep('^all odds$', b)
ht = b[alloddsindex-9]
at = b[alloddsindex-8]
hwinodds = b[alloddsindex-6]
drawodds = b[alloddsindex-4]
awinodds = b[alloddsindex-2]

allgamedf = tibble(ht = ht, at = at, hwinodds = hwinodds, drawodds = drawodds, awinodds = awinodds)

### this includes all game,s just get user to select current ones firstly
print('what is the row number of the last current game?')
print(allgamedf)
finalgamerownumber = askcond(T, F)

currentgamedf = allgamedf[1:finalgamerownumber,]

print('do all games have the wdl streak of length 5 at the end of team names (y/n)?')
dum = askcond(F, F)
if (dum == 'n') {
	stop('Have not written code for this scenario, start editing strip_oddschecker!\n')
}

currentgamedf$ht = with(currentgamedf, substr(ht, 1, nchar(ht) - 5))
currentgamedf$at = with(currentgamedf, substr(at, 1, nchar(at) - 5))

# then convert odds to probabilities and take out overround
oddstoprob = function(myodds) {
	lhs = as.numeric(gsub('/.+$', '', myodds))
	rhs = as.numeric(gsub('^.+/', '', myodds))
	prob = rhs / (lhs + rhs)
	return(prob)
}
	
currentgamedf$hwinprob = with(currentgamedf, oddstoprob(hwinodds))
currentgamedf$drawprob = with(currentgamedf, oddstoprob(drawodds))
currentgamedf$awinprob = with(currentgamedf, oddstoprob(awinodds))

currentgamedf$totalprob = with(currentgamedf, hwinprob + drawprob + awinprob)
currentgamedf = currentgamedf %>%
			mutate(hwinprob = hwinprob / totalprob,
					awinprob = awinprob / totalprob,
					drawprob = drawprob / totalprob)

# now the tricky bit, converting that to expected goals
suptottoprob = function(lam, mu) {
	scoreprobmat = dpois(0:10, lam) %*% t(dpois(0:10, mu))
	hwinprob = sum(scoreprobmat * lower.tri(scoreprobmat))
	drawprob = sum(diag(scoreprobmat))
	awinprob = sum(scoreprobmat * upper.tri(scoreprobmat))

	return(c(hwinprob, drawprob, awinprob))
}
	
tryouttheta = function(theta,truehda) {
	lam = exp(theta[1])
	mu = exp(theta[2])
	candidatehda = suptottoprob(lam, mu)
	sqdiff = sum( (candidatehda - truehda)^2)
	return(sqdiff)
}
	
getlammu = function(truehwin, truedraw, trueawin) {
	
	truehda = c(truehwin, truedraw, trueawin)
	maxinfo = nlm(tryouttheta, p = c(log(1.5), log(1)), truehda = truehda)
	lam = exp(maxinfo$est[1])
	mu = exp(maxinfo$est[2])
	return(data.frame(lam = lam, mu = mu))
}

dum = currentgamedf %>%
			rowwise() %>%
			do(getlammu(.$hwinprob, .$drawprob, .$awinprob))

currentgamedf = cbind(currentgamedf, dum)
