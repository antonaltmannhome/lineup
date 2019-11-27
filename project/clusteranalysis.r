### some cluster analysis of fantasy footie data

source('c:/research/lineup/ff_startup.r')
source('model_startup.r')

#sax=with(summarydf, which(season==1718 & minute > 300 & ffposition != 'g'))
sax=with(summarydf, which(season==1718 & minute > 300))
validcolumn = c('shotoob', 'shotib', 'openplay', 'offt', 'ont', 'block', 'shortkp')
myplayer=data.frame(summarydf[sax,validcolumn])
rownames(myplayer)=with(summarydf[sax,], paste(player, team))

### now standardise the variables
medians = apply(myplayer,2,median)
mads = apply(myplayer,2,mad)
scaledplayer = scale(myplayer,center=medians,scale=mads)

player.dist = dist(scaledplayer)
player.hclust = hclust(player.dist)

groups.3 = cutree(player.hclust,3)

# this adds nothing
# don't know, that's on the right track, but we should get rid of players without enough appearances
# also, it should really be proportion of the total that you get, not outright, otherwise AMs for big teams look like strikers for weak teams
## but you could do it within a team...?


#sax=with(summarydf, which(season==1718 & minute > 300 & ffposition != 'g'))
sax=with(summarydf, which(team=='liverpool' & season == 1718 & minute > 300))
validcolumn = c('shotoob', 'shotib', 'openplay', 'offt', 'ont', 'block', 'shortkp')
myplayer=data.frame(summarydf[sax,validcolumn])
rownames(myplayer)=with(summarydf[sax,], player)

### now standardise the variables
medians = apply(myplayer,2,median)
mads = apply(myplayer,2,mad)
scaledplayer = scale(myplayer,center=medians,scale=mads)

player.dist = dist(scaledplayer)
player.hclust = hclust(player.dist)

plot(player.hclust)
groups.3 = cutree(player.hclust,3)

### starting to look sensible. but it mixes up attacking defenders with AMs. unfortunately we haven't stored any defensive stats so this is a bit of a problem
