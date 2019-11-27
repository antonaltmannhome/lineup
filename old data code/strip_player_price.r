### want to get player names off web and match them to modeldf

getplayerprice=function() {
rawdata=scan('https://fantasy.premierleague.com/player-list/','',sep='\n',quiet=T,encoding='UTF-8')
rawdata=tolower(rawdata)
### get positional markers
allposition=c('goalkeepers','defenders','midfielders','forwards')
positiongrep=rep(NA,length(allposition))
for (j in 1:length(allposition)) positiongrep[j]=grep(allposition[j],rawdata)

pricegrep=grep('<td>.+[0-9]+\\.[0-9]',rawdata)

price=gsub('(^.+)([0-9]+\\.[0-9])(.+$)','\\2',rawdata[pricegrep])
team=gsub(' ','',gsub('(^.+>)([^<]+)(<.+$)','\\2',rawdata[pricegrep-2]))
team=matchteam(team,'premierleague')
player=gsub('(^.+>)([^<]+)(<.+$)','\\2',rawdata[pricegrep-3])
player=iconv(player,from='UTF-8',to='ascii//translit')
player=gsub('\'','',player)
player=gsub('-',' ',player)
position=allposition[findInterval(pricegrep,positiongrep)]

pricedf=data.frame(team=team,player=player,price=price,position=position)

### awesome, only problem now is matching up player names to model of course

mostrecentdir=getmostrecentdir(mysource='whoscored')
model=read.csv(paste(DATAPATH,'/',mostrecentdir,'/model_',mostrecentdir,'.csv',sep=''),as.is=T)
model$surname=gsub('^[^ ]+ ','',tolower(model$player))
### now try to match, mark exceptions
modeltopricemap=match(paste(model$team,model$surname),paste(pricedf$team,pricedf$player))

### match up the ones we can, the others it's a little tricky
playerabbn=read.csv(paste(DATAPATH,'player_abbn.csv',sep=''))
playerabbn=playerabbn[playerabbn$source=='premierleague',]
sax=which(is.na(modeltopricemap))
## this is weird, we match up full names here even though we matched just surname earlier
model$surname[sax]=playerabbn$wrongname[match(paste(model$team[sax],model$player[sax]),paste(playerabbn$team,playerabbn$correctname))]

## then recalculate this
modeltopricemap=match(paste(model$team,model$surname),paste(pricedf$team,pricedf$player))

model$price=pricedf[modeltopricemap,'price']
### let's have the official position too
model$ffposition=pricedf[modeltopricemap,'position']

return(model)
}
