source('c:/research/lineup/ff_startup.r')
source(paste(USERPATH,'data fetching/whoscored_funct.r',sep=''))
source(paste(USERPATH,'player_funct.r',sep=''))

datetouse = 1819
beforeseason = FALSE
ishistoric = TRUE

allfiletype=c('summary','shotzone','shotsit','shotacc','goalsit','keypass')
teamdf = read.csv(paste(DATAPATH,'/teampage.csv',sep=''))

# then make the 1819 folder in whoscored_data. open it in atom

## now do the following for the three promoted teams:

# navigate to their main page on whoscored.
# click on 'historic' tab
# select-all/copy/paste/save to d:/whoscored_data/1819/sheffieldutd_summary.txt
# then detailed / accum -> total -> select-all/copy/paste/save to sheffieldutd_shotzone.txt
# then subcat: situations -> select-all/copy/paste/save to sheffieldutd_shotsit.txt
# then subcat: accuracy -> select-all/copy/paste/save to sheffieldutd_shotacc.txt
# then cat: goals / subcat: situations: -> select-all/copy/paste/save to sheffieldutd_goalsit.txt
# then cat: keypass: -> select-all/copy/paste/save to sheffieldutd_keypass.txt

combineteamfile(datetouse, beforeseason = FALSE, ishistoric = TRUE)

allteamdata = read.csv(paste0(DATAPATH, datetouse, '/combined_data.csv'),as.is=T)
allteamdata=processdeserved(allteamdata)
fileout=paste(DATAPATH,datetouse,'/model.csv',sep = '')
write.csv(file=fileout, allteamdata, row.names=FALSE)
