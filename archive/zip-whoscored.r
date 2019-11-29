### let's move all the whoscore stripped files into a single zipped file instead
# two stages to this:
# 1. zipping all the whoscored files up, then erasing orignial copies
# 2. putting the 'combined' and active player files into their own directory
# we'll do this when we actually have new data:
# 3. making the data fetching process do this:
  # 1 download all whoscored files into a temporary place
  # 2 add them to the single zip file that resides on dropbox
  # 3 put the new combined file into the same directory as above

# for fun, current size of whoscored dir is: 1.75GB
# hmm, only reduced it to 1.63GB

dum = list.files('d:/whoscored_data/')
allWsDir = dum[grep('^[0-9]{8}$', dum)]

setwd('d:/dropbox/whoscored_data')
for (j in 1:length(allWsDir)) {
  currentYear = substr(allWsDir[j], 1, 4)
  targetZipFile = paste0('d:/whoscored_data/whoscored', currentYear, '.7z')
  myCommand = paste0('7z a ', targetZipFile, ' ', allWsDir[j], '\\*.txt')
  system(myCommand)
  if (FALSE) {
    allFileInDir = list.files(allWsDir[j])
    fileToDelete = allFileInDir[grep('\\.txt$', allFileInDir)]
    for (k in fileToDelete) {
      file.remove(paste0(allWsDir[j], '/', k))
    }
  }
  print(allWsDir[j])
}

# now we'll put all the combined files etc into their own directory
setwd('d:/whoscored_data')
outputdir = 'summarised_whoscored_data/'
for (j in 1:length(allWsDir)) {
  fileToMove = list.files(allWsDir[j])
  newName = paste0(outputdir, gsub('(^.+)(\\..+$)', '\\1', fileToMove), '_', allWsDir[j], gsub('(^.+)(\\..+$)', '\\2', fileToMove))
  for (k in 1:length(fileToMove)) {
    file.rename(paste(allWsDir[j], fileToMove[k], sep = '/'), newName[k])
  }
}

# check they're all now empty
for (j in 1:length(allWsDir)) {
  dum = list.files(allWsDir[j])
  print(length(dum))
}
