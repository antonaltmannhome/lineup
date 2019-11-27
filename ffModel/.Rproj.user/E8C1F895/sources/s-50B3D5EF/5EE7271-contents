ffPath = c(paste0(USERPATH, 'ffDataLoading'),
            paste0(USERPATH, 'ffDataJoining'),
            paste0(USERPATH, 'ffModel'))

for (fi in 1:length(ffPath)) {
  # usethis::create_package(ffPath[fi])
  devtools::install(ffPath[fi])
  unloadNamespace(ffPath[fi])
}
