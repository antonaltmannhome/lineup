### combine the team spreadsheets into one nice dataframe
source('c:/research/general_startup.r')
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(lubridate))
suppressWarnings(library(readr))
USERPATH='c:/git/lineup/'
DATAPATH='d:/whoscored_data/'
TEMPPATH='c:/temp/'
setwd(USERPATH)
source('admin_funct.r')

currentseason = 2021

options(warn=2, dplyr.print_max = 1e9)

seasonInfoDF = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''))
