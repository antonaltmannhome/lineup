### combine the team spreadsheets into one nice dataframe
source('c:/research/general_startup.r')
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(lubridate))
suppressWarnings(library(readr))

# SQLPATH='c:/temp/sqlscript/'
AHKPATH='c:/research/utils/autohotkeys/'
source(paste(AHKPATH, 'ahkfunct.r', sep = ''))
USERPATH='c:/git/lineup/'
DATAPATH='d:/whoscored_data/'
TEMPPATH='c:/temp/'
setwd(USERPATH)
source('admin_funct.r')
source('team_funct.r')
source('player_funct.r')
thiscomputer = Sys.info()[['nodename']]

seasoninfo = read.csv(paste(DATAPATH,'seasoninfo.csv',sep=''))
currentseason = 1920

options(warn=2, dplyr.print_max = 1e9)
