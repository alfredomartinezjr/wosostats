source("abbreviations.R")
source("excel_to_db_functions.R")
library(readr)
matches_tbl <- read_csv("source/temp_database/matches.csv", col_types = cols())
lineups_tbl <- read_csv("source/temp_database/lineups.csv", col_types = cols())

library(readxl)
library(lubridate)
library(dplyr)
TidyMatchExcel <- function(filename, directory="source/excel", 
                           matches=matches_tbl,
                           lineups=lineups_tbl) {
  mypath <- paste(directory, filename, sep="/")
  match_id <- GetMatchId(path = mypath, matches = matches)
  match_source <- ReadMatchLog(path = mypath)
  match_source <- CleanUpCells(match_source)
  for (i in seq_along(match_source$time)) 
    if(is.na(match_source$time[i])) 
      match_source$time[i] <- match_source$time[i-1]
  match_source <- CalcEventValues(match_source = match_source)
  match_source <- ExpandAbbreviations(match_source = match_source)
  mytables_ls <- CreateTables(match_source = match_source, 
                              match_id = match_id)
  mytables_ls <- SetPlayerInfo(mytables_ls = mytables_ls,
                               lineups = lineups_tbl, 
                               match_id = match_id)
  if (matches$location_data[matches$match_id == match_id]) {
    mytables_ls[["defend_events"]]$def_location <- 
      GetDefLocation(mytables_ls = mytables_ls)
  }
  mytables_ls[["events"]]$poss_action[IsCompletedPass(mytables_ls)] <- 
    paste0(mytables_ls[["events"]]$poss_action[IsCompletedPass(mytables_ls)], ".c")
  mytables_ls
}

