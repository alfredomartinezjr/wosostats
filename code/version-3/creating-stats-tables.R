library(RCurl)
# About offline mode:
# Trying to do work on a plane & don't want to pay $8 for Wi-Fi? Stuck in a train tunnel?
# Assign "offline" to  online_mode and, assuming you've got the GitHub repo duplicated in
# your working directory, you can just read the files instead of going online.
# Otherwise, if online_mode hasn't been created yet, you'll just source the "functions.R"## file from the GitHub site
if(!exists("online_mode")){
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/testing/creating-stats-columns.R")
} else if(exists("online_mode") && online_mode == "offline"){
  source("~/wosostats/code/testing/creating-stats-columns")
}

getStatsForMatch <- function(matchURL=NA, filename=NA, match_csv=NA, matchup=NA, type="basic", database=NA) {
  if(!is.na(matchURL)) {
    match_sheet <- getURL(matchURL)
    match_sheet <- read.csv(textConnection(match_sheet), stringsAsFactors = FALSE)
  } else if (!is.na(filename)) {
    match_sheet <- read.csv(filename, stringsAsFactors = FALSE)
  } else if (exists("match_csv") && !is.na(match_csv)) {
    match_sheet <- match_csv
  } else if (!is.na(matchup)){
    if(is.na(database)) {
      database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
      database <- read.csv(textConnection(database), stringsAsFactors = FALSE)
    }
    matchURL <- database[database[,"matchup"]==strsplit(matchup,split="_")[[1]][1] & database[,"date"]==strsplit(matchup,split="_")[[1]][2],"match.csv.link"]
    match_sheet <- getURL(matchURL)
    match_sheet <- read.csv(textConnection(match_sheet), stringsAsFactors = FALSE)
  }
  
  if(type == "basic"){
    stats_list <- list()
    stats_list[[1]] <- createPlayersColumns(match_sheet = match_sheet)
    stats_list[[2]] <- createShotsColumns(match_sheet = match_sheet)
    stats_list[[3]] <- createChancesColumns(match_sheet = match_sheet) 
    stats_list[[4]] <- createPassingColumns(match_sheet = match_sheet)
    stats_list[[5]] <- createPassRangeColumns(match_sheet = match_sheet)
    stats_list[[6]] <- createSetPieceColumns(match_sheet = match_sheet)
    stats_list[[7]] <- createPossessionColumns(match_sheet = match_sheet)
    stats_list[[8]] <- createRecoveryColumns(match_sheet = match_sheet)
    stats_list[[9]] <- createAerialColumns(match_sheet = match_sheet)
    stats_list[[10]] <- createDefActionsColumns(match_sheet = match_sheet)
    stats_list[[11]] <- createDisciplineColumns(match_sheet = match_sheet)
    stats_list[[12]] <- createGkDefenseColumns(match_sheet = match_sheet)
    stats_list[[13]] <- createGkDistColumns(match_sheet = match_sheet)
  }
  
  #merge stats list
  for(index in 1:length(stats_list)) {
    if(exists("match_stats")) {
      match_stats <- merge(match_stats, stats_list[[index]], by=c("Player","Team","Number"), all.x=TRUE)
    } else {
      match_stats <- stats_list[[index]]
    }
  }
  
  #clean up match_stats
  match_stats[is.na(match_stats)] <- 0
  names(match_stats) <- gsub(" ",".", names(match_stats))
  match_stats
}

getMatchCsvFiles <- function(competition.slug, team=NA, round=NA, multi_round=NA, month_year=NA, location_complete=FALSE, database=database) {
  if(competition.slug == "database"){
    if(location_complete == TRUE){
      matches <- database[!is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes","match.csv.link"]
      names_matchup <- database[!is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes","matchup"]
      dates_matchup <- database[!is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes","date"]
      names <- paste(names_matchup,dates_matchup,sep = "-")
    } else {
      matches <- database[!is.na(database[,"match.csv.link"]),"match.csv.link"]
      names_matchup <- database[!is.na(database[,"match.csv.link"]),"matchup"]
      dates_matchup <- database[!is.na(database[,"match.csv.link"]),"date"]
      names <- paste(names_matchup,dates_matchup,sep = "-")
    }
  } else {
    if (!is.na(round)){
      database <- database[database[,"round"]==round,]
    } else if(!is.na(month_year)) {
      month <- strsplit(month_year, "_")[[1]][1]
      year <- strsplit(month_year,"_")[[1]][2]
      database <- database[database[,"month"]==month & database[,"year"]==year,]
    } else if(length(multi_round) > 1) {
      database <- database[database[,"round"] %in% multi_round,]
    }
    if(!is.na(team)){
      database <- database[database[,"home.team"] %in% team | database[,"away.team"] %in% team,]
    }
    if(location_complete == TRUE) {
      matches <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes","match.csv.link"]
      names_matchup <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes","matchup"]
      dates_matchup <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes","date"]
      names <- paste(names_matchup,dates_matchup,sep = "-")
    } else {
      matches <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),"match.csv.link"]
      names_matchup <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),"matchup"]
      dates_matchup <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),"date"]
      names <- paste(names_matchup,dates_matchup,sep = "-")
    }
  }
  match_list <- vector("list", 0)
  x <- 1
  while (x <= length(matches)) {
    d <- getURL(matches[x])
    d <- read.csv(textConnection(d), stringsAsFactors = FALSE)
    match_list[[x]] <- d
    x <- x + 1
  }
  assign("match_list",match_list, pos=1)
  assign("match_names", names, pos=1)
}

getStatsInBulk <- function(competition.slug,team=NA, round=NA, multi_round=NA, month_year=NA, location_complete = FALSE) {
  database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
  database <- read.csv(textConnection(database), stringsAsFactors = FALSE)
  
  getMatchCsvFiles(competition.slug=competition.slug, team=team, round=round, multi_round=multi_round, month_year=month_year, location_complete=location_complete, database=database)

  stats_list <- list()
  for (index in 1:length(match_list)) {
    all <- getStatsForMatch(match_csv = match_list[[index]])
    stats_list[[index]] <- all
  }
  
  stats_list
}

#if(writeSheets == TRUE) {
#  #Writes csv files in bulk into whatever directory you're in
#  for (index in 1:length(stats_list)) {
#    file_name <- strsplit(matches_names[index], "/")[[1]][[length(strsplit(matches_names[index], "/")[[1]])]]
#    write.csv(stats_list[[index]], file=file_name, row.names = FALSE)
#  }
#}