library(RCurl)
# About offline mode:
# Trying to do work on a plane & don't want to pay $8 for Wi-Fi? Stuck in a train tunnel?
# Assign "offline" to  online_mode and, assuming you've got the GitHub repo duplicated in
# your working directory, you can just read the files instead of going online.
# Otherwise, if online_mode hasn't been created yet, you'll just source the "functions.R"## file from the GitHub site
if(!exists("online_mode")){
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-3/creating-stats-columns.R")
} else if(exists("online_mode") && online_mode == "offline"){
  source("~/wosostats/code/version-3/creating-stats-columns")
}

getStatsForMatch <- function(matchURL=NA, filename=NA, match_csv=NA, matchup=NA, location = "none", database=NA, per90=FALSE) {
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
  
  if(location=="zones") {
    match_sheet <- addMultiColumnsForQualifiers(patterns = c("D6"="D6", "D18"="D18", "DL"="D3L|DL", "DC"="D3C|DC","DR"="D3R|DR", 
                                                             "DML"="DM3L|DML", "DMC"="DM3C|DMC", "DMR"="DM3R|DMR", "AML"="AM3L|AML",
                                                             "AMC"="AM3C|AMC", "AMR"="AM3R|AMR", "AL"="A3L|AL", "AC"="A3C|AC", "AR"="A3R|AR",
                                                             "A18"="A18", "A6"="A6"),
                                                ogdf = match_sheet, ndf = match_sheet,
                                                pattern_locations = "poss.location")
  } else if(location=="thirds"){
    match_sheet <- addMultiColumnsForQualifiers(patterns = c("D3"="D6|D18|D3L|DL|D3C|DC|D3R|DR", "M3"="M", "A3"="A6|A18|A3L|AL|A3C|AC|A3R|AR"),
                                                ogdf = match_sheet,ndf = match_sheet, pattern_locations = "poss.location")
  }
  if(location == "none"){
    stats_list <- list()
    stats_list[[length(stats_list)+1]] <- createPlayersColumns(match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createShotsColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createChancesColumns(location=location, match_sheet = match_sheet) 
    stats_list[[length(stats_list)+1]] <- createPassingColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createSetPieceColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createPossessionColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createRecoveryColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createAerialColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createDefActionsColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createDisciplineColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createGkDefenseColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createGkDistColumns(location=location, match_sheet = match_sheet)
  } else if (location == "thirds" | location=="zones") {
    stats_list <- list()
    stats_list[[length(stats_list)+1]] <- createPlayersColumns(match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createShotsColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createChancesColumns(location=location, match_sheet = match_sheet) 
    stats_list[[length(stats_list)+1]] <- createPassingColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createPassRangeColumns(match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createSetPieceColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createPossessionColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createRecoveryColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createAerialColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createDefActionsColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createDisciplineColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createGkDefenseColumns(location=location, match_sheet = match_sheet)
    stats_list[[length(stats_list)+1]] <- createGkDistColumns(location=location, match_sheet = match_sheet)
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
  
  if(per90==TRUE) {
    #calculate p90 columns
    colnamesForp90 <- grep("Player|Team|Number|^GP$|^MP$|^GS$|[Pp]ct|[Aa]ccuracy|rFreq|GperSOG|GperBCSOG", colnames(match_stats),invert = TRUE)
    match_stats[,paste0(names(match_stats[,colnamesForp90]),".per.90")] <- (match_stats[,colnamesForp90]/match_stats$MP)*90
  }
  
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