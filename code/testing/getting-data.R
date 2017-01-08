require(RCurl)
require(plyr)
require(dplyr)

#Gets database of matches from GitHub
database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
database <- read.csv(textConnection(database), stringsAsFactors = FALSE)

#Create function to get certain subset of match csv files from database. Can set a specific "round" or month
#where "month" must be in "M_YYYY" format. Or multi-round can be set as a vector of different "rounds" you want
#such as c("Week-1","Week-2", "Week-3")
getMatchFiles <- function(competition.slug, sheet_type=NA, round=NA, multi_round=NA, month_year=NA, team=NA, location_complete=FALSE) {
  if (sheet_type == "stats") {
    sheet_type_col <- "stats.csv.link"
  } else if (sheet_type == "match_sheet") {
    sheet_type_col <- "match.csv.link"
  }
  
  if(competition.slug == "database"){
    
    if(location_complete == TRUE){
      matches <- database[!is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes",sheet_type_col]
      names <- database[!is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes","matchup"]
    } else {
      matches <- database[!is.na(database[,"match.csv.link"]),sheet_type_col]
      names <- database[!is.na(database[,"match.csv.link"]),"matchup"]
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
      matches <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes",sheet_type_col]
      names <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]) & database[,"location.data"]=="yes","matchup"]
    } else {
      matches <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),sheet_type_col]
      names <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),"matchup"]
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