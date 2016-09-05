require(readxl)
require(RCurl)
require(plyr)
require(dplyr)

#Gets database of matches from GitHub
database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
database <- read.csv(textConnection(database), stringsAsFactors = FALSE)

#Create function to get certain subset of match csv files from database. Can set a specific "round" or month
#where "month" must be in "M_YYYY" format. Or multi-round can be set as a vector of different "rounds" you want
#such as c("Week-1","Week-2", "Week-3")
getMatchCsvFiles <- function(competition.slug, round=NA, multi_round=NA, month_year=NA, team=NA) {
  if(competition.slug == "database"){
    matches <- database[!is.na(database[,"match.csv.link"]),"match.csv.link"]
    names <- database[!is.na(database[,"match.csv.link"]),"matchup"]
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
    matches <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),"match.csv.link"]
    names <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),"matchup"]
  }
  match_list <- vector("list", 0)
  x <- 1
  while (x <= length(matches)) {
    d <- getURL(matches[x])
    d <- read.csv(textConnection(d), stringsAsFactors = FALSE)
    match_list[[x]] <- d
    x <- x + 1
  }
  match_list
}

##CREATES CSV FILE FOR STATS TABLE
createcsv <- function(name) {
  for (i in 1:length(stats_list)) {
    file_name <- paste0(match_names[i],"-",match_stat,".csv")
    write.csv(stats_list[[i]], file=file_name, row.names = FALSE)
  }
}