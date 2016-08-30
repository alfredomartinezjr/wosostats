require(readxl)
require(RCurl)
require(plyr)
require(dplyr)

database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
database <- read.csv(textConnection(database), stringsAsFactors = FALSE)

#Create function to get certain subset of match csv files
getMatchCsvFiles <- function(competition.slug, round=NA) {
  if(competition.slug == "database"){
    matches <- database[!is.na(database[,"match.csv.link"]),"match.csv.link"]
    names <- database[!is.na(database[,"match.csv.link"]),"matchup"]
  } else {
    if (!is.na(round)){
      database <- database[database[,"round"]==round,]
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

#Get list of match csv files for Week 1
match_list <- getMatchCsvFiles("nwsl-2016", round="week-1")
match_names <- database[!is.na(database[,"match.csv.link"]) & database[,"round"]=="week-1","matchup"]

#Create heat maps for these specific stats:
# 1. Attempted pases (attempted-passes)
# 2. Completed passes (completed-passes)
# 3. Passing completion percentage (pass-comp-pct)
# 4. Interceptions (interceptions)
# 5. Take ons won (take-ons-won)
# 6. Take ons lost (take-ons-lost)
# 7. Aerial duels won (aerial_duels-won)
# 8. Aerial duels lost (aerial-duels-lost)
# 9. Tackles (tackles)
# 10. Pressure/Challenges (pressure)
# 11. Recoveries (recoveries)

#Set the stat you want as "match_stat."
#"match_stat" MUST be written exactly as in the list above in
#the parentheses, and it MUST be one of the eight stats listed.
#For now, haven't yet figured out how to create heat maps for
#other stats.
match_stat <- "everything"

#For every match csv file in match_list, create a stats table
stats_list <- vector("list", 0)
for (i in match_list){
  df <- i
  source("/Users/alfredo/wosostats/code/version-2/create-location-stats-table.R")
  stats_list[[length(stats_list)+1]] <- stats
}

##CREATES CSV FILE FOR STATS TABLE
createcsv <- function(name) {
  for (i in 1:length(stats_list)) {
    file_name <- paste0(match_names[i],"-",match_stat,".csv")
    write.csv(stats_list[[i]], file=file_name, row.names = FALSE)
  }
}