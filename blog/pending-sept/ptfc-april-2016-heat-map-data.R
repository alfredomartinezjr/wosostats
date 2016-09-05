##Used for Tobin Heath's Week 1-3 heat maps, but also applies to all PTFC players from that timespan

source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/create-heat-maps-data.R")

#Get list of match csv files for PTFC from Week 1-3
match_list <- getMatchCsvFiles("nwsl-2016", multi_round = c("week-1", "week-2", "week-3"), team = "PTFC")
match_names <- database[!is.na(database[,"match.csv.link"]) & 
                          database[,"round"] %in% c("week-1", "week-2", "week-3") & 
                          (database[,"home.team"] == "PTFC"|database[,"away.team"] == "PTFC"),"matchup"]


#Create stats table for each match
match_stat <- "everything"
stats_list <- vector("list", 0)
#For every match csv file in match_list, create a stats table
for (matchSheet in match_list){
  df <- matchSheet
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/create-location-stats-table.R")
  stats_list[[length(stats_list)+1]] <- stats
  rm(i, stats, df, matchSheet)
}

#Write csv files into your directory
createcsv()