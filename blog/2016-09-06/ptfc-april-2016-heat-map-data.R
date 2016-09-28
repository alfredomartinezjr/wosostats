##Used for PTFC's Week 1-3 heat maps, but also applies to all PTFC players from that timespan

source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/getting-data.R")
source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/create-location-stats-table.R")

#Get list of match csv files for PTFC from Week 1-3
getMatchCsvFiles("nwsl-2016", multi_round = c("week-1", "week-2", "week-3"), team = "PTFC", location_complete = TRUE)

#Create stats table for each match
stats_list <- createMultiLocStatsTabs(match_list, "everything")

#Write csv files into your directory
writeFiles(stats_list, match_names = match_names, match_stat = "everything")