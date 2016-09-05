##Used for Tobin Heath's Week 1-3 heat maps, but also applies to all PTFC players from that timespan

source("")

#Get list of match csv files for PTFC from Week 1-3
match_list <- getMatchCsvFiles("nwsl-2016", multi_round = c("week-1", "week-2", "week-3"), team = "PTFC")
match_names <- database[!is.na(database[,"match.csv.link"]) & 
                          database[,"round"] %in% c("week-1", "week-2", "week-3") & 
                          (database[,"home.team"] == "PTFC"|database[,"away.team"] == "PTFC"),"matchup"]

#Get match stats
match_stat <- "everything"
stats_list <- createStatsTables()

#Write csv files into your directory
createcsv()