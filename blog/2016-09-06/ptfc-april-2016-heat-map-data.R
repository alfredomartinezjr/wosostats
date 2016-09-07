##Used for PTFC's Week 1-3 heat maps, but also applies to all PTFC players from that timespan

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


#Creates a blank overall table
d <- do.call("rbind", stats_list)
d <- d[d[,"Team"]=="PTFC",]
players <- unique(d[,c("Player", "Team")])
stats <- as.data.frame(matrix(rep(0, length(names(d))), nrow = 1))
stats <- stats[-1,]
names(stats) <- names(d)
stats$Player <- as.character(stats$Player)
stats$Team <- as.character(stats$Team)
overall <- merge(players, stats, by=c("Player", "Team"), all=TRUE)
#for each row in "overall", gets each column's colSums for that row's "Player"-"Team" combo in d
x <- 1
while(x <= nrow(overall)) {
  sub <- d[d[,"Player"] == overall[x,"Player"] & d[,"Team"] == overall[x,"Team"],]
  overall[x,3:ncol(overall)] <- colSums(sub[,3:ncol(sub)])
  x <- x + 1
}

#Write csv files into your directory
createcsv()