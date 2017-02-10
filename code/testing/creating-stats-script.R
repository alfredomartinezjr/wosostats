library(RCurl)

# Get match spreadsheet---------
# matchURL, which must be a string, is the URL for the match spreadsheet in tidied .csv format
# test 1: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-crs-052216.csv"
# test 2: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-wnyf-was-042916.csv"
# test 3 (has big chances & key passes): matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-sbfc-041716.csv"

if(!exists("online_mode")){
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/testing/creating-stats-base-functions.R")
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/testing/creating-stats-columns.R")
} else if(exists("online_mode") && online_mode == "offline"){
  source("~/wosostats/code/testing/creating-stats-base-functions.R")
  source("~/wosostats/code/testing/creating-stats-base-columns")
}

if(!exists("match_sheet")){
  match_sheet <- getURL(matchURL)
  match_sheet <- read.csv(textConnection(match_sheet), stringsAsFactors = FALSE)
}

stats_list <- list()
stats_list[[1]] <-  createPlayersColumns()
stats_list[[2]] <- createShotsColumns()
stats_list[[3]] <- createKeyPassesColumns() #this takes too long
stats_list[[4]] <- createChancesColumns() 
stats_list[[5]] <- createPassingColumns() #this takes too long
stats_list[[6]] <- createPassingDirectionColumns() #this takes too long
stats_list[[7]] <- createPassingLocationColumns() #this takes too long
stats_list[[8]] <- createPassRangeColumns()
stats_list[[9]] <- createSpecialPassColumns() #this takes too long
stats_list[[10]] <- createSetPieceColumns() #this takes too long
stats_list[[11]] <- createPossessionColumns()
stats_list[[12]] <- createRecoveryColumns()
stats_list[[13]] <- createAerialColumns()
stats_list[[14]] <- createTacklesColumns()
stats_list[[15]] <- createBallDisruptionColumns()
stats_list[[16]] <- createGkSogColumns()
stats_list[[17]] <- createGkHighBallColumns()
stats_list[[18]] <- createGkSmotherColumns()
stats_list[[19]] <- createGkDistColumns()

for(index in 1:length(stats_list)) {
  if(exists("match_stats")) {
    match_stats <- merge(match_stats, stats_list[[index]], by=c("Player","Team","Number"), all.x=TRUE)
  } else {
    match_stats <- stats_list[[index]]
  }
}


#CLEANING UP TABLE----------
match_stats[is.na(match_stats)] <- 0
names(match_stats) <- gsub(" ",".", names(match_stats))
