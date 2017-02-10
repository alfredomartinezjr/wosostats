library(RCurl)

# Get match spreadsheet---------
# matchURL, which must be a string, is the URL for the match spreadsheet in tidied .csv format
# test 1: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-crs-052216.csv"
# test 2: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-wnyf-was-042916.csv"
# test 3 (has big chances & key passes): matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-sbfc-041716.csv"

if(!exists("match_sheet")){
  match_sheet <- getURL(matchURL)
  match_sheet <- read.csv(textConnection(match_sheet), stringsAsFactors = FALSE)
}

 all_players <- createPlayersColumns()
 all_shots <- createShotsColumns(type="legacy")
 all_stats <- merge(all_players, all_shots, by=c("Player","Team","Number"), all.x=TRUE)
 all_keyPasses <- createkeyPassesColumns()
 all_stats <- merge(all_stats, all_keyPasses, by=c("Player","Team","Number"), all.x=TRUE)
 all_bigChances <- createChancesColumns()
 all_stats <- merge(all_stats, all_bigChances, by=c("Player","Team","Number"), all.x=TRUE)
 all_passes <- createPassingColumns()
 all_stats <- merge(all_stats, all_passes, by=c("Player","Team","Number"), all.x=TRUE)
 all_passesByDirection <- createPassingDirectionColumns()
 all_stats <- merge(all_stats, all_passesByDirection, by=c("Player","Team","Number"), all.x=TRUE)
 all_passesByLocation <- createPassingLocationColumns()
 all_stats <- merge(all_stats, all_passesByLocation, by=c("Player","Team","Number"), all.x=TRUE)
 all_passesByRange <- createPassRangeColumns()
 all_stats <- merge(all_stats, all_passesByRange, by=c("Player","Team","Number"), all.x=TRUE)
 all_specialPasses <- createSpecialPassColumns()
 all_stats <- merge(all_stats, all_specialPasses, by=c("Player","Team","Number"), all.x=TRUE)
 all_setPieces <- createSetPieceColumns()
 all_stats <- merge(all_stats, all_setPieces, by=c("Player","Team","Number"), all.x=TRUE)
 all_possessions <- createPossessionColumns()
 all_stats <- merge(all_stats, all_possessions, by=c("Player","Team","Number"), all.x=TRUE)
 all_recoveries <- createRecoveryColumns()
 all_stats <- merge(all_stats, all_recoveries, by=c("Player","Team","Number"), all.x=TRUE)
 all_aerialDuels <- createAerialColumns()
 all_stats <- merge(all_stats, all_aerialDuels, by=c("Player","Team","Number"), all.x=TRUE)
 all_tackles <- createTacklesColumns()
 all_stats <- merge(all_stats, all_tackles, by=c("Player","Team","Number"), all.x=TRUE)
 all_balldisrupt <- createBallDisruptionColumns()
 all_stats <- merge(all_stats, all_balldisrupt, by=c("Player","Team","Number"), all.x=TRUE)
 all_gkshots <- createGkSogColumns()
 all_stats <- merge(all_stats, all_gkshots, by=c("Player","Team","Number"), all.x=TRUE)
 all_highballs <- createGkHighBallColumns()
 all_stats <- merge(all_stats, all_highballs, by=c("Player","Team","Number"), all.x=TRUE)
 all_smothers <- createGkSmotherColumns()
 all_stats <- merge(all_stats, all_smothers, by=c("Player","Team","Number"), all.x=TRUE)
all_gkPasses <- createGkDistColumns()
all_stats <- merge(all_stats, all_gkPasses, by=c("Player","Team","Number"), all.x=TRUE)


#CLEANING UP TABLE----------
all_stats[is.na(all_stats)] <- 0
names(all_stats) <- gsub(" ",".", names(all_stats))
