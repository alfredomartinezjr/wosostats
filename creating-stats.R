library(RCurl)

# learn how to use this by reading this: https://github.com/amj2012/wosostats/blob/master/code/version-3/creating-stats-how-to.md

# Sample match sheets
# Sample 1: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-crs-052216.csv"
# Sample 2: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-wnyf-was-042916.csv"

if(!exists("online_mode")){
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-3/creating-stats-tables.R")
} else if(exists("online_mode") && online_mode == "online") {
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-3/creating-stats-tables.R")
} else if(exists("online_mode") && online_mode == "offline"){
  source("~/wosostats/code/version-3/creating-stats-columns.R")
}

# your_stats <- getStatsForMatch(matchup=NA, match_csv=NA, filename=NA, matchURL=NA, location="none", per90=FALSE, database=NA)
#
# your_stats_list <- getStatsInBulk(competition.slug, type, team=NA, round=NA, multi_round=NA, month_year=NA, location="none", location_complete = FALSE, per90=FALSE)
#
# getMatchFiles(competition.slug = "nwsl-2016", type="stats.csv.link")
#
# for (index in 1:length(your_stats_list)) {
#   file_name <- paste0(gsub("/","",match_names[index]),".csv")
#   write.csv(your_stats_list[[index]], file=file_name, row.names = FALSE)
# }
#
# your_stats <- mergeStatsList(stats_list = your_stats_list, add_per90 = TRUE, location = "none")
#