library(RCurl)

# Get match spreadsheet---------
# matchURL, which must be a string, is the URL for the match spreadsheet in tidied .csv format
# test 1: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-crs-052216.csv"
# test 2: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-wnyf-was-042916.csv"
# test 3 (has big chances & key passes): matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-sbfc-041716.csv"

if(!exists("online_mode")){
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/testing/creating-stats-tables.R")
} else if(exists("online_mode") && online_mode == "offline"){
  source("~/wosostats/code/testing/creating-stats-columns")
}

# getting stats from each match csv sheet can take approximately 3 seconds, for now

# I. Get stats for one match
# • You can get stats for one match given one of the following
#   1. "match_up" - the matchup and date in "TEAM1-TEAM2_M/D/YY" format. Must include underscore between team acronyms and date. Date
#       must be exactly as it is in the data column in the database. So, no leading zeroes, and in month/day/year format.
#   2. "match_csv" - if you've got the match csv sheet in your working environment, assign this value to it.
#   3. "filename" - the location of the match csv sheet in your computer
#   4. "matchURL" - the URL link for the match csv sheet in the WoSo Stats GitHub repo, found under in the match.csv.link column in the database
#   5. "location" - if you want your stats broken down by location on the field, and if so by what location. default is "none" and other options are by
#       "thirds" or by "zone".
######
# your_stats <- getStatsForMatch(matchURL=NA, filename=NA, match_csv=NA, matchup=NA, location="none", database=NA)
######
#
#
# II. Get stats for multiple matches! Requires internet connection.
# • Select your matches based on the following.
#   1. "competition.slug" - this is mandatory. the string for the competition exactly as it appears in the competition.slug column in the database.
#       If you're feeling brave (and want to see how long it takes), you can just create stats for the entire database by assigning
#       this value to "database." You'll be sitting there for several minutes.
#   2. "type" - this is mandatory. "stats.csv.link" to get a list of stats tables for each match. 
#       "match.csv.link" to get a list of match actions for each match.
#   3. "team" - optional. the string for the team you're looking for, exactly as it appears in the database.
#   4. "round"- optional. the string for the "round" of the competition (AKA "week") exactly as it appears in the database
#   5. "multi_round" - optional. a vector in c("Week X", "Week Y", "Week Z", "etc.") format with multiple "rounds" of a 
#       competition, again exactly as it appears in the database.
#   6. "month_year" - optional. the string for the month and year of the competition in "M_YYYY" format (no leading zeroes for month).
#   7. "location_complete" - assign this to TRUE if you only want matches with complete location data (will have 
#       "yes" in the location.data column in the database)
######
# your_stats_list <- getStatsInBulk(competition.slug, team=NA, round=NA, multi_round=NA, month_year=NA, location_complete = FALSE)
#######
#
# Then, you can write those csv files in bulk into whatever your working directory is in. 
#  for (index in 1:length(stats_list)) {
#    file_name <- strsplit(matches_names[index], "/")[[1]][[length(strsplit(matches_names[index], "/")[[1]])]]
#    write.csv(stats_list[[index]], file=file_name, row.names = FALSE)
#  }
# }
#
#
# III. Calculate stats for multiple matches. Can be used with the getStatsInBulk function above.
# • 
######
# your_stats <- mergeStatsList(stats_list = your_stats_list, add_per90 = FALSE)
######

