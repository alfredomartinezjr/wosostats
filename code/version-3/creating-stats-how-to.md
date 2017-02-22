# Intro
The creating-stats R files in this folder will create stats tables for the WoSo matches [in our database](https://github.com/amj2012/wosostats/blob/master/database.csv). You will be able to create stats for matches based on criteria you can set in the R functions in the creating-stats.R files, and the stats can be either without location data, or broken down by thirds of the field or by ["zones" of the field](https://camo.githubusercontent.com/7df8218c1603fee2ffd1ea25cfa1d30f4572c6ae/687474703a2f2f692e696d6775722e636f6d2f45514c6d7059702e706e67). Unless you're interested in delving into the nitty-gritty of how the different code files work to further tailor the type of stats you get, you only need to use the creating-stats.R file.

# I. Sample match sheets
The following are matches you can use to quickly get started and get yourself acquainted with how the functions below work.
* **Sample 1**: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-crs-052216.csv"
* **Sample 2**: matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-wnyf-was-042916.csv"
* **Sample 3** (has big chances & key passes): matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/nwsl-2016/nwsl-2016-srfc-sbfc-041716.csv"

# II. Determining file location
Most of you can ignore this, unless you've created a copy of this WoSo Stats repo in your computer and would like to run this code while away from an internet connection, such as on a plane.

In which case, assign "offline" to "online_mode." 

`online_mode <- "offline"`

Otherwise, don't do anything and you'll just source everything from online.

# 1. Get stats for one match
* Uses the `getStatsForMatch` function.
* Given a match (as a match up string, a data frame in your working environment, a filename in your working directory, or a URL to a match .csv file), you can calculate stats from that match spreadsheet.
* Additional arguments ("location" and "per_90") can be used to further customize the type of stats table you create.
* Getting stats from each match csv sheet can take approximately 3 seconds, for now, and it can take longer if you ask for stats broken down by location zones.
* You can get stats for one match with the getStatsForMatch function based on the following arguments:
   1. `match_up`   - the matchup and date in "TEAM1-TEAM2_M/D/YY" format. Must include underscore between team acronyms and date. Date must be exactly as it is in the data column in the database. So, no leading zeroes, and in month/day/year format.
   2. `match_csv`  - OR, if you've got the match csv sheet in your working environment, assign that name to this.
   3. `filename`   - OR, the location of the match csv sheet in your computer
   4. `matchURL`   - OR, the URL link for the match csv sheet in the WoSo Stats GitHub repo, found under in the match.csv.link column in the database
   5. `location`   - this is optional. if you want your stats broken down by location on the field, and if so by what location. default is "none". other options are by "thirds" or by "zone".
   6. `per_90`     - logical (TRUE or FALSE). this is optional and set to FALSE by default. set this as TRUE if you want "per 90" stats added to your stats table.
   7. `database`   - this is optional. most of the time, leave this alone, unless you know what you're doing and have a database spreadsheet different from what's in the WoSo Stats GitHub repo
* Run this, with the above arguments filled in, to get your stats:
  * `your_stats <- getStatsForMatch(matchup=NA, match_csv=NA, filename=NA, matchURL=NA, location="none", per90=FALSE, database=NA)`

# 2. Get match stats for multiple matches
* Uses the getStatsInBulk function.
* Given criteria for the competition, and additional information such as the teams, rounds, or dates, you'll calculate stats for multiple matches and get all those stats as a list.
* You can also, as with getStatsForMatch, further customize your stats with additional arguments (location
* Requires an internet connection
* Select your matches with the getStatsInBulk function based on the following arguments:
   1. `competition.slug`   - this is mandatory. the string for the competition exactly as it appears in the competition.slug column in the database. If you're feeling brave (and want to see how long it takes), you can just create stats for the entire database by assigning this value to "database." You'll be sitting there for several minutes, at least.
   3. `team`               - optional. the string for the team you're looking for, exactly as it appears in the database.
   4. `round`              - optional. the string for the "round" of the competition (AKA "week"), exactly as it appears in the database
   5. `multi_round`        - optional. a vector in c("Week X", "Week Y", "Week Z", "etc.") format with multiple "rounds" of a competition, again exactly as they appear in the database.
   6. `month_year`         - optional. the string for the month and year of the competition in "M_YYYY" format (no leading zeroes for month).
   7. `location_complete`  - optional. assign TRUE to this if you only want to calculate stats for matches with complete location data (will have "yes" in the location.data column in the database)
   8. `location`            - optional. different from `location_complete`. Assign "zones" to this if you want a stats tables with stats broken down by zones of the field, or assign "thirds" to this if you want a stats table with stas broken down by thirds of the field.
   9. `per_90`             - optional. assign TRUE to this you want to add "per 90" stats to your stats table 
   10. `database`           - this is optional. most of the time, leave this alone, unless you know what you're doing and have a database spreadsheet different from what's in the WoSo Stats GitHub repo
* Run this, with the above arguments filled in:
  * `your_stats_list <- getStatsInBulk(competition.slug, team=NA, round=NA, multi_round=NA, month_year=NA, location="none", location_complete = FALSE, per_90=FALSE)`
* Then, you can write those stats .csv files in bulk into whatever your working directory is by running the following:
  * `for (index in 1:length(stats_list)) {
     file_name <- strsplit(matches_names[index], "/")[[1]][[length(strsplit(matches_names[index], "/")[[1]])]]
     write.csv(stats_list[[index]], file=file_name, row.names = FALSE)
   }`

# 3. Calculate stats for multiple matches.
* Use the getStatsInBulk function above, with the type argument set as "match_csv.link", to get a list of stats tables
#   for the set of matches you want (i.e. only Portland matches, only matches from first 10 weeks, etc.)
# • Combine those stats into one table with the mergeStatsList function based on the following arguments:
#   1. "stats_list"   - this is mandatory. assign this to the name of the stat list you created with the getStatsInBulk function. In
#                       the sample code above, we named it your_stats_list.
#   2. "add_per90"    - optional. set as TRUE if you want to add "per 90" stats to the table. Will do this automatically if there are
#                       already "per 90" columns in the stats tables in the stats_list list.
#   3. "location"     - optional, but MUST be set as the same as what was used in getStatsInBulk. Set this as "thirds" if the stats
#                       tables in the stats list are broken down by thirds of the field, and set as "zones" if they are broken down 
#                       by zones of the field. If you didn't do anything with "location" in getStatsInBulk, then you can leave this blank.
# • Run this, with the above arguments filled in:
#                     your_stats <- mergeStatsList(stats_list = your_stats_list, add_per90 = FALSE, location = "none")

