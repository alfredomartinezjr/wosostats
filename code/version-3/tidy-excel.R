## Running this code takes an Excel file in your directory for a raw, unedited match actions 
## spreadsheet and turns it into a data.frame that can be read by the creating-stats.R code

# Install packages if necessary--------
require(RCurl)
require(R6)

# Offline mode--------
# Trying to do work on a plane & don't want to pay $8 for Wi-Fi? Stuck in a train tunnel?
# Assign "offline" to  online_mode and, assuming you've got the GitHub repo duplicated in
# your working directory, you can just read the files instead of going online.
# Otherwise, if online_mode hasn't been created yet, "online" is assigned to online_mode
if(!exists("online_mode")){
  online_mode <- "online"
}

# Internal files to include--------
if(online_mode == "online") {
  base_directory = "https://raw.githubusercontent.com/amj2012/wosostats/master/"
  code_directory = paste(base_directory, "code/version-3/", sep="")
  source(paste(code_directory, "abbreviations.R", sep=""))
  source(paste(code_directory, "tidy-excel-functions.R", sep=""))
  rosters <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/rosters/nwsl-2016.csv")
  rosters <- read.csv(textConnection(rosters), stringsAsFactors = FALSE)
  rm(base_directory, code_directory)
} else if(online_mode == "offline") {
  source("~/wosostats/code/version-3/abbreviations.R")
  source("~/wosostats/code/version-3/tidy-excel-functions.R")
  rosters <- read.csv("~/wosostats/rosters/nwsl-2016.csv")
}

# will return a "tidied_list" list with each match in a data frame, and a "tidied_names" vector with filenames
# for when you write the files
#
# match.sheet <- TidyMatchExcel(match.file = "nameoffileinworkingdirectory")

# TidyMultiMatchExcels(competition.slug, team="teamacronym", round="week-X")

# for (index in 1:length(tidied.list)) {
#   file_name <- tidied.names[index]
#   write.csv(tidied_list[[index]], file=file_name, row.names = FALSE)
# }

