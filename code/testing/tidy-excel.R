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
  code_directory = paste(base_directory, "code/testing/", sep="")
  source(paste(code_directory, "abbreviations.R", sep=""))
  source(paste(code_directory, "tidy-excel-functions.R", sep=""))
  rosters <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/rosters/nwsl-2016.csv")
  rosters <- read.csv(textConnection(rosters), stringsAsFactors = FALSE)
  rm(base_directory, code_directory)
} else if(online_mode == "offline") {
  source("~/wosostats/code/testing/abbreviations.R")
  source("~/wosostats/code/testing/tidy-excel-functions.R")
  rosters <- read.csv("~/wosostats/rosters/nwsl-2016.csv")
}

# match_sheet <- tidyMatchExcel(match.file = "nameoffileinworkingdirectory")

# multiple_sheets <- tidyMultiMatchExcels <- function(competition.slug, team="teamacronym")

