## Running this code takes an Excel file in your directory for a raw, unedited match actions 
## spreadsheet and turns it into a data.frame that can be read by the creating-stats.R code

# Offline mode--------
# Trying to do work on a plane & don't want to pay $8 for Wi-Fi? Stuck in a train tunnel?
# Assign "offline" to  online_mode and, assuming you've got the GitHub repo duplicated in
# your working directory, you can just read the files instead of going online.
# Otherwise, if online_mode hasn't been created yet, "online" is assigned to online_mode
if(!exists("online_mode")){
  online_mode <- "online"
}

# Install packages if necessary--------
require(readxl)
require(RCurl)
require(R6)

# Internal files to include--------
if(online_mode == "online") {
  base_directory = "https://raw.githubusercontent.com/amj2012/wosostats/master/"
  code_directory = paste(base_directory, "code/version-2/", sep="")
  source(paste(code_directory, "abbreviations.R", sep=""))
  source(paste(code_directory, "tidy-excel-functions.R", sep=""))
  rosters <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/rosters/nwsl-2016.csv")
  rosters <- read.csv(textConnection(rosters), stringsAsFactors = FALSE)
  rm(base_directory, code_directory)
} else if(online_mode == "offline") {
  source("~/wosostats/code/version-2/abbreviations.R")
  source("~/wosostats/code/version-2/tidy-excel-functions.R")
  rosters <- read.csv("~/wosostats/rosters/nwsl-2016.csv")
}

# Objects to reference--------
posslocations <- c("A6", "A18", "A3L", "A3C", "A3R", "AM3L", "AM3C", 
                   "AM3R", "DM3L", "DM3C", "DM3R", "D3L", "D3C", "D3R", 
                   "D18", "D6", "AL", "AC", "AR", "AML", "AMC", 
                   "AMR", "DML", "DMC", "DMR", "DL", "DC", "DR")
deflocations <- c("D6", "D18", "D3R", "D3C", "D3L", "DM3R", "DM3C",
                  "DM3L", "AM3R", "AM3C", "AM3L", "A3R", "A3C", "A3L", 
                  "A18", "A6", "DR", "DC", "DL", "DMR", "DMC",
                  "DML", "AMR", "AMC", "AML", "AR", "AC", "AL")
opposites <- data.frame(posslocations, deflocations)
abbreviation_processor = AbbreviationProcessor$new()

# Reading the Excel file----------
# "match.file" must be a string value and the Excel file must be in the working directory
match_df <- as.data.frame(read_excel(match.file))

# Cleaning up excess cells and characters--------
match_df <- trimRowsColumns(match_df)
match_df <- cleanUpCells(match_df)

# Get metadata----------
getMetaData(match_df)
match_df <- match_df[grep("kickoff", match_df[,"poss.action"])[1]:nrow(match_df),]

# Expand shortcuts and calculate missing values
for (sheet_row in ((grep("kickoff", match_df[,"poss.action"])[1])+1):nrow(match_df)){
  match_df[sheet_row,] = abbreviation_processor$process_row(match_df[sheet_row,])
  match_df[sheet_row,"time"] <- calcTimeValue(sheet_row, match_df = match_df)
  match_df[sheet_row,"event"] <- calcEventValue(sheet_row, match_df = match_df)
  match_df[sheet_row,c("poss.position", "poss.team","poss.number","poss.player")] <- setPlayerInfo(sheet_row, match_df = match_df, col_set = "poss")
  match_df[sheet_row,c("def.position", "def.team","def.number","def.player")] <- setPlayerInfo(sheet_row, match_df = match_df, col_set = "def")
  if (!is.na(match_df[sheet_row,"def.action"]) & is.na(match_df[sheet_row,"def.location"])){
    match_df[sheet_row,c("def.location")] <- getDefLocation(sheet_row, match_df = match_df)
  }
}

# Calculate completed passes and missing "poss.play.destination" locations----------
for (sheet_row in ((grep("kickoff", match_df[,"poss.action"])[1])+1):nrow(match_df)){
  if (grepl("pass", match_df[sheet_row,"poss.action"]) & !grepl("c", match_df[sheet_row,"poss.action"])) {
    if(isCompletedPass(sheet_row, match_df = match_df) == TRUE) {
      # add a ".c" to the end of the "poss.action" value to signify that it's a completed pass,
      match_df[sheet_row,"poss.action"] <- paste0(match_df[sheet_row,"poss.action"], ".c")
      # set value in "poss.play.destination" if NA
      if(is.na(match_df[sheet_row,"poss.play.destination"])) {
        sheet_event_next <- as.numeric(match_df[sheet_row,"event"][1])+1
        sheet_row_nextevent <- grep(paste0("^",sheet_event_next,"$"),match_df[,"event"])[1]
        match_df[sheet_row,"poss.play.destination"] <- match_df[sheet_row_nextevent, "poss.location"]
      }
    }
  }
}

rm(list=setdiff(ls(), "match_df"))