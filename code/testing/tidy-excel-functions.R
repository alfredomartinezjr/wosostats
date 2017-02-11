# Install packages if necessary--------
require(readxl)
require(RCurl)

# Objects to reference--------
opposites <- data.frame(posslocations=c("A6", "A18", "A3L", "A3C", "A3R", "AM3L", "AM3C", 
                                        "AM3R", "DM3L", "DM3C", "DM3R", "D3L", "D3C", "D3R", 
                                        "D18", "D6", "AL", "AC", "AR", "AML", "AMC", 
                                        "AMR", "DML", "DMC", "DMR", "DL", "DC", "DR"),
                        deflocations=c("D6", "D18", "D3R", "D3C", "D3L", "DM3R", "DM3C",
                                       "DM3L", "AM3R", "AM3C", "AM3L", "A3R", "A3C", "A3L", 
                                       "A18", "A6", "DR", "DC", "DL", "DMR", "DMC",
                                       "DML", "AMR", "AMC", "AML", "AR", "AC", "AL"))
abbreviation_processor = AbbreviationProcessor$new()


# Functions for individual sheets---------

getMetaData <- function(match_df) {
  meta_df <- match_df[1:(grep("kickoff", match_df[,"poss.action"])[1]-1),]
  meta_df[,"poss.player"] <- trimws(meta_df[,"poss.player"])
  meta_df[,"def.player"] <- trimws(meta_df[,"def.player"])
  
  assign("meta_df", meta_df, pos=1)
}

trimRowsColumns <- function(match_df) {
  # removes NA columns
  match_df <- match_df[,!grepl("^NA",names(match_df))]
  
  # removes blank rows after the row with "end.of.match" in the "poss.action" column
  match_df <- match_df[1:max(grep("end.of.match", match_df[,"poss.action"])),]
  
  # adds any missing columns
  if(!("xG" %in% colnames(match_df))) (match_df$xG <- NA)
  if(!("poss.number" %in% colnames(match_df))) (match_df$poss.number <- NA)
  if(!("def.number" %in% colnames(match_df))) (match_df$def.number <- NA)
  
  match_df
}

cleanUpCells <- function(match_df) {
  match_df[(match_df) == "-"] <- NA
  match_df[(match_df) == " "] <- NA
  match_df[(match_df) == ""] <- NA
  match_df <- as.data.frame(apply(match_df,2,trimws), stringsAsFactors = FALSE)
  
  match_df$poss.action <- tolower(match_df$poss.action)
  match_df$play.type <- tolower(match_df$play.type)
  match_df$def.action <- tolower(match_df$def.action)
  match_df$gk.ball.stop <- tolower(match_df$gk.ball.stop)
  match_df$gk.s.o.g.attempt <- tolower(match_df$gk.s.o.g.attempt)
  match_df$poss.player.disciplinary <- tolower(match_df$poss.player.disciplinary)
  match_df$poss.notes <- tolower(match_df$poss.notes)
  match_df$def.player.disciplinary <- tolower(match_df$def.player.disciplinary)
  match_df$def.notes <- tolower(match_df$def.notes)
  match_df$poss.location <- toupper(match_df$poss.location)
  match_df$poss.play.destination <- toupper(match_df$poss.play.destination)
  match_df$def.location <- toupper(match_df$def.location)
  
  match_df
}

calcTimeValue <- function(sheet_row, match_df) {
  # calculate the time value, if necessary
  if (is.na(match_df[sheet_row,"time"])) {
    #if time column is " " or a "-", set it as whatever the time is in the above row
    match_df[sheet_row-1, "time"]
  } else {
    match_df[sheet_row, "time"]
  }
}

calcEventValue <- function(sheet_row, match_df) {
  # If there are "-"'s or " " values in "event" column, assign them as "NA"
  if (is.na(match_df[sheet_row,"event"])) {
    match_df[sheet_row,"event"] <- NA
  }
  # Checks if "poss.player" is NA, "-", or " " and set appropriate event value.
  # The logic is that if the "poss.player" column is blank, everything in the row
  # is part of the same event as the row above it, assuming it's logged correctly.
  if(
    (is.na(match_df[sheet_row,"poss.player"])) &&
    !grepl(("end.of.match|stoppage.in.play|halftime|playcutoff"),match_df[sheet_row,"poss.action"])
  )
  {
    #sets event value as previous row's event value
    match_df[sheet_row-1,"event"]
  } else {
    #sets event value as 1 plus previous row's event value
    as.numeric(match_df[sheet_row-1,"event"]) + 1
  }
}

setPlayerInfo <- function(sheet_row, match_df, col_set) {
  if (col_set == "poss") {
    player_col <- "poss.player"
    team_col <- "poss.team"
    position_col <- "poss.position"
    number_col <- "poss.number"
  } else if (col_set == "def") {
    player_col <- "def.player"
    team_col <- "def.team"
    position_col <- "def.position"
    number_col <- "def.number"
  }
  player_info <- match_df[sheet_row,c(position_col,team_col,number_col,player_col)]
  if (!is.na(match_df[sheet_row,player_col])) {
    player_string <- match_df[sheet_row,player_col]
    player_team <- match_df[sheet_row,team_col]
    player_position <- match_df[sheet_row,position_col]
    player_number <- match_df[sheet_row,number_col]
    # checks if the player string is the name with the number in parentheses
    # checks if the player string exists in the metadata
    if(tolower(strsplit(player_string," \\(")[[1]][1]) %in% 
       sapply(meta_df[,player_col], function(x) tolower(strsplit(x," \\(")[[1]][1]))) {
      # get the rows in metadata where the name exists
      meta_rownum <- grep(paste0("^",tolower(strsplit(player_string," \\(")[[1]][1]),"$"),sapply(meta_df[,player_col], function(x) tolower(strsplit(x," \\(")[[1]][1])))
      # checks if the player string appears only once in the metadata
      if (length(meta_rownum) == 1) {
        player_info <- meta_df[meta_rownum,c(position_col,team_col,number_col,player_col)]
        player_info[1,player_col] <- strsplit(player_info[1,player_col]," \\(")[[1]][1]
      } else {
        # if the player string appears more than once in the metadata
        # players with the same name in the same game! argh!
        
        # if the player string is text-only, at least one of the team,
        # number, or position columns has to be filled in.
        
        # check if team column is filled in for sheet_row
        if(!is.na(player_team)) {
          meta_rownum_pos <- grep(paste0("^",player_team,"$"), meta_df[meta_rownum,team_col])
          meta_rownum <- meta_rownum[meta_rownum_pos]
        }
        # if meta_rownum is still more than 1 number, check if number column is filled for sheet_row
        if(!is.na(player_number) && length(meta_rownum) > 1) {
          meta_rownum_pos <- grep(paste0("^",player_number,"$"), meta_df[meta_rownum,number_col])
          meta_rownum <- meta_rownum[meta_rownum_pos]
        }
        # if meta_rownumn is still more than 1 number, check if position column is filled for sheet_row
        if(!is.na(player_position) && length(meta_rownum) > 1) {
          meta_rownum_pos <- grep(paste0("^",player_position,"$"), meta_df[meta_rownum,position_col])
          meta_rownum <- meta_rownum[meta_rownum_pos]
        }
        # if meta_rownumn is still more than 1 number, check if the number is in the player string
        if(grepl("\\(", player_string) && length(meta_rownum) > 1){
          player_split_num <- gsub("\\(|\\)","",strsplit(player_string," ")[[1]][length(strsplit(player_string," ")[[1]])])
          meta_rownum_pos <- grep(paste0("^",player_split_num,"$"), meta_df[meta_rownum,number_col])
          meta_rownum <- meta_rownum[meta_rownum_pos]
        }
        if(length(meta_rownum) == 1) {
          player_info <- meta_df[meta_rownum,c(position_col,team_col,number_col,player_col)]
          player_info[1,player_col] <- strsplit(player_info[1,player_col]," \\(")[[1]][1]
        }
      }
    }
    #### code that reads rosters.csv, in case player string is not in metadata, goes here ####
    #### remember to add a warning that there is a player not in metadata. this may be due to human error ####
  }
  player_info
}

# Determine if an action's location is invertible based on the
# location of certain opposing players' action
actionIsInvertible <- function(match_action, sheet_col, match_df) {
  grepl("pressure|challenge|aerial|tackle|dispossess|dribble|pass|move|take|shots",match_df[match_action, sheet_col])
}

getDefLocation <- function(sheet_row, match_df) {
  # checks if the defensive action can have its location determined
  # based on the inverse of certain possessing actions its acting upon
  if (actionIsInvertible(sheet_row, sheet_col = "def.action", match_df = match_df)) {
    # checks if the corresponding event has a value in "poss.location"
    sheet_event <- match_df[sheet_row,"event"]
    event_poss_location <- match_df[!is.na(match_df[,"event"]) & match_df[,"event"]==sheet_event,"poss.location"][1]
    if(!is.na(event_poss_location)) {
      # returns the opposite of event_poss_location
      as.character(opposites[as.character(opposites[,"posslocations"]) == as.character(event_poss_location),"deflocations"])
    } else {
      NA
      # def location cannot be determined
      #### INSERT WARNING MESSAGES HERE ####
    }
  } 
  # checks if "def.location" is blank for interceptions, which can have its location
  # determined based on location of next action, which is by definition by the intercepting
  # player at the location of the interception
  else if (grepl("interceptions", match_df[sheet_row,"def.action"])) {
    # find location of next poss.player
    sheet_event <- as.numeric(match_df[sheet_row,"event"][1])
    sheet_event_next <- as.numeric(sheet_event) + 1
    match_df[!is.na(match_df[,"event"]) & match_df[,"event"] == sheet_event_next,"poss.location"][1]
  } else {
    NA
    # def location cannot be determined
    #### INSERT WARNING MESSAGES HERE ####
  }
}

isCompletedPass <- function(sheet_row, match_df) {
  sheet_event <- as.numeric(match_df[sheet_row,"event"][1])
  sheet_event_next <- as.numeric(sheet_event) + 1
  sheet_row_nextevent <- grep(paste0("^",sheet_event_next,"$"),match_df[,"event"])[1]
  # checks if next event isn't an instance that cut off the broadcast or stopped play.
  !grepl("playcutoffbybroadcast|stoppage|
           substitution|halftime|fulltime|end.of.match", match_df[sheet_row_nextevent,"poss.action"]) &&
    # checks if next event isn't a lost aerial duel
    !grepl("aerial.lost", match_df[sheet_row_nextevent, "poss.action"]) &&
    # checks if next event isn't a recovery
    !grepl("recoveries", match_df[sheet_row_nextevent, "poss.action"]) &&
    # checks if the defensive action isn't something that by definition disrupted a pass attempt
    !grepl("interceptions|blocks|clearances|shield|high.balls.won|smothers.won|loose.balls.won", match_df[match_df[,"event"] == sheet_event & !is.na(match_df[,"event"]),"def.action"]) &&
    # checks if the "gk.ball.stop" column isn't a value besides "missed.the.ball"
    !grepl("caught|punched|dropped|collected|parried|deflected", match_df[match_df[,"event"] == sheet_event & !is.na(match_df[,"event"]),"gk.ball.stop"]) &&
    # checks if "poss.player.disciplinary" column is blank
    !grepl("fouls|fouls|yellow|red|penalties", match_df[match_df[,"event"] == sheet_event & !is.na(match_df[,"event"]),"poss.player.disciplinary"]) &&
    # checks if the ball didn't go out of bounds
    !grepl("out.of.bounds", match_df[match_df[,"event"] == sheet_event & !is.na(match_df[,"event"]),"poss.notes"])
}

readMatchExcel <- function(match.file) {
  
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
  
  match_df
}