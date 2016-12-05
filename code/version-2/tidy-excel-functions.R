getMetaData <- function(excel_df) {
  meta_df <- excel_df[1:(grep("kickoff", excel_df[,"poss.action"])[1]-1),]
  meta_df[,"poss.player"] <- trimws(meta_df[,"poss.player"])
  meta_df[,"def.player"] <- trimws(meta_df[,"def.player"])
  ### Creates a vector for the "home" team and the "away" team, excluding possible NA values
  teams <- as.character(unique(meta_df$poss.team))
  teams <- teams[!is.na(teams) & !(teams=="-") & !(teams==" ") & !(teams=="")]
  ### home team should always be listed first
  hometeam <- teams[1]
  awayteam <- teams[2]
  rm(teams)
  meta_home <- meta_df[meta_df[,"poss.team"]==hometeam,c("poss.position","poss.team","poss.number", "poss.player")]
  meta_away <- meta_df[meta_df[,"poss.team"]==awayteam,c("poss.position","poss.team","poss.number" ,"poss.player")]
  
  assign("meta_df", meta_df, pos=1)
  assign("hometeam", hometeam, pos=1)
  assign("awayteam", awayteam, pos=1)
  assign("meta_home", meta_home, pos=1)
  assign("meta_away", meta_away, pos=1)
}

trimRowsColumns <- function(excel_df) {
  # removes NA columns
  excel_df <- excel_df[,!grepl("^NA",names(excel_df))]
  
  # removes blank rows after the row with "end.of.match" in the "poss.action" column
  excel_df <- excel_df[1:max(grep("end.of.match", excel_df[,"poss.action"])),]
  
  # adds any missing columns
  if(!("xG" %in% colnames(excel_df))) (excel_df$xG <- NA)
  if(!("poss.number" %in% colnames(excel_df))) (excel_df$poss.number <- NA)
  if(!("def.number" %in% colnames(excel_df))) (excel_df$def.number <- NA)
  
  excel_df
}

cleanUpCells <- function(excel_df) {
  excel_df[(excel_df) == "-"] <- NA
  excel_df[(excel_df) == " "] <- NA
  excel_df[(excel_df) == ""] <- NA
  excel_df <- as.data.frame(apply(excel_df,2,trimws), stringsAsFactors = FALSE)
  
  excel_df$poss.action <- tolower(excel_df$poss.action)
  excel_df$play.type <- tolower(excel_df$play.type)
  excel_df$def.action <- tolower(excel_df$def.action)
  excel_df$gk.ball.stop <- tolower(excel_df$gk.ball.stop)
  excel_df$gk.s.o.g.attempt <- tolower(excel_df$gk.s.o.g.attempt)
  excel_df$poss.player.disciplinary <- tolower(excel_df$poss.player.disciplinary)
  excel_df$poss.notes <- tolower(excel_df$poss.notes)
  excel_df$def.player.disciplinary <- tolower(excel_df$def.player.disciplinary)
  excel_df$def.notes <- tolower(excel_df$def.notes)
  excel_df$poss.location <- toupper(excel_df$poss.location)
  excel_df$poss.play.destination <- toupper(excel_df$poss.play.destination)
  excel_df$def.location <- toupper(excel_df$def.location)
  
  excel_df
}

calcTimeValue <- function(sheet_row, excel_df) {
  # calculate the time value, if necessary
  if (is.na(excel_df[sheet_row,"time"])) {
    #if time column is " " or a "-", set it as whatever the time is in the above row
    excel_df[sheet_row-1, "time"]
  } else {
    excel_df[sheet_row, "time"]
  }
}

calcEventValue <- function(sheet_row, excel_df) {
  # If there are "-"'s or " " values in "event" column, assign them as "NA"
  if (is.na(excel_df[sheet_row,"event"])) {
    excel_df[sheet_row,"event"] <- NA
  }
  # Checks if "poss.player" is NA, "-", or " " and set appropriate event value.
  # The logic is that if the "poss.player" column is blank, everything in the row
  # is part of the same event as the row above it, assuming it's logged correctly.
  if(
    (is.na(excel_df[sheet_row,"poss.player"])) &&
    !grepl(("end.of.match|stoppage.in.play|halftime|playcutoff"),excel_df[sheet_row,"poss.action"])
  )
  {
    #sets event value as previous row's event value
    excel_df[sheet_row-1,"event"]
  } else {
    #sets event value as 1 plus previous row's event value
    as.numeric(excel_df[sheet_row-1,"event"]) + 1
  }
}

setPlayerInfo <- function(sheet_row, excel_df, col_set) {
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
  player_info <- excel_df[sheet_row,c(position_col,team_col,number_col,player_col)]
  if (!is.na(excel_df[sheet_row,player_col])) {
    player_string <- excel_df[sheet_row,player_col]
    player_team <- excel_df[sheet_row,team_col]
    player_position <- excel_df[sheet_row,position_col]
    player_number <- excel_df[sheet_row,number_col]
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
actionIsInvertible <- function(match_action, sheet_col, excel_df) {
  grepl("pressure|challenge|aerial|tackle|dispossess|dribble|pass|move|take|shots",excel_df[match_action, sheet_col])
}

getDefLocation <- function(sheet_row, excel_df) {
  # checks if the defensive action can have its location determined
  # based on the inverse of certain possessing actions its acting upon
  if (actionIsInvertible(sheet_row, sheet_col = "def.action", excel_df = excel_df)) {
    # checks if the corresponding event has a value in "poss.location"
    sheet_event <- excel_df[sheet_row,"event"]
    event_poss_location <- excel_df[!is.na(excel_df[,"event"]) & excel_df[,"event"]==sheet_event,"poss.location"][1]
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
  else if (grepl("interceptions", excel_df[sheet_row,"def.action"])) {
    # find location of next poss.player
    sheet_event <- as.numeric(excel_df[sheet_row,"event"][1])
    sheet_event_next <- as.numeric(sheet_event) + 1
    excel_df[!is.na(excel_df[,"event"]) & excel_df[,"event"] == sheet_event_next,"poss.location"][1]
  } else {
    NA
    # def location cannot be determined
    #### INSERT WARNING MESSAGES HERE ####
  }
}

isCompletedPass <- function(sheet_row, excel_df) {
  sheet_event <- as.numeric(excel_df[sheet_row,"event"][1])
  sheet_event_next <- as.numeric(sheet_event) + 1
  sheet_row_nextevent <- grep(paste0("^",sheet_event_next,"$"),excel_df[,"event"])[1]
  # checks if next event isn't an instance that cut off the broadcast or stopped play.
  !grepl("playcutoffbybroadcast|stoppage|
           substitution|halftime|fulltime|end.of.match", excel_df[sheet_row_nextevent,"poss.action"]) &&
    # checks if next event isn't a lost aerial duel
    !grepl("aerial.lost", excel_df[sheet_row_nextevent, "poss.action"]) &&
    # checks if next event isn't a recovery
    !grepl("recoveries", excel_df[sheet_row_nextevent, "poss.action"]) &&
    # checks if the defensive action isn't something that by definition disrupted a pass attempt
    !grepl("interceptions|blocks|clearances|shield|high.balls.won|smothers.won|loose.balls.won", excel_df[excel_df[,"event"] == sheet_event & !is.na(excel_df[,"event"]),"def.action"]) &&
    # checks if the "gk.ball.stop" column isn't a value besides "missed.the.ball"
    !grepl("caught|punched|dropped|collected|parried|deflected", excel_df[excel_df[,"event"] == sheet_event & !is.na(excel_df[,"event"]),"gk.ball.stop"]) &&
    # checks if "poss.player.disciplinary" column is blank
    !grepl("fouls|fouls|yellow|red|penalties", excel_df[excel_df[,"event"] == sheet_event & !is.na(excel_df[,"event"]),"poss.player.disciplinary"]) &&
    # checks if the ball didn't go out of bounds
    !grepl("out.of.bounds", excel_df[excel_df[,"event"] == sheet_event & !is.na(excel_df[,"event"]),"poss.notes"])
}