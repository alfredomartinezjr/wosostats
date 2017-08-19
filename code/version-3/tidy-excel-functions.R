# Install packages if necessary--------
require(readxl)
require(RCurl)

# Functions for individual sheets---------
GetMetaData <- function(match.df) {
  meta.df <- match.df[1:(grep("kickoff", match.df[, "poss.action"])[1] - 1), ]
  meta.df[, "poss.player"] <- trimws(meta.df[, "poss.player"])
  meta.df[, "def.player"] <- trimws(meta.df[, "def.player"])
  for(player.row in 1:nrow(meta.df)) {
    if (grepl("\\(", meta.df[player.row, "poss.player"]) && is.na(meta.df[player.row, "poss.number"])) {
      player.split.num <- gsub("\\(|\\)", "", strsplit(meta.df[player.row, "poss.player"], " ")[[1]][length(strsplit(meta.df[player.row, "poss.player"], " ")[[1]])])
      meta.df[player.row, "poss.number"] <- player.split.num
    }
    if (grepl("\\(", meta.df[player.row, "def.player"]) && is.na(meta.df[player.row, "def.number"])) {
      player.split.num <- gsub("\\(|\\)", "", strsplit(meta.df[player.row, "def.player"], " ")[[1]][length(strsplit(meta.df[player.row, "def.player"], " ")[[1]])])
      meta.df[player.row, "def.number"] <- player.split.num
    }
  }
  assign("meta.df", meta.df, pos=1)
}

TrimRowsColumns <- function(match.df) {
  # removes NA columns
  match.df <- match.df[, !grepl("^NA", names(match.df))]
  # removes blank rows after the row with "end.of.match" in the "poss.action" column
  match.df <- match.df[1:max(grep("end.of.match", match.df[, "poss.action"])), ]
  # adds any missing columns
  if (!("xG" %in% colnames(match.df))) (match.df$xG <- NA)
  if (!("poss.number" %in% colnames(match.df))) (match.df$poss.number <- NA)
  if (!("def.number" %in% colnames(match.df))) (match.df$def.number <- NA)
  match.df
}

CleanUpCells <- function(match.df) {
  match.df[(match.df) == "-"] <- NA
  match.df[(match.df) == " "] <- NA
  match.df[(match.df) == ""] <- NA
  match.df <- as.data.frame(apply(match.df, 2, trimws), stringsAsFactors = FALSE)
  match.df$poss.action              <- tolower(match.df$poss.action)
  match.df$play.type                <- tolower(match.df$play.type)
  match.df$def.action               <- tolower(match.df$def.action)
  match.df$gk.ball.stop             <- tolower(match.df$gk.ball.stop)
  match.df$gk.s.o.g.attempt         <- tolower(match.df$gk.s.o.g.attempt)
  match.df$poss.player.disciplinary <- tolower(match.df$poss.player.disciplinary)
  match.df$poss.notes               <- tolower(match.df$poss.notes)
  match.df$def.player.disciplinary  <- tolower(match.df$def.player.disciplinary)
  match.df$def.notes                <- tolower(match.df$def.notes)
  match.df$poss.location            <- toupper(match.df$poss.location)
  match.df$poss.play.destination    <- toupper(match.df$poss.play.destination)
  match.df$def.location             <- toupper(match.df$def.location)
  match.df
}

CalcTimeValue <- function(sheet.row, match.df) {
  # calculate the time value, if necessary
  if (is.na(match.df[sheet.row, "time"])) {
    #if time column is " " or a "-", set it as whatever the time is in the above row
    match.df[sheet.row - 1, "time"]
  } else {
    match.df[sheet.row, "time"]
  }
}

CalcEventValue <- function(sheet.row, match.df) {
  # If there are "-"'s or " " values in "event" column, assign them as "NA"
  if (is.na(match.df[sheet.row, "event"])) {
    match.df[sheet.row, "event"] <- NA
  }
  # Checks if "poss.player" is NA, "-", or " " and set appropriate event value.
  # The logic is that if the "poss.player" column is blank, everything in the row
  # is part of the same event as the row above it, assuming it's logged correctly.
  if ((is.na(match.df[sheet.row, "poss.player"])) &&
    !grepl(("end.of.match|stoppage.in.play|halftime|playcutoff"), match.df[sheet.row, "poss.action"])) {
    #sets event value as previous row's event value
    match.df[sheet.row - 1, "event"]
  } else {
    #sets event value as 1 plus previous row's event value
    as.numeric(match.df[sheet.row - 1, "event"]) + 1
  }
}

SetPlayerInfo <- function(sheet.row, match.df, col.set) {
  if (col.set == "poss") {
    player.col   <- "poss.player"
    team.col     <- "poss.team"
    position.col <- "poss.position"
    number.col   <- "poss.number"
  } else if (col.set == "def") {
    player.col   <- "def.player"
    team.col     <- "def.team"
    position.col <- "def.position"
    number.col   <- "def.number"
  }
  player.info <- match.df[sheet.row, c(position.col, team.col, number.col, player.col)]
  if (!is.na(match.df[sheet.row, player.col])) {
    player.string   <- match.df[sheet.row, player.col]
    player.team     <- match.df[sheet.row, team.col]
    player.position <- match.df[sheet.row, position.col]
    player.number   <- match.df[sheet.row, number.col]
    # checks if the player string is the name with the number in parentheses
    # checks if the player string exists in the metadata
    if (tolower(strsplit(player.string, " \\(")[[1]][1]) %in% 
       sapply(meta.df[, player.col], function(x) tolower(strsplit(x, " \\(")[[1]][1]))) {
      # get the rows in metadata where the name exists
      meta.rownum <- grep(paste0("^", tolower(strsplit(player.string, " \\(")[[1]][1]), "$"), sapply(meta.df[, player.col], function(x) tolower(strsplit(x, " \\(")[[1]][1])))
      # checks if the player string appears only once in the metadata
      if (length(meta.rownum) == 1) {
        player.info <- meta.df[meta.rownum, c(position.col, team.col, number.col, player.col)]
        player.info[1, player.col] <- strsplit(player.info[1, player.col]," \\(")[[1]][1]
      } else {
        # if the player string appears more than once in the metadata
        # players with the same name in the same game! argh!
        # if the player string is text-only, at least one of the team,
        # number, or position columns has to be filled in.
        # check if team column is filled in for sheet.row
        if (!is.na(player.team)) {
          meta.rownum.pos <- grep(paste0("^", player.team,"$"), meta.df[meta.rownum, team.col])
          meta.rownum     <- meta.rownum[meta.rownum.pos]
        }
        # if meta.rownum is still more than 1 number, check if number column is filled for sheet.row
        if (!is.na(player.number) && length(meta.rownum) > 1) {
          meta.rownum.pos <- grep(paste0("^", player.number, "$"), meta.df[meta.rownum, number.col])
          meta.rownum     <- meta.rownum[meta.rownum.pos]
        }
        # if meta_rownumn is still more than 1 number, check if position column is filled for sheet.row
        if (!is.na(player.position) && length(meta.rownum) > 1) {
          meta.rownum.pos <- grep(paste0("^", player.position, "$"), meta.df[meta.rownum, position.col])
          meta.rownum     <- meta.rownum[meta.rownum.pos]
        }
        # if meta_rownumn is still more than 1 number, check if the number is in the player string
        if (grepl("\\(", player.string) && length(meta.rownum) > 1) {
          player.split.num <- gsub("\\(|\\)", "", strsplit(player.string, " ")[[1]][length(strsplit(player.string, " ")[[1]])])
          meta.rownum.pos  <- grep(paste0("^", player.split.num, "$"), meta.df[meta.rownum, number.col])
          meta.rownum      <- meta.rownum[meta.rownum.pos]
        }
        if (length(meta.rownum) == 1) {
          player.info                <- meta.df[meta.rownum, c(position.col, team.col, number.col, player.col)]
          player.info[1, player.col] <- strsplit(player.info[1, player.col], " \\(")[[1]][1]
        }
      }
    }
    #### code that reads rosters.csv, in case player string is not in metadata, goes here ####
    #### remember to add a warning that there is a player not in metadata. this may be due to human error ####
  }
  player.info
}

# Determine if an action's location is invertible based on the
# location of certain opposing players' action
ActionIsInvertible <- function(match.action, sheet.col, match.df) {
  grepl("pressure|challenge|aerial|ground|tackle|dispossess|dribble|pass|touch|move|take|shots", match.df[match.action, sheet.col])
}

GetDefLocation <- function(sheet.row, match.df) {
  kOpposites <- data.frame(posslocations=c("A6", "A18", "A3L", "A3C", "A3R", "AM3L", "AM3C", 
                                           "AM3R", "DM3L", "DM3C", "DM3R", "D3L", "D3C", "D3R", 
                                           "D18", "D6", "AL", "AC", "AR", "AML", "AMC", 
                                           "AMR", "DML", "DMC", "DMR", "DL", "DC", "DR"),
                           deflocations=c("D6", "D18", "D3R", "D3C", "D3L", "DM3R", "DM3C",
                                          "DM3L", "AM3R", "AM3C", "AM3L", "A3R", "A3C", "A3L", 
                                          "A18", "A6", "DR", "DC", "DL", "DMR", "DMC",
                                          "DML", "AMR", "AMC", "AML", "AR", "AC", "AL"))
  # checks if the defensive action can have its location determined
  # based on the inverse of certain possessing actions its acting upon
  if (ActionIsInvertible(sheet.row, sheet.col = "def.action", match.df = match.df)) {
    # checks if the corresponding event has a value in "poss.location"
    sheet.event <- match.df[sheet.row, "event"]
    event.poss.location <- match.df[!is.na(match.df[, "event"]) & match.df[, "event"]==sheet.event, "poss.location"][1]
    if (!is.na(event.poss.location)) {
      # returns the opposite of event_poss_location
      as.character(kOpposites[as.character(kOpposites[, "posslocations"]) == as.character(event.poss.location), "deflocations"])
    } else {
      NA
      # def location cannot be determined
      #### INSERT WARNING MESSAGES HERE ####
    }
  # checks if "def.location" is blank for interceptions, which can have its location
  # determined based on location of next action, which is by definition by the intercepting
  # player at the location of the interception  
  } else if (grepl("interceptions", match.df[sheet.row, "def.action"])) {
    # find location of next poss.player
    sheet.event <- as.numeric(match.df[sheet.row, "event"][1])
    sheet.event.next <- as.numeric(sheet.event) + 1
    match.df[!is.na(match.df[, "event"]) & match.df[, "event"] == sheet.event.next, "poss.location"][1]
  } else {
    NA
    # def location cannot be determined
    #### INSERT WARNING MESSAGES HERE ####
  }
}

IsCompletedPass <- function(sheet.row, match.df) {
  # Returns TRUE if, based on surrounding data, a pass attempt is completed
  event      <- as.numeric(match.df[sheet.row, "event"][1])
  next.event <- as.numeric(event) + 1
  row.of.next.event <- grep(paste0("^", next.event,"$"), match.df[, "event"])[1]
  # Series of logical variables & their dependent variables about what 
  # happened in an event, to be used to determine if a pass was completed.
  cutoff.actions             <- "playcutoffbybroadcast|stoppage|substitution|halftime|fulltime|end.of.match|offside"
  next.event.wasnt.cutoff    <- !grepl(cutoff.actions, match.df[row.of.next.event, "poss.action"])
  next.event.wasnt.lost.duel <- !grepl("aerial.lost|ground.50.50.lost", match.df[row.of.next.event, "poss.action"])
  next.event.wasnt.recovery  <- !grepl("recoveries", match.df[row.of.next.event, "poss.action"])
  ball.disrupt.actions       <- "interceptions|blocks|clearances|shield|high.balls.won|smothers.won|loose.balls.won"
  def.actions                <- match.df[match.df[, "event"] == event & !is.na(match.df[, "event"]), "def.action"]
  ball.wasnt.disrupted       <- !(TRUE %in% grepl(ball.disrupt.actions, def.actions))
  gk.disrupt.actions         <- "caught|punched|dropped|collected|parried|deflected"
  gk.actions                 <- match.df[match.df[, "event"] == event & !is.na(match.df[, "event"]), "gk.ball.stop"]
  gk.didnt.stop.ball         <- !grepl(gk.disrupt.actions, gk.actions)
  poss.player.didnt.foul     <- !grepl("fouls|fouls|yellow|red|penalties", match.df[match.df[, "event"] == event & !is.na(match.df[, "event"]), "poss.player.disciplinary"])
  ball.didnt.go.out          <- !(TRUE %in% grepl("out.of.bounds", unlist(match.df[match.df[, "event"] == event & !is.na(match.df[, "event"]), c("play.type","poss.notes")])))
  # Series of if statements to determine if event is a completed pass, creates
  # the TRUE/FALSE statement to be returned by the function.
  next.event.wasnt.cutoff && next.event.wasnt.lost.duel && 
    next.event.wasnt.recovery && ball.wasnt.disrupted && gk.didnt.stop.ball && 
    poss.player.didnt.foul && ball.didnt.go.out
}

TidyMatchExcel <- function(match.file) {
  # Reads a match spreadsheet in untidied, Excel format and creates a match
  # spreadsheet in .csv format that can be read by creating-stats.R files.
  match.df <- as.data.frame(read_excel(match.file))
  match.df <- TrimRowsColumns(match.df) # removes excess columns & rows
  match.df <- CleanUpCells(match.df) # trims whitespace, creates NAs
  GetMetaData(match.df) # creates meta.df in global environment
  match.df <- match.df[grep("kickoff", match.df[,"poss.action"])[1]:nrow(match.df),]
  kKickoffRow <- grep("kickoff", match.df[, "poss.action"])[1] + 1
  kEndOfMatchRow <- nrow(match.df)
  abbreviation_processor <- AbbreviationProcessor$new()
  # Expands shorcuts, calculate missing values
  for (sheet.row in kKickoffRow:kEndOfMatchRow) {
    match.df[sheet.row, ] = abbreviation_processor$process_row(match.df[sheet.row,])
    match.df[sheet.row, "time"] <- CalcTimeValue(sheet.row, match.df)
    match.df[sheet.row, "event"] <- CalcEventValue(sheet.row, match.df)
    match.df[sheet.row, c("poss.position", "poss.team", "poss.number", "poss.player")] <- SetPlayerInfo(sheet.row, match.df, "poss")
    match.df[sheet.row, c("def.position", "def.team", "def.number", "def.player")] <- SetPlayerInfo(sheet.row, match.df, "def")
    has.missing.def.location <- !is.na(match.df[sheet.row, "def.action"]) & is.na(match.df[sheet.row, "def.location"])
    if (has.missing.def.location) {
      match.df[sheet.row, c("def.location")] <- GetDefLocation(sheet.row, match.df = match.df)
    }
  }
  # Calculates completed passes and missing "poss.play.destination" locations
  for (sheet.row in kKickoffRow:kEndOfMatchRow) {
    is.inconclusive.pass <-
      grepl("pass", match.df[sheet.row, "poss.action"]) &
      !grepl("c", match.df[sheet.row, "poss.action"])
    if (is.inconclusive.pass &
        IsCompletedPass(sheet.row, match.df = match.df) == TRUE) {
      match.df[sheet.row, "poss.action"] <-
        paste0(match.df[sheet.row, "poss.action"], ".c") # add a ".c" to the end of the "pass.f/s/b"
      # Sets value in "poss.play.destination" if it's an NA
      if (is.na(match.df[sheet.row, "poss.play.destination"])) {
        next.event <- as.numeric(match.df[sheet.row, "event"][1]) + 1
        sheet.row.of.next.event <-
          grep(paste0("^", next.event, "$"), match.df[, "event"])[1]
        match.df[sheet.row, "poss.play.destination"] <-
          match.df[sheet.row.of.next.event, "poss.location"]
      }
    }
  }
  match.df$event <- as.integer(match.df$event)
  match.df
}

TidyMultiMatchExcels <- function(competition.slug, team=NA, round=NA) {
  excel.list <- grep(".xlsx", list.files(),value = TRUE)
  excel.list <- grep(competition.slug, excel.list, value = TRUE)
  if (!is.na(team)) {
    excel.list <- grep(tolower(team), excel.list, value = TRUE)
  }
  if (!is.na(round)) {
    if (!exists("database") & (online_mode=="online")) {
      database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
      database <- read.csv(textConnection(database), stringsAsFactors = FALSE)
    } else if (!exists("database") & (online_mode=="offline")) {
      database <- read.csv("~/wosostats/database.csv")
    }
    match.strings <- database[database[,"round"]==round,"match.csv.link"]
    match.strings <- sapply(match.strings, function(x) strsplit(x, split="/")[[1]][length(strsplit(x, split="/")[[1]])])
    match.strings <- sapply(match.strings, function(x) strsplit(x, split=".csv")[[1]])
    match.strings <- paste0(match.strings, ".xlsx")
    excel.list <- excel.list[excel.list %in% match.strings]
  }
  tidied.list <- list()
  file.names <- c()
  for (index in 1:length(excel.list)) {
    tidied.list[[index]] <- TidyMatchExcel(match.file = excel.list[index])
    file.names[index] <- paste0(strsplit(excel.list[index], ".xlsx")[[1]], ".csv")
  }
  assign("tidied.names", file.names, pos=1)
  assign("tidied.list", tidied.list, pos=1)
}