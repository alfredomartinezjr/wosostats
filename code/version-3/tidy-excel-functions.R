####Abbreviations####
# This section contains logic responsible for converting abbreviations in the
# Excel match spreadsheet into their non-abbreviated form.
#
# An AbbreviationConverter_<NAME> class should be created for each column within
# the Excel match spreadsheet (ie "poss.action", "play.type", etc) that contains
# abbreviations that need converting. The AbbreviationConverter classes should
# define a list of abbreviations and their associated replacements.
#
# To "AbbreviationProcessor" class is the interface used to perform abbreviations
# for the rows and columns from an Excel match spreadsheet.
library(R6)
# AbbreviationConverter for the "poss.action" column.
AbbreviationConverter_PossAction <-
  R6Class("Abbreviation_PossAction", inherit = AbbreviationConverter_Base, 
          public = list(column_name = "poss.action", 
                        initialize = function() {
                          self$add_abbreviation("^sgk",                  "shots.stopped.by.gk")
                          self$add_abbreviation("^sdef",                 "shots.stopped.by.def")
                          self$add_abbreviation("^sb",                   "shots.blocked")
                          self$add_abbreviation("^sc",                   "shots.scored")
                          self$add_abbreviation("^sm",                   "shots.missed")
                          self$add_abbreviation("^pf",                   "passes.f")
                          self$add_abbreviation("^ps",                   "passes.s")
                          self$add_abbreviation("^pb",                   "passes.b")
                          self$add_abbreviation("^m",                    "movement")
                          self$add_abbreviation("^tkw",                  "take.on.won")
                          self$add_abbreviation("^tkl",                  "take.on.lost")
                          self$add_abbreviation("^d",                    "dispossessed")
                          self$add_abbreviation("^lt",                   "lost.touch")
                          self$add_abbreviation("^bt",                   "ball.touch")
                          self$add_abbreviation("^gw",                   "ground.50.50.won")
                          self$add_abbreviation("^gl",                   "ground.50.50.lost")
                          self$add_abbreviation("^aw",                   "aerial.won")
                          self$add_abbreviation("^al",                   "aerial.lost")
                          self$add_abbreviation("^r",                    "recoveries")
                          self$add_abbreviation("^bs",                   "ball.shield")
                          self$add_abbreviation("^cl$|^cl ",             "clearances")
                          self$add_abbreviation("^playcutoff",           "playcutoffbybroadcast")
                          self$add_abbreviation("^fw",                   "fouls.won")
                          self$add_abbreviation("^fc",                   "fouls.conceded")
                        }
          )
  )
# AbbreviationConverter for the "play.type" column.
AbbreviationConverter_PlayType <- 
  R6Class("Abbreviation_PlayType", inherit = AbbreviationConverter_Base,
          public = list(column_name = "play.type", 
                        initialize = function() {
                          self$add_abbreviation("^th|^th$",             "through")
                          self$add_abbreviation("^cc|^cr|^dc",             "crosses")
                          self$add_abbreviation("^s",                      "switch")
                          self$add_abbreviation("^lau",                    "launch")
                          self$add_abbreviation("^ti",                     "throw.in")
                          self$add_abbreviation("^fk",                     "free.kick")
                          self$add_abbreviation("^h",                      "headed")
                          self$add_abbreviation("^ck",                     "corner.kick")
                          self$add_abbreviation("^gk$|gk ",                "goal.kick")
                          self$add_abbreviation("^gkt",                    "gk.throws")
                          self$add_abbreviation("^gkdk",                   "gk.drop.kick")
                          self$add_abbreviation("^pk",                     "penalty.kick")
                          self$add_abbreviation("^pip",                    "pass.into.pressure")
                          self$add_abbreviation("^keep.poss|^kept.poss",   "out.of.bounds.keep.poss")
                          self$add_abbreviation("^lost.poss|^lose.poss",   "out.of.bounds.lost.poss")
                        }
          )
  )
# AbbreviationConverter for the "def.action" column.
AbbreviationConverter_DefAction <-
  R6Class("Abbreviation_DefAction", inherit = AbbreviationConverter_Base,
          public = list(column_name = "def.action",
                        initialize = function() {
                          self$add_abbreviation("^dbs|^bs",            "ball.shield")
                          self$add_abbreviation("^dis|^ds|^dlt",       "dispossessed")
                          self$add_abbreviation("^tb|^tba|^tbw",       "tackles.ball")
                          self$add_abbreviation("^dtm|^dor|^dt|^dr",   "dribbled")
                          self$add_abbreviation("^p",                  "pressured")
                          self$add_abbreviation("^ch",                 "challenged")
                          self$add_abbreviation("^bl",                 "blocks")
                          self$add_abbreviation("^int",                "interceptions")
                          self$add_abbreviation("^bd",                 "ball.shield")
                          self$add_abbreviation("^cl",                 "clearances")
                          self$add_abbreviation("^cl.h.p$|^cl.p.h$",   "clearances.headed.pressed")
                          self$add_abbreviation("^cl.h$",              "clearances.headed")
                          self$add_abbreviation("^cl.p$",              "clearances.pressed")
                          self$add_abbreviation("^aw",                 "aerial.won")
                          self$add_abbreviation("^al",                 "aerial.lost")
                          self$add_abbreviation("^fw",                 "fouls.won")
                          self$add_abbreviation("^fc",                 "fouls.conceded")
                          self$add_abbreviation("^bt",                 "ball.touch")
                          self$add_abbreviation("^gw",                 "ground.50.50.won")
                          self$add_abbreviation("^gl",                 "ground.50.50.lost")
                        }
          )
  )
# AbbreviationConverter for the "poss.player.disciplinary" column.
AbbreviationConverter_PossPlayerDisciplinary <- 
  R6Class("PossPlayerDisciplinary", inherit = AbbreviationConverter_Base,
          public = list(column_name = "poss.player.disciplinary",
                        initialize = function() {
                          self$add_abbreviation("^fw",   "fouls.won")
                          self$add_abbreviation("^fc",   "fouls.conceded")
                        }
          )
  )
# AbbreviationConverter for the "def.player.disciplinary" column.
AbbreviationConverter_DefPlayerDisciplinary <-
  R6Class("DefPlayerDisciplinary", inherit = AbbreviationConverter_Base, 
          public = list(column_name = "def.player.disciplinary",
                        initialize = function() {
                          self$add_abbreviation("^fw",   "fouls.won")
                          self$add_abbreviation("^fc",   "fouls.conceded")
                        }
          )
  )
# AbbreviationConverter for the "poss.notes" column.
AbbreviationConverter_PossNotes <-
  R6Class("PossNotes", inherit = AbbreviationConverter_Base, 
          public = list(column_name = "poss.notes",
                        initialize = function() {
                          self$add_abbreviation("^keep.poss|^kept.poss",   "out.of.bounds.keep.poss")
                          self$add_abbreviation("^lost.poss|^lose.poss",   "out.of.bounds.lost.poss")
                        }
          )
  )
# AbbreviationConverter_Base:
# Contains shared logic that all derived AbbreviationConverter
# classes use for abbreviation replacement.
AbbreviationConverter_Base <-
  R6Class("AbbreviationConverter_Base",
          public = list(abbreviations = list(),
                        # @brief  Searches list of abbreviations determining if any abbreviation
                        #         within matches @value and is eligible for replacement.
                        #
                        # @param value  The value to check for replacement.
                        convert = function(value) {
                          for (abbreviation in self$abbreviations) {
                            if (grepl(abbreviation[1], value)) {
                              return(abbreviation[2])
                            }
                          }
                          return(value)
                        },
                        # @brief  Adds an abbreviation and its replacement to the abbreviation list.
                        #
                        # @param abbrev   The abbreviated value (regex) that should be replaced.
                        # @param replace  The value to use as the replacement.
                        add_abbreviation = function(abbrev, replace) {
                          self$abbreviations[[length(self$abbreviations)+1]] = c(abbrev, replace)
                        }
          )
  )
# AbbreviationProcessor
#
# The interface used for replacing abbreviations in an
# Excel match spreadsheet.
AbbreviationProcessor <-
  R6Class("AbbreviationProcessor", 
          public = list(abbreviation_converters = list(), 
                        initialize = function(){
                          self$abbreviation_converters[[1]] = AbbreviationConverter_PossAction$new()
                          self$abbreviation_converters[[2]] = AbbreviationConverter_PlayType$new()
                          self$abbreviation_converters[[3]] = AbbreviationConverter_DefAction$new()
                          self$abbreviation_converters[[4]] = AbbreviationConverter_PossPlayerDisciplinary$new()
                          self$abbreviation_converters[[5]] = AbbreviationConverter_DefPlayerDisciplinary$new()
                          self$abbreviation_converters[[6]] = AbbreviationConverter_PossNotes$new()
                        },
                        # @brief  Replaces all abbreviations in a single row from
                        #         an Excel match spreadsheet.
                        #
                        # @param row  The row to perform abbreviation replacements on.
                        process_row = function(row) {
                          for(converter in self$abbreviation_converters) {
                            row[converter$column_name] = converter$convert(row[converter$column_name])
                          }
                          return(row)
                        }
          )
  )

####Tidying####
# This section is for functions that tidy up common errors
# and shortcuts taken while logging a match, as well as 
# extracting the match metadata.
TrimRowsColumns        <- function(match.df) {
  # removes NA columns
  match.df <- match.df[, !grepl("^NA", names(match.df))] 
  # removes rows after the "end.of.match"
  match.df <- match.df[1:max(grep("end.of.match", match.df[, "poss.action"])), ]
  # adds missing columns
  if (!("xG" %in% colnames(match.df))) {
    (match.df$xG <- NA)
  }
  if (!("poss.number" %in% colnames(match.df))) {
    (match.df$poss.number <- NA)
  }
  if (!("def.number" %in% colnames(match.df))) {
    (match.df$def.number <- NA)
  }
  if (!("event" %in% colnames(match.df))) {
    (match.df$event <- NA)
  }
  match.df
}

CleanUpCells           <- function(match.df) {
  # Trims whitespace, creates NAs
  match.df[(match.df) == "-" | (match.df) == " " | (match.df) == ""] <- NA
  match.df <- as.data.frame(apply(match.df, 2, trimws), stringsAsFactors = FALSE)
  kColumnsToLower   <- c("poss.action", "play.type", "def.action", 
                           "gk.ball.stop", "gk.s.o.g.attempt", 
                           "poss.player.disciplinary", "poss.notes",
                           "def.player.disciplinary", "def.notes")
  kColumnsToUpper <- c("poss.location", "poss.play.destination",
                           "def.location")
  match.df[,kColumnsToLower] <- as.data.frame(sapply(match.df[,kColumnsToLower], 
                                                       tolower), stringsAsFactors = FALSE)
  match.df[,kColumnsToUpper] <- as.data.frame(sapply(match.df[,kColumnsToUpper], 
                                                         toupper), stringsAsFactors = FALSE)
  match.df$event <- as.integer(match.df$event)
  match.df
}

GetMetaData            <- function(match.df) {
  metadata.range <- grep("kickoff", 
                          match.df[, "poss.action"])[1] - 1
  meta.df <- match.df[1:metadata.range, ]
  meta.df[["poss.player"]] <- trimws(meta.df[["poss.player"]])
  meta.df[["def.player"]] <- trimws(meta.df[["def.player"]])
  for(player.row in 1:metadata.range) {
    if (grepl("\\(", meta.df[player.row, "poss.player"]) && 
        is.na(meta.df[player.row, "poss.number"])) {
      player.strsplit <- strsplit(meta.df[player.row, "poss.player"], " ")
      player.num <- gsub("\\(|\\)", "", 
                         player.strsplit[[1]][length(player.strsplit[[1]])])
      meta.df[player.row, "poss.number"] <- player.num
    }
    if (grepl("\\(", meta.df[player.row, "def.player"]) && 
        is.na(meta.df[player.row, "def.number"])) {
      player.strsplit <- strsplit(meta.df[player.row, "def.player"], " ")
      player.num <- gsub("\\(|\\)", "", player.strsplit[[1]][length(player.strsplit[[1]])])
      meta.df[player.row, "def.number"] <- player.num
    }
  }
  meta.df
}

####Mutating####
# This section is for functions that add data that
# was left missing while logging matches.
CalcEventValue         <- function(sheet.row, i, event.col) {
  stoppage.events <- "end.of|stoppage.in.play|halftime|fulltime|playcutoff"
  if (is.na(sheet.row[["poss.player"]]) &&
      !grepl(stoppage.events, sheet.row[["poss.action"]])) {
    event.value <- event.col[i - 1]
  } else {
    event.value <- as.numeric(event.col[i - 1]) + 1
  }
  event.value
}

SetPlayerInfo          <- function(sheet.row, col.set, meta.df) {
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
  if (!is.na(sheet.row[[player.col]])) {
    player.string   <- sheet.row[[player.col]]
    player.team     <- sheet.row[[team.col]]
    player.position <- sheet.row[[position.col]]
    player.number   <- sheet.row[[number.col]]
    # checks if the player string exists in the metadata
    player.strsplit <- strsplit(player.string, " \\(")[[1]][1]
    allplayers.strsplit <- sapply(meta.df[, player.col], 
                                  function(x) tolower(strsplit(x, " \\(")[[1]][1]))
    if (tolower(player.strsplit) %in% allplayers.strsplit) {
      # get the rows in metadata where the name exists
      meta.rownum <- grep(paste0("^", tolower(player.strsplit), "$"), allplayers.strsplit)
      # checks if the player string appears only once in the metadata
      if (length(meta.rownum) == 1) {
        player.info <- meta.df[meta.rownum, c(position.col, team.col, number.col, player.col)]
        player.sans.num <- strsplit(player.info[1, player.col]," \\(")[[1]][1]
        player.info[[player.col]] <- player.sans.num
      } else {
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
        } else {
          stop(paste("Unable to determine all necessary information about", player.strsplit))
        }
      }
    }
  } else {
    player.info <- sheet.row[c(position.col, team.col, number.col, player.col)]
  }
  player.info
  if (col.set == "poss") {
    sheet.row[c("poss.position", "poss.team", 
               "poss.number", "poss.player")] <- player.info
  } else if (col.set == "def") {
    sheet.row[c("def.position", "def.team", 
                "def.number", "def.player")] <- player.info
    
  }
  sheet.row
}

GetDefLocation         <- function(sheet.row, match.df) {
  kOpposites <- data.frame(posslocations=c("A6", "A18", "A3L", "A3C", "A3R", "AM3L", "AM3C", 
                                           "AM3R", "DM3L", "DM3C", "DM3R", "D3L", "D3C", "D3R", 
                                           "D18", "D6", "AL", "AC", "AR", "AML", "AMC", 
                                           "AMR", "DML", "DMC", "DMR", "DL", "DC", "DR"),
                           deflocations=c("D6", "D18", "D3R", "D3C", "D3L", "DM3R", "DM3C",
                                          "DM3L", "AM3R", "AM3C", "AM3L", "A3R", "A3C", "A3L", 
                                          "A18", "A6", "DR", "DC", "DL", "DMR", "DMC",
                                          "DML", "AMR", "AMC", "AML", "AR", "AC", "AL"))
  ActionIsInvertible <- function(sheet.row, sheet.col) {
    # Determine if an action's location is invertible based on the
    # location of certain opposing players' action
    grepl("pressure|challenge|aerial|ground|tackle|dispossess|dribble|pass|touch|move|take|shots", 
          sheet.row[[sheet.col]])
  }
  # checks if the defensive action can have its location determined
  # based on the inverse of certain possessing actions its acting upon
  if (ActionIsInvertible(sheet.row, "def.action")) {
    # checks if the corresponding event has a value in "poss.location"
    sheet.event <- sheet.row[["event"]]
    event.rows <- match.df[!is.na(match.df[, "event"]) & 
                             match.df[, "event"]==sheet.event, ]
    event.poss.location <- event.rows[["poss.location"]][1]
    if (!is.na(event.poss.location)) {
      # returns the opposite of event_poss_location
      as.character(kOpposites[as.character(kOpposites[, "posslocations"]) == as.character(event.poss.location), "deflocations"])
    } else {
      NA # def location cannot be determined
    }
    # checks if "def.location" is blank for interceptions, which can have its location
    # determined based on location of next action, which is by definition by the intercepting
    # player at the location of the interception  
  } else if (grepl("interceptions", sheet.row[["def.action"]])) {
    # find location of next poss.player
    sheet.event <- as.numeric(sheet.row[["event"]][1])
    sheet.event.next <- as.numeric(sheet.event) + 1
    match.df[!is.na(match.df[, "event"]) & match.df[, "event"] == sheet.event.next, "poss.location"][1]
  } else {
    NA # def location cannot be determined    
  }
}

IsConclusivePass      <- function(sheet.row) {
    grepl("pass", sheet.row[["poss.action"]]) &
    !grepl("c", sheet.row[["poss.action"]])
}

IsCompletedPass        <- function(sheet.row, nevent.rows, index.event) {
  # Returns TRUE if, based on surrounding data, a pass attempt is completed
  event      <- as.numeric(sheet.row[["event"]])
  if (nrow(nevent.rows) > 1) nevent.rows <- nevent.rows[1,]
  next.event <- nevent.rows[["event"]]
  # Series of logical variables & their dependent variables about what 
  # happened in an event, to be used to determine if a pass was completed.
  cutoff.actions             <- "playcutoffbybroadcast|stoppage|substitution|halftime|fulltime|end.of.match|offside"
  next.event.wasnt.cutoff    <- !grepl(cutoff.actions, nevent.rows[["poss.action"]])
  
  next.event.wasnt.lost.duel <- !grepl("aerial.lost|ground.50.50.lost", 
                                       nevent.rows[["poss.action"]])
  
  next.event.wasnt.recovery  <- !grepl("recoveries", nevent.rows[["poss.action"]])
  
  ball.disrupt.actions       <- "interceptions|blocks|clearances|shield|high.balls.won|smothers.won|loose.balls.won"
  ball.wasnt.disrupted       <- !(TRUE %in% grepl(ball.disrupt.actions, 
                                                  index.event[["def.action"]]))
  
  gk.disrupt.actions         <- "caught|punched|dropped|collected|parried|deflected"
  gk.didnt.stop.ball         <- !grepl(gk.disrupt.actions, 
                                       index.event[["gk.ball.stop"]])
  
  poss.player.didnt.foul     <- !grepl("fouls|fouls|yellow|red|penalties", 
                                       index.event[["poss.player.disciplinary"]])
  
  ball.didnt.go.out          <- !(TRUE %in% grepl("out.of.bounds", 
                                                  unlist(index.event[c("play.type","poss.notes")])))
  
  # Series of if statements to determine if event is a completed pass, creates
  # the TRUE/FALSE statement to be returned by the function.
  next.event.wasnt.cutoff && next.event.wasnt.lost.duel && 
    next.event.wasnt.recovery && ball.wasnt.disrupted && gk.didnt.stop.ball && 
    poss.player.didnt.foul && ball.didnt.go.out
}

####Output####
# This section is for functions ultimately take the
# above functions and output the finished match sheet.
require(RCurl)
require(readxl)
TidyMatchExcel <- function(path) {
  # Reads a match spreadsheet in untidied, Excel format and creates a match
  # spreadsheet in .csv format that can be read by creating-stats.R files.
  abbreviation_processor <- AbbreviationProcessor$new()
  match.df <- as.data.frame(read_excel(path))
  match.df <- TrimRowsColumns(match.df)
  match.df <- CleanUpCells(match.df)
  meta.df  <- GetMetaData(match.df) # creates metadata subset
  kMatchDataRange <- grep("kickoff", 
                           match.df[,"poss.action"])[1]:nrow(match.df)
  match.df <- match.df[kMatchDataRange,]
  kFirstActionRow <- grep("kickoff", match.df[, "poss.action"])[1] + 1
  kEndOfMatchRow  <- nrow(match.df)
  for (i in kFirstActionRow:kEndOfMatchRow) {
    sheet.row <- match.df[i,]
    sheet.row <- abbreviation_processor$process_row(unlist(sheet.row))
    if (is.na(sheet.row[["time"]]))
      sheet.row[["time"]] <- match.df[["time"]][i - 1]
    sheet.row[["event"]] <- CalcEventValue(sheet.row, i, match.df[["event"]])
    match.df[i, ] <- sheet.row
  }
  for (i in kFirstActionRow:kEndOfMatchRow) {
    sheet.row <- match.df[i,]
    if (sum(match.df[["event"]] %in% sheet.row[["event"]]) > 1) {
      index.event <- match.df[match.df[["event"]] %in% sheet.row[["event"]],]
    } else {
      index.event <- sheet.row
    }
    if (i < kEndOfMatchRow) {
      nevent.value <- as.numeric(sheet.row[["event"]])+1
      nevent.rows <- match.df[match.df[["event"]]==nevent.value & !is.na(match.df[["event"]]),]
    }
    sheet.row <- SetPlayerInfo(sheet.row, "poss", meta.df)
    sheet.row <- SetPlayerInfo(sheet.row, "def", meta.df)
    missing.dlocation <- !is.na(sheet.row[["def.action"]]) & 
      is.na(sheet.row[["def.location"]])
    if (missing.dlocation)
      sheet.row[["def.location"]] <- GetDefLocation(sheet.row, match.df)
    if (IsConclusivePass(sheet.row) &
        IsCompletedPass(sheet.row, nevent.rows, index.event)) {
      sheet.row[["poss.action"]] <- paste0(sheet.row[["poss.action"]], ".c") 
      if (is.na(sheet.row[["poss.play.destination"]])) {
        sheet.row[["poss.play.destination"]] <-
          nevent.rows[["poss.location"]][1]
      }
    }
    match.df[i, ] <- sheet.row
  }
  match.df$event <- as.integer(match.df$event)
  match.df
}

TidyMultiMatchExcels <- function(path) {
  excel.list <- grep(".xlsx$", list.files(path), value = TRUE)
  tidied.list <- list()
  for (i in 1:length(excel.list)) {
    match.path <- paste(path, excel.list[i], sep = "/")
    tidied.list[[i]] <- TidyMatchExcel(match.path)
  }
  tidied.list
}