library(readr)
library(dplyr)
library(lubridate)
matches <- read_csv("source/temp_database/matches.csv", col_types = cols())
matches$filename <- paste(matches$competition_slug, tolower(matches$matchup),
                          format(mdy(matches$date), "%m%d%y"),
                          sep="-")

directory <- "source/csv"
fpaths <- list.files(directory)
mytables <- list()
for (i in seq_along(fpaths)) {
  # get match source data
  dat <- read_csv(paste(directory, fpaths[i], sep = "/"), 
                  col_types = paste(c("d", rep("c", 19), "d", "c", "c"), 
                                    collapse=""))
  names(dat) <- gsub("\\.","_",names(dat))
  # get matchid
  matchid_indx <- which(gsub("\\.csv","",fpaths[i]) == matches$filename)
  if (length(matchid_indx)==0) {
    print(paste("Error: file", fpaths[i], "does not represent a match that could be found in the match database."))
    break
  }
  if (length(matchid_indx)>1) {
    print("Error with database check: filename appears more than once in match database.")
    break
  }
  matchid <- matches$matchid[matchid_indx]
  events_tbl <- select(dat, c("event", "time", "poss_team",
                              "poss_number", "poss_position", "poss_player", 
                              "poss_action", "poss_location", 
                              "poss_play_destination"))
  events_tbl <- filter(events_tbl, !is.na(poss_action))
  if ("events_tbl" %in% names(mytables)) {
    matchid_in_tbl <- matchid %in% mytables[["events_tbl"]]$matchid
    if (matchid_in_tbl == FALSE) {
      mytables[["events_tbl"]] <- rbind(mytables[["events_tbl"]],
                                      cbind(matchid, events_tbl))
    } else if (matchid_in_tbl == TRUE) {
      print("Error: match data already exists in table")
      break
    }
  } else {
    mytables[["events_tbl"]] <- cbind(matchid, events_tbl)
  }
  
  eventtype_tbl <- select(dat, c("event", "play_type"))
  eventtype_tbl <- filter(eventtype_tbl, !is.na(play_type))
  if (nrow(eventtype_tbl) > 0) {
    if ("eventtype_tbl" %in% names(mytables)) {
      matchid_in_tbl <- matchid %in% mytables[["eventtype_tbl"]]$matchid
      if (matchid_in_tbl == FALSE) {
        event_type_id <- c(1001:(1001+nrow(eventtype_tbl)-1))
        mytables[["eventtype_tbl"]] <- rbind(mytables[["eventtype_tbl"]],
                                             cbind(matchid, event_type_id, 
                                                   eventtype_tbl))
      } else if (matchid_in_tbl == TRUE) {
        print("Error: match data already exists in table")
        break
      }
    } else {
      event_type_id <- c(1001:(1001+nrow(eventtype_tbl)-1))
      mytables[["eventtype_tbl"]] <- cbind(matchid, event_type_id, eventtype_tbl)
    }
  }
  
  defend_tbl <- select(dat, c("event", "def_team", "def_number", 
                              "def_position", "def_player", "def_action",
                              "def_location", "gk_ball_stop"))
  defend_tbl <- filter(defend_tbl, !is.na(def_action))
  if (nrow(defend_tbl) > 0) {
    if ("defend_tbl" %in% names(mytables)) {
      matchid_in_tbl <- matchid %in% mytables[["defend_tbl"]]$matchid
      if (matchid_in_tbl == FALSE) {
        defend_id <- c(1001:(1001+nrow(defend_tbl)-1))
        mytables[["defend_tbl"]] <- rbind(mytables[["defend_tbl"]],
                                          cbind(matchid, defend_id, 
                                                defend_tbl))
      } else if (matchid_in_tbl == TRUE) {
        print("Error: match data already exists in table")
        break
      }
    } else {
      defend_id <- c(1001:(1001+nrow(defend_tbl)-1))
      mytables[["defend_tbl"]] <- cbind(matchid, defend_id, defend_tbl)
    }
  }
  
  possdiscp_tbl <- select(dat, c("event", "poss_player_disciplinary"))
  possdiscp_tbl <- filter(possdiscp_tbl, !is.na(poss_player_disciplinary))
  if (nrow(possdiscp_tbl) > 0) {
    if ("possdiscp_tbl" %in% names(mytables)) {
      matchid_in_tbl <- matchid %in% mytables[["possdiscp_tbl"]]$matchid
      if (matchid_in_tbl == FALSE) {
        possdiscp_id <- c(1001:(1001+nrow(possdiscp_tbl)-1))
        mytables[["possdiscp_tbl"]] <- rbind(mytables[["possdiscp_tbl"]],
                                             cbind(matchid, possdiscp_id, 
                                                   possdiscp_tbl))
      } else if (matchid_in_tbl == TRUE) {
        print("Error: match data already exists in table")
        break
      }
    } else {
      possdiscp_id <- c(1001:(1001+nrow(possdiscp_tbl)-1))
      mytables[["possdiscp_tbl"]] <- cbind(matchid, possdiscp_id, possdiscp_tbl)
    }
  }
  
  defdiscp_tbl <- select(dat, c("event", "def_team", "def_number", 
                                "def_position", "def_player",
                                "def_player_disciplinary"))
  defdiscp_tbl <- filter(defdiscp_tbl, !is.na(def_player_disciplinary))
  if (nrow(defdiscp_tbl) > 0) {
    if ("defdiscp_tbl" %in% names(mytables)) {
      matchid_in_tbl <- matchid %in% mytables[["defdiscp_tbl"]]$matchid
      if (matchid_in_tbl == FALSE) {
        defdiscp_id <- c(1001:(1001+nrow(defdiscp_tbl)-1))
        mytables[["defdiscp_tbl"]] <- rbind(mytables[["defdiscp_tbl"]],
                                            cbind(matchid, defdiscp_id, 
                                                  defdiscp_tbl))
      } else if (matchid_in_tbl == TRUE) {
        print("Error: match data already exists in table")
        break
      }
    } else {
      defdiscp_id <- c(1001:(1001+nrow(defdiscp_tbl)-1))
      mytables[["defdiscp_tbl"]] <- cbind(matchid, defdiscp_id, defdiscp_tbl)
    }
  }
  
  possnotes_tbl <- select(dat, c("event", "poss_notes"))
  possnotes_tbl <- filter(possnotes_tbl, !is.na(poss_notes))
  if (nrow(possnotes_tbl) > 0) {
    if ("possnotes_tbl" %in% names(mytables)) {
      matchid_in_tbl <- matchid %in% mytables[["possnotes_tbl"]]$matchid
      if (matchid_in_tbl == FALSE) {
        possnotes_id <- c(1001:(1001+nrow(possnotes_tbl)-1))
        mytables[["possnotes_tbl"]] <- rbind(mytables[["possnotes_tbl"]],
                                             cbind(matchid, possnotes_id, 
                                                   possnotes_tbl))
      } else if (matchid_in_tbl == TRUE) {
        print("Error: match data already exists in table")
        break
      }
    } else {
      possnotes_id <- c(1001:(1001+nrow(possnotes_tbl)-1))
      mytables[["possnotes_tbl"]] <- cbind(matchid, possnotes_id, possnotes_tbl)
    }
  }

  defnotes_tbl <- select(dat, c("event", "def_notes"))
  defnotes_tbl <- filter(defnotes_tbl, !is.na(def_notes))
  if (nrow(defnotes_tbl) > 0) {
    if ("defnotes_tbl" %in% names(mytables)) {
      matchid_in_tbl <- matchid %in% mytables[["defnotes_tbl"]]$matchid
      if (matchid_in_tbl == FALSE) {
        defnotes_id <- c(1001:(1001+nrow(defnotes_tbl)-1))
        mytables[["defnotes_tbl"]] <- rbind(mytables[["defnotes_tbl"]],
                                            cbind(matchid, defnotes_id, 
                                                  defnotes_tbl))
      } else if (matchid_in_tbl == TRUE) {
        print("Error: match data already exists in table")
        break
      }
    } else {
      defnotes_id <- c(1001:(1001+nrow(defnotes_tbl)-1))
      mytables[["defnotes_tbl"]] <- cbind(matchid, defnotes_id, defnotes_tbl)
    }
  }
}

uniq_event_id <- c(1000000:(1000000+nrow(mytables[["events_tbl"]])-1))
mytables[["events_tbl"]] <- cbind(uniq_event_id, mytables[["events_tbl"]])

write_csv(mytables[["events_tbl"]], "source/temp_database/events.csv", na = "")
write_csv(mytables[["eventtype_tbl"]], "source/temp_database/event_type.csv", na = "")
write_csv(mytables[["defend_tbl"]], "source/temp_database/defending.csv", na = "")
write_csv(mytables[["possdiscp_tbl"]], "source/temp_database/poss_discipline.csv", na = "")
write_csv(mytables[["defdiscp_tbl"]], "source/temp_database/def_discipline.csv", na = "")
write_csv(mytables[["possnotes_tbl"]], "source/temp_database/poss_notes.csv", na = "")
write_csv(mytables[["defnotes_tbl"]], "source/temp_database/def_notes.csv", na = "")
