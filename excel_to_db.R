source("abbreviations.R")
source("excel_to_db_functions.R")
library(readr)
matches_tbl <- read_csv("source/temp_database/matches.csv", col_types = cols())
lineups_tbl <- read_csv("source/temp_database/lineups.csv", col_types = cols())

library(readxl)
library(lubridate)
library(dplyr)
TidyMatchExcel <- function(filename, directory="source/excel", 
                           matches=matches_tbl,
                           lineups=lineups_tbl) {
  mypath <- paste(directory, filename, sep="/")
  match_id <- GetMatchId(path = mypath, matches = matches)
  match_source <- ReadMatchLog(path = mypath)
  match_source <- CleanUpCells(match_source)
  for (i in seq_along(match_source$time)) 
    if(is.na(match_source$time[i])) 
      match_source$time[i] <- match_source$time[i-1]
  match_source <- CalcEventValues(match_source = match_source)
  match_source <- ExpandAbbreviations(match_source = match_source)
  mytables_ls <- CreateTables(match_source = match_source, 
                              match_id = match_id)
  mytables_ls <- SetPlayerInfo(mytables_ls = mytables_ls,
                               lineups = lineups_tbl, 
                               match_id = match_id)
  if (matches$location_data[matches$match_id == match_id]) {
    mytables_ls[["defend_events"]]$def_location <- 
      unlist(GetDefLocation(mytables_ls = mytables_ls))
  }
  if (class(mytables_ls[["defend_events"]]$def_location) == "list") {
    print("Error at line 31: check why def_location is a list")
    break
  }
  mytables_ls[["events"]]$poss_action[IsCompletedPass(mytables_ls)] <- 
    paste0(mytables_ls[["events"]]$poss_action[IsCompletedPass(mytables_ls)], ".c")
  mytables_ls
}

alltables_ls <- list()
for (i in list.files("source/excel")) {
  print(paste("reading ", i, "..."))
  itables_ls <- TidyMatchExcel(filename = i)
  print(paste(i, ": excel successfully tidied."))
  if(!exists("alltables_ls")) {
    alltables_ls <- itables_ls
    if (class(alltables_ls[["defend_events"]]$def_location) == "list") {
      print("Error at line 47: check why def_location is a list")
      break
    }
  } else {
    alltables_ls[["events"]] <- rbind(alltables_ls[["events"]],
                                       itables_ls[["events"]])
    alltables_ls[["ev_type"]] <- rbind(alltables_ls[["ev_type"]],
                                       itables_ls[["ev_type"]])
    alltables_ls[["defend_events"]] <- rbind(alltables_ls[["defend_events"]],
                                        itables_ls[["defend_events"]])
    if (class(alltables_ls[["defend_events"]]$def_location) == "list") {
      print("Error at line 58: check why def_location is a list")
      break
    }
    alltables_ls[["poss_discp"]] <- rbind(alltables_ls[["poss_discp"]],
                                        itables_ls[["poss_discp"]])
    alltables_ls[["def_discp"]] <- rbind(alltables_ls[["def_discp"]],
                                        itables_ls[["def_discp"]])
    alltables_ls[["poss_notes"]] <- rbind(alltables_ls[["poss_notes"]],
                                        itables_ls[["poss_notes"]])
    alltables_ls[["def_notes"]] <- rbind(alltables_ls[["def_notes"]],
                                        itables_ls[["def_notes"]])
  }
  print(paste("database has", nrow(alltables_ls[["events"]]), "events", 
              "and represents", length(unique(alltables_ls[["events"]]$match_id)),
              "matches"))
}

alltables_ls[["events"]] <- 
  cbind(uniq_event_id = c(1000000:(1000000+nrow(alltables_ls[["events"]])-1)), 
        alltables_ls[["events"]])
alltables_ls[["ev_type"]] <- 
  cbind(uniq_ev_type_id = c(1000000:(1000000+nrow(alltables_ls[["ev_type"]])-1)),
        alltables_ls[["ev_type"]])
alltables_ls[["defend_events"]] <- 
  cbind(uniq_def_ev_id = c(1000000:(1000000+nrow(alltables_ls[["defend_events"]])-1)),
        alltables_ls[["defend_events"]])
if (class(alltables_ls[["defend_events"]]$def_location) == "list") {
  print("Error at line 85: check why def_location is a list")
  break
}
alltables_ls[["poss_discp"]] <- 
  cbind(uniq_poss_discp_id = c(1000000:(1000000+nrow(alltables_ls[["poss_discp"]])-1)),
        alltables_ls[["poss_discp"]])
alltables_ls[["def_discp"]] <- 
  cbind(uniq_def_discp_id = c(1000000:(1000000+nrow(alltables_ls[["def_discp"]])-1)),
        alltables_ls[["def_discp"]])
alltables_ls[["poss_notes"]] <- 
  cbind(uniq_poss_notes_id = c(1000000:(1000000+nrow(alltables_ls[["poss_notes"]])-1)),
        alltables_ls[["poss_notes"]])
alltables_ls[["def_notes"]] <- 
  cbind(uniq_def_notes_id = c(1000000:(1000000+nrow(alltables_ls[["def_notes"]])-1)),
        alltables_ls[["def_notes"]])

write_csv(alltables_ls[["events"]], "source/temp_database/events.csv", na = "")
write_csv(alltables_ls[["ev_type"]], "source/temp_database/event_type.csv", na = "")
write_csv(alltables_ls[["defend_events"]], "source/temp_database/defending.csv", na = "")
write_csv(alltables_ls[["poss_discp"]], "source/temp_database/poss_discipline.csv", na = "")
write_csv(alltables_ls[["def_discp"]], "source/temp_database/def_discipline.csv", na = "")
write_csv(alltables_ls[["poss_notes"]], "source/temp_database/poss_notes.csv", na = "")
write_csv(alltables_ls[["def_notes"]], "source/temp_database/def_notes.csv", na = "")
