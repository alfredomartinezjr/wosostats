#' Reads multiple Excel match logs
#' 
#' \code{read_multi_match_log} reads all the match logs (in Excel format) in your
#'  working directory and creates a list of data frames aggregating the different
#'  types of events in each match.
#' 
#' @param matches A data frame representing the "matches" table from the WoSo 
#'  Stats database.
#' @param lineups A data frame representing the "lineups" table from the WoSo 
#'  Stats database.
#' @return A list of data frames each representing various types of data sourced 
#'  from the Excel files in your working directory.
#' @import dplyr
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' matches_tbl <- get_table("matches")
#' lineups_tbl <- get_table("lineups")
#' match_tables <- read_multi_match_log(matches = matches_tbl, lineups = lineups_tbl)
#' }
read_multi_match_log <- function(matches, lineups) {
  alltables_ls <- list()
  for (i in list.files()) {
    print(paste("reading ", i, "..."))
    itables_ls <- read_match_log(filename = i, 
                                 matches = matches, 
                                 lineups = lineups)
    print(paste(i, ": excel successfully tidied."))
    if(!exists("alltables_ls")) {
      alltables_ls <- itables_ls
      if (class(alltables_ls[["defend_events"]]$def_location) == "list") {
        stop("Check why def_location is a list")
      }
    } else {
      alltables_ls[["events"]] <- rbind(alltables_ls[["events"]],
                                        itables_ls[["events"]])
      alltables_ls[["ev_type"]] <- rbind(alltables_ls[["ev_type"]],
                                         itables_ls[["ev_type"]])
      alltables_ls[["defend_events"]] <- rbind(alltables_ls[["defend_events"]],
                                               itables_ls[["defend_events"]])
      if (class(alltables_ls[["defend_events"]]$def_location) == "list") {
        stop("Check why def_location is a list")
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
    cbind(uniq_event_id = c(1000001:
                              (1000001+nrow(alltables_ls[["events"]])-1)),
          alltables_ls[["events"]])
  alltables_ls[["ev_type"]] <- 
    cbind(uniq_evtype_id = c(1000001:
                               (1000001+nrow(alltables_ls[["ev_type"]])-1)),
          right_join(select(alltables_ls[["events"]], 
                            .data$uniq_event_id, .data$match_id, .data$event),
                     alltables_ls[["ev_type"]],
                     by = c("match_id", "event")))
  if (class(alltables_ls[["defend_events"]]$def_location) == "list") {
    stop("Check why def_location returns a list")
  }
  alltables_ls[["defend_events"]] <- 
    cbind(uniq_defend_id = c(1000001:
                               (1000001+nrow(alltables_ls[["defend_events"]])-1)),
          right_join(select(alltables_ls[["events"]], 
                            .data$uniq_event_id, .data$match_id, .data$event),
                     alltables_ls[["defend_events"]],
                     by = c("match_id", "event")))
  alltables_ls[["poss_discp"]] <- 
    cbind(uniq_poss_discp_id = c(1000001:
                                   (1000001+nrow(alltables_ls[["poss_discp"]])-1)),
          right_join(select(alltables_ls[["events"]], 
                            .data$uniq_event_id, .data$match_id, .data$event),
                     alltables_ls[["poss_discp"]],
                     by = c("match_id", "event")))
  alltables_ls[["def_discp"]] <- 
    cbind(uniq_def_discp_id = c(1000001:
                                  (1000001+nrow(alltables_ls[["def_discp"]])-1)),
          right_join(select(alltables_ls[["events"]], 
                            .data$uniq_event_id, .data$match_id, .data$event),
                     alltables_ls[["def_discp"]],
                     by = c("match_id", "event")))
  alltables_ls[["poss_notes"]] <- 
    cbind(uniq_poss_notes_id = c(1000001:
                                   (1000001+nrow(alltables_ls[["poss_notes"]])-1)),
          right_join(select(alltables_ls[["events"]], 
                            .data$uniq_event_id, .data$match_id, .data$event),
                     alltables_ls[["poss_notes"]],
                     by = c("match_id", "event")))
  alltables_ls[["def_notes"]] <- 
    cbind(uniq_def_notes_id = c(1000001:
                                  (1000001+nrow(alltables_ls[["def_notes"]])-1)),
          right_join(select(alltables_ls[["events"]], 
                            .data$uniq_event_id, .data$match_id, .data$event),
                     alltables_ls[["def_notes"]],
                     by = c("match_id", "event")))
  alltables_ls
}