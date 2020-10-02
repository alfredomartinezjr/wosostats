#' Get a table from the WoSo Stats database 
#' 
#' \code{get_table} creates a data frame for a specified table in the WoSo Stats
#'  database. There are specific tables you can read, outlined below. Each table 
#'  is an Amazon S3 object which is ultimately, within \code{get_table}, read
#'  as a csv file.
#' 
#' @param object The name of the object in the WoSo Stats database.
#' @return A data frame representing \code{object}.
#' @export
#' @examples
#' dat <- get_table("events")
#' dat <- get_table("defending")
get_table <- function(object) {
  host <- "https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/"
  if (object == "def_discipline")
    object_path <- "def_discipline.csv"
  else if (object == "def_notes") 
    object_path <- "def_notes.csv"
  else if (object == "defending")
    object_path <- "defending.csv"
  else if (object == "event_type")
    object_path <- "event_type.csv"
  else if (object == "events")
    object_path <- "events.csv"
  else if (object == "lineups")
    object_path <- "lineups.csv"
  else if (object == "matches")
    object_path <- "matches.csv"
  else if (object == "poss_discipline")
    object_path <- "poss_discipline.csv"
  else if (object == "poss_notes")
    object_path <- "poss_notes.csv"
  else if (object == "rosters")
    object_path <- "rosters.csv"
  fullpath <- paste0(host, object_path)
  db_table <- utils::read.csv(fullpath, stringsAsFactors = FALSE)
  db_table
}