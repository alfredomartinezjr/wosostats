#' Get all tables from the WoSo Stats database 
#' 
#' \code{get_table_all} creates a list of data frames for all tables in the WoSo 
#'  Stats database. Each table is an Amazon S3 object which is ultimately, 
#'  within \code{get_table_all}, read as a csv file and assigned to a data frame.
#' 
#' @return A list of data frames.
#' @export
#' @examples
#' all_tables <- get_table_all()
get_table_all <- function() {
  db_tables <- list()
  db_tables[["def_discipline"]] <- get_table("def_discipline")
  db_tables[["def_notes"]] <- get_table("def_notes")
  db_tables[["defending"]] <- get_table("defending")
  db_tables[["event_type"]] <- get_table("event_type")
  db_tables[["events"]] <- get_table("events")
  db_tables[["lineups"]] <- get_table("lineups")
  db_tables[["matches"]] <- get_table("matches")
  db_tables[["poss_discipline"]] <- get_table("poss_discipline")
  db_tables[["poss_notes"]] <- get_table("poss_notes")
  db_tables[["rosters"]] <- get_table("rosters")
  db_tables
}