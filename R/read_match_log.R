#' Read an Excel match log
#' 
#' \code{read_match_log} reads a match log (in Excel format) in your working 
#'  directory and creates a list of data frames representing different types of
#'  events in the match log. Match logs can be downloaded from a \href{https://drive.google.com/drive/folders/13-8Ws14GougTk_FZBv4k-VUCaRyml1hj?usp=sharing}{a Google Drive folder}
#'  which is also linked in the README.
#' 
#' @param filename The filename of the Excel file in your working directory 
#'  representing the match log you want to read.
#' @param matches A data frame representing the "matches" table from the WoSo 
#'  Stats database.
#' @param lineups A data frame representing the "lineups" table from the WoSo 
#'  Stats database.
#' 
#' @return A list of data frames
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' matches_tbl <- get_table("matches")
#' lineups_tbl <- get_table("lineups")
#' match_tables <- read_match_log(filename = "nwsl-2016-hou-orl-052016.xlsx",
#' matches = matches_tbl, lineups = lineups_tbl)
#' }
read_match_log <- function(filename, matches, lineups) {
  mypath <- paste(filename, sep="/")
  match_id <- GetMatchId(path = mypath, matches = matches)
  match_source <- ReadMatchLog(path = mypath)
  match_source <- CleanUpCells(match_source)
  for (i in seq_along(match_source$time)) 
    if(is.na(match_source$time[i])) 
      match_source$time[i] <- match_source$time[i-1]
  match_source <- CalcEventValues(match_source = match_source)
  match_source <- ExpandAbbrevs(match_source = match_source)
  mytables_ls <- CreateTables(match_source = match_source, 
                              match_id = match_id)
  mytables_ls <- SetPlayerInfo(mytables_ls = mytables_ls,
                               lineups = lineups, 
                               match_id = match_id)
  if (matches$location_data[matches$match_id == match_id]) {
    mytables_ls[["defend_events"]]$def_location <- 
      unlist(GetDefLocation(mytables_ls = mytables_ls))
  }
  if (class(mytables_ls[["defend_events"]]$def_location) == "list") {
    stop("Check why def_location is a list")
  } else {
    mytables_ls[["events"]]$poss_action[IsCompletedPass(mytables_ls)] <- 
      paste0(mytables_ls[["events"]]$poss_action[IsCompletedPass(mytables_ls)], ".c")
    mytables_ls
  }
}