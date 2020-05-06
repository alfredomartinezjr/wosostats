library(readr)
library(dplyr)
library(lubridate)
library(readxl)

matches <- read_csv("source/temp_database/matches.csv", col_types = cols())
matches$filename <- paste(matches$competition_slug, tolower(matches$matchup),
                          format(mdy(matches$date), "%m%d%y"),
                          sep="-")

directory <- "source/excel"
fpaths <- list.files(directory)
mytables <- list()

calcNumber <- function(meta) {
  if (TRUE %in% grepl("\\(", meta$player)) {
    meta$number <- gsub(".*\\(|\\).*|[A-z]", "", meta$player)
    meta$player <- trimws(gsub("\\(.*", "", meta$player))
  } else {
    meta$number <- NA
  }
  meta
}

grabMeta <- function(fpath) {
  meta <- read_excel(paste(directory, fpath, sep = "/"), 
                     na = c("","-"," "), 
                     col_types = "text", 
                     n_max = 80) %>%
    rename(team = poss.team, position = poss.position,
           player = poss.player)
  meta_range <- max(which(meta$poss.action %in% "kickoff")) - 1
  meta <- meta[1:meta_range, ]
  meta <- meta %>% select(c(team, position, player))
  meta <- meta %>%  filter(!is.na(player)) %>% 
    filter(!grepl("^http", player))
  meta$player <- trimws(meta$player)
  meta$position <- trimws(meta$position)
  meta$team <- trimws(meta$team)
  meta <- calcNumber(meta)
  lineup_player_id <- c(101:(101+nrow(meta)-1))
  matchid_indx <- which(gsub("\\.xlsx","",fpath) == matches$filename)
  if (length(matchid_indx)==0) {
    print(paste("Error: file", fpath, "does not represent a match that could be found in the match database."))
    break
  }
  if (length(matchid_indx)>1) {
    print("Error with database check: filename appears more than once in match database.")
    break
  }
  match_id <- matches$match_id[matchid_indx]
  meta <- cbind(match_id, lineup_player_id, meta)
}
lineups <- lapply(fpaths, grabMeta)
lineups <- do.call(rbind, lineups)

write_csv(lineups, "source/temp_database/lineups.csv", na = "")
