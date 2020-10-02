#' @import dplyr
#' @importFrom rlang .data
GetMinsPlayed <- function(events) {
  calcTime <- function(x) {
    sum(as.numeric(unlist(strsplit(x, "\\+"))))
  }
  calcHalf <- function(x) {
    regtime <- as.numeric(trimws(strsplit(x, "\\+")[[1]][1]))
    if (regtime <= 45) 1
    else if (regtime <= 90) 2
    else if (regtime <= 105) 3
    else if (regtime <= 120) 4
  }
  events <- select(events, .data$match_id, .data$time, 
                   .data$lineup_player_id, .data$poss_action)
  match_dat <- events %>% group_by(.data$match_id) %>%
    summarise(firsth_endmin = .data$time[.data$poss_action == "halftime"],
              secondh_endmin = ifelse("fulltime" %in% .data$poss_action, 
                                      .data$time[.data$poss_action == "fulltime"], 
                                      .data$time[.data$poss_action == "end.of.match"]),
              firstet_endmin = ifelse("end.of.1.et" %in% .data$poss_action,
                                      .data$time[.data$poss_action == "end.of.1.et"],
                                      ""),
              secondet_endmin = ifelse("end.of.1.et" %in% .data$poss_action,
                                       .data$time[.data$poss_action == "end.of.2.et" | 
                                                    .data$poss_action == "end.of.match"],
                                       "")
    )
  plyr_mins <- events  %>%
    filter(!is.na(.data$lineup_player_id))  %>%
    left_join(match_dat, by = "match_id") %>%
    group_by(.data$match_id, .data$lineup_player_id) %>% 
    summarise(subbedon = "substitution.on" %in% .data$poss_action,
              subbedoff = "substitution.off" %in% .data$poss_action,
              startplay_min = ifelse(.data$subbedon, 
                                     .data$time[.data$poss_action %in% "substitution.on"], "1"),
              endplay_min = ifelse(.data$subbedoff, 
                                   .data$time[.data$poss_action %in% "substitution.off"],
                                   ifelse(sum(.data$secondet_endmin == "") > 0,
                                          max(.data$secondh_endmin), 
                                          max(.data$secondet_endmin))),
              startplay_half = calcHalf(.data$startplay_min),
              endplay_half = calcHalf(.data$endplay_min),
              firsth_end = max(.data$firsth_endmin),
              secondh_end = max(.data$secondh_endmin),  
              firstet_end = max(.data$firstet_endmin), 
              secondet_end = max(.data$secondet_endmin),
              firsth_startplay = ifelse(1 %in% .data$startplay_half:.data$endplay_half,
                                        .data$startplay_min, 
                                        "-1"),
              firsth_endplay = ifelse(1 %in% seq(from = .data$startplay_half, 
                                                 to = .data$endplay_half),
                                      ifelse(.data$endplay_half == 1,
                                             .data$endplay_min,
                                             .data$firsth_end),
                                      "-2"),
              secondh_startplay = ifelse(2 %in% .data$startplay_half:.data$endplay_half,
                                         ifelse(.data$startplay_half == 2, 
                                                .data$startplay_min,
                                                "46"),
                                         "-1"),
              secondh_endplay = ifelse(2 %in% .data$startplay_half:.data$endplay_half,
                                       ifelse(.data$endplay_half == 2,
                                              .data$endplay_min,
                                              .data$secondh_end),
                                       "-2"),
              firstet_startplay = ifelse(3 %in% .data$startplay_half:.data$endplay_half,
                                         ifelse(.data$startplay_half == 3,
                                                .data$startplay_min,
                                                "91"),
                                         "-1"),
              firstet_endplay = ifelse(3 %in% .data$startplay_half:.data$endplay_half,
                                       ifelse(.data$endplay_half == 3,
                                              .data$endplay_min,
                                              .data$firstet_end),
                                       "-2"),
              secondet_startplay = ifelse(4 %in% .data$startplay_half:.data$endplay_half,
                                          ifelse(.data$startplay_half == 4,
                                                 .data$startplay_min,
                                                 "105"),
                                          "-1"),
              secondet_endplay = ifelse(4 %in% .data$startplay_half:.data$endplay_half,
                                        .data$endplay_min,
                                        "-2"),
              firsth_minplayed = calcTime(.data$firsth_endplay) - calcTime(.data$firsth_startplay) +1,
              secondh_minplayed = calcTime(.data$secondh_endplay) - calcTime(.data$secondh_startplay) +1,
              firstet_minplayed = calcTime(.data$firstet_endplay) - calcTime(.data$firstet_startplay) +1,
              secondet_minplayed = calcTime(.data$secondet_endplay) - calcTime(.data$secondet_startplay) +1,
              MP = .data$firsth_minplayed + .data$secondh_minplayed + .data$firstet_minplayed + .data$secondet_minplayed
    )
  plyr_mins <- select(plyr_mins, .data$match_id, .data$lineup_player_id, .data$MP)
  plyr_mins
}

AddMetadata <- function(tbl_all, matches, lineups) {
  tbl_all <- select(lineups, !(.data$number)) %>% 
    right_join(tbl_all, by = c("match_id", "lineup_player_id"))
  tbl_all <- matches %>%
    select(.data$match_id, .data$competition_slug, .data$date, .data$matchup) %>%
    right_join(tbl_all, by = "match_id")
  tbl_all
}