#' Gets player stats from the WoSo Stats database
#' 
#' \code{get_player_stats} creates a data frame representing a summary of advanced
#'  stats for each player for each match in the WoSo Stats database. A glossary for
#'  each metric is at \url{https://www.wosostats.com/glossary/}.
#' 
#' @param tbl_list A list of data frames representing the different types of tables
#'  in the WoSo Stats database. You can build this data frame yourself by running
#'  \code{get_table} multiple times or by running \code{get_table_all}. If built 
#'  by hand, each element in the list must be named exactly how it is named when
#'  running \code{get_table_all}.
#' 
#' @return A data frame
#' 
#' @import dplyr
#' @importFrom rlang .data
#' @export
#' 
get_player_stats <- function(tbl_list) {
  def_discipline <- tbl_list[["def_discipline"]]
  def_notes <- tbl_list[["def_notes"]]
  defending <- tbl_list[["defending"]]
  event_type <- tbl_list[["event_type"]]
  events <- tbl_list[["events"]]
  lineups <- tbl_list[["lineups"]]
  matches <- tbl_list[["matches"]]
  poss_discipline <- tbl_list[["poss_discipline"]]
  poss_notes <- tbl_list[["poss_notes"]]
  rosters <- tbl_list[["rosters"]]
  tbl_minsplyd <- GetMinsPlayed(events)
  events <- events %>% filter(!is.na(.data$lineup_player_id))
  defending <- defending %>% filter(!is.na(.data$lineup_player_id))
  def_notes <- def_notes %>% filter(!is.na(.data$lineup_player_id))
  
  pressd_ev <- events$uniq_event_id %in% 
    unique(defending$uniq_event_id[grep("press|challeng",
                                        defending$def_action)])
  openplay_ev <- !events$uniq_event_id %in% 
    unique(event_type$uniq_event_id[grepl("free\\.kick|corner\\.kick|goal\\.kick|throw", 
                                          event_type$play_type)])
  cross_ev <- events$uniq_event_id %in% 
    unique(event_type$uniq_event_id[grep("cross", 
                                         event_type$play_type)])
  ck_ev <- events$uniq_event_id %in% 
    unique(event_type$uniq_event_id[grep("corner\\.kick", 
                                         event_type$play_type)])
  fk_ev <- events$uniq_event_id %in% 
    unique(event_type$uniq_event_id[grep("free\\.kick", 
                                         event_type$play_type)])
  through_ev <- events$uniq_event_id %in% 
    unique(event_type$uniq_event_id[grep("through", 
                                         event_type$play_type)])
  throw_ev <- events$uniq_event_id %in% 
    unique(event_type$uniq_event_id[grep("throw\\.in", 
                                         event_type$play_type)])
  launch_ev <- events$uniq_event_id %in% 
    unique(event_type$uniq_event_id[grep("launch", 
                                         event_type$play_type)])
  assist_ev <- events$uniq_event_id %in% 
    unique(poss_notes$uniq_event_id[grep("^assists$", 
                                         poss_notes$poss_notes)])
  keypass_ev <- events$uniq_event_id %in% 
    unique(poss_notes$uniq_event_id[grep("^key\\.pass", 
                                         poss_notes$poss_notes)])
  def_passes_ev <- defending$uniq_event_id %in%
    unique(events$uniq_event_id[grep("passes", events$poss_action)])
  def_shots_ev <- defending$uniq_event_id %in%
    unique(events$uniq_event_id[grep("^shot", events$poss_action)])
  def_bigchances <- defending$uniq_event_id %in%
    unique(def_notes$uniq_event_id[grep("^big\\.", poss_notes$poss_notes)])
  
  
  
  tbl_events <- events %>% 
    cbind(pressd_ev = pressd_ev, openplay_ev = openplay_ev, cross_ev = cross_ev,
          ck_ev = ck_ev, fk_ev = fk_ev, through_ev = through_ev, throw_ev = throw_ev,
          launch_ev = launch_ev, assist_ev = assist_ev, keypass_ev = keypass_ev) %>%
    group_by(.data$match_id, .data$lineup_player_id) %>%
    summarise(aerial_duels_p = sum(grepl("^aerial", .data$poss_action)),
              aerials_won_p = sum(grepl("aerial\\.won", .data$poss_action)),
              clearances_p = sum(grepl("clearances", .data$poss_action)),
              dispossessed = sum(grepl("dispossessed", .data$poss_action)),
              pass_att = sum(grepl("passes", .data$poss_action)),
              pass_comp = sum(grepl("passes.*c", .data$poss_action)),
              ppass_att = sum(grepl("passes", .data$poss_action) & .data$pressd_ev),
              ppass_comp = sum(grepl("passes.*c", .data$poss_action) & .data$pressd_ev),
              op_pass_att = sum(grepl("passes", .data$poss_action) & .data$openplay_ev),
              op_pass_comp = sum(grepl("passes.*c", .data$poss_action) & .data$openplay_ev),
              op_ppass_att = sum(grepl("passes", .data$poss_action) & .data$pressd_ev & .data$openplay_ev),
              op_ppass_comp = sum(grepl("passes.*c", .data$poss_action) & .data$pressd_ev & .data$openplay_ev),
              fw_pass_att = sum(grepl("passes\\.f", .data$poss_action)),
              fw_pass_comp = sum(grepl("passes\\.f\\.c", .data$poss_action)),
              s_pass_att = sum(grepl("passes\\.s", .data$poss_action)),
              s_pass_comp = sum(grepl("passes\\.s\\.c", .data$poss_action)),
              b_pass_att = sum(grepl("passes\\.b", .data$poss_action)),
              b_pass_comp = sum(grepl("passes\\.b\\.c", .data$poss_action)),
              fw_op_pass_att = sum(grepl("passes\\.f", .data$poss_action) & .data$openplay_ev),
              fw_op_pass_comp = sum(grepl("passes\\.f\\.c", .data$poss_action) & .data$openplay_ev),
              s_op_pass_att = sum(grepl("passes\\.s", .data$poss_action) & .data$openplay_ev),
              s_op_pass_comp = sum(grepl("passes\\.s\\.c", .data$poss_action) & .data$openplay_ev),
              b_op_pass_att = sum(grepl("passes\\.b", .data$poss_action) & .data$openplay_ev),
              b_op_pass_comp = sum(grepl("passes\\.b\\.c", .data$poss_action) & .data$openplay_ev),
              fw_ppass_att = sum(grepl("passes\\.f", .data$poss_action) & .data$pressd_ev),
              fw_ppass_comp = sum(grepl("passes\\.f\\.c", .data$poss_action) & .data$pressd_ev),
              s_ppass_att = sum(grepl("passes\\.s", .data$poss_action) & .data$pressd_ev),
              s_ppass_comp = sum(grepl("passes\\.s\\.c", .data$poss_action) & .data$pressd_ev),
              b_ppass_att = sum(grepl("passes\\.b", .data$poss_action) & .data$pressd_ev),
              b_ppass_comp = sum(grepl("passes\\.b\\.c", .data$poss_action) & .data$pressd_ev),
              recoveries = sum(grepl("recoveries", .data$poss_action)),
              shots = sum(grepl("^shot", .data$poss_action)),
              goals = sum(grepl("^shots\\.scored", .data$poss_action)),
              shots_missed = sum(grepl("^shots\\.missed", .data$poss_action)),
              shots_blocked = sum(grepl("^shots\\.blocked", .data$poss_action)),
              shots_saved = sum(grepl("^shots\\.stopped", .data$poss_action)),
              shots_pressed = sum(grepl("^shot", .data$poss_action) & .data$pressd_ev),
              take_ons = sum(grepl("^take\\.on", .data$poss_action)),
              take_ons_won = sum(grepl("^take\\.on\\.won", .data$poss_action)),
              crosses = sum(.data$uniq_event_id %in% .data$cross_ev),
              crosses_comp = sum(grepl("passes.*c", .data$poss_action) & .data$cross_ev),
              launch_att = sum(.data$uniq_event_id %in% .data$launch_ev),
              launch_comp = sum(grepl("passes.*c", .data$poss_action) & .data$launch_ev),
              ck_taken = sum(.data$ck_ev),
              ck_comp = sum(grepl("passes.*c", .data$poss_action) & .data$ck_ev),
              ck_assists = sum(.data$ck_ev & .data$assist_ev),
              ck_key_passes = sum(.data$ck_ev & .data$keypass_ev),
              fk_taken = sum(.data$fk_ev),
              fk_passatt = sum(grepl("passes", .data$poss_action) & .data$fk_ev),
              fk_passcomp = sum(grepl("passes.*c", .data$poss_action) & .data$fk_ev),
              fk_shot = sum(grepl("^shot", .data$poss_action) & .data$fk_ev),
              fk_scored = sum(grepl("^shots\\.scored", .data$poss_action) & .data$fk_ev),
              fk_assists = sum(.data$fk_ev & .data$assist_ev),
              fk_key_passes = sum(.data$fk_ev & .data$keypass_ev),
              through_att = sum(.data$through_ev),
              through_comp = sum(grepl("passes.*c", .data$poss_action) & .data$through_ev),
              throwin_att = sum(.data$throw_ev),
              throwin_comp = sum(grepl("passes.*c", .data$poss_action) & .data$throw_ev),
              assists = sum(.data$assist_ev),
    )
  
  tbl_defending <- defending %>% 
    group_by(.data$match_id, .data$lineup_player_id) %>%
    cbind(def_passes_ev = def_passes_ev, def_shots_ev = def_shots_ev,
          def_bigchances = def_bigchances) %>%
    summarise(aerial_duels_d = sum(grepl("^aerial", .data$def_action)),
              aerials_won_d = sum(grepl("aerial\\.won", .data$def_action)),
              blocks = sum(grepl("blocks", .data$def_action)),
              shot_blocks = sum(grepl("blocks", .data$def_action) & .data$def_shots_ev),
              pass_blocks = sum(grepl("blocks", .data$def_action) & .data$def_passes_ev),
              clearances_d = sum(grepl("clearances", .data$def_action)),
              dispossess_opp = sum(grepl("^dispossess", .data$def_action)),
              interceptions = sum(grepl("^intercept", .data$def_action)),
              tackles = sum(grepl("tackles|^tkw", .data$def_action)),
              dribbled_byopp = sum(grepl("dribbled", .data$def_action)),
              pressured_opp = sum(grepl("pressure", .data$def_action)),
              challenged_opp = sum(grepl("challenge", .data$def_action)),
              ball_shields = sum(grepl("ball\\.shield", .data$def_action)),
              gk_saves = sum(grepl("gk.s.o.g.stop", .data$def_action)),
              gk_goal_conceded = sum(grepl("gk.s.o.g.scored", .data$def_action)),
              gk_bigchances_saved = sum(grepl("gk.s.o.g.stop", .data$def_action) & 
                                          .data$def_bigchances),
              gk_bigchances_conceded = 
                sum(grepl("gk.s.o.g.scored", .data$def_action) & 
                      .data$def_bigchances),
              gk_bigchances_sog_faced = 
                sum(grepl("gk.*score|gk.s.o.g.stop|gk.shot.on.goal", .data$def_action) & 
                      .data$def_bigchances),
              gk_highballs = sum(grepl("^gk\\.high\\.ball", .data$def_action)),
              gk_highballs_won = sum(grepl("^gk\\.high\\.ball.*won", 
                                           .data$def_action))
    )
  tbl_all <- full_join(tbl_events, tbl_defending, 
                       by = c("match_id", "lineup_player_id"))
  tbl_all <- replace(tbl_all, is.na(tbl_all), 0)
  tbl_all <- tbl_all %>%
    mutate(aerial_duels_p = .data$aerial_duels_p + .data$aerial_duels_d,
           aerials_won_p = .data$aerials_won_p + .data$aerials_won_d,
           clearances_p = .data$clearances_p + .data$clearances_d) %>%
    rename(aerial_duels = .data$aerial_duels_p,
           aerials_won = .data$aerials_won_p,
           clearances = .data$clearances_p) %>%
    select(!c(.data$aerial_duels_d, .data$aerials_won_d, .data$clearances_d))
  
  tbl_possnotes <- left_join(poss_notes, 
                             select(events, .data$uniq_event_id, .data$lineup_player_id),
                             by = c("uniq_event_id")) %>%
    group_by(.data$match_id, .data$lineup_player_id) %>%
    summarise(big_chances = sum(grepl("^big\\.", .data$poss_notes) & 
                                  !grepl("created", .data$poss_notes)),
              bc_goals = sum(grepl("^big\\..*scored", .data$poss_notes)),
              bc_sog = sum(grepl("^big\\..*on\\.goal", .data$poss_notes)),
              bc_smiss = sum(grepl("^big\\..*miss", .data$poss_notes)),
              bc_dispossessed = sum(grepl("^big\\..*dispossessed", .data$poss_notes)),
              bc_lost = sum(grepl("^big\\..*lost", .data$poss_notes)),
              bc_created = sum(grepl("^big\\..*created", .data$poss_notes) &
                                 grepl("^key\\.pass", .data$poss_notes)),
              key_passes = sum(grepl("^key\\.pass", .data$poss_notes)),
              key_assists = sum(grepl("^key\\.pass", .data$poss_notes) &
                                  grepl("^assists$", .data$poss_notes)),
              second_assists = sum(grepl("second\\.assist", .data$poss_notes)),
              err_togoals_p = sum(grepl("error.*to\\.goal", .data$poss_notes)),
              err_tobc_p = sum(grepl("errors.*chance", .data$poss_notes))
    )
  tbl_all <- left_join(tbl_all, tbl_possnotes, 
                       by = c("match_id", "lineup_player_id"))
  tbl_all <- replace(tbl_all, is.na(tbl_all), 0)
  
  tbl_defnotes <- def_notes %>%
    group_by(.data$match_id, .data$lineup_player_id) %>%
    summarise(bc_stopped = sum(grepl("^big\\..*\\.stopped$", .data$def_notes)),
              own_goals = sum(grepl("own\\.goals", .data$def_notes)),
              err_togoals_d = sum(grepl("error.*to\\.goal", .data$def_notes)),
              err_tobc_d = sum(grepl("errors.*chance", .data$def_notes))
    )
  tbl_all <- left_join(tbl_all, tbl_defnotes, 
                       by = c("match_id", "lineup_player_id"))
  tbl_all <- replace(tbl_all, is.na(tbl_all), 0)
  tbl_all <- tbl_all %>%
    mutate(err_togoals_p = .data$err_togoals_p + .data$err_togoals_d,
           err_tobc_p = .data$err_tobc_p + .data$err_tobc_d) %>%
    rename(err_togoals = .data$err_togoals_p,
           err_tobc = .data$err_tobc_p) %>%
    select(!c(.data$err_togoals_d, .data$err_tobc_d))
  tbl_all <- left_join(tbl_minsplyd, tbl_all, by= c("match_id", "lineup_player_id"))
  tbl_all <- AddMetadata(tbl_all, matches, lineups)
  tbl_all
}