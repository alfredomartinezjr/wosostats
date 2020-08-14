library(tidyverse)

getStats <- function() {
  events <- read_csv("https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/events.csv", 
                     col_types = cols()) %>%
    filter(!is.na(lineup_player_id))
  event_type <- read_csv("https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/event_type.csv", 
                         col_types = cols())
  defending <- read_csv("https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/defending.csv", 
                        col_types = cols()) %>%
    filter(!is.na(lineup_player_id))
  poss_notes <- read_csv("https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/poss_notes.csv", 
                         col_types = cols())
  def_notes <- read_csv("https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/def_notes.csv", 
                        col_types = cols()) %>%
    filter(!is.na(lineup_player_id))
  
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
    group_by(match_id, lineup_player_id) %>%
    summarise(aerial_duels_p = sum(grepl("^aerial", poss_action)),
              aerials_won_p = sum(grepl("aerial\\.won", poss_action)),
              clearances_p = sum(grepl("clearances", poss_action)),
              dispossessed = sum(grepl("dispossessed", poss_action)),
              pass_att = sum(grepl("passes", poss_action)),
              pass_comp = sum(grepl("passes.*c", poss_action)),
              ppass_att = sum(grepl("passes", poss_action) & pressd_ev),
              ppass_comp = sum(grepl("passes.*c", poss_action) & pressd_ev),
              op_pass_att = sum(grepl("passes", poss_action) & openplay_ev),
              op_pass_comp = sum(grepl("passes.*c", poss_action) & openplay_ev),
              op_ppass_att = sum(grepl("passes", poss_action) & pressd_ev & openplay_ev),
              op_ppass_comp = sum(grepl("passes.*c", poss_action) & pressd_ev & openplay_ev),
              fw_pass_att = sum(grepl("passes\\.f", poss_action)),
              fw_pass_comp = sum(grepl("passes\\.f\\.c", poss_action)),
              s_pass_att = sum(grepl("passes\\.s", poss_action)),
              s_pass_comp = sum(grepl("passes\\.s\\.c", poss_action)),
              b_pass_att = sum(grepl("passes\\.b", poss_action)),
              b_pass_comp = sum(grepl("passes\\.b\\.c", poss_action)),
              fw_op_pass_att = sum(grepl("passes\\.f", poss_action) & openplay_ev),
              fw_op_pass_comp = sum(grepl("passes\\.f\\.c", poss_action) & openplay_ev),
              s_op_pass_att = sum(grepl("passes\\.s", poss_action) & openplay_ev),
              s_op_pass_comp = sum(grepl("passes\\.s\\.c", poss_action) & openplay_ev),
              b_op_pass_att = sum(grepl("passes\\.b", poss_action) & openplay_ev),
              b_op_pass_comp = sum(grepl("passes\\.b\\.c", poss_action) & openplay_ev),
              fw_ppass_att = sum(grepl("passes\\.f", poss_action) & pressd_ev),
              fw_ppass_comp = sum(grepl("passes\\.f\\.c", poss_action) & pressd_ev),
              s_ppass_att = sum(grepl("passes\\.s", poss_action) & pressd_ev),
              s_ppass_comp = sum(grepl("passes\\.s\\.c", poss_action) & pressd_ev),
              b_ppass_att = sum(grepl("passes\\.b", poss_action) & pressd_ev),
              b_ppass_comp = sum(grepl("passes\\.b\\.c", poss_action) & pressd_ev),
              recoveries = sum(grepl("recoveries", poss_action)),
              shots = sum(grepl("^shot", poss_action)),
              goals = sum(grepl("^shots\\.scored", poss_action)),
              shots_missed = sum(grepl("^shots\\.missed", poss_action)),
              shots_blocked = sum(grepl("^shots\\.blocked", poss_action)),
              shots_saved = sum(grepl("^shots\\.stopped", poss_action)),
              shots_pressed = sum(grepl("^shot", poss_action)  & pressd_ev),
              take_ons = sum(grepl("^take\\.on", poss_action)),
              take_ons_won = sum(grepl("^take\\.on\\.won", poss_action)),
              crosses = sum(uniq_event_id %in% cross_ev),
              crosses_comp = sum(grepl("passes.*c", poss_action) & cross_ev),
              launch_att = sum(uniq_event_id %in% launch_ev),
              launch_comp = sum(grepl("passes.*c", poss_action) & launch_ev),
              ck_taken = sum(ck_ev),
              ck_comp = sum(grepl("passes.*c", poss_action) & ck_ev),
              ck_assists = sum(ck_ev & assist_ev),
              ck_key_passes = sum(ck_ev & keypass_ev),
              fk_taken = sum(fk_ev),
              fk_passatt = sum(grepl("passes", poss_action) & fk_ev),
              fk_passcomp = sum(grepl("passes.*c", poss_action) & fk_ev),
              fk_shot = sum(grepl("^shot", poss_action) & fk_ev),
              fk_scored = sum(grepl("^shots\\.scored", poss_action) & fk_ev),
              fk_assists = sum(fk_ev & assist_ev),
              fk_key_passes = sum(fk_ev & keypass_ev),
              through_att = sum(through_ev),
              through_comp = sum(grepl("passes.*c", poss_action) & through_ev),
              throwin_att = sum(throw_ev),
              throwin_comp = sum(grepl("passes.*c", poss_action) & throw_ev),
              assists = sum(assist_ev),
    )
  
  tbl_defending <- defending %>% 
    group_by(match_id, lineup_player_id) %>%
    cbind(def_passes_ev = def_passes_ev, def_shots_ev = def_shots_ev,
          def_bigchances = def_bigchances) %>%
    summarise(aerial_duels_d = sum(grepl("^aerial", def_action)),
              aerials_won_d = sum(grepl("aerial\\.won", def_action)),
              blocks = sum(grepl("blocks", def_action)),
              shot_blocks = sum(grepl("blocks", def_action) & def_shots_ev),
              pass_blocks = sum(grepl("blocks", def_action) & def_passes_ev),
              clearances_d = sum(grepl("clearances", def_action)),
              dispossess_opp = sum(grepl("^dispossess", def_action)),
              interceptions = sum(grepl("^intercept", def_action)),
              tackles = sum(grepl("tackles|^tkw", def_action)),
              dribbled_byopp = sum(grepl("dribbled", def_action)),
              pressured_opp = sum(grepl("pressure", def_action)),
              challenged_opp = sum(grepl("challenge", def_action)),
              ball_shields = sum(grepl("ball\\.shield", def_action)),
              gk_saves = sum(grepl("gk.s.o.g.stop", def_action)),
              gk_goal_conceded = sum(grepl("gk.s.o.g.scored", def_action)),
              gk_bigchances_saved = sum(grepl("gk.s.o.g.stop", def_action) & 
                                          def_bigchances),
              gk_bigchances_conceded = 
                sum(grepl("gk.s.o.g.scored", def_action) & 
                      def_bigchances),
              gk_bigchances_sog_faced = 
                sum(grepl("gk.*score|gk.s.o.g.stop|gk.shot.on.goal", def_action) & 
                      def_bigchances),
              gk_highballs = sum(grepl("^gk\\.high\\.ball", def_action)),
              gk_highballs_won = sum(grepl("^gk\\.high\\.ball.*won", 
                                           def_action))
    )
  tbl_all <- full_join(tbl_events, tbl_defending, 
                       by = c("match_id", "lineup_player_id")) %>%
    replace(., is.na(.), 0) %>%
    mutate(aerial_duels_p = aerial_duels_p + aerial_duels_d,
           aerials_won_p = aerials_won_p + aerials_won_d,
           clearances_p = clearances_p + clearances_d) %>%
    rename(aerial_duels = aerial_duels_p,
           aerials_won = aerials_won_p,
           clearances = clearances_p) %>%
    select(-aerial_duels_d, -aerials_won_d, -clearances_d)
  
  tbl_possnotes <- left_join(poss_notes, select(events, uniq_event_id, lineup_player_id), 
                             by = c("uniq_event_id")) %>%
    group_by(match_id, lineup_player_id) %>%
    summarise(big_chances = sum(grepl("^big\\.", poss_notes) & 
                                  !grepl("created", poss_notes)),
              bc_goals = sum(grepl("^big\\..*scored", poss_notes)),
              bc_sog = sum(grepl("^big\\..*on\\.goal", poss_notes)),
              bc_smiss = sum(grepl("^big\\..*miss", poss_notes)),
              bc_dispossessed = sum(grepl("^big\\..*dispossessed", poss_notes)),
              bc_lost = sum(grepl("^big\\..*lost", poss_notes)),
              bc_created = sum(grepl("^big\\..*created", poss_notes) &
                                 grepl("^key\\.pass", poss_notes)),
              key_passes = sum(grepl("^key\\.pass", poss_notes)),
              key_assists = sum(grepl("^key\\.pass", poss_notes) &
                                  grepl("^assists$", poss_notes)),
              second_assists = sum(grepl("second\\.assists", poss_notes)),
              err_togoals_p = sum(grepl("errors\\.to\\.goals", poss_notes)),
              err_tobc_p = sum(grepl("errors\\..*chances", poss_notes))
    )
  tbl_all <- left_join(tbl_all, tbl_possnotes, 
                       by = c("match_id", "lineup_player_id")) %>%
    replace(., is.na(.), 0)
  
  tbl_defnotes <- def_notes %>%
    group_by(match_id, lineup_player_id) %>%
    summarise(bc_stopped = sum(grepl("^big\\..*\\.stopped$", def_notes)),
              own_goals = sum(grepl("own\\.goals", def_notes)),
              err_togoals_d = sum(grepl("errors\\.to\\.goals", def_notes)),
              err_tobc_d = sum(grepl("errors\\..*chances", def_notes))
    )
  tbl_all <- left_join(tbl_all, tbl_defnotes, 
                       by = c("match_id", "lineup_player_id")) %>%
    replace(., is.na(.), 0) %>%
    mutate(err_togoals_p = err_togoals_p + err_togoals_d,
           err_tobc_p = err_tobc_p + err_tobc_d) %>%
    rename(err_togoals = err_togoals_p,
           err_tobc = err_tobc_p) %>%
    select(-err_togoals_d, -err_tobc_d)
  
  tbl_all
}

addMetadata <- function(tbl_all) {
  tbl_all <- read_csv("https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/lineups.csv", 
                      col_types = cols()) %>% 
    select(-number) %>%
    right_join(tbl_all, by = c("match_id", "lineup_player_id"))
  tbl_all <- read_csv("https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/matches.csv", 
                      col_types = cols()) %>%
    select(match_id, competition_slug, date, matchup) %>%
    right_join(tbl_all, by = "match_id")
  tbl_all
}

tbl_all <- getStats()
tbl_all <- addMetadata(tbl_all)
