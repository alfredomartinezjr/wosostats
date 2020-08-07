library(tidyverse)

getStats <- function() {
  events <- read_csv("source/temp_database/events.csv", 
                     col_types = cols()) %>%
    filter(!is.na(lineup_player_id))
  
  tbl_events <- events %>% group_by(match_id, lineup_player_id) %>%
    summarise(aerial_duels_p = sum(grepl("^aerial", poss_action)), # to be merged with same stat from defending tblable
              aerials_won_p = sum(grepl("aerial\\.won", poss_action)), # to be merged with same stat from defending tblable
              clearances_p = sum(grepl("clearances", poss_action)), # to be merged with same stat from defending tblable
              dispossessed = sum(grepl("dispossessed", poss_action)),
              pass_att = sum(grepl("passes", poss_action)),
              pass_comp = sum(grepl("passes.*c", poss_action)),
              recoveries = sum(grepl("recoveries", poss_action)),
              shots = sum(grepl("^shot", poss_action)),
              goals = sum(grepl("^shots\\.scored", poss_action)),
              shots_missed = sum(grepl("^shots\\.missed", poss_action)),
              shots_blocked = sum(grepl("^shots\\.blocked", poss_action)),
              shots_saved = sum(grepl("^shots\\.stopped", poss_action)),
              take_ons = sum(grepl("^take\\.on", poss_action)),
              take_ons_won = sum(grepl("^take\\.on\\.won", poss_action))
    )
  
  event_type <- read_csv("source/temp_database/event_type.csv", 
                         col_types = cols())
  tbl_evtype <- left_join(events, event_type, by = c("match_id", "event")) %>%
    group_by(match_id, lineup_player_id) %>%
    summarise(crosses = sum(grepl("crosses", play_type)),
              crosses_comp = sum(grepl("crosses", play_type) & 
                                   grepl("passes.*c", poss_action)),
              ck_taken = sum(grepl("corner\\.kick", play_type)),
              ck_comp = sum(grepl("corner\\.kick", play_type) & 
                              grepl("passes.*c", poss_action)),
              fk_taken = sum(grepl("free\\.kick", play_type)),
              fk_passatt = sum(grepl("free\\.kick", play_type) & 
                                 grepl("passes", poss_action)),
              fk_passcomp = sum(grepl("free\\.kick", play_type) & 
                                  grepl("passes.*c", poss_action)),
              fk_shot = sum(grepl("free\\.kick", play_type) & 
                              grepl("^shot", poss_action)),
              fk_scored = sum(grepl("free\\.kick", play_type) & 
                                grepl("^shots\\.scored", poss_action)),
              through_att = sum(grepl("through", play_type)),
              through_comp = sum(grepl("through", play_type) & 
                                   grepl("passes.*c", poss_action)),
              throwin_att = sum(grepl("throw\\.in", play_type)),
              throwin_comp = sum(grepl("throw\\.in", play_type) & 
                                   grepl("passes.*c", poss_action))
    )
  tbl_all <- left_join(tbl_events, tbl_evtype, 
                       by = c("match_id", "lineup_player_id"))
  
  defending <- read_csv("source/temp_database/defending.csv", 
                        col_types = cols()) %>%
    filter(!is.na(lineup_player_id))
  tbl_defending <- defending %>% group_by(match_id, lineup_player_id) %>%
    summarise(aerial_duels_d = sum(grepl("^aerial", def_action)), # to be merged with same stat from events tblable
              aerials_won_d = sum(grepl("aerial\\.won", def_action)), # to be merged with same stat from events tblable
              blocks = sum(grepl("blocks", def_action)),
              clearances_d = sum(grepl("clearances", def_action)), # to be merged with same stat from events tblable
              dispossess_opp = sum(grepl("^dispossess", def_action)),
              interceptions = sum(grepl("^intercept", def_action)),
              tackles = sum(grepl("tackles|^tkw", def_action)),
              dribbled_byopp = sum(grepl("dribbled", def_action)),
              gk_saves = sum(grepl("gk.s.o.g.stop", def_action)),
              gk_goal_conceded = sum(grepl("gk.s.o.g.scored", def_action)),
              gk_highballs = sum(grepl("^gk\\.high\\.ball", def_action)),
              gk_highballs_won = sum(grepl("^gk\\.high\\.ball.*won", def_action))
    )
  tbl_all <- full_join(tbl_all, tbl_defending, 
                       by = c("match_id", "lineup_player_id")) %>%
    replace(., is.na(.), 0) %>%
    mutate(aerial_duels_p = aerial_duels_p + aerial_duels_d,
           aerials_won_p = aerials_won_p + aerials_won_d,
           clearances_p = clearances_p + clearances_d) %>%
    rename(aerial_duels = aerial_duels_p,
           aerials_won = aerials_won_p,
           clearances = clearances_p) %>%
    select(-aerial_duels_d, -aerials_won_d, -clearances_d)
  
  poss_notes <- read_csv("source/temp_database/poss_notes.csv", 
                         col_types = cols())
  tbl_possnotes <- left_join(events, poss_notes, by = c("match_id", "event")) %>%
    group_by(match_id, lineup_player_id) %>%
    summarise(assists = sum(grepl("^assists$", poss_notes)),
              big_chances = sum(grepl("^big\\.", poss_notes) & 
                                  !grepl("created", poss_notes)),
              bc_goals = sum(grepl("^big\\..*scored", poss_notes)),
              bc_sog = sum(grepl("^big\\..*on\\.goal", poss_notes)),
              bc_smiss = sum(grepl("^big\\..*miss", poss_notes)),
              bc_dispossessed = sum(grepl("^big\\..*dispossessed", poss_notes)),
              bc_lost = sum(grepl("^big\\..*lost", poss_notes)),
              bc_created = sum(grepl("^big\\..*created", poss_notes) &
                                 grepl("^key\\.pass", poss_notes)),
              key_passes = sum(grepl("^key\\.pass", poss_notes)),
              second_assists = sum(grepl("second\\.assists", poss_notes)),
              err_togoals_p = sum(grepl("errors\\.to\\.goals", poss_notes)),
              err_tobc_p = sum(grepl("errors\\..*chances", poss_notes))
    )
  tbl_all <- left_join(tbl_all, tbl_possnotes, 
                       by = c("match_id", "lineup_player_id")) %>%
    replace(., is.na(.), 0)
  
  def_notes <- read_csv("source/temp_database/def_notes.csv", 
                        col_types = cols()) %>%
    filter(!is.na(lineup_player_id))
  tbl_defnotes <- left_join(defending, def_notes, 
                            by = c("match_id", "event", "lineup_player_id")) %>%
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
  
  poss_discipline <- read_csv("source/temp_database/poss_discipline.csv", 
                              col_types = cols())
  tbl_possdiscp <- left_join(events, poss_discipline,
                             by = c("match_id","event")) %>%
    group_by(match_id, lineup_player_id) %>%
    summarise(fouls_won_p = sum(grepl("foul.*won", poss_action) |
                                  grepl("foul.*won", poss_player_disciplinary)),
              fouls_conceded_p = sum(grepl("foul.*concede", poss_action) |
                                       grepl("foul.*concede", poss_player_disciplinary)),
              yellowcards_p = sum(grepl("yellow", poss_action) |
                                    grepl("yellow", poss_player_disciplinary)),
              redcards_p = sum(grepl("red", poss_action) |
                                 grepl("red", poss_player_disciplinary)),
              pk_won_p = sum(grepl("penalties\\.won", poss_action) |
                               grepl("penalties\\.won", poss_player_disciplinary)),
              pk_conceded_p = sum(grepl("penalties\\.concede", poss_action) |
                                    grepl("penalties\\.concede", poss_player_disciplinary))
    )
  tbl_all <- left_join(tbl_all, tbl_possdiscp, 
                       by = c("match_id", "lineup_player_id")) %>%
    replace(., is.na(.), 0)
  
  def_discipline <- read_csv("source/temp_database/def_discipline.csv", 
                             col_types = cols()) %>%
    filter(!is.na(lineup_player_id))
  tbl_defdiscp <- left_join(defending, def_discipline,
                            by = c("match_id", "event", "lineup_player_id")) %>%
    group_by(match_id, lineup_player_id) %>%
    summarise(fouls_won_d = sum(grepl("foul.*won", def_action) |
                                  grepl("foul.*won", def_player_disciplinary)),
              fouls_conceded_d = sum(grepl("foul.*concede", def_action) |
                                       grepl("foul.*concede", def_player_disciplinary)),
              yellowcards_d = sum(grepl("yellow", def_action) |
                                    grepl("yellow", def_player_disciplinary)),
              redcards_d = sum(grepl("red", def_action) |
                                 grepl("red", def_player_disciplinary)),
              pk_won_d = sum(grepl("penalties\\.won", def_action) |
                               grepl("penalties\\.won", def_player_disciplinary)),
              pk_conceded_d = sum(grepl("penalties\\.concede", def_action) |
                                    grepl("penalties\\.concede", def_player_disciplinary))
    )
  tbl_all <- left_join(tbl_all, tbl_defdiscp, 
                       by = c("match_id", "lineup_player_id")) %>%
    replace(., is.na(.), 0) %>%
    mutate(fouls_won_p = fouls_won_p + fouls_won_d,
           fouls_conceded_p = fouls_conceded_p + fouls_conceded_d,
           yellowcards_p = yellowcards_p + yellowcards_d,
           redcards_p = redcards_p + redcards_d,
           pk_won_p = pk_won_p + pk_won_d,
           pk_conceded_p = pk_conceded_p + pk_conceded_d) %>%
    rename(fouls_won = fouls_won_p,
           fouls_conceded = fouls_conceded_p,
           yellowcards = yellowcards_p,
           redcards = redcards_p,
           pk_won = pk_won_p,
           pk_conceded = pk_conceded_p) %>%
    select(-fouls_won_d, -fouls_conceded_d, -yellowcards_d, 
           -redcards_d, -pk_won_d, -pk_conceded_d)
  tbl_all
}

addMetadata <- function(tbl_all) {
  tbl_all <- read_csv("source/temp_database/lineups.csv", 
                      col_types = cols()) %>% 
    select(-number) %>%
    right_join(tbl_all, by = c("match_id", "lineup_player_id"))
  tbl_all <- read_csv("source/temp_database/matches.csv", 
                      col_types = cols()) %>%
    select(match_id, competition_slug, date, matchup) %>%
    right_join(tbl_all, by = "match_id")
  tbl_all
}

tbl_all <- getStats()
tbl_all <- addMetadata(tbl_all)
tbl_all
