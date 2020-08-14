## Excel match logs can be downloaded from this Google Drive folder:
## https://drive.google.com/drive/folders/13-8Ws14GougTk_FZBv4k-VUCaRyml1hj?usp=sharing

library(readr)
GetMatchId <- function(path, matches) {
  filenames <- paste(matches$competition_slug, 
                     tolower(matches$matchup), 
                     format(mdy(matches$date), "%m%d%y"),
                     sep="-")
  match_indx <- which(gsub("source\\/excel\\/|\\.xlsx","",path) == filenames)
  if (length(match_indx)==0) {
    print(paste("Error: file", path, "does not represent a match that could be found in the match database."))
    break
  }
  if (length(match_indx)>1) {
    print("Error with database check: filename appears more than once in match database.")
    break
  }
  match_id <- matches$match_id[match_indx]
  match_id
}

ReadMatchLog <- function(path) {
  match_source <- read_excel(path, na = c("","-"," "), col_types = "text")
  events_range <- c(min(which(match_source$poss.action %in% "kickoff")), 
                    max(which(match_source$poss.action %in% "end.of.match")))
  match_source <- match_source[events_range[1]:events_range[2], ]
}

CleanUpCells <- function(match_source) {
  match_ls <- lapply(match_source, trimws)
  kColsToLower   <- c("poss.action", "play.type", "def.action", 
                      "gk.ball.stop", "gk.s.o.g.attempt", 
                      "poss.player.disciplinary", "poss.notes",
                      "def.player.disciplinary", "def.notes")
  kColsToUpper <- c("poss.location", "poss.play.destination",
                    "def.location")
  match_ls[kColsToLower] <- lapply(match_ls[kColsToLower], tolower)
  match_ls[kColsToUpper] <- lapply(match_ls[kColsToUpper], toupper)
  match_ls$event <- as.integer(match_ls$event)
  match_source <- as.data.frame(match_ls, stringsAsFactors = FALSE)
  names(match_source) <- gsub("\\.","_",names(match_source))
  match_source
}

CalcEventValues <- function(match_source) {
  is_newevent <- !is.na(match_source$poss_action)
  for (i in seq_along(match_source$event)[-1]) {
    if (!is_newevent[i]) {
      match_source$event[i] <- match_source$event[i-1]
    } else if (is_newevent[i]){
      match_source$event[i] <- match_source$event[i-1]+1
    }
  }
  match_source
}

ExpandAbbrevs <- function(match_source) {
  poss_action <- match_source$poss_action
  poss_action[grep("^sgk", poss_action)] <- "shots.stopped.by.gk"
  poss_action[grep("^sdef", poss_action)] <- "shots.stopped.by.def"
  poss_action[grep("^sb", poss_action)] <- "shots.blocked"
  poss_action[grep("^sc", poss_action)] <- "shots.scored"
  poss_action[grep("^sm", poss_action)] <- "shots.missed"
  poss_action[grep("^pf", poss_action)] <- "passes.f"
  poss_action[grep("^ps", poss_action)] <- "passes.s"
  poss_action[grep("^pb", poss_action)] <- "passes.b"
  poss_action[grep("^m", poss_action)] <- "movement"
  poss_action[grep("^tkw", poss_action)] <- "take.on.won"
  poss_action[grep("^tkl", poss_action)] <- "take.on.lost"
  poss_action[grep("^d", poss_action)] <- "dispossessed"
  poss_action[grep("^lt", poss_action)] <- "lost.touch"
  poss_action[grep("^bt", poss_action)] <- "ball.touch"
  poss_action[grep("^gw", poss_action)] <- "ground.50.50.won"
  poss_action[grep("^gl", poss_action)] <- "ground.50.50.lost"
  poss_action[grep("^aw", poss_action)] <- "aerial.won"
  poss_action[grep("^al", poss_action)] <- "aerial.lost"
  poss_action[grep("^r", poss_action)] <-  "recoveries"
  poss_action[grep("^bs", poss_action)] <- "ball.shield"
  poss_action[grep("^cl$|^cl ", poss_action)] <- "clearances"
  poss_action[grep("^playcutoff", poss_action)] <- "playcutoffbybroadcast"
  poss_action[grep("^fw", poss_action)] <- "fouls.won"
  poss_action[grep("^fc", poss_action)] <- "fouls.conceded"
  match_source$poss_action <- poss_action
  
  play_type <- match_source$play_type
  play_type[grep("^th|^th$", play_type)] <- "through"
  play_type[grep("^cc|^cr|^dc", play_type)] <- "crosses"
  play_type[grep("^s", play_type)] <-          "switch"
  play_type[grep("^lau", play_type)] <-        "launch"
  play_type[grep("^ti", play_type)] <-         "throw.in"
  play_type[grep("^fk", play_type)] <-         "free.kick"
  play_type[grep("^h", play_type)] <-          "headed"
  play_type[grep("^ck", play_type)] <-         "corner.kick"
  play_type[grep("^gk$|gk ", play_type)] <-    "goal.kick"
  play_type[grep("^gkt", play_type)] <-        "gk.throws"
  play_type[grep("^gkdk", play_type)] <-       "gk.drop.kick"
  play_type[grep("^pk", play_type)] <-         "penalty.kick"
  play_type[grep("^pip", play_type)] <-        "pass.into.pressure"
  play_type[grep("^keep.poss|^kept.poss", play_type)] <- "out.of.bounds.keep.poss"
  play_type[grep("^lost.poss|^lose.poss", play_type)] <- "out.of.bounds.lost.poss"
  match_source$play_type <- play_type
  
  def_action <- match_source$def_action
  def_action[grep("^dbs|^bs", def_action)] <- "ball.shield"
  def_action[grep("^dis|^ds|^dlt", def_action)] <- "dispossessed"
  def_action[grep("^tb|^tba|^tbw", def_action)] <- "tackles.ball"
  def_action[grep("^dtm|^dor|^dt|^dr", def_action)] <- "dribbled"
  def_action[grep("^p", def_action)] <-       "pressured"
  def_action[grep("^ch", def_action)] <-      "challenged"
  def_action[grep("^bl", def_action)] <-      "blocks"
  def_action[grep("^int", def_action)] <-     "interceptions"
  def_action[grep("^bd", def_action)] <-      "ball.shield"
  def_action[grep("^cl", def_action)] <-      "clearances"
  def_action[grep("^cl.h.p$|^cl.p.h$", def_action)] <-      "clearances.headed.pressed"
  def_action[grep("^cl.h$", def_action)] <-   "clearances.headed"
  def_action[grep("^cl.p$", def_action)] <-   "clearances.pressed"
  def_action[grep("^aw", def_action)] <-      "aerial.won"
  def_action[grep("^al", def_action)] <-      "aerial.lost"
  def_action[grep("^fw", def_action)] <-      "fouls.won"
  def_action[grep("^fc", def_action)] <-      "fouls.conceded"
  def_action[grep("^bt", def_action)] <-      "ball.touch"
  def_action[grep("^gw", def_action)] <-      "ground.50.50.won"
  def_action[grep("^gl", def_action)] <-      "ground.50.50.lost"
  match_source$def_action <- def_action
  
  pp_discip <- match_source$poss_player_disciplinary
  pp_discip[grep("^fw", pp_discip)] <-      "fouls.won"
  pp_discip[grep("^fc", pp_discip)] <-      "fouls.conceded"
  match_source$poss_player_disciplinary <- pp_discip
  
  dp_discip <- match_source$def_player_disciplinary
  dp_discip[grep("^fw", dp_discip)] <- "fouls.won"
  dp_discip[grep("^fc", dp_discip)] <- "fouls.conceded"
  match_source$def_player_disciplinary <- dp_discip
  
  poss_notes <- match_source$poss_notes
  poss_notes[grep("^keep.poss|^kept.poss", poss_notes)] <- "out.of.bounds.keep.poss"
  poss_notes[grep("^lost.poss|^lose.poss", poss_notes)] <- "out.of.bounds.lost.poss"
  match_source$poss_notes <- poss_notes
  match_source
}

CreateTables <- function(match_source = match_source, 
                         match_id = match_id) {
  mytables_ls <- list()
  mytables_ls[["events"]] <- match_source %>% 
    select(c("event", "time", "poss_team","poss_position", 
             "poss_player","poss_action", "poss_location", 
             "poss_play_destination")) %>% 
    filter(!is.na(poss_action)) %>%
    rename(team = poss_team, 
           position = poss_position, 
           player = poss_player)
  mytables_ls[["events"]] <- cbind(match_id, mytables_ls[["events"]])
  mytables_ls[["ev_type"]] <- match_source %>% 
    select(c("event", "play_type")) %>%
    filter(!is.na(play_type))
  if (nrow(mytables_ls[["ev_type"]]) > 0) {
    mytables_ls[["ev_type"]] <- cbind(match_id, 
                                      evtype_id = c(1001:(1001+nrow(mytables_ls[["ev_type"]])-1)), 
                                      mytables_ls[["ev_type"]])
  } else if (nrow(mytables_ls[["ev_type"]]) == 0) {
    mytables_ls[["ev_type"]] <- cbind(match_id = numeric(), 
                                      evtype_id = numeric(), 
                                      mytables_ls[["ev_type"]])
  }
  mytables_ls[["defend_events"]] <- match_source %>% 
    select(c("event", "def_team", "def_position", 
             "def_player", "def_action",
             "def_location", "gk_ball_stop")) %>% 
    filter(!is.na(def_action)) %>%
    rename(team = def_team, position = def_position, player = def_player)
  if (nrow(mytables_ls[["defend_events"]]) > 0) {
    mytables_ls[["defend_events"]] <- cbind(match_id, 
                                            defend_id = c(1001:(1001+nrow(mytables_ls[["defend_events"]])-1)), 
                                            mytables_ls[["defend_events"]])
  } else if (nrow(mytables_ls[["defend_events"]]) == 0) {
    mytables_ls[["defend_events"]] <- cbind(match_id = numeric(), 
                                            defend_id = numeric(), 
                                            mytables_ls[["defend_events"]])
  }
  mytables_ls[["poss_discp"]] <- match_source %>% 
    select(c("event", "poss_player_disciplinary")) %>%
    filter(!is.na(poss_player_disciplinary))
  if (nrow(mytables_ls[["poss_discp"]]) > 0) {
    mytables_ls[["poss_discp"]] <- cbind(match_id,
                                         possdiscp_id = c(1001:(1001+nrow(mytables_ls[["poss_discp"]])-1)),
                                         mytables_ls[["poss_discp"]])
  } else if (nrow(mytables_ls[["poss_discp"]]) == 0) {
    mytables_ls[["poss_discp"]] <- cbind(match_id = numeric(),
                                         possdiscp_id = numeric(),
                                         mytables_ls[["poss_discp"]])
  }
  mytables_ls[["def_discp"]] <- match_source %>% 
    select(c("event", "def_team", "def_position", 
             "def_player", "def_player_disciplinary")) %>% 
    filter(!is.na(def_player_disciplinary)) %>%
    rename(team = def_team, position = def_position, player = def_player)
  if (nrow(mytables_ls[["def_discp"]]) > 0) {
    mytables_ls[["def_discp"]] <- cbind(match_id,
                                        defdiscp_id = c(1001:(1001+nrow(mytables_ls[["def_discp"]])-1)),
                                        mytables_ls[["def_discp"]])
  } else if (nrow(mytables_ls[["def_discp"]]) == 0) {
    mytables_ls[["def_discp"]] <- cbind(match_id = numeric(),
                                        defdiscp_id = numeric(),
                                        mytables_ls[["def_discp"]])
  }
  mytables_ls[["poss_notes"]] <- match_source %>%
    select(c("event", "poss_notes")) %>%
    filter(!is.na(poss_notes))
  if (nrow(mytables_ls[["poss_notes"]]) > 0) {
    mytables_ls[["poss_notes"]] <- cbind(match_id,
                                         possnotes_id = c(1001:(1001+nrow(mytables_ls[["poss_notes"]])-1)),
                                         mytables_ls[["poss_notes"]])
  } else if (nrow(mytables_ls[["poss_notes"]]) == 0) {
    mytables_ls[["poss_notes"]] <- cbind(match_id = numeric(),
                                         possnotes_id = numeric(),
                                         mytables_ls[["poss_notes"]])
  }
  mytables_ls[["def_notes"]] <- match_source %>% 
    select(c("event", "def_team", "def_position", 
             "def_player", "def_notes")) %>% 
    filter(!is.na(def_notes)) %>% 
    rename(team = def_team, position = def_position, player = def_player)
  if (nrow(mytables_ls[["def_notes"]]) > 0) {
    mytables_ls[["def_notes"]] <- cbind(match_id,
                                        defnotes_id = c(1001:(1001+nrow(mytables_ls[["def_notes"]])-1)),
                                        mytables_ls[["def_notes"]])
  } else if (nrow(mytables_ls[["def_notes"]]) == 0) {
    mytables_ls[["def_notes"]] <- cbind(match_id = numeric(), 
                                        defnotes_id = numeric(), 
                                        mytables_ls[["def_notes"]])
  }
  mytables_ls
}

SetPlayerInfo <- function(mytables_ls,
                          lineups, match_id) {
  GetPlayerIds <- function(my_tbl, lineups, 
                           match_id) {
    my_ids <- vector(mode = "numeric", length = nrow(my_tbl))
    is_player <- !is.na(my_tbl$player)
    id_missing <- is_player & my_ids == 0
    
    my_lineup <- lineups[lineups$match_id == match_id,]
    my_players <- tolower(gsub(" \\(.*", "", my_tbl$player))
    name_matches <- lapply(my_players, 
                           function(x) 
                             which(x == tolower(my_lineup$player)))
    my_ids[id_missing] <- sapply(name_matches[id_missing],
                                 function(x) 
                                   ifelse(length(x) != 1, 
                                          0, 
                                          my_lineup$lineup_player_id[x[1]])
    )
    id_missing <- is_player & my_ids == 0
    
    if (length(my_ids[id_missing]) > 0) {
      # cross check player names with numbers
      my_playernums <- as.numeric(tolower(gsub("[^0-9]", "", my_tbl$player)))
      number_matches <- lapply(my_playernums, 
                               function(x) 
                                 which(x == my_lineup$number))
      for (i in 1:length(name_matches[id_missing])) {
        namenum_check <- name_matches[id_missing][[i]] %in% number_matches[id_missing][[i]]
        lineup_index <- name_matches[id_missing][[i]][namenum_check]
        my_ids[id_missing][i] <- ifelse(length(lineup_index) != 1,
                                        0,
                                        my_lineup$lineup_player_id[lineup_index]
        )
      }
      id_missing <- is_player & my_ids == 0
    }
    
    if (length(my_ids[id_missing]) > 0) {
      # cross check player names with teams
      my_playerteams <- tolower(my_tbl$team)
      team_matches <- lapply(my_playerteams, 
                             function(x) 
                               which(x == tolower(my_lineup$team)))
      for (i in 1:length(name_matches[id_missing])) {
        nameteam_check <- name_matches[id_missing][[i]] %in% team_matches[id_missing][[i]]
        lineup_index <- name_matches[id_missing][[i]][nameteam_check]
        my_ids[id_missing][i] <- ifelse(length(lineup_index) != 1,
                                        0,
                                        my_lineup$lineup_player_id[lineup_index]
        )
      }
      id_missing <- is_player & my_ids == 0
    }
    
    if (length(my_ids[id_missing]) > 0) {
      # cross check player names with positions
      my_playerpos <- tolower(my_tbl$position)
      position_matches <- lapply(my_playerpos, 
                                 function(x) 
                                   which(x == tolower(my_lineup$position)))
      for (i in 1:length(name_matches[id_missing])) {
        namepos_check <- name_matches[id_missing][[i]] %in% position_matches[id_missing][[i]]
        lineup_index <- name_matches[id_missing][[i]][namepos_check]
        my_ids[id_missing][i] <- ifelse(length(lineup_index) != 1,
                                        0,
                                        my_lineup$lineup_player_id[lineup_index]
        )
      }
      id_missing <- is_player & my_ids == 0
    }
    if (length(my_ids[id_missing]) > 0){
      print(paste("Error: Was unable to find every player in the log in the lineup.", 
                  "Match. Players missing: ", 
                  my_players[id_missing]))
      break
    }
    my_ids[my_ids == 0] <- NA
    my_ids
  }
  
  mytables_ls[["events"]] <- cbind(select(mytables_ls[["events"]], 
                                          c("match_id", "event", "time")), 
                                   lineup_player_id = GetPlayerIds(my_tbl = mytables_ls[["events"]], 
                                                                   lineups = lineups, 
                                                                   match_id = match_id),
                                   select(mytables_ls[["events"]], 
                                          -c("match_id", "event", "time", 
                                             "team", "position", "player"))
  )
  mytables_ls[["defend_events"]] <- cbind(select(mytables_ls[["defend_events"]], 
                                                 c("match_id", "defend_id", 
                                                   "event")), 
                                          lineup_player_id = GetPlayerIds(my_tbl = mytables_ls[["defend_events"]], 
                                                                          lineups = lineups, 
                                                                          match_id = match_id),
                                          select(mytables_ls[["defend_events"]], 
                                                 -c("match_id", "defend_id", 
                                                    "event", "team", 
                                                    "position", "player"))
  )
  mytables_ls[["def_discp"]] <- cbind(select(mytables_ls[["def_discp"]], 
                                             c("match_id", "defdiscp_id", 
                                               "event")), 
                                      lineup_player_id = GetPlayerIds(my_tbl = mytables_ls[["def_discp"]], 
                                                                      lineups = lineups, 
                                                                      match_id = match_id),
                                      select(mytables_ls[["def_discp"]], 
                                             -c("match_id", "defdiscp_id", 
                                                "event", "team", 
                                                "position", "player"))
  )
  mytables_ls[["def_notes"]] <- cbind(select(mytables_ls[["def_notes"]], 
                                             c("match_id", "defnotes_id", 
                                               "event")), 
                                      lineup_player_id = GetPlayerIds(my_tbl = mytables_ls[["def_notes"]], 
                                                                      lineups = lineups, 
                                                                      match_id = match_id),
                                      select(mytables_ls[["def_notes"]], 
                                             -c("match_id", "defnotes_id", 
                                                "event", "team", 
                                                "position", "player"))
  )
  
  mytables_ls
}

GetDefLocation         <- function(mytables_ls) {
  poss_events <- mytables_ls[["events"]]
  poss_events2 <- poss_events[c("event", "poss_location")] %>%
    filter(!is.na(poss_location))
  def_events <- left_join(mytables_ls[["defend_events"]], 
                          poss_events2, 
                          by = "event")
  ev_vals_int <- def_events$event[grep("intercept", def_events$def_action)]
  ev_vals_afterint <- poss_events$event[which(poss_events$event %in% ev_vals_int) + 1]
  ev_loc_afterint <- poss_events$poss_location[poss_events$event %in% ev_vals_afterint]
  if (length(ev_vals_int) != length(ev_loc_afterint)) {
    print(paste("Error: a defend_id is duplicated due to a missing player value"))
    break
  }
  poss_evs_afterint <- as.data.frame(cbind(event = ev_vals_int,
                                           poss_location_afterint = ev_loc_afterint), 
                                     stringsAsFactors = FALSE) %>%
    mutate(event = as.numeric(event))
  def_events <- left_join(def_events, poss_evs_afterint, by = "event")
  kPossLocation <- c("A6", "A18", "A3L", "A3C", 
                     "A3R", "AM3L", "AM3C", "AM3R", 
                     "DM3L", "DM3C", "DM3R", "D3L", 
                     "D3C", "D3R", "D18", "D6", 
                     "AL", "AC", "AR", "AML", "AMC", 
                     "AMR", "DML", "DMC", "DMR", 
                     "DL", "DC", "DR")
  kDefLocation <-   c("D6", "D18", "D3R", "D3C", "D3L",
                      "DM3R", "DM3C", "DM3L", "AM3R", 
                      "AM3C", "AM3L", "A3R", "A3C", 
                      "A3L", "A18", "A6", "DR", "DC", 
                      "DL", "DMR", "DMC", "DML", "AMR", 
                      "AMC", "AML", "AR", "AC", "AL")
  kInvertActions <- paste(c("pressure", "challenge", "aerial", 
                            "ground", "tackle", "dispossess", 
                            "dribble", "pass", "touch", "move", 
                            "take", "shots"), collapse = "|")
  newdefloc <- def_events$def_location
  invert_indx1 <- grepl(kInvertActions, def_events$def_action) & 
    is.na(def_events$def_location)
  invert_locs1 <- sapply(def_events$poss_location[invert_indx1], 
                         function(x)
                           ifelse(is.na(x),
                                  NA,
                                  kDefLocation[which(kPossLocation %in% x)]
                           )
  )
  newdefloc[invert_indx1] <- invert_locs1
  invert_indx2 <- grepl("intercept", def_events$def_action) & 
    is.na(def_events$def_location)
  invert_locs2 <- sapply(def_events$poss_location_afterint[invert_indx2],
                         function(x)
                           ifelse(is.na(x),
                                  NA,
                                  x)
  )
  newdefloc[invert_indx2] <- invert_locs2
  newdefloc
}

IsCompletedPass        <- function(my_tbl_ls) {
  my_tbl <- my_tbl_ls[["events"]]
  is_pass <- grepl("pass", my_tbl$poss_action)
  isnt_comp_pass <- !grepl("c$", my_tbl$poss_action)
  cutoff_actions <- paste(c("playcutoffbybroadcast", "stoppage", 
                            "substitution", "halftime", "fulltime", 
                            "end.of.match", "offside"), 
                          collapse = "|")
  nev_cutoff <- grep(cutoff_actions, my_tbl$poss_action) - 1
  nev_notcutoff <- !(seq_along(my_tbl$poss_action) %in% nev_cutoff)
  nev_lostduel <- grep("aerial.lost|ground.50.50.lost",
                       my_tbl$poss_action) - 1
  nev_notlostduel <- !(seq_along(my_tbl$poss_action) %in% nev_lostduel)
  nev_recovery <- grep("recoveries", my_tbl$poss_action) - 1
  nev_notrecovery <- !(seq_along(my_tbl$poss_action) %in% nev_recovery)
  actions_bdisrupt <- paste(c("interceptions", "blocks", "clearance", "shield",
                              "high.balls.won", "smothers.won", "loose.balls.won"),
                            collapse = "|")
  ev_bdisruptd <- grep(actions_bdisrupt, my_tbl$poss_action)
  ev_notbdisruptd <- !(seq_along(my_tbl$poss_action) %in% ev_bdisruptd)
  my_tbldef <- my_tbl_ls[["defend_events"]]
  actions_gk <- paste(c("caught", "punched", "dropped", "collected",
                        "parried", "deflected"),
                      collapse = "|")
  indx_gk <- grep(actions_gk, my_tbldef$gk_ball_stop)
  ev_gkdisrupt <- which(my_tbl$event %in% my_tbldef$event[indx_gk])
  ev_notgkdisrupt <- !(seq_along(my_tbl$poss_action) %in% ev_gkdisrupt)
  my_tbldiscp <- my_tbl_ls[["poss_discp"]]
  foul_actions <- paste(c("fouls", "yellow", "red", "penalties"),
                        collapse = "|")
  indx_discp <- grep(foul_actions, my_tbldiscp$poss_player_disciplinary)
  ev_possfouled <- which(my_tbl$event %in% my_tbldiscp$event[indx_discp])
  ev_notpossfouled <- !(seq_along(my_tbl$poss_action) %in% ev_possfouled)
  my_tbltype <- my_tbl_ls[["ev_type"]]
  my_tblpnotes <- my_tbl_ls[["poss_notes"]]
  indx_ballout1 <- my_tbltype$event[grep("out.of.bounds", my_tbltype$play_type)]
  indx_ballout2 <- my_tblpnotes$event[grep("out.of.bounds", my_tblpnotes$poss_notes)]
  indx_ballout <- unique(c(indx_ballout1, indx_ballout2))
  ev_ballout <- which(my_tbl$event %in% indx_ballout)
  ev_notballout <- !(seq_along(my_tbl$poss_action) %in% ev_ballout)
  
  is_pass & isnt_comp_pass & nev_notcutoff & nev_notlostduel & 
    nev_notrecovery & ev_notbdisruptd & ev_notgkdisrupt & 
    ev_notpossfouled & ev_notballout
}

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
  match_source <- ExpandAbbrevs(match_source = match_source)
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
  cbind(uniq_event_id = c(1000001:
                            (1000001+nrow(alltables_ls[["events"]])-1)),
        alltables_ls[["events"]])
alltables_ls[["ev_type"]] <- 
  cbind(uniq_evtype_id = c(1000001:
                             (1000001+nrow(alltables_ls[["ev_type"]])-1)),
        right_join(select(alltables_ls[["events"]], 
                          uniq_event_id, match_id, event),
                   alltables_ls[["ev_type"]],
                   by = c("match_id", "event")))
if (class(alltables_ls[["defend_events"]]$def_location) == "list") {
  print("Error: check why def_location returns a list")
  break
}
alltables_ls[["defend_events"]] <- 
  cbind(uniq_defend_id = c(1000001:
                             (1000001+nrow(alltables_ls[["defend_events"]])-1)),
        right_join(select(alltables_ls[["events"]], 
                          uniq_event_id, match_id, event),
                   alltables_ls[["defend_events"]],
                   by = c("match_id", "event")))
alltables_ls[["poss_discp"]] <- 
  cbind(uniq_poss_discp_id = c(1000001:
                                 (1000001+nrow(alltables_ls[["poss_discp"]])-1)),
        right_join(select(alltables_ls[["events"]], 
                          uniq_event_id, match_id, event),
                   alltables_ls[["poss_discp"]],
                   by = c("match_id", "event")))
alltables_ls[["def_discp"]] <- 
  cbind(uniq_def_discp_id = c(1000001:
                                (1000001+nrow(alltables_ls[["def_discp"]])-1)),
        right_join(select(alltables_ls[["events"]], 
                          uniq_event_id, match_id, event),
                   alltables_ls[["def_discp"]],
                   by = c("match_id", "event")))
alltables_ls[["poss_notes"]] <- 
  cbind(uniq_poss_notes_id = c(1000001:
                                 (1000001+nrow(alltables_ls[["poss_notes"]])-1)),
        right_join(select(alltables_ls[["events"]], 
                          uniq_event_id, match_id, event),
                   alltables_ls[["poss_notes"]],
                   by = c("match_id", "event")))
alltables_ls[["def_notes"]] <- 
  cbind(uniq_def_notes_id = c(1000001:
                                (1000001+nrow(alltables_ls[["def_notes"]])-1)),
        right_join(select(alltables_ls[["events"]], 
                          uniq_event_id, match_id, event),
                   alltables_ls[["def_notes"]],
                   by = c("match_id", "event")))

write_csv(alltables_ls[["events"]], "source/temp_database/events.csv", na = "")
write_csv(alltables_ls[["ev_type"]], "source/temp_database/event_type.csv", na = "")
write_csv(alltables_ls[["defend_events"]], "source/temp_database/defending.csv", na = "")
write_csv(alltables_ls[["poss_discp"]], "source/temp_database/poss_discipline.csv", na = "")
write_csv(alltables_ls[["def_discp"]], "source/temp_database/def_discipline.csv", na = "")
write_csv(alltables_ls[["poss_notes"]], "source/temp_database/poss_notes.csv", na = "")
write_csv(alltables_ls[["def_notes"]], "source/temp_database/def_notes.csv", na = "")
