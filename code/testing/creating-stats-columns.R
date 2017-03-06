# Install if necessary---------------#
library(RCurl)

# About offline mode:
# Trying to do work on a plane & don't want to pay $8 for Wi-Fi? Stuck in a train tunnel?
# Assign "offline" to  online_mode and, assuming you've got the GitHub repo duplicated in
# your working directory, you can just read the files instead of going online.
# Otherwise, if online_mode hasn't been created yet, you'll just source the "functions.R"## file from the GitHub site
if(!exists("online_mode")){
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-3/creating-stats-base-functions.R")
  #rosters <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/rosters/nwsl-2016.csv")
  #rosters <- read.csv(textConnection(rosters), stringsAsFactors = FALSE)
} else if(exists("online_mode") && online_mode == "offline"){
  source("~/wosostats/code/version-3/creating-stats-base-functions.R")
  #rosters <- read.csv("~/wosostats/rosters/nwsl-2016.csv")
}

#MATCH PLAYERS---------
## Gets data frame that binds data frames of every player who shows up in "poss.player" and "def.player" column
createPlayersColumns <- function(use_rosters=FALSE, match_positions=FALSE, match_sheet=match_sheet) {
  
  poss_columns <- unique(match_sheet[,c("poss.player", "poss.team", "poss.number")])
  names(poss_columns) <- c("Player", "Team", "Number")
  def_columns <- unique(match_sheet[,c("def.player", "def.team", "def.number")])
  names(def_columns) <- c("Player", "Team", "Number")
  players_sheet <- rbind(poss_columns, def_columns)
  players_sheet <- players_sheet[!is.na(players_sheet[,"Player"]),]
  players_sheet <- unique(players_sheet[,])
  rm(poss_columns, def_columns)
  
  matchlength <- length(unique(match_sheet$time))
  substitutions <- match_sheet[grepl("substitution",match_sheet[,"poss.action"]),]
  
  # create blank vectors for minutes played and games started
  minutesPlayed <- numeric(nrow(players_sheet)); names(minutesPlayed) <- players_sheet$Player
  gamesStarted <- numeric(nrow(players_sheet)); names(gamesStarted) <- players_sheet$Player
  
  all_substitutions <- paste(substitutions$poss.player, substitutions$poss.team, substitutions$poss.number)
  all_subs_on <- substitutions[substitutions[,"poss.action"]=="substitution.on",]
  all_subs_on <- paste(all_subs_on$poss.player, all_subs_on$poss.team, all_subs_on$poss.number)
  all_subs_off <- substitutions[substitutions[,"poss.action"]=="substitution.off",]
  all_subs_off <- paste(all_subs_off$poss.player, all_subs_off$poss.team, all_subs_off$poss.number)
    
  for (i in 1:nrow(players_sheet)) {
    indexed_player <- paste(players_sheet[i,"Player"],players_sheet[i,"Team"],players_sheet[i,"Number"])
    
    # if the player doesn't appear in the substitutions data frame, the player played the entire match
    if (!(indexed_player %in% all_substitutions)) {
      minutesPlayed[i] <- matchlength
      gamesStarted[i] <- 1
    } else if (indexed_player %in% all_substitutions) {
      # check if she was a starter, based on if she was only ever substituted into the game
      if (!(indexed_player %in% all_subs_on)) {
        lastevent <- substitutions[substitutions[,"poss.player"] == players_sheet$Player[i],"event"]
        lasteventrow <- grep(lastevent, match_sheet$event)
        
        minutesPlayed[i] <- length(unique(match_sheet[1:lasteventrow,"time"]))
        gamesStarted[i] <- 1
      } 
      # check if she was not a starter, got subbed on, and did not get subbed off
      else if (!(indexed_player %in% all_subs_off)) {
        lastevent <- substitutions[substitutions[,"poss.player"] == players_sheet$Player[i],"event"]
        firsteventrow <- grep(lastevent, match_sheet[,"event"])[1]
        minutesPlayed[i] <- length(unique(match_sheet[firsteventrow:nrow(match_sheet),"time"]))
        gamesStarted[i] <- 0
      }
      # else, she she was not a starter, got subbed on, and was later subbed off
      else {
        lastevent <- substitutions[substitutions[,"poss.player"] == players_sheet$Player[i],"event"]
        firsteventrow <- grep(lastevent[1], match_sheet[,"event"])
        lasteventrow <- grep(lastevent[2], match_sheet[,"event"])
        minutesPlayed[i] <- length(unique(match_sheet[firsteventrow:lasteventrow,"time"]))
        gamesStarted[i] <- 0
      }
    }
  }
  
  players_sheet$GP <- 1
  players_sheet$MP <- minutesPlayed
  players_sheet$GS <- gamesStarted
  
  players_sheet
}

#SHOTS---------------
createShotsColumns <- function(location="none", match_sheet=match_sheet){
  match_subset <- addColumnForQualifier(newcol = "pressed",pattern = "pressured|challenged",patternLocation = "def.action",ogdf = match_sheet,
                                        ndf = createSubset(c("shots", "accuracy", "shots.scored", "shots.stopped.by.gk", 
                                                                     "shots.stopped.by.def", "shots.missed", "shots.blocked") ,"poss.action", source_df = match_sheet))
  
  stats_cols <- list()
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("shots.scored", "shots.stopped.by.gk", "shots.stopped.by.def", "shots.missed", "shots.blocked"), 
                                                         target_col = "poss.action", source_df = match_subset, location = location,
                                                         new_sumcol = list(name = "shots", summed="shots"), 
                                                         new_divcol = list(name = "accuracy", 
                                                                           numerator = c("shots.scored", "shots.stopped.by.gk", "shots.stopped.by.def"), 
                                                                           denominator = c("shots.scored", "shots.stopped.by.gk", "shots.stopped.by.def", "shots.missed")),
                                                         stat_names = c("Goals", "Shots Stopped by GK", "Shots Stopped by Def", "Shots Missed","Shots Blocked","All Shots","Shot Accuracy"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c(TRUE, FALSE), target_col = "pressed", source_df = match_subset, location = location,
                                                         new_sumcol = list(name = "shots", summed="TRUE|FALSE"),
                                                         new_divcol = list(name= "pct", numerator = "TRUE.", denominator = c("TRUE.","FALSE.")),
                                                         drop_cols = "Shots",
                                                         stat_names = c("Shots Pressed", "Shots Not Pressed", "Shots", "Pct Shots Pressed"))

  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  merged_stats
}

#KEY PASSES & CHANCES---------------
createChancesColumns <- function(location="none", match_sheet) {
  match_subset <- createSubset(pattern = c("assists", "key.passes","second.assists","big.chances","big.chances.scored","big.chances.shot.on.goal",
                                              "big.chances.shot.missed","big.chances.dispossessed","big.chances.created","big.chances.lost"),
                                  col = "poss.notes", source_df = match_sheet)
  match_subset <- addMultiColumnsForQualifiers(patterns = c("assists"="^assists", "key.passes"="key.pass|second.assist", "second.assists"="second.assist"),
                                               pattern_locations = "poss.notes",
                                               ogdf = match_sheet, 
                                               ndf = match_subset)
  match_subset <- addColumnForMultiQualifiers(newcol = "key.assists", pattern = c("assists"=TRUE,"key.passes"=TRUE), 
                                              source_df = match_subset,
                                              exp = "AND")

  stats_cols <- list()
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("big.chances.scored","big.chances.shot.on.goal", "big.chances.shot.missed", 
                                                  "big.chances.dispossessed","big.chances.created", "big.chances.lost"),
                                      target_col = "poss.notes",
                                      source_df = fillBlanks(match_subset), location = location,
                                      new_sumcol = list(name="big.chances", summed="scored|goal|missed|dispossessed|lost"), 
                                      new_divcol = list(name = "big.chances.conversion", 
                                                        numerator = "big.chances.scored", 
                                                        denominator = "big.chances"),
                                      stat_names = c("BC Scored","BC SOG", "BC Shot Miss", "BC Dispossess", "BC Created", "BC Lost", "Big Chances","BC Conversion Pct"))
  
  match_subset <- createSubset(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", source_df = match_subset,clean=TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = TRUE,target_col = "assists", source_df = match_subset, location = location,
                                      stat_names = c("Assists"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = TRUE,target_col = "key.passes", source_df = match_subset, location = location,
                                      stat_names = c("Key Passes"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c(TRUE),target_col = "key.assists", source_df = match_subset, location = location,
                                      stat_names = c("Key Assists"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c(TRUE),target_col = "second.assists", source_df = match_subset, location = location,
                                      stat_names = c("Second Assists"))
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  merged_stats
}

#PASSING---------------
createPassingColumns <- function(location="none", match_sheet) {
  match_subset <- createSubset(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action",source_df = match_sheet, clean = TRUE)
  match_subset <- addColumnForQualifier(newcol = "pressed",pattern = "pressured|challenged",patternLocation = "def.action",ogdf = match_sheet,ndf = match_subset)
  match_subset <- addColumnForQualifier(newcol = "opPass", 
                                        pattern="throw|gk|corner.kick|free.kick|goal.kick", patternLocation = "play.type",
                                        ogdf = match_sheet, ndf = match_subset, invert = TRUE)
  match_subset <- addMultiColumnsForQualifiers(patterns = c("isCross"="cross","isLaunch"="launch","isThrough"="through", "isThrowIn" = "throw.in"),
                                               pattern_locations = "play.type", ogdf = match_sheet, ndf = match_subset)
  
  stats_cols <- list()
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset, location = location, stat_names = c("Pass Comp", "Pass Att", "Pass Comp Pct"),isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"opPass"]==TRUE,], location=location, stat_names = c("opPass Comp","opPass Att","opPass Comp Pct"), isPassing=TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"pressed"] == TRUE & match_subset[,"opPass"] == TRUE,], location = location, 
                                                         stat_names = c("PPass Comp", "PPass Att","PPass Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"opPass"]==TRUE,],  location = location, target_col = "pressed", pattern = c(TRUE, FALSE), 
                                      new_divcol = list(name="pct", numerator="TRUE.", denominator=c("TRUE.","FALSE.")),
                                      drop_cols = c("TRUE.","FALSE."),
                                      stat_names = c("Pct opPass Pressed"))
  #special passes
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"isCross"]==TRUE,], location = location, 
                                        stat_names = c("Cross Comp", "Cross Att","Cross Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"isLaunch"]==TRUE,], location = location, 
                                        stat_names = c("Launch Comp", "Launch Att", "Launch Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"isThrough"]==TRUE,], location = location,
                                        stat_names = c("Through Comp", "Through Att", "Through Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"isThrowIn"]==TRUE,], location = location,
                                        stat_names = c("Throw In Comp", "Throw In Att", "Throw In Comp Pct"), isPassing = TRUE)
  #special open play passes
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"isCross"]==TRUE & match_subset[,"opPass"]==TRUE,], location = location, 
                                                         stat_names = c("Cross Comp", "Cross Att","Cross Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"isLaunch"]==TRUE & match_subset[,"opPass"]==TRUE,], location = location, 
                                                         stat_names = c("Launch Comp", "Launch Att", "Launch Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"isThrough"]==TRUE & match_subset[,"opPass"]==TRUE,], location = location,
                                                         stat_names = c("Through Comp", "Through Att", "Through Comp Pct"), isPassing = TRUE)
  #by direction - all passes
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.f.c", "passes.f"), "poss.action", source_df = match_subset, clean = TRUE), location = location, 
                                        stat_names = c("fwPass Comp", "fwPass Att", "fwPass Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.s.c", "passes.s"), "poss.action", source_df = match_subset, clean = TRUE), location = location, 
                                        stat_names = c("sPass Comp", "sPass Att", "sPass Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.b.c", "passes.b"), "poss.action", source_df = match_subset, clean = TRUE), location = location, 
                                        stat_names = c("bPass Comp", "bPass Att", "bPass Comp Pct"), isPassing = TRUE)
  #by direction - open play passes
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.f.c", "passes.f"), "poss.action", source_df = match_subset[match_subset[,"opPass"]==TRUE,], clean = TRUE), location = location, 
                                        stat_names = c("fwopPass Comp", "fwopPass Att", "fwopPass Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.s.c", "passes.s"), "poss.action", source_df = match_subset[match_subset[,"opPass"]==TRUE,], clean = TRUE), location = location, 
                                        stat_names = c("sopPass Comp", "sopPass Att", "sopPass Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.b.c", "passes.b"), "poss.action", source_df = match_subset[match_subset[,"opPass"]==TRUE,], clean = TRUE), location = location, 
                                        stat_names = c("bopPass Comp", "bopPass Att", "bopPass Comp Pct"), isPassing = TRUE)
  #by direction - pressed passses
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.f.c", "passes.f"), "poss.action", source_df = match_subset[match_subset[,"pressed"]==TRUE,], clean = TRUE), location = location, 
                                        stat_names = c("fwPPass Comp", "fwPPass Att", "fwPPass Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.s.c", "passes.s"), "poss.action", source_df = match_subset[match_subset[,"pressed"]==TRUE,], clean = TRUE), location = location, 
                                        stat_names = c("sPPass Comp", "sPPass Att", "sPPass Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = createSubset(c("passes.b.c", "passes.b"), "poss.action", source_df = match_subset[match_subset[,"pressed"]==TRUE,], clean = TRUE), location = location, 
                                        stat_names = c("bPPass Comp", "bPPass Att", "bPPass Comp Pct"), isPassing = TRUE)
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  allPassAtt_cols <- grep("fwPass Att|sPass Att|bPass Att", names(merged_stats),value = TRUE)
  opPassAtt_cols <- grep("fwopPass Att|sopPass Att|bopPass Att", names(merged_stats),value = TRUE)
  pressPassAtt_cols <- grep("fwPPass Att|sPPass Att|bPPass Att", names(merged_stats),value = TRUE)
  if(location == "none"){
    # frequency of all passes by direction
    allPassAtt <- rowSums(merged_stats[,allPassAtt_cols],na.rm = TRUE)
    merged_stats[,c("rFreq Pass Fwd", "rFreq Pass Side", "rFreq Pass Back")] <- merged_stats[,c("fwPass Att","sPass Att","bPass Att")]/allPassAtt
    # frequency of open play passes by direction
    opPassAtt <- rowSums(merged_stats[,opPassAtt_cols],na.rm = TRUE)
    merged_stats[,c("rFreq opPass Fwd","rFreq opPass Side","rFreq opPass Back")] <-  merged_stats[,c("fwopPass Att","sopPass Att","bopPass Att")]/opPassAtt
    # frequency of pressed passes by direction
    pressPassAtt <- rowSums(merged_stats[,pressPassAtt_cols],na.rm = TRUE)
    merged_stats[,c("rFreq PPass Fwd","rFreq PPass Side","rFreq PPass Back")] <-  merged_stats[,c("fwPPass Att","sPPass Att","bPPass Att")]/pressPassAtt
  } else if(location == "thirds" | location == "zones" | location == "wings"){
    if(location=="zones"){
      locations <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML", "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
    } else if(location=="thirds"){
      locations <- c("D3", "M3", "A3")
    } else if(location=="wings"){
      locations <- c("L3", "C3", "R3")
    }
    for(index in locations) {
      # frequency of all passes by direction
      allPassAtt <- merged_stats[,paste(index,"Pass Att")]
      merged_stats[,paste(index,c("rFreq Pass Fwd", "rFreq Pass Side", "rFreq Pass Back"))] <- merged_stats[,grep(index,allPassAtt_cols,value = TRUE)]/allPassAtt
      # frequency of open play passes by direction
      opPassAtt <- merged_stats[,paste(index,"opPass Att")]
      merged_stats[,paste(index,c("rFreq opPass Fwd","rFreq opPass Side","rFreq opPass Back"))] <- merged_stats[,grep(index,opPassAtt_cols,value = TRUE)]/opPassAtt
      # frequency of pressed passes by direction
      pressPassAtt <- merged_stats[,paste(index,"PPass Att")]
      merged_stats[,paste(index,c("rFreq PPass Fwd","rFreq PPass Side","rFreq PPass Back"))] <-  merged_stats[,grep(index,pressPassAtt_cols,value = TRUE)]/pressPassAtt
    }
  }

  merged_stats
}

#PASSING BY ORIGIN & DESTINATION----------
createPassRangeColumns <- function(match_sheet) {
  match_subset <- addColumnForQualifier(newcol = "completed", pattern = "passes.*.c", 
                                        patternLocation = "poss.action", ogdf = match_sheet,
                                        ndf = createSubset(col = "poss.action", clean = TRUE, source_df = match_sheet,
                                                                   pattern = c("passes.f.c", "passes.f", "passes.s.c",
                                                                               "passes.s", "passes.b.c", "passes.b")))
  match_subset <- match_subset[match_subset[,"completed"]==TRUE,]
  
  passRange <- character()
  for(i in 1:nrow(match_subset)) {
    passOrigin <- character()
    if(grepl("D6|D18|D3",match_subset[i,"poss.location"])) {
      passOrigin <- "D3"
    } else if (grepl("M",match_subset[i,"poss.location"])) {
      passOrigin <- "M3"
    } else if (grepl("A6|A18|A3",match_subset[i,"poss.location"])) {
      passOrigin <- "A3"
    }
    passDestination <- character()
    if(grepl("D6|D18|D3",match_subset[i,"poss.play.destination"])) {
      passDestination <- "D3"
    } else if (grepl("M",match_subset[i,"poss.play.destination"])) {
      passDestination <- "M3"
    } else if (grepl("A6|A18|A3",match_subset[i,"poss.play.destination"])) {
      passDestination <- "A3"
    }
    passRange[i] <- paste0("Pass.Comp.",passOrigin,"to",passDestination)
  }
  match_subset$pass.range <- passRange
  
  merged_stats <- createStatsTable(pattern = c("Pass.Comp.D3toD3", "Pass.Comp.D3toM3", "Pass.Comp.D3toA3", "Pass.Comp.M3toD3",
                                               "Pass.Comp.M3toM3", "Pass.Comp.M3toA3", "Pass.Comp.A3toD3", "Pass.Comp.A3toM3", 
                                               "Pass.Comp.A3toA3"),
                                   target_col = "pass.range", source_df = match_subset)
  merged_stats
}

#SET PIECES--------
createSetPieceColumns <- function(location="none", match_sheet) {
  match_subset <- createSubset(pattern = c("corner.kick", "free.kick"),col = "play.type",source_df = match_sheet)
  match_subset <- addMultiColumnsForQualifiers(patterns = c("corner.kick"="corner.kick", "free.kick"="free.kick", "shot"="shot", "scored"="scored", 
                                                            "assist"="^assist", "keypass"="key.pass|^second.assist"),
                                               pattern_locations = c("play.type", "play.type", "poss.action", "poss.action", "poss.notes", "poss.notes"),
                                               ogdf = match_sheet,
                                               ndf = match_subset)
  
  stats_cols <- list()
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"corner.kick"]==TRUE,], location = location,
                                        stat_names = c("Corner Kicks Completed", "Corner Kicks Taken", "CK Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"corner.kick"]==TRUE,], location = location,
                                      pattern = TRUE, target_col = c("assist"), stat_names = "CK Assist")
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"corner.kick"]==TRUE,], location = location,
                                      pattern = TRUE, target_col = c("keypass"), stat_names = "CK Key Pass")
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,], location = location,
                                        stat_names = c("FK Pass Comp", "FK Pass Att", "FK Pass Comp Pct"), isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,], location = location,
                                      pattern = TRUE, target_col = c("assist"), stat_names = "FK Assist")
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,], location = location,
                                      pattern = TRUE, target_col = c("keypass"), stat_names = "FK Key Pass")
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,], location = location,
                                      pattern = TRUE, target_col = c("shot"), stat_names = "FK Shot")
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,], location = location,
                                      pattern = TRUE, target_col = c("scored"), stat_names = "FK Scored")
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  if(location=="none"){
    merged_stats[,"Free Kicks Taken"] <- rowSums(merged_stats[,c("FK Pass Att", "FK Shot")], na.rm=TRUE)
  } else if(location=="zones" | location=="thirds" | location=="wings"){
    if(location=="zones"){
      locations <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML", "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
    } else if(location=="thirds"){
      locations <- c("D3", "M3", "A3")
    } else if(location=="wings"){
      locations <- c("L3", "C3", "R3")
    }
    for(index in locations) {
      merged_stats[,paste(index,"Free Kicks Taken")] <- rowSums(merged_stats[,paste(index,c("FK Pass Att","FK Shot"))])
    }
  }
  
  merged_stats
}

#POSSESSION--------
createPossessionColumns <- function(location="none", match_sheet) {
  stats_cols <- list()
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("take.on.won","take.on.lost"),
                                   target_col = "poss.action", source_df = match_sheet, location = location,
                                   new_sumcol = list(name="take.ons", summed="take.on"),
                                   new_divcol = list(name="take.on.win.pct", numerator="take.on.won", denominator=c("take.on.won","take.on.lost")),
                                   stat_names = c("TO Won", "TO Lost", "Take Ons", "TO Win Pct"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("take.on.lost",  "dispossessed", "lost.touch"),
                                      target_col = "poss.action", source_df = match_sheet, location = location,
                                      new_sumcol = list(name="all.possessions.disrupted", summed="take.on|dispossessed|lost.touch"),
                                      drop_cols = c("take.on.lost"),
                                      stat_names = c("Dispossessed by Opp","Lost Touches","All Possessions Disrupted"))
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  merged_stats
  
}

#RECOVERIES---------------
createRecoveryColumns <- function(location="none", match_sheet) {
  recoveryEvents <- sort(c(match_sheet[match_sheet[,"poss.action"] %in% "recoveries","event"],
                         match_sheet[match_sheet[,"poss.action"] %in% "recoveries","event"]-1))
  recoveryEvents <- paste0("^", recoveryEvents, "$")
  
  match_subset <- match_sheet[grep(paste(recoveryEvents, collapse = "|"), match_sheet[,"event"]),]
  match_subset <- fillBlanks(match_subset)
  
  match_subset$isDefRecovery <- c(FALSE, (match_subset[2:nrow(match_subset),"poss.action"]=="recoveries" & (match_subset[2:nrow(match_subset),"poss.team"] != match_subset[1:(nrow(match_subset)-1),"poss.team"])))
  match_subset$isPossRecovery <- c(FALSE, (match_subset[2:nrow(match_subset),"poss.action"]=="recoveries" & (match_subset[2:nrow(match_subset),"poss.team"] == match_subset[1:(nrow(match_subset)-1),"poss.team"])))
  
  stats_cols <- list()
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(target_col = "poss.action", pattern = "recoveries", source_df = match_subset, location = location,
                                      stat_names = "Recoveries")
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(target_col = "isDefRecovery", pattern = TRUE, source_df = match_subset, location = location,
                                      stat_names = "Def Recoveries")
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(target_col = "isPossRecovery", pattern = TRUE, source_df = match_subset, location = location,
                                      stat_names = "Poss Recoveries")
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  merged_stats
}

#AERIAL DUELS---------------
createAerialColumns <- function(location="none", match_sheet) {
  match_subset <- createSubset(pattern = c("aerial.won","aerial.lost"),col = "poss.action", source_df = match_sheet)
  match_subset <- fillBlanks(match_subset)
  
  possAerials <- createStatsTable(pattern = c("aerial.won", "aerial.lost"), target_col = "poss.action", source_df = match_subset, location = location,
                                      stat_names = c("Poss.Aerial.Won","Poss.Aerial.Lost"))
  defAerials <- createStatsTable(pattern = c("aerial.won", "aerial.lost"), target_col = "def.action", source_df = match_subset, location = location,
                                      stat_names = c("Def.Aerial.Won","Def.Aerial.Lost"), team = "def")
  merged_stats <- merge(possAerials,defAerials,by=c("Player","Team","Number"), all=TRUE)
  
  if(location=="none"){
    merged_stats$'Aerial Duels' <- rowSums(merged_stats[,c("Poss.Aerial.Won","Poss.Aerial.Lost","Def.Aerial.Won","Def.Aerial.Lost")],na.rm = TRUE)
    merged_stats$'AD Won' <- rowSums(merged_stats[,c("Poss.Aerial.Won","Def.Aerial.Won")],na.rm = TRUE)
    merged_stats$'AD Lost' <- rowSums(merged_stats[,c("Poss.Aerial.Lost","Def.Aerial.Lost")],na.rm = TRUE)
    merged_stats$'AD Win Pct' <- merged_stats$'AD Won'/merged_stats$'Aerial Duels'
    merged_stats <- merged_stats[,!names(merged_stats) %in% c("Poss.Aerial.Won","Poss.Aerial.Lost","Def.Aerial.Won","Def.Aerial.Lost")]
  } else if(location == "thirds" | location == "zones" | location=="wings"){
    if(location=="zones"){
      locations <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML", "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
    } else if(location=="thirds"){
      locations <- c("D3", "M3", "A3")
    } else if(location=="wings"){
      locations <- c("L3", "C3", "R3")
    }
    for(index in locations) {
      merged_stats[,paste(index,"Aerial Duels")] <- rowSums(merged_stats[,paste(index, c("Poss.Aerial.Won","Poss.Aerial.Lost","Def.Aerial.Won","Def.Aerial.Lost"))],na.rm = TRUE)
      merged_stats[,paste(index,"AD Won")] <- rowSums(merged_stats[,paste(index,c("Poss.Aerial.Won","Def.Aerial.Won"))],na.rm = TRUE)
      merged_stats[,paste(index, "AD Lost")] <- rowSums(merged_stats[,paste(index,c("Poss.Aerial.Lost","Def.Aerial.Lost"))],na.rm = TRUE)
      merged_stats[,paste(index, "AD Win Pct")] <- merged_stats[,paste(index,"AD Won")]/merged_stats[,paste(index,"Aerial Duels")]
      merged_stats <- merged_stats[,!names(merged_stats) %in% paste(index, c("Poss.Aerial.Won","Poss.Aerial.Lost","Def.Aerial.Won","Def.Aerial.Lost"))]
    }
  }
  
  merged_stats
}

#DEFENSIVE DISRUPTIONS---------------
createDefActionsColumns <- function(location="none", match_sheet) {
  match_subset <- createSubset(pattern = c("dispossessed", "tackles.ball.away", "tackles.ball.won", "tackles.ball","dribbled.tackles.missed", 
                                              "dribbled.out.run","dribbled.turned", "pressured", "challenged","interceptions","clearances", "ball.shield", "blocks"),
                               col = "def.action", source_df = match_sheet)
  
  # overrites location columns with the value that's in the "def.location" column
  #creates a new column for each qualifier in "patterns"
  if(location=="zones" | location =="thirds" | location=="wings"){
    if(location=="zones"){
      patterns <- c("D6"="D6", "D18"="D18", "DL"="D3L|DL", "DC"="D3C|DC","DR"="D3R|DR", "DML"="DM3L|DML", "DMC"="DM3C|DMC", "DMR"="DM3R|DMR", "AML"="AM3L|AML", "AMC"="AM3C|AMC", "AMR"="AM3R|AMR", "AL"="A3L|AL", "AC"="A3C|AC", "AR"="A3R|AR", "A18"="A18", "A6"="A6")
    } else if(location=="thirds"){
      patterns <- c("D3"="D6|D18|D3L|DL|D3C|DC|D3R|DR", "M3"="M", "A3"="A6|A18|A3L|AL|A3C|AC|A3R|AR")
    } else if(location=="wings"){
      patterns = c("L3"="L", "C3"="C|6|18", "R3"="R")
    }
    for(i in 1:length(patterns)) {
      newcol_vec <- c(grepl(patterns[i], match_subset[,"def.location"]))
      match_subset[,names(patterns[i])] <- newcol_vec
    }
  }
  
  stats_cols <- list()
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("dispossessed", "pressured", "challenged", 
                                               "tackles.ball.away", "tackles.ball.won", "tackles.ball",
                                               "dribbled.tackles.missed", "dribbled.out.run","dribbled.turned"),
                                   target_col = "def.action", source_df = match_subset, team = "def", location=location,
                                   stat_names = c("Dispossessed Opp","Pressured Opp","Challenged Opp","Tackles Ball Away","Tackles Ball Won",
                                                  "Tackles Ball","Dribbled Tackles Missed","Dribbled Out Run","Dribbled Turned"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("interceptions","blocks", "clearances", "ball.shield"),
                                      target_col = "def.action",source_df = match_subset,team = "def", location=location,
                                      stat_names = c("Interceptions", "Blocks","Clearances","Ball Shields"))
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  if(location=="none"){
    merged_stats$Tackles <- rowSums(merged_stats[,c("Tackles Ball Away","Tackles Ball Won","Tackles Ball")])
    merged_stats$Dribbled <- rowSums(merged_stats[,c("Dribbled Tackles Missed","Dribbled Out Run","Dribbled Turned")])
    merged_stats$'All Opp Poss Disrupted' <- rowSums(merged_stats[,c("Tackles","Dispossessed Opp")])
    merged_stats <- merged_stats[,!names(merged_stats) %in% c("Tackles Ball Away","Tackles Ball Won", "Tackles Ball","Dribbled Tackles Missed","Dribbled Out Run","Dribbled Turned")]
  } else if(location=="thirds" | location == "zones"| location=="wings") {
    if(location=="zones"){
      locations <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML", "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
    } else if(location=="thirds"){
      locations <- c("D3", "M3", "A3")
    } else if(location=="wings"){
      locations <- c("L3"="L", "C3"="C|6|18", "R3"="R")
    }
    for(index in locations){
      merged_stats[,paste(index,"Tackles")] <- rowSums(merged_stats[,paste(index, c("Tackles Ball Away","Tackles Ball Won","Tackles Ball"))])
      merged_stats[,paste(index, "Dribbled")] <- rowSums(merged_stats[,paste(index, c("Dribbled Tackles Missed","Dribbled Out Run","Dribbled Turned"))])
      merged_stats[,paste(index, "All Opp Poss Disrupted")] <- rowSums(merged_stats[,paste(index, c("Tackles","Dispossessed Opp"))])
      merged_stats <- merged_stats[,!names(merged_stats) %in% paste(index, c("Tackles Ball Away","Tackles Ball Won", "Tackles Ball","Dribbled Tackles Missed","Dribbled Out Run","Dribbled Turned"))]
    }
  }

  merged_stats
}

#DISCIPLINARY ACTIONS--------
createDisciplineColumns <- function(location="none", match_sheet) {
  pattern <- c("fouls.won","fouls.conceded","yellow.cards","red.cards","penalties.won","penalties.conceded")
  events_poss <- match_sheet[match_sheet[,"poss.player.disciplinary"] %in% c(pattern),"event"]
  events_def <- match_sheet[match_sheet[,"def.player.disciplinary"] %in% c(pattern),"event"]
  events_all <- sort(unique(c(events_poss, events_def)))
  events_all <- paste0("^", events_all, "$")
  
  match_subset <- match_sheet[grep(paste(events_all, collapse = "|"), match_sheet[,"event"]),]
  match_subset <- fillBlanks(match_subset)
  
  possDiscipline <- createStatsTable(pattern = c("fouls.won","fouls.conceded","yellow.cards","red.cards","penalties.won","penalties.conceded"), target_col = "poss.player.disciplinary", source_df = match_subset,location = location,
                                  stat_names = c("Poss.Fouls.Won","Poss.Fouls.Conceded","Poss.Yellow.Cards","Poss.Red.Cards","Poss.Penalties.Won","Poss.Penalties.Conceded"))
  
  if(location=="zones" | location =="thirds" | location == "wings"){
    if(location=="zones"){
      patterns <- c("D6"="D6", "D18"="D18", "DL"="D3L|DL", "DC"="D3C|DC","DR"="D3R|DR", "DML"="DM3L|DML", "DMC"="DM3C|DMC", "DMR"="DM3R|DMR", "AML"="AM3L|AML", "AMC"="AM3C|AMC", "AMR"="AM3R|AMR", "AL"="A3L|AL", "AC"="A3C|AC", "AR"="A3R|AR", "A18"="A18", "A6"="A6")
    } else if(location=="thirds"){
      patterns <- c("D3"="D6|D18|D3L|DL|D3C|DC|D3R|DR", "M3"="M", "A3"="A6|A18|A3L|AL|A3C|AC|A3R|AR")
    } else if(location=="wings"){
      patterns <- c("L3"="L", "C3"="C|6|18", "R3"="R")
    }
    for(i in 1:length(patterns)) {
      newcol_vec <- c(grepl(patterns[i], match_subset[,"def.location"]))
      match_subset[,names(patterns[i])] <- newcol_vec
    }
  }
  
  defDiscipline <- createStatsTable(pattern = c("fouls.won","fouls.conceded","yellow.cards","red.cards","penalties.won","penalties.conceded"), target_col = "def.player.disciplinary", source_df = match_subset, location = location,
                                 stat_names = c("Def.Fouls.Won","Def.Fouls.Conceded","Def.Yellow.Cards","Def.Red.Cards","Def.Penalties.Won","Def.Penalties.Conceded"), team = "def")
  merged_stats <- merge(possDiscipline,defDiscipline,by=c("Player","Team","Number"), all=TRUE)
  
  if(location=="none"){
    merged_stats$'Fouls Won' <- rowSums(merged_stats[,c("Poss.Fouls.Won","Def.Fouls.Won")],na.rm = TRUE)
    merged_stats$'Fouls Conceded' <- rowSums(merged_stats[,c("Poss.Fouls.Conceded","Def.Fouls.Conceded")],na.rm = TRUE)
    merged_stats$'Yellow Cards' <- rowSums(merged_stats[,c("Poss.Yellow.Cards","Def.Yellow.Cards")],na.rm = TRUE)
    merged_stats$'Red Cards' <- rowSums(merged_stats[,c("Poss.Red.Cards","Def.Red.Cards")],na.rm = TRUE)
    merged_stats$'Penalties Won' <- rowSums(merged_stats[,c("Poss.Penalties.Won","Def.Penalties.Won")],na.rm = TRUE)
    merged_stats$'Penalties Conceded' <- rowSums(merged_stats[,c("Poss.Penalties.Conceded","Def.Penalties.Conceded")],na.rm = TRUE)
    merged_stats <- merged_stats[,!names(merged_stats) %in% c("Poss.Fouls.Won","Def.Fouls.Won","Poss.Fouls.Conceded","Def.Fouls.Conceded","Poss.Yellow.Cards","Def.Yellow.Cards",
                                                              "Poss.Red.Cards","Def.Red.Cards","Poss.Penalties.Won","Def.Penalties.Won","Poss.Penalties.Conceded","Def.Penalties.Conceded")]
  } else if(location=="thirds" | location == "zones" | location == "wings"){
    if(location=="zones"){
      locations <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML", "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
    } else if(location=="thirds"){
      locations <- c("D3", "M3", "A3")
    } else if(location=="wings"){
      locations <- c("L3", "C3", "R3")
    }
    for(index in locations){
      merged_stats[,paste(index, "Fouls Won")] <- rowSums(merged_stats[,paste(index, c("Poss.Fouls.Won","Def.Fouls.Won"))],na.rm = TRUE)
      merged_stats[,paste(index, "Fouls Conceded")] <- rowSums(merged_stats[,paste(index, c("Poss.Fouls.Conceded","Def.Fouls.Conceded"))],na.rm = TRUE)
      merged_stats[,paste(index, "Yellow Cards")] <- rowSums(merged_stats[,paste(index, c("Poss.Yellow.Cards","Def.Yellow.Cards"))],na.rm = TRUE)
      merged_stats[,paste(index, "Red Cards")] <- rowSums(merged_stats[,paste(index, c("Poss.Red.Cards","Def.Red.Cards"))],na.rm = TRUE)
      merged_stats[,paste(index, "Penalties Won")] <- rowSums(merged_stats[,paste(index, c("Poss.Penalties.Won","Def.Penalties.Won"))],na.rm = TRUE)
      merged_stats[,paste(index, "Penalties Conceded")] <- rowSums(merged_stats[,paste(index, c("Poss.Penalties.Conceded","Def.Penalties.Conceded"))],na.rm = TRUE)
      merged_stats <- merged_stats[,!names(merged_stats) %in% paste(index, c("Poss.Fouls.Won","Def.Fouls.Won","Poss.Fouls.Conceded","Def.Fouls.Conceded","Poss.Yellow.Cards","Def.Yellow.Cards",
                                                                "Poss.Red.Cards","Def.Red.Cards","Poss.Penalties.Won","Def.Penalties.Won","Poss.Penalties.Conceded","Def.Penalties.Conceded"))]
    }
  }
  
  merged_stats
}

#GK DEFENSIVE ACTIONS----------
createGkDefenseColumns <- function(location="none", match_sheet) {
  match_subset <- createSubset(pattern = c("gk.s.o.g.stop", "gk.s.o.g.def.stop", "gk.s.o.g.scored", "gk.shot.miss",
                                              "gk.high.balls.won", "gk.high.balls.lost","gk.smothers.won", "gk.smothers.lost"), 
                               col = "def.action", source_df = match_sheet)
  match_subset <- addColumnForQualifier(newcol = "BC.SOG.Faced",
                                        pattern = "big.chances.scored|big.chances.shot.on.goal", 
                                        patternLocation = "poss.notes", ndf = match_subset, ogdf = match_sheet)
  match_subset <- addMultiColumnsForQualifiers(patterns = c("cross"="cross", "corner.kick"="corner.kick","free.kick"="free.kick", 
                                                            "foul.won"="fouls.won"),
                                               pattern_locations = c("play.type","play.type", "play.type",
                                                                     "def.player.disciplinary"),
                                               ndf = match_subset, ogdf = match_sheet)
  match_subset <- match_subset[grep("gk", match_subset[,"def.action"]),]
    
  stats_cols <- list()
  
  if(location=="zones" | location =="thirds" | location=="wings"){
    if(location=="zones"){
      patterns <- c("D6"="D6", "D18"="D18", "DL"="D3L|DL", "DC"="D3C|DC","DR"="D3R|DR", "DML"="DM3L|DML", "DMC"="DM3C|DMC", "DMR"="DM3R|DMR", "AML"="AM3L|AML", "AMC"="AM3C|AMC", "AMR"="AM3R|AMR", "AL"="A3L|AL", "AC"="A3C|AC", "AR"="A3R|AR", "A18"="A18", "A6"="A6")
    } else if(location=="thirds"){
      patterns <- c("D3"="D6|D18|D3L|DL|D3C|DC|D3R|DR", "M3"="M", "A3"="A6|A18|A3L|AL|A3C|AC|A3R|AR")
    } else if(location=="wings"){
      patterns = c("L3"="L", "C3"="C|6|18", "R3"="R")
    }
    for(i in 1:length(patterns)) {
      newcol_vec <- c(grepl(patterns[i], match_subset[,"def.location"]))
      match_subset[,names(patterns[i])] <- newcol_vec
    }
  }
  # Shots on goal faced
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"),
                                      target_col = "def.action",source_df = match_subset,  location = location,
                                      new_sumcol = list(name="sog.faced", summed=c("s.o.g")), 
                                      new_divcol = list(name="gpersog", numerator="gk.s.o.g.scored", denominator=c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored")),
                                      team = "def", 
                                      stat_names = c("Saves","GK SOG Def Stop","Goals Allowed" ,"GK SOG Faced", "GperSOG"),
                                      drop_cols = "GK SOG Def Stop")
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"), 
                                      target_col = "def.action", team = "def",  location = location,
                                      source_df = match_subset[match_subset[,"BC.SOG.Faced"]==TRUE,], 
                                      new_sumcol = list(name="BC SOG Faced",summed="s.o.g"), 
                                      new_divcol = list(name="GperBCSOG", numerator="gk.s.o.g.scored",denominator="BC SOG Faced"),
                                      stat_names = c("BC Saves", "gk.s.o.g.def.stop","BC Goals Allowed","BC SOG Faced", "GperBCSOG"),
                                      drop_cols = "gk.s.o.g.def.stop")
  # high balls faced
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("gk.high.balls.won","gk.high.balls.lost"),
                                      target_col = "def.action",source_df = match_subset, team="def",  location = location,
                                      new_sumcol = list(name="High Balls Faced", summed="high.balls"),
                                      new_divcol = list(name="HB Win Pct", numerator="gk.high.balls.won",denominator="High Balls Faced"), 
                                      stat_names = c( "HB Won", "HB Lost","High Balls Faced", "HB Win Pct"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("caught", "punched","parried","collected"),target_col = "gk.ball.stop",
                                      team = "def", source_df = match_subset,  location = location,
                                      stat_names = c("HB Caught", "HB Punched", "HB Parried", "HB Collected"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("gk.high.balls.won","gk.high.balls.lost"),
                                      target_col = "def.action",source_df = match_subset[match_subset[,"cross"]==TRUE,], team="def",  location = location,
                                      new_sumcol = list(name="Crosses GK Faced", summed="high.balls"),
                                      stat_names = c("Crosses GK Won","Crosses GK Lost","Crosses GK Faced"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("gk.high.balls.won","gk.high.balls.lost"),
                                      target_col = "def.action",source_df = match_subset[match_subset[,"corner.kick"]==TRUE,], team="def",  location = location,
                                      new_sumcol = list(name="CKs GK Faced", summed="high.balls"),
                                      stat_names = c("CKs GK Won","CKs GK Lost","CKs GK Faced"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("gk.high.balls.won","gk.high.balls.lost"),
                                      target_col = "def.action",source_df = match_subset[match_subset[,"free.kick"]==TRUE,], team="def",  location = location,
                                      new_sumcol = list(name="FKs GK Faced", summed="high.balls"),
                                      stat_names = c("FKs GK Won","FKs GK Lost","FKs GK Faced"))
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = TRUE, target_col = "foul.won", source_df = match_subset, team="def",  location = location,
                                      stat_names = "HB Fouls Won")
  # smothers faced
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(pattern = c("gk.smothers.won", "gk.smothers.lost"),target_col = "def.action",
                   source_df = match_subset, team = "def",  location = location,
                   stat_names = c("Smothers Won", "Smothers Lost"))
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  merged_stats
  
}

#GK DISTRIBUTION----------
#Create clean data frame with only goalkeeper passing events
createGkDistColumns <- function(location="none", match_sheet) {
  match_subset <- createSubset(pattern = c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"),
                                  col = "poss.action", source_df = match_sheet[grep("[Gg][Kk]", match_sheet[,"poss.position"]),])
  match_subset <- addMultiColumnsForQualifiers(patterns=c("gkthrow"="gk.throw", "gkdropkick"="gk.drop.kick","gkfk"="goal.kick|free.kick"),
                                               pattern_locations = "play.type",
                                               ogdf = match_sheet, ndf = match_subset)
  
  stats_cols <- list()
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset, stat_names = c("GK Overall Pass Comp", "GK Overall Pass Att", "GK Overall Pass Comp Pct"), location = location, isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"gkthrow"]==TRUE,], stat_names = c("GK Throw Comp", "GK Throw Att", "GK Throw Comp Pct"), location = location, isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"gkdropkick"]==TRUE,], stat_names = c("GK Drop Kick Comp", "GK Drop Kick Att", "GK Drop Kick Comp Pct"), location = location, isPassing = TRUE)
  stats_cols[[length(stats_cols)+1]] <- createStatsTable(source_df = match_subset[match_subset[,"gkfk"]==TRUE,], stat_names = c("GKFK Comp", "GKFK Att", "GKFK Comp Pct"), location = location, isPassing = TRUE)
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  merged_stats
  
}

recalculatePctColumns <- function(match_sheet, location="none") {
  if(location == "none") {
    match_sheet[,"Shot Accuracy"] <- rowSums(match_sheet[,c("Goals","Shots Stopped by GK", "Shots Stopped by Def")])/rowSums(match_sheet[,c("Goals","Shots Stopped by GK", "Shots Stopped by Def","Shots Missed")])
    match_sheet[,"Pct Shots Pressed"] <- match_sheet[,"Shots Pressed"]/match_sheet[,"Shots"]
    match_sheet[,"BC Conversion Pct"] <- match_sheet[,"BC Scored"]/match_sheet[,"Big Chances"]
    match_sheet[,"Pass Comp Pct"] <- match_sheet[,"Pass Comp"]/match_sheet[,"Pass Att"]
    match_sheet[,"opPass Comp Pct"] <- match_sheet[,"opPass Comp"]/match_sheet[,"opPass Att"]
    match_sheet[,"PPass Comp Pct"] <- match_sheet[,"PPass Comp"]/match_sheet[,"PPass Att"]
    match_sheet[,"Cross Comp Pct"] <- match_sheet[,"Cross Comp"]/match_sheet[,"Cross Att"]
    match_sheet[,"Launch Comp Pct"] <- match_sheet[,"Launch Comp"]/match_sheet[,"Launch Att"]
    match_sheet[,"Through Comp Pct"] <- match_sheet[,"Through Comp"]/match_sheet[,"Through Att"]
    match_sheet[,"Throw In Comp Pct"] <- match_sheet[,"Throw In Comp"]/match_sheet[,"Throw In Att"]
    match_sheet[,"fwPass Comp Pct"] <- match_sheet[,"fwPass Comp"]/match_sheet[,"fwPass Att"]
    match_sheet[,"sPass Comp Pct"] <- match_sheet[,"sPass Comp"]/match_sheet[,"sPass Att"]
    match_sheet[,"bPass Comp Pct"] <- match_sheet[,"bPass Comp"]/match_sheet[,"bPass Att"]
    match_sheet[,"fwopPass Comp Pct"] <- match_sheet[,"fwopPass Comp"]/match_sheet[,"fwopPass Att"]
    match_sheet[,"sopPass Comp Pct"] <- match_sheet[,"sopPass Comp"]/match_sheet[,"sopPass Att"]
    match_sheet[,"bopPass Comp Pct"] <- match_sheet[,"bopPass Comp"]/match_sheet[,"bopPass Att"]
    match_sheet[,"fwPPass Comp Pct"] <- match_sheet[,"fwPPass Comp"]/match_sheet[,"fwPPass Att"]
    match_sheet[,"sPPass Comp Pct"] <- match_sheet[,"sPPass Comp"]/match_sheet[,"sPPass Att"]
    match_sheet[,"bPPass Comp Pct"] <- match_sheet[,"bPPass Comp"]/match_sheet[,"bPPass Att"]
    match_sheet[,"Pct opPass Pressed"] <- match_sheet[,"PPass Att"]/match_sheet[,"opPass Att"]
    match_sheet[,c("rFreq Pass Fwd", "rFreq Pass Side", "rFreq Pass Back")] <- match_sheet[,c("fwPass Att","sPass Att","bPass Att")]/match_sheet[,"Pass Att"]
    match_sheet[,c("rFreq opPass Fwd","rFreq opPass Side","rFreq opPass Back")] <-  match_sheet[,c("fwopPass Att","sopPass Att","bopPass Att")]/match_sheet[,"opPass Att"]
    match_sheet[,c("rFreq PPass Fwd","rFreq PPass Side","rFreq PPass Back")] <-  match_sheet[,c("fwPPass Att","sPPass Att","bPPass Att")]/match_sheet[,"PPass Att"]
    match_sheet[,"CK Comp Pct"] <- match_sheet[,"Corner Kicks Completed"]/match_sheet[,"Corner Kicks Taken"]
    match_sheet[,"FK Pass Comp Pct"] <- match_sheet[,"FK Pass Comp"]/match_sheet[,"FK Pass Att"]
    match_sheet[,"TO Win Pct"] <- match_sheet[,"TO Won"]/match_sheet[,"Take Ons"]
    match_sheet[,"AD Win Pct"] <- match_sheet[,"AD Won"]/match_sheet[,"Aerial Duels"]
    match_sheet[,"GperSOG"] <- match_sheet[,"Goals Allowed"]/match_sheet[,"GK SOG Faced"]
    match_sheet[,"GperBCSOG"] <- match_sheet[,"BC Goals Allowed"]/match_sheet[,"BC SOG Faced"]
    match_sheet[,"HB Win Pct"] <- match_sheet[,"HB Won"]/match_sheet[,"High Balls Faced"]
    match_sheet[,"GK Overall Pass Comp Pct"] <- match_sheet[,"GK Overall Pass Comp"]/match_sheet[,"GK Overall Pass Att"]
    match_sheet[,"GK Throw Comp Pct"] <- match_sheet[,"GK Throw Comp"]/match_sheet[,"GK Throw Att"]
    match_sheet[,"GK Drop Kick Comp Pct"] <- match_sheet[,"GK Drop Kick Comp"]/match_sheet[,"GK Drop Kick Att"]
    match_sheet[,"GKFK Comp Pct"] <- match_sheet[,"GKFK Comp"]/match_sheet[,"GKFK Att"]
    
    
  } else if(location == "zones" | location == "thirds" | location == "wings") {
    if(location=="zones"){
      locations <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML", "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
    } else if(location=="thirds"){
      locations <- c("D3", "M3", "A3")
    } else if(location=="wings"){
      locations <- c("L3", "C3", "R3")
    }
    for(index in locations){
      match_sheet[,paste(index, "Shot Accuracy")] <- rowSums(match_sheet[,paste(index, c("Goals","Shots Stopped by GK", "Shots Stopped by Def"))])/rowSums(match_sheet[,paste(index, c("Goals","Shots Stopped by GK", "Shots Stopped by Def","Shots Missed"))])
      match_sheet[,paste(index,"Pct Shots Pressed")] <- match_sheet[,paste(index,"Shots Pressed")]/match_sheet[,paste(index,"Shots")]
      match_sheet[,paste(index,"BC Conversion Pct")] <- match_sheet[,paste(index,"BC Scored")]/match_sheet[,paste(index,"Big Chances")]
      match_sheet[,paste(index,"Pass Comp Pct")] <- match_sheet[,paste(index, "Pass Comp")]/match_sheet[,paste(index,"Pass Att")]
      match_sheet[,paste(index,"opPass Comp Pct")] <- match_sheet[,paste(index,"opPass Comp")]/match_sheet[,paste(index,"opPass Att")]
      match_sheet[,paste(index,"PPass Comp Pct")] <- match_sheet[,paste(index,"PPass Comp")]/match_sheet[,paste(index,"PPass Att")]
      match_sheet[,paste(index,"Cross Comp Pct")] <- match_sheet[,paste(index,"Cross Comp")]/match_sheet[,paste(index,"Cross Att")]
      match_sheet[,paste(index,"Launch Comp Pct")] <- match_sheet[,paste(index,"Launch Comp")]/match_sheet[,paste(index,"Launch Att")]
      match_sheet[,paste(index,"Through Comp Pct")] <- match_sheet[,paste(index,"Through Comp")]/match_sheet[,paste(index,"Through Att")]
      match_sheet[,paste(index,"Throw In Comp Pct")] <- match_sheet[,paste(index,"Throw In Comp")]/match_sheet[,paste(index,"Throw In Att")]
      match_sheet[,paste(index,"fwPass Comp Pct")] <- match_sheet[,paste(index,"fwPass Comp")]/match_sheet[,paste(index,"fwPass Att")]
      match_sheet[,paste(index,"sPass Comp Pct")] <- match_sheet[,paste(index,"sPass Comp")]/match_sheet[,paste(index,"sPass Att")]
      match_sheet[,paste(index,"bPass Comp Pct")] <- match_sheet[,paste(index,"bPass Comp")]/match_sheet[,paste(index,"bPass Att")]
      match_sheet[,paste(index,"fwopPass Comp Pct")] <- match_sheet[,paste(index,"fwopPass Comp")]/match_sheet[,paste(index,"fwopPass Att")]
      match_sheet[,paste(index,"sopPass Comp Pct")] <- match_sheet[,paste(index,"sopPass Comp")]/match_sheet[,paste(index,"sopPass Att")]
      match_sheet[,paste(index,"bopPass Comp Pct")] <- match_sheet[,paste(index,"bopPass Comp")]/match_sheet[,paste(index,"bopPass Att")]
      match_sheet[,paste(index,"fwPPass Comp Pct")] <- match_sheet[,paste(index,"fwPPass Comp")]/match_sheet[,paste(index,"fwPPass Att")]
      match_sheet[,paste(index,"sPPass Comp Pct")] <- match_sheet[,paste(index,"sPPass Comp")]/match_sheet[,paste(index,"sPPass Att")]
      match_sheet[,paste(index,"bPPass Comp Pct")] <- match_sheet[,paste(index,"bPPass Comp")]/match_sheet[,paste(index,"bPPass Att")]
      match_sheet[,paste(index,"Pct opPass Pressed")] <- match_sheet[,paste(index,"PPass Att")]/match_sheet[,paste(index,"opPass Att")]
      match_sheet[,paste(index,c("rFreq Pass Fwd", "rFreq Pass Side", "rFreq Pass Back"))] <- match_sheet[,paste(index,c("fwPass Att","sPass Att","bPass Att"))]/match_sheet[,paste(index,"Pass Att")]
      match_sheet[,paste(index,c("rFreq opPass Fwd","rFreq opPass Side","rFreq opPass Back"))] <-  match_sheet[,paste(index,c("fwopPass Att","sopPass Att","bopPass Att"))]/match_sheet[,paste(index,"opPass Att")]
      match_sheet[,paste(index,c("rFreq PPass Fwd","rFreq PPass Side","rFreq PPass Back"))] <-  match_sheet[,paste(index,c("fwPPass Att","sPPass Att","bPPass Att"))]/match_sheet[,paste(index,"PPass Att")]
      match_sheet[,paste(index,"CK Comp Pct")] <- match_sheet[,paste(index,"Corner Kicks Completed")]/match_sheet[,paste(index,"Corner Kicks Taken")]
      match_sheet[,paste(index,"FK Pass Comp Pct")] <- match_sheet[,paste(index,"FK Pass Comp")]/match_sheet[,paste(index,"FK Pass Att")]
      match_sheet[,paste(index,"TO Win Pct")] <- match_sheet[,paste(index,"TO Won")]/match_sheet[,paste(index,"Take Ons")]
      match_sheet[,paste(index, "AD Win Pct")] <- match_sheet[,paste(index,"AD Won")]/match_sheet[,paste(index,"Aerial Duels")]
      match_sheet[,paste(index,"GperSOG")] <- match_sheet[,paste(index,"Goals Allowed")]/match_sheet[,paste(index,"GK SOG Faced")]
      match_sheet[,paste(index,"GperBCSOG")] <- match_sheet[,paste(index,"BC Goals Allowed")]/match_sheet[,paste(index,"BC SOG Faced")]
      match_sheet[,paste(index,"HB Win Pct")] <- match_sheet[,paste(index,"HB Won")]/match_sheet[,paste(index,"High Balls Faced")]
      match_sheet[,paste(index,"GK Overall Pass Comp Pct")] <- match_sheet[,paste(index,"GK Overall Pass Comp")]/match_sheet[,paste(index,"GK Overall Pass Att")]
      match_sheet[,paste(index,"GK Throw Comp Pct")] <- match_sheet[,paste(index,"GK Throw Comp")]/match_sheet[,paste(index,"GK Throw Att")]
      match_sheet[,paste(index,"GK Drop Kick Comp Pct")] <- match_sheet[,paste(index,"GK Drop Kick Comp")]/match_sheet[,paste(index,"GK Drop Kick Att")]
      match_sheet[,paste(index,"GKFK Comp Pct")] <- match_sheet[,paste(index,"GKFK Comp")]/match_sheet[,paste(index,"GKFK Att")]
    }
  }
  match_sheet
}