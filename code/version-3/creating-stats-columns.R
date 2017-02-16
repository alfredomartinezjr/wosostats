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
createPlayersColumns <- function(use_rosters=FALSE, match_positions=FALSE, match_sheet) {
  
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
createShotsColumns <- function(match_sheet){

    match_subset <- addMultiColumnsForQualifiers(patterns = c("pressured"="pressure", "challenged"="challenge"), 
                                      pattern_locations = c("def.action", "def.action"), ogdf = match_sheet,
                                      ndf = createCleanDataFrame(c("shots", "accuracy", "shots.scored", "shots.stopped.by.gk", 
                                                                   "shots.stopped.by.def", "shots.missed", "shots.blocked") ,"poss.action", match_sheet))
    match_subset <- addColumnForMultiQualifiers(c("pressured"=TRUE,"challenged"=TRUE), newcol = "pressed",
                                     source_df = match_subset, exp="OR")
    match_subset <- addMultiColumnsForQualifiers(patterns = c("D6"="D6", "D18"="D18", "DL"="D3L|DL", "DC"="D3C|DC","DR"="D3R|DR", 
                                                     "DML"="DM3L|DML", "DMC"="DM3C|DMC", "DMR"="DM3R|DMR", "AML"="AM3L|AML",
                                                     "AMC"="AM3C|AMC", "AMR"="AM3R|AMR", "AL"="A3L|AL", "AC"="A3C|AC", "AR"="A3R|AR",
                                                     "A18"="A18", "A6"="A6"),
                                        ogdf = match_sheet,ndf = match_subset,
                                        pattern_locations = c("poss.location","poss.location","poss.location","poss.location",
                                                              "poss.location","poss.location","poss.location", "poss.location",
                                                              "poss.location","poss.location","poss.location","poss.location",
                                                              "poss.location","poss.location","poss.location","poss.location"))
    
    stats_cols <- list()
    stats_cols[[1]] <- createStatsTable(pattern = c("shots.per.90", "shots.scored", "shots.stopped.by.gk", "shots.stopped.by.def", "shots.missed", "shots.blocked"), 
                                        target_col = "poss.action", source_df = match_subset, 
                                new_sumcol = list(name = "shots", summed="shots"), 
                                new_divcol = list(name = "accuracy", 
                                               numerator = c("shots.scored", "shots.stopped.by.gk", "shots.stopped.by.def"), 
                                               denominator = c("shots.scored", "shots.stopped.by.gk", "shots.stopped.by.def", "shots.missed")),
                                stat_names = c("Shots per 90","Goals", "Shots Stopped by GK", "Shots Stopped by Def", "Shots Missed","Shots Blocked","Shots","Shot Accuracy"))
    stats_cols[[2]] <- createStatsTable(pattern = c(TRUE, FALSE), target_col = "pressed", source_df = match_subset, 
                                        new_sumcol = list(name = "shots", summed="TRUE|FALSE"),
                                        new_divcol = list(name= "pct", numerator = "TRUE.", denominator = c("TRUE.","FALSE.")),
                                        stat_names = c("Shots Pressed", "Shots Not Pressed", "All Shots", "Pct Shots Pressed"))
    stats_cols[[3]] <- createLocationStatsTable(c("shots.scored", "shots.stopped.by.gk", 
                                              "shots.stopped.by.def", "shots.missed", "shots.blocked"), "poss.action", 
                                              source_df = match_subset, 
                                              stat_names = c("Goals", "Shot GK Stop", "Shot Def Stop", "Shot Miss", "Shot Block"))
    
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
createChancesColumns <- function(match_sheet) {
  match_subset <- createDataFrame(pattern = c("assists", "key.passes","second.assists","big.chances","big.chances.scored","big.chances.shot.on.goal",
                                              "big.chances.shot.missed","big.chances.dispossessed","big.chances.created","big.chances.lost"),
                                  col = "poss.notes",df = match_sheet)
  match_subset <- addMultiColumnsForQualifiers(patterns = c("assists"="^assists", "key.passes"="key.pass|second.assist", "second.assists"="second.assist"),
                                               pattern_locations = c("poss.notes", "poss.notes", "poss.notes"),
                                               ogdf = match_sheet, 
                                               ndf = match_subset)
  match_subset <- addColumnForMultiQualifiers(newcol = "key.assists", pattern = c("assists"=TRUE,"key.passes"=TRUE), 
                                              source_df = match_subset,
                                              exp = "AND")
  
  stats_cols <- list()
  stats_cols[[1]] <- createStatsTable(pattern = c("big.chances.per.90", "big.chances.scored","big.chances.shot.on.goal", "big.chances.shot.missed", 
                                                  "big.chances.dispossessed","big.chances.created", "big.chances.lost"),
                                      target_col = "poss.notes",
                                      source_df = fillBlanks(match_subset),
                                      new_sumcol = list(name="big.chances", summed="scored|goal|missed|dispossessed|lost"), 
                                      new_divcol = list(name = "big.chances.conversion", 
                                                        numerator = "big.chances.scored", 
                                                        denominator = "big.chances"),
                                      stat_names = c("Big Chances per 90", "BC Scored","BC SOG", "BC Shot Miss", "BC Dispossess", "BC Created", "BC Lost", "Big Chances","BC Conversion Pct"))
  
  match_subset <- createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", match_subset,
                                       keep_cols = c("assists", "key.passes", "second.assists", "key.assists"))
  stats_cols[[2]] <- createStatsTable(pattern = c(TRUE,"assists.per.90"),target_col = "assists", source_df = match_subset,
                                      stat_names = c("Assists", "Assists per 90"))
  stats_cols[[3]] <- createStatsTable(pattern = c(TRUE,"assists.per.90"),target_col = "assists", source_df = match_subset,
                                      stat_names = c("Assists", "Assists per 90"))
  stats_cols[[4]] <- createStatsTable(pattern = c(TRUE, "key.passes.per.90"),target_col = "key.passes", source_df = match_subset,
                                      stat_names = c("Key Passes", "Key Passes per 90"))
  stats_cols[[5]] <- createStatsTable(pattern = c(TRUE),target_col = "key.assists", source_df = match_subset,
                                      stat_names = c("Key Assists"))
  stats_cols[[6]] <- createStatsTable(pattern = c(TRUE),target_col = "second.assists", source_df = match_subset,
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
createPassingColumns <- function(type=NA, match_sheet) {
  match_subset <- createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", match_sheet)
  match_subset <- addColumnForQualifier(newcol = "pressed",pattern = "pressured|challenged",patternLocation = "def.action",ogdf = match_sheet,ndf = match_subset)
  match_subset <- addColumnForQualifier(newcol = "opPass", 
                                        pattern="throw|gk|corner.kick|free.kick|goal.kick", 
                                        patternLocation = "play.type",
                                        ogdf = match_sheet,
                                        ndf = match_subset,
                                        invert = TRUE)
  match_subset <- addMultiColumnsForQualifiers(patterns = c("isCross"="cross","isLaunch"="launch","isThrough"="through", "isThrowIn" = "throw.in"),
                                               pattern_locations = c("play.type","play.type","play.type"),
                                               ogdf = match_sheet, 
                                               ndf = match_subset)
  
  stats_cols <- list()
  stats_cols[[1]] <- createPassingTable(source_df = match_subset, extra="pass.att.per.90", 
                                        stat_names = c("Pass Comp", "Pass Att", "Pass Comp Pct","Pass Att per 90"))
  stats_cols[[2]] <- createPassingTable(source_df = match_subset[match_subset[,"opPass"]==TRUE,], extra="oppass.att.per.90",
                                        stat_names = c("opPass Comp","opPass Att","opPass Comp Pct","opPass Att per 90"))
  stats_cols[[3]] <- createPassingTable(source_df = match_subset[match_subset[,"pressed"] == TRUE & match_subset[,"opPass"] == TRUE,],
                                        extra = "opppass.att.per.90",
                                        stat_names = c("opPPass Comp", "opPPass Att","opPPass Comp Pct", "opPPass Att per 90"))
  stats_cols[[4]] <- createStatsTable(source_df = match_subset[match_subset[,"opPass"]==TRUE,], target_col = "pressed", pattern = c(TRUE, FALSE), 
                                      new_divcol = list(name="pct", numerator="TRUE.", denominator=c("TRUE.","FALSE.")),
                                      drop_cols = c("TRUE.","FALSE."),
                                      stat_names = c("Pct opPass Pressed"))
  #special passes
  stats_cols[[5]] <- createPassingTable(source_df = match_subset[match_subset[,"isCross"]==TRUE,], extra = c("cross.att.per.90","cross.att.per.oppass"),
                                        stat_names = c("Cross Comp", "Cross Att", "Cross Att per 90","Cross Comp Pct", "Cross Att per Pass"))
  stats_cols[[6]] <- createPassingTable(source_df = match_subset[match_subset[,"isLaunch"]==TRUE,], extra = c("launch.att.per.90","launch.att.per.pass"),
                                        stat_names = c("Launch Comp", "Launch Att", "Launch Comp Pct", "Launch Att per 90", "Launch Att per Pass"))
  stats_cols[[7]] <- createPassingTable(source_df = match_subset[match_subset[,"isThrough"]==TRUE,], extra = c("through.att.per.90","through.att.per.pass"),
                                        stat_names = c("Through Comp", "Through Att", "Through Comp Pct", "Through Att per 90", "Through Att per Pass"))
  stats_cols[[8]] <- createPassingTable(source_df = match_subset[match_subset[,"isThrowIn"]==TRUE,], extra = c("throw.in.att.per.90","throw.in.att.per.pass"),
                                        stat_names = c("Throw In Comp", "Throw In Att", "Throw In Comp Pct", "Throw In Att per 90", "Throw In Att per Pass"))
  #by direction - all passes
  stats_cols[[9]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.f.c", "passes.f"), "poss.action", match_subset), 
                                        extra = "fwpass.att.per.90",
                                        stat_names = c("fwPass.Comp", "fwPass.Att", "fwPass.Comp.Pct", "fwPass.Att.per.90"))
  stats_cols[[10]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.s.c", "passes.s"), "poss.action", match_subset), 
                                        extra = "spass.att.per.90",
                                        stat_names = c("sPass.Comp", "sPass.Att", "sPass.Comp.Pct", "sPass.Att.per.90"))
  stats_cols[[11]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.b.c", "passes.b"), "poss.action", match_subset), 
                                        extra = "bpass.att.per.90",
                                        stat_names = c("bPass.Comp", "bPass.Att", "bPass.Comp.Pct", "bPass.Att.per.90"))
  #by direction - open play passes
  stats_cols[[12]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.f.c", "passes.f"), "poss.action", match_subset[match_subset[,"opPass"]==TRUE,]), 
                                        extra = "fwoppass.att.per.90",
                                        stat_names = c("fwopPass.Comp", "fwopPass.Att", "fwopPass.Comp.Pct", "fwopPass.Att.per.90"))
  stats_cols[[13]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.s.c", "passes.s"), "poss.action", match_subset[match_subset[,"opPass"]==TRUE,]), 
                                        extra = "soppass.att.per.90",
                                        stat_names = c("sopPass.Comp", "sopPass.Att", "sopPass.Comp.Pct", "sopPass.Att.per.90"))
  stats_cols[[14]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.b.c", "passes.b"), "poss.action", match_subset[match_subset[,"opPass"]==TRUE,]), 
                                        extra = "boppass.att.per.90",
                                        stat_names = c("bopPass.Comp", "bopPass.Att", "bopPass.Comp.Pct", "bopPass.Att.per.90"))
  #by direction - pressed passses
  stats_cols[[15]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.f.c", "passes.f"), "poss.action", match_subset[match_subset[,"pressed"]==TRUE,]),
                                        extra = "fwPPass.Att.per.90",
                                        stat_names = c("fwPPass.Comp", "fwPPass.Att", "fwPPass.Comp.Pct", "fwPPass.Att.per.90"))
  stats_cols[[16]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.s.c", "passes.s"), "poss.action", match_subset[match_subset[,"pressed"]==TRUE,]),
                                        extra = "sPPass.Att.per.90",
                                        stat_names = c("sPPass.Comp", "sPPass.Att", "sPPass.Comp.Pct", "sPPass.Att.per.90"))
  stats_cols[[17]] <- createPassingTable(source_df = createCleanDataFrame(c("passes.b.c", "passes.b"), "poss.action", match_subset[match_subset[,"pressed"]==TRUE,]),
                                        extra = "bPPass.Att.per.90",
                                        stat_names = c("bPPass.Comp", "bPPass.Att", "bPPass.Comp.Pct", "bPPass.Att.per.90"))
  if(!is.na(type) & type=="location") {
    #by location - all passes
    stats_cols[[18]] <- createPassingTable(source_df = match_subset[grep("A3L|AL|A3C|AC|A3R|AR|A18|A6",match_subset$poss.location),],
                                           extra = "A3Pass.Att.per.90",
                                           stat_names = c("A3Pass.Comp", "A3Pass.Att", "A3Pass.Comp.Pct", "A3Pass.Att.per.90"))
    stats_cols[[19]] <- createPassingTable(source_df = match_subset[grep("AM3L|AML|AM3C|AMC|AM3R|AMR|DM3L|DML|DM3C|DMC|DM3R|DMR",match_subset$poss.location),],
                                          extra = "M3Pass.Att.per.90",
                                          stat_names = c("M3Pass.Comp", "M3Pass.Att", "M3Pass.Comp.Pct", "M3Pass.Att.per.90"))
    stats_cols[[20]] <- createPassingTable(source_df = match_subset[grep("D3L|DL|D3C|DC|D3R|DR|D18|D6",match_subset$poss.location),], 
                                          extra = "D3Pass.Att.per.90",
                                          stat_names = c("D3Pass.Comp", "D3Pass.Att", "D3Pass.Comp.Pct", "D3Pass.Att.per.90"))
    #by location - open play passes
    stats_cols[[21]] <- createPassingTable(source_df = match_subset[grepl("A3L|AL|A3C|AC|A3R|AR|A18|A6",match_subset$poss.location) == TRUE & match_subset[,"opPass"]==TRUE,], 
                                          extra = "A3opPass.Att.per.90",
                                          stat_names = c("A3opPass.Comp", "A3opPass.Att", "A3opPass.Comp.Pct", "A3opPass.Att.per.90"))
    stats_cols[[22]] <- createPassingTable(source_df = match_subset[grepl("AM3L|AML|AM3C|AMC|AM3R|AMR|DM3L|DML|DM3C|DMC|DM3R|DMR",match_subset$poss.location) == TRUE & match_subset[,"opPass"]==TRUE,], 
                                          extra = "M3opPass.Att.per.90",
                                          stat_names = c("M3opPass.Comp", "M3opPass.Att", "M3opPass.Comp.Pct", "M3opPass.Att.per.90"))
    stats_cols[[23]] <- createPassingTable(source_df = match_subset[grepl("D3L|DL|D3C|DC|D3R|DR|D18|D6",match_subset$poss.location) == TRUE & match_subset[,"opPass"]==TRUE,], 
                                          extra = "D3opPass.Att.per.90",
                                          stat_names = c("D3opPass.Comp", "D3opPass.Att", "D3opPass.Comp.Pct", "D3opPass.Att.per.90"))
    #by location - pressured passes
    stats_cols[[24]] <- createPassingTable(source_df = match_subset[grepl("A3L|AL|A3C|AC|A3R|AR|A18|A6",match_subset$poss.location) == TRUE & match_subset[,"pressed"]==TRUE,], 
                                          extra = "A3PPass.Att.per.90",
                                          stat_names = c("A3PPass.Comp", "A3PPass.Att", "A3PPass.Comp.Pct", "A3PPass.Att.per.90"))
    stats_cols[[25]] <- createPassingTable(source_df = match_subset[grepl("AM3L|AML|AM3C|AMC|AM3R|AMR|DM3L|DML|DM3C|DMC|DM3R|DMR",match_subset$poss.location) == TRUE & match_subset[,"pressed"]==TRUE,], 
                                          extra = "M3PPass.Att.per.90",
                                          stat_names = c("M3PPass.Comp", "M3PPass.Att", "M3PPass.Comp.Pct", "M3PPass.Att.per.90"))
    stats_cols[[26]] <- createPassingTable(source_df = match_subset[grepl("D3L|DL|D3C|DC|D3R|DR|D18|D6",match_subset$poss.location) == TRUE & match_subset[,"pressed"]==TRUE,], 
                                          extra = "D3PPass.Att.per.90",
                                          stat_names = c("D3PPass.Comp", "D3PPass.Att", "D3PPass.Comp.Pct", "D3PPass.Att.per.90"))
  }
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  allPassAtt <- rowSums(merged_stats[,c("fwPass.Att","sPass.Att","bPass.Att")],na.rm = TRUE)
  merged_stats[,c("rFreq Pass Fwd", "rFreq Pass Side", "rFreq Pass Back")] <- merged_stats[,c("fwPass.Att","sPass.Att","bPass.Att")]/allPassAtt
  opPassAtt <- rowSums(merged_stats[,c("fwopPass.Att","sopPass.Att","bopPass.Att")],na.rm = TRUE)
  merged_stats[,c("rFreq.opPass.Fwd","rFreq.opPass.Side","rFreq.opPass.Back")] <-  merged_stats[,c("fwopPass.Att","sopPass.Att","bopPass.Att")]/opPassAtt
  pressPassAtt <- rowSums(merged_stats[,c("fwPPass.Att","sPPass.Att","bPPass.Att")],na.rm = TRUE)
  merged_stats[,c("rFreq.PPass.Fwd","rFreq.PPass.Side","rFreq.PPass.Back")] <-  merged_stats[,c("fwPPass.Att","sPPass.Att","bPPass.Att")]/pressPassAtt
  if(!is.na(type) & type=="location") {
    location_allPassAtt <- rowSums(merged_stats[,c("A3Pass.Att","M3Pass.Att","D3Pass.Att")],na.rm = TRUE)
    merged_stats[,c("rFreq.A3.Passes", "rFreq.M3.Passes", "rFreq.D3.Passes")] <- merged_stats[,c("A3Pass.Att","M3Pass.Att","D3Pass.Att")]/location_allPassAtt
    location_opPassAtt <- rowSums(merged_stats[,c("A3opPass.Att","M3opPass.Att","D3opPass.Att")],na.rm = TRUE)
    merged_stats[,c("rFreq.A3.opPasses", "rFreq.M3.opPasses", "rFreq.D3.opPasses")] <- merged_stats[,c("A3opPass.Att","M3opPass.Att","D3opPass.Att")]/location_opPassAtt
    location_pressPassAtt <- rowSums(merged_stats[,c("A3PPass.Att","M3PPass.Att","D3PPass.Att")],na.rm = TRUE)
    merged_stats[,c("rFreq.A3.PPasses", "rFreq.M3.PPasses", "rFreq.D3.PPasses")] <- merged_stats[,c("A3PPass.Att","M3PPass.Att","D3PPass.Att")]/location_pressPassAtt
  }
  merged_stats
  
}

#PASSING BY ORIGIN & DESTINATION----------
createPassRangeColumns <- function(match_sheet) {
  match_subset <- addColumnForQualifier(newcol = "completed", pattern = "passes.*.c", 
                                        patternLocation = "poss.action", ogdf = match_sheet,
                                        ndf = createCleanDataFrame(col = "poss.action", df = match_sheet,
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
createSetPieceColumns <- function(match_sheet) {
  match_subset <- createDataFrame(pattern = c("corner.kick", "free.kick"),col = "play.type",df = match_sheet)
  match_subset <- addMultiColumnsForQualifiers(patterns = c("corner.kick"="corner.kick", "free.kick"="free.kick", "shot"="shot", "scored"="scored", 
                                                            "assist"="^assist", "keypass"="key.pass|^second.assist"),
                                               pattern_locations = c("play.type", "play.type", "poss.action", "poss.action", "poss.notes", "poss.notes"),
                                               ogdf = match_sheet,
                                               ndf = match_subset)
  
  stats_cols <- list()
  stats_cols[[1]] <- createPassingTable(source_df = match_subset[match_subset[,"corner.kick"]==TRUE,],
                                        stat_names = c("Corner Kicks Completed", "Corner Kicks Taken", "CK Comp Pct"))
  stats_cols[[2]] <- createStatsTable(source_df = match_subset[match_subset[,"corner.kick"]==TRUE,],
                                      pattern = TRUE, target_col = c("assist"), stat_names = "CK Assist")
  stats_cols[[3]] <- createStatsTable(source_df = match_subset[match_subset[,"corner.kick"]==TRUE,],
                                      pattern = TRUE, target_col = c("keypass"), stat_names = "CK Key Pass")
  stats_cols[[4]] <- createPassingTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,],
                                        stat_names = c("FK Pass Comp", "FK Pass Att", "FK Pass Comp Pct"))
  stats_cols[[5]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,],
                                      pattern = TRUE, target_col = c("assist"), stat_names = "FK Assist")
  stats_cols[[6]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,],
                                      pattern = TRUE, target_col = c("keypass"), stat_names = "FK Key Pass")
  stats_cols[[7]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,],
                                      pattern = TRUE, target_col = c("shot"), stat_names = "FK Shot")
  stats_cols[[8]] <- createStatsTable(source_df = match_subset[match_subset[,"free.kick"]==TRUE,],
                                      pattern = TRUE, target_col = c("scored"), stat_names = "FK Scored")
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  merged_stats[,"Free Kicks Taken"] <- rowSums(merged_stats[,c("FK Pass Att", "FK Shot")], na.rm=TRUE)
  
  merged_stats
}

#POSSESSION--------
createPossessionColumns <- function(match_sheet) {
  stats_cols <- list()
  stats_cols[[1]] <- createStatsTable(pattern = c("take.on.won","take.on.lost", "take.ons.per.90"),
                                   target_col = "poss.action", source_df = match_sheet, 
                                   new_sumcol = list(name="take.ons", summed="take.on"),
                                   new_divcol = list(name="take.on.win.pct", numerator="take.on.won", denominator=c("take.on.won","take.on.lost")),
                                   stat_names = c("TO Won", "TO Lost", "Take Ons per 90", "Take Ons", "TO Win Pct"))
  stats_cols[[2]] <- createStatsTable(pattern = c("take.on.lost",  "dispossessed", "lost.touch", "poss.disrupted.per.90"),
                                      target_col = "poss.action", source_df = match_sheet, 
                                      new_sumcol = list(name="all.possessions.disrupted", summed="take.on|dispossessed|lost.touch"),
                                      drop_cols = c("take.on.lost"),
                                      stat_names = c("Dispossessed","Lost Touches","Poss Disrupted per 90","All Possessions Disrupted"))
  
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
createRecoveryColumns <- function(match_sheet) {

  recoveryEvents <- sort(c(match_sheet[match_sheet[,"poss.action"] %in% "recoveries","event"],
                         match_sheet[match_sheet[,"poss.action"] %in% "recoveries","event"]-1))
  recoveryEvents <- paste0("^", recoveryEvents, "$")
  
  match_subset <- match_sheet[grep(paste(recoveryEvents, collapse = "|"), match_sheet[,"event"]),]
  match_subset <- fillBlanks(match_subset)
  
  match_subset$isDefRecovery <- c(FALSE, (match_subset[2:nrow(match_subset),"poss.action"]=="recoveries" & (match_subset[2:nrow(match_subset),"poss.team"] != match_subset[1:(nrow(match_subset)-1),"poss.team"])))
  match_subset$isPossRecovery <- c(FALSE, (match_subset[2:nrow(match_subset),"poss.action"]=="recoveries" & (match_subset[2:nrow(match_subset),"poss.team"] == match_subset[1:(nrow(match_subset)-1),"poss.team"])))
  
  stats_cols <- list()
  stats_cols[[1]] <- createStatsTable(target_col = "poss.action", pattern = c("recoveries","recoveries.per.90"), source_df = match_subset,
                                      stat_names = c("Recoveries","Recoveries per 90"))
  stats_cols[[2]] <- createStatsTable(target_col = "isDefRecovery", pattern = TRUE, source_df = match_subset,
                                      stat_names = "Def Recoveries")
  stats_cols[[3]] <- createStatsTable(target_col = "isPossRecovery", pattern = TRUE, source_df = match_subset,
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
createAerialColumns <- function(match_sheet) {
  match_subset <- createDataFrame(pattern = c("aerial.won","aerial.lost"),col = "poss.action",df = match_sheet)
  match_subset <- fillBlanks(match_subset)
  
  possAerials <- createStatsTable(pattern = c("aerial.won", "aerial.lost"), target_col = "poss.action", source_df = match_subset,
                                      stat_names = c("Poss.Aerial.Won","Poss.Aerial.Lost"))
  defAerials <- createStatsTable(pattern = c("aerial.won", "aerial.lost"), target_col = "def.action", source_df = match_subset,
                                      stat_names = c("Def.Aerial.Won","Def.Aerial.Lost"), team = "def")
  merged_stats <- merge(possAerials,defAerials,by=c("Player","Team","Number"), all=TRUE)
  
  merged_stats$'Aerial Duels' <- rowSums(merged_stats[,c("Poss.Aerial.Won","Poss.Aerial.Lost","Def.Aerial.Won","Def.Aerial.Lost")],na.rm = TRUE)
  merged_stats$'AD Won' <- rowSums(merged_stats[,c("Poss.Aerial.Won","Def.Aerial.Won")],na.rm = TRUE)
  merged_stats$'AD Lost' <- rowSums(merged_stats[,c("Poss.Aerial.Lost","Def.Aerial.Lost")],na.rm = TRUE)
  merged_stats$'AD Win Pct' <- merged_stats$'AD Won'/merged_stats$'Aerial Duels'
  merged_stats$'Aerial Duels per 90' <- NA 
  merged_stats <- merged_stats[,!names(merged_stats) %in% c("Poss.Aerial.Won","Poss.Aerial.Lost","Def.Aerial.Won","Def.Aerial.Lost")]
  
  merged_stats
}

#DEFENSIVE DISRUPTIONS---------------
createDefActionsColumns <- function(match_sheet) {
  match_subset <- createDataFrame(pattern = c("dispossessed", "tackles.ball.away", "tackles.ball.won", "tackles.ball","dribbled.tackles.missed", 
                                              "dribbled.out.run","dribbled.turned", "pressured", "challenged","interceptions","clearances", "ball.shield", "blocks"),
                                  col = "def.action", df = match_sheet)
  
  stats_cols <- list()
  stats_cols[[1]] <- createStatsTable(pattern = c("dispossessed", "pressured", "challenged", 
                                               "tackles.ball.away", "tackles.ball.won", "tackles.ball",
                                               "dribbled.tackles.missed", "dribbled.out.run","dribbled.turned"),
                                   target_col = "def.action", source_df = match_subset, team = "def",
                                   stat_names = c("Dispossessed","Pressured Opp","Challenged Opp","Tackles Ball Away","Tackles Ball Won",
                                                  "Tackles Ball","Dribbled Tackles Missed","Dribbled Out Run","Dribbled Turned"))
  stats_cols[[2]] <- createStatsTable(pattern = c("interceptions", "interceptions.per.90","blocks", "blocks.per.90", "clearances", "clearances.per.90", "ball.shield"),
                                      target_col = "def.action",source_df = match_subset,team = "def", 
                                      stat_names = c("Interceptions","Int per 90", "Blocks","Blocks per 90", "Clearances", "Clearances per 90", "Ball Shields"))
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  merged_stats$Tackles <- rowSums(merged_stats[,c("Tackles Ball Away","Tackles Ball Won","Tackles Ball")])
  merged_stats$Dribbled <- rowSums(merged_stats[,c("Dribbled Tackles Missed","Dribbled Out Run","Dribbled Turned")])
  merged_stats$'All Opp Poss Disrupted' <- rowSums(merged_stats[,c("Tackles","Dispossessed")])
  merged_stats[,c("Opp Poss Disrupted per 90","Dribbled per 90")] <- NA
  merged_stats <- merged_stats[,!names(merged_stats) %in% c("Tackles Ball Away","Tackles Ball Won", "Tackles Ball","Dribbled Tackles Missed","Dribbled Out Run","Dribbled Turned")]
  
  merged_stats
}

#DISCIPLINARY ACTIONS--------
createDisciplineColumns <- function(match_sheet) {
  pattern <- c("fouls.won","fouls.conceded","yellow.cards","red.cards","penalties.won","penalties.conceded")
  events_poss <- match_sheet[match_sheet[,"poss.player.disciplinary"] %in% c(pattern),"event"]
  events_def <- match_sheet[match_sheet[,"def.player.disciplinary"] %in% c(pattern),"event"]
  events_all <- sort(unique(c(events_poss, events_def)))
  events_all <- paste0("^", events_all, "$")
  
  match_subset <- match_sheet[grep(paste(events_all, collapse = "|"), match_sheet[,"event"]),]
  match_subset <- fillBlanks(match_subset)
  
  possDiscipline <- createStatsTable(pattern = c("fouls.won","fouls.conceded","yellow.cards","red.cards","penalties.won","penalties.conceded"), target_col = "poss.player.disciplinary", source_df = match_subset,
                                  stat_names = c("Poss.Fouls.Won","Poss.Fouls.Conceded","Poss.Yellow.Cards","Poss.Red.Cards","Poss.Penalties.Won","Poss.Penalties.Conceded"))
  defDiscipline <- createStatsTable(pattern = c("fouls.won","fouls.conceded","yellow.cards","red.cards","penalties.won","penalties.conceded"), target_col = "def.player.disciplinary", source_df = match_subset,
                                 stat_names = c("Def.Fouls.Won","Def.Fouls.Conceded","Def.Yellow.Cards","Def.Red.Cards","Def.Penalties.Won","Def.Penalties.Conceded"), team = "def")
  merged_stats <- merge(possDiscipline,defDiscipline,by=c("Player","Team","Number"), all=TRUE)
  merged_stats$'Fouls Won' <- rowSums(merged_stats[,c("Poss.Fouls.Won","Def.Fouls.Won")],na.rm = TRUE)
  merged_stats$'Fouls Conceded' <- rowSums(merged_stats[,c("Poss.Fouls.Conceded","Def.Fouls.Conceded")],na.rm = TRUE)
  merged_stats$'Yellow Cards' <- rowSums(merged_stats[,c("Poss.Yellow.Cards","Def.Yellow.Cards")],na.rm = TRUE)
  merged_stats$'Red Cards' <- rowSums(merged_stats[,c("Poss.Red.Cards","Def.Red.Cards")],na.rm = TRUE)
  merged_stats$'Penalties Won' <- rowSums(merged_stats[,c("Poss.Penalties.Won","Def.Penalties.Won")],na.rm = TRUE)
  merged_stats$'Penalties Conceded' <- rowSums(merged_stats[,c("Poss.Penalties.Conceded","Def.Penalties.Conceded")],na.rm = TRUE)
  merged_stats <- merged_stats[,!names(merged_stats) %in% c("Poss.Fouls.Won","Def.Fouls.Won","Poss.Fouls.Conceded","Def.Fouls.Conceded","Poss.Yellow.Cards","Def.Yellow.Cards",
                                                            "Poss.Red.Cards","Def.Red.Cards","Poss.Penalties.Won","Def.Penalties.Won","Poss.Penalties.Conceded","Def.Penalties.Conceded")]
  
  merged_stats
}

#GK DEFENSIVE ACTIONS----------
createGkDefenseColumns <- function(match_sheet) {
  match_subset <- createDataFrame(pattern = c("gk.s.o.g.stop", "gk.s.o.g.def.stop", "gk.s.o.g.scored", "gk.shot.miss",
                                              "gk.high.balls.won", "gk.high.balls.lost","gk.smothers.won", "gk.smothers.lost"),
                                  col = "def.action",df = match_sheet)
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
  # Shots on goal faced
  stats_cols[[1]] <- createStatsTable(pattern = c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"),
                                      target_col = "def.action",source_df = match_subset,
                                      new_sumcol = list(name="sog.faced", summed=c("s.o.g")), 
                                      new_divcol = list(name="gpersog", numerator="gk.s.o.g.scored", denominator=c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored")),
                                      team = "def", 
                                      stat_names = c("Saves","GK SOG Def Stop","Goals Allowed" ,"GK SOG Faced", "GperSOG"),
                                      drop_cols = "GK SOG Def Stop")
  stats_cols[[2]] <- createStatsTable(pattern = c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"), 
                                      target_col = "def.action", team = "def",
                                      source_df = match_subset[match_subset[,"BC.SOG.Faced"]==TRUE,], 
                                      new_sumcol = list(name="BC SOG Faced",summed="s.o.g"), 
                                      new_divcol = list(name="GperBCSOG", numerator="gk.s.o.g.scored",denominator="BC SOG Faced"),
                                      stat_names = c("BC Saves", "gk.s.o.g.def.stop","BC Goals Allowed","BC SOG Faced", "GperBCSOG"),
                                      drop_cols = "gk.s.o.g.def.stop")
  # high balls faced
  stats_cols[[3]] <- createStatsTable(pattern = c("gk.high.balls.won","gk.high.balls.lost"),
                                      target_col = "def.action",source_df = match_subset, team="def",
                                      new_sumcol = list(name="High Balls Faced", summed="high.balls"),
                                      new_divcol = list(name="HB Win Pct", numerator="gk.high.balls.won",denominator="High Balls Faced"), 
                                      stat_names = c( "HB Won", "HB Lost","High Balls Faced", "HB Win Pct"))
  stats_cols[[4]] <- createStatsTable(pattern = c("caught", "punched","parried","collected"),target_col = "gk.ball.stop",
                                      team = "def", source_df = match_subset,
                                      stat_names = c("HB Caught", "HB Punched", "HB Parried", "HB Collected"))
  stats_cols[[5]] <- createStatsTable(pattern = c("gk.high.balls.won","gk.high.balls.lost"),
                                      target_col = "def.action",source_df = match_subset[match_subset[,"cross"]==TRUE,], team="def",
                                      new_sumcol = list(name="Crosses GK Faced", summed="high.balls"),
                                      stat_names = c("Crosses GK Won","Crosses GK Lost","Crosses GK Faced"))
  stats_cols[[6]] <- createStatsTable(pattern = c("gk.high.balls.won","gk.high.balls.lost"),
                                      target_col = "def.action",source_df = match_subset[match_subset[,"corner.kick"]==TRUE,], team="def",
                                      new_sumcol = list(name="CKs GK Faced", summed="high.balls"),
                                      stat_names = c("CKs GK Won","CKs GK Lost","CKs GK Faced"))
  stats_cols[[7]] <- createStatsTable(pattern = c("gk.high.balls.won","gk.high.balls.lost"),
                                      target_col = "def.action",source_df = match_subset[match_subset[,"free.kick"]==TRUE,], team="def",
                                      new_sumcol = list(name="FKs GK Faced", summed="high.balls"),
                                      stat_names = c("FKs GK Won","FKs GK Lost","FKs GK Faced"))
  stats_cols[[8]] <- createStatsTable(pattern = TRUE, target_col = "foul.won", source_df = match_subset, team="def",
                                      stat_names = "HB Fouls Won")
  # smothers faced
  stats_cols[[9]] <- createStatsTable(pattern = c("gk.smothers.won", "gk.smothers.lost"),target_col = "def.action",
                   source_df = match_subset, team = "def",
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
createGkDistColumns <- function(match_sheet) {
  match_subset <- createDataFrame(pattern = c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"),
                                  col = "poss.action", df = match_sheet[grep("[Gg][Kk]", match_sheet[,"poss.position"]),])
  match_subset <- addMultiColumnsForQualifiers(patterns=c("gkthrow"="gk.throw", "gkdropkick"="gk.drop.kick","gkfk"="goal.kick|free.kick"),
                                               pattern_locations = c("play.type","play.type","play.type"),
                                               ogdf = match_sheet, ndf = match_subset)
  
  stats_cols <- list()
  stats_cols[[1]] <- createPassingTable(source_df = match_subset, stat_names = c("GK Overall Pass Comp", "GK Overall Pass Att", "GK Overall Pass Comp Pct"))
  stats_cols[[2]] <- createPassingTable(source_df = match_subset[match_subset[,"gkthrow"]==TRUE,], stat_names = c("GK Throw Comp", "GK Throw Att", "GK Throw Comp Pct"))
  stats_cols[[3]] <- createPassingTable(source_df = match_subset[match_subset[,"gkdropkick"]==TRUE,], stat_names = c("GK Drop Kick Comp", "GK Drop Kick Att", "GK Drop Kick Comp Pct"))
  stats_cols[[4]] <- createPassingTable(source_df = match_subset[match_subset[,"gkfk"]==TRUE,], stat_names = c("GKFK Comp", "GKFK Att", "GKFK Comp Pct"))
  
  for(index in 1:length(stats_cols)) {
    if(exists("merged_stats")) {
      merged_stats <- merge(merged_stats, stats_cols[[index]], by=c("Player","Team","Number"), all=TRUE)
    } else {
      merged_stats <- stats_cols[[index]]
    }
  }
  
  merged_stats
  
}