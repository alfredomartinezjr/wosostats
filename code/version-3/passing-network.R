# match_sheet <- getURL(matchURL)
# match_sheet <- read.csv(textConnection(match_sheet), stringsAsFactors = FALSE)
#
# OR 
# match_sheet <- read.csv(matchfile, stringsAsFactors = FALSE)
#
# OR
# source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-3/creating-stats-tables.R")
# database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
# database <- read.csv(textConnection(database), stringsAsFactors = FALSE)
# getMatchFiles(competition.slug = "nwsl-2016", type="match.csv.link", team="PTFC")

createPassingSubset <- function(match_sheet) {
  #create logical vector to check if a given row was a completed pass
  rowIsACompletedPass <- c(grepl("passes.*.c", match_sheet$poss.action))
  #create empty vector for what will be all the players who received a respective pass
  pass_recipient_player <- character(nrow(match_sheet))
  pass_recipient_team <- character(nrow(match_sheet))
  #loops through each passing action to get names of players who received a completed pass
  for(i in 1:nrow(match_sheet)) {
    if(rowIsACompletedPass[i]){
      sheet_event_next <- as.numeric(match_sheet[i,"event"][1])+1
      sheet_row_nextevent <- grep(paste0("^",sheet_event_next,"$"), match_sheet[,"event"])[1]
      pass_recipient_player[i] <- match_sheet[sheet_row_nextevent,"poss.player"]
      pass_recipient_team[i] <- match_sheet[sheet_row_nextevent,"poss.team"]
    }
  }
  #add column to the match spreadsheet for each completed pass event's recipient
  match_sheet$pass_recipient <- pass_recipient_player
  match_sheet$pass_recipient_team <- pass_recipient_team
  #subset the match spreadsheet to only include completed passes
  match_sheet <- match_sheet[grep("passes.*.c",match_sheet[,"poss.action"]),]
  match_sheet
}

createPassingNetwork <- function(data_source, team=NA){
  if(class(data_source) == "list") {
    match_list <- data_source
    #create empty list where all the subsets of the match spreadsheets for only completed passes will go
    passing_data <- list()
    #create passing network aggregated from multiple matches
    for(index in 1:length(match_list)) {
      match_sheet <- match_list[[index]]
      match_sheet <- match_sheet[,c("event","poss.team","poss.number","poss.player","poss.action")]
      match_sheet <- createPassingSubset(match_sheet)
      if(!is.na(team)) {
        match_sheet <- match_sheet[match_sheet[,"poss.team"]==team,]
        match_sheet <- match_sheet[match_sheet[,"pass_recipient_team"]==team,]
      }
      passing_data[[index]] <- match_sheet
    }
    for(index in 1:length(passing_data)) {
      if(exists("passing_data_binded")) {
        passing_data_binded <- rbind(passing_data_binded, passing_data[[index]])
      } else {
        passing_data_binded <- passing_data[[index]]
      }
    }
    passing_network <- as.data.frame(unclass(table(passing_data_binded$poss.player,passing_data_binded$pass_recipient)))
    passing_network
  } 
  #create passing network for one match
  else if(class(data_source) == "data.frame"){
    match_sheet <- data_source
    match_sheet <- match_sheet[,c("event","poss.team","poss.number","poss.player","poss.action")]
    #create logical vector to check if a given row was a completed pass
    match_sheet <- createPassingSubset(match_sheet)
    if(!is.na(team)) {
      match_sheet <- match_sheet[match_sheet[,"poss.team"]==team,]
      match_sheet <- match_sheet[match_sheet[,"pass_recipient_team"]==team,]
      }
    #create the passing network table
    passing_network <- as.data.frame(unclass(table(match_sheet$poss.player,match_sheet$pass_recipient)))
    passing_network
  }
}

createSharedMinsSubset <- function(match_sheet){
  #get list of players from game
  poss_columns <- unique(match_sheet[,c("poss.player", "poss.team", "poss.number")])
  names(poss_columns) <- c("Player", "Team", "Number")
  def_columns <- unique(match_sheet[,c("def.player", "def.team", "def.number")])
  names(def_columns) <- c("Player", "Team", "Number")
  players_sheet <- rbind(poss_columns, def_columns)
  players_sheet <- players_sheet[!is.na(players_sheet[,"Player"]),]
  players_sheet <- unique(players_sheet[,])
  rm(poss_columns, def_columns)
  #### get numbers for match minutes and match length
  matchminutes <- unique(match_sheet$time)
  matchlength <- length(matchminutes)
  #### get list of players subbed off and on
  substitutions <- match_sheet[grepl("substitution",match_sheet[,"poss.action"]),]
  #### create blank vectors for minutes played, first minute, last minute, and games started
  minutesPlayed <- numeric(nrow(players_sheet)); names(minutesPlayed) <- players_sheet$Player
  first_minute <- numeric(nrow(players_sheet)); names(first_minute) <- players_sheet$Player
  last_minute <- numeric(nrow(players_sheet)); names(last_minute) <- players_sheet$Player
  gamesStarted <- numeric(nrow(players_sheet)); names(gamesStarted) <- players_sheet$Player
  #### all player subbed off and on - as vectors 
  all_substitutions <- paste(substitutions$poss.player, substitutions$poss.team, substitutions$poss.number)
  all_subs_on <- substitutions[substitutions[,"poss.action"]=="substitution.on",]
  all_subs_on <- paste(all_subs_on$poss.player, all_subs_on$poss.team, all_subs_on$poss.number)
  all_subs_off <- substitutions[substitutions[,"poss.action"]=="substitution.off",]
  all_subs_off <- paste(all_subs_off$poss.player, all_subs_off$poss.team, all_subs_off$poss.number)
  #### add minutes data for players
  for (i in 1:nrow(players_sheet)) {
    indexed_player <- paste(players_sheet[i,"Player"],players_sheet[i,"Team"],players_sheet[i,"Number"])
    # if the player doesn't appear in the substitutions data frame, the player played the entire match
    if (!(indexed_player %in% all_substitutions)) {
      minutesPlayed[i] <- matchlength
      first_minute[i] <- 1
      last_minute[i] <- matchlength
      gamesStarted[i] <- 1
    } else if (indexed_player %in% all_substitutions) {
      # check if she was a starter, based on if she was only ever substituted into the game
      if (!(indexed_player %in% all_subs_on)) {
        lastevent <- substitutions[substitutions[,"poss.player"] == players_sheet$Player[i],"event"]
        lasteventrow <- grep(lastevent, match_sheet$event)
        
        minutesPlayed[i] <- length(unique(match_sheet[1:lasteventrow,"time"]))
        first_minute[i] <- 1
        last_minute[i] <- length(unique(match_sheet[1:lasteventrow,"time"]))
        gamesStarted[i] <- 1
      } 
      # check if she was not a starter, got subbed on, and did not get subbed off
      else if (!(indexed_player %in% all_subs_off)) {
        lastevent <- substitutions[substitutions[,"poss.player"] == players_sheet$Player[i],"event"]
        firsteventrow <- grep(lastevent, match_sheet[,"event"])[1]
        minutesPlayed[i] <- length(unique(match_sheet[firsteventrow:nrow(match_sheet),"time"]))
        first_minute[i] <- matchlength - minutesPlayed[i] + 1
        last_minute[i] <- matchlength
        gamesStarted[i] <- 0
      }
      # else, she she was not a starter, got subbed on, and was later subbed off
      else {
        lastevent <- substitutions[substitutions[,"poss.player"] == players_sheet$Player[i],"event"]
        firsteventrow <- grep(lastevent[1], match_sheet[,"event"])
        lasteventrow <- grep(lastevent[2], match_sheet[,"event"])
        minutesPlayed[i] <- length(unique(match_sheet[firsteventrow:lasteventrow,"time"]))
        first_minute[i] <- match(match_sheet[firsteventrow,"time"],matchminutes)
        last_minute[i] <- match(match_sheet[lasteventrow,"time"], matchminutes)
        gamesStarted[i] <- 0
      }
    }
  }
  players_sheet$GP <- 1
  players_sheet$MP <- minutesPlayed
  players_sheet$FirstMinute <- first_minute
  players_sheet$LastMinute <- last_minute
  players_sheet$GS <- gamesStarted
  
  match_shared_minutes_data <- data.frame(minute=integer(), firstplayer=character(), 
                                          firstplayerteam=character(), sharedplayer=character(), 
                                          sharedplayerteam=character(), stringsAsFactors = FALSE)
  for (minute in 1:matchlength) {
    player_vector <- character()
    player_team_vector <- character()
    for(player in 1:length(players_sheet$Player)) {
      if(minute %in% players_sheet$FirstMinute[player]:players_sheet$LastMinute[player]) {
        player_vector[length(player_vector)+1] <- players_sheet$Player[player]
        player_team_vector[length(player_team_vector)+1] <- players_sheet$Team[player]
      }
    }
    minutesCol <- rep(minute, times = (length(player_vector)^2))
    firstPlayerCol <- rep(player_vector, each = length(player_vector))
    firstPlayerTeamCol <- rep(player_team_vector, each = length(player_team_vector))
    sharedPlayerCol <- rep(player_vector, times = length(player_vector))
    sharedPlayerTeamCol <- rep(player_team_vector, times = length(player_team_vector))
    newRows <- (nrow(match_shared_minutes_data)+1):(nrow(match_shared_minutes_data)+(length(player_vector)^2))
    match_shared_minutes_data[newRows,"minute"] <- minutesCol
    match_shared_minutes_data[newRows,"firstplayer"] <- firstPlayerCol
    match_shared_minutes_data[newRows,"firstplayerteam"] <- firstPlayerTeamCol
    match_shared_minutes_data[newRows,"sharedplayer"] <- sharedPlayerCol
    match_shared_minutes_data[newRows,"sharedplayerteam"] <- sharedPlayerTeamCol
    
  }
  match_shared_minutes_data
}

createSharedMinsNetwork <- function(data_source, team=NA) {
  if(class(data_source)=="list") {
    match_list <- data_source
    #create empty list where all the subsets of the match spreadsheets for only completed passes will go
    shared_minutes_data <- list()
    
    for(index in 1:length(match_list)) {
      match_sheet <- match_list[[index]]
      match_shared_minutes_data <- createSharedMinsSubset(match_sheet)
      if(!is.na(team)) {
        match_shared_minutes_data <- match_shared_minutes_data[match_shared_minutes_data[,"firstplayerteam"]==team,]
        match_shared_minutes_data <- match_shared_minutes_data[match_shared_minutes_data[,"sharedplayerteam"]==team,]
      }
      shared_minutes_data[[index]] <- match_shared_minutes_data
    }
    
    for(index in 1:length(shared_minutes_data)) {
      if(exists("shared_minutes_data_binded")) {
        shared_minutes_data_binded <- rbind(shared_minutes_data_binded, shared_minutes_data[[index]])
      } else {
        shared_minutes_data_binded <- shared_minutes_data[[index]]
      }
    }
    
    shared_minutes_network <- as.data.frame(unclass(table(shared_minutes_data_binded$firstplayer,shared_minutes_data_binded$sharedplayer)))
    shared_minutes_network
  }
  else if (class(data_source)=="data.frame"){
    match_sheet <- data_source
    match_shared_minutes_data <- createSharedMinsSubset(match_sheet)
    if(!is.na(team)) {
      match_shared_minutes_data <- match_shared_minutes_data[match_shared_minutes_data[,"firstplayerteam"]==team,]
      match_shared_minutes_data <- match_shared_minutes_data[match_shared_minutes_data[,"sharedplayerteam"]==team,]
    }
    shared_minutes_network <- as.data.frame(unclass(table(match_shared_minutes_data$firstplayer,match_shared_minutes_data$sharedplayer)))
    shared_minutes_network
  }
}
