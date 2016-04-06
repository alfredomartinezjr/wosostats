#R function that reads from the database, given a competition, and computes one overall table
# for the competition
library(RCurl)
database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
database <- read.csv(textConnection(database), stringsAsFactors = FALSE)


#Set "competition" as the competition slug that is in the database
#competition <- competitionsluggoeshere
getCompetitionMatches <- function(competition.slug) {
  if(competition.slug == "database"){
    matches <- database[!is.na(database[,"match.csv.link"]),"stats.csv.link"]
    names <- database[!is.na(database[,"match.csv.link"]),"matchup"]
  } else {
    matches <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),"stats.csv.link"]
    names <- database[database[,"competition.slug"] == competition.slug & !is.na(database[,"match.csv.link"]),"matchup"]
  }
  match_list <- vector("list", 0)
  x <- 1
  while (x <= length(matches)) {
    d <- getURL(matches[x])
    d <- read.csv(textConnection(d), stringsAsFactors = FALSE)
    match_list[[x]] <- d
    x <- x + 1
  }
  match_list
}

#Set team as the team acronymn as its shown in the database
getTeamMatches <- function(team){
  matches <- database[database[,"home.team"] == team & !is.na(database[,"match.csv.link"]),"stats.csv.link"]
  names <- database[database[,"home.team"] == team & !is.na(database[,"match.csv.link"]),"matchup"]
  match_list <- vector("list", 0)
  x <- 1
  while (x <= length(matches)) {
    d <- getURL(matches[x])
    d <- read.csv(textConnection(d), stringsAsFactors = FALSE)
    d$Match <- names[[x]]
    match_list[[x]] <- d
    x <- x + 1
  }
  names(match_list) <- names
  match_list
}

#Given a match_list list with all the matches, rbinds them
cbindCompetitionMatches <- function(match_list) {
  #Creates a blank overall table
  d <- do.call("rbind", match_list)
  players <- unique(d[,c("Player", "Team")])
  stats <- as.data.frame(matrix(rep(0, length(names(d))), nrow = 1))
  stats <- stats[-1,]
  names(stats) <- names(d)
  stats$Player <- as.character(stats$Player)
  stats$Team <- as.character(stats$Team)
  overall <- merge(players, stats, by=c("Player", "Team"), all=TRUE)

  #for each row in "overall", gets each column's colSums for that row's "Player"-"Team" combo in d
  x <- 1
  while(x <= nrow(overall)) {
    sub <- d[d[,"Player"] == overall[x,"Player"] & d[,"Team"] == overall[x,"Team"],]
    overall[x,3:ncol(overall)] <- colSums(sub[,3:ncol(sub)])
    x <- x + 1
  }
  
  #for certain columns, we need to recalculate the values
  overall[,"Shot.Accuracy"] <- (overall$Goals + overall$Shot.GK.Stop + overall$Shot.Def.Stop)/
    (overall$Goals + overall$Shot.GK.Stop + overall$Shot.Def.Stop + overall$Shot.Miss)
  overall[,"Pct.Shots.Pressd"] <- overall[,"Shot.Pressd"]/overall[,"Shots"]
  overall[,"Shots.per.90"] <- (overall$Shots/overall$MP)*90
  overall[,"KP.to.Goal"] <- (overall[,"Key.Assists"] + overall[,"Second.Assists"])/overall[,"Key.Passes"]
  overall[,"BC.to.Goal"] <- overall[,"BC.Scored"]/overall[,"Big.Chances"]
  overall[,"Big.Chances.per.90"] <- (overall[,"Big.Chances"]/overall$MP)*90
  overall[,"Assists.per.90"] <- (overall$Assists/overall$MP)*90
  overall[,"Key.Passes.per.90"] <- (overall[,"Key.Passes"]/overall$MP)*90
  overall[,"Cross.Comp.Pct"] <- overall[,"Cross.Comp"]/overall[,"Cross.Att"]
  overall[,"Cross.Att.per.90"] <- (overall[,"Cross.Att"]/overall$MP)*90
  overall[,"Cross.Att.per.Pass"] <- (overall[,"Cross.Att"]/overall["Pass.Att"])
  overall[,"Cross.Att.per.opPass"] <- (overall[,"Cross.Att"]/overall["opPass.Att"])
  overall[,"Launch.Comp.Pct"] <- overall[,"Launch.Comp"]/overall[,"Launch.Att"]
  overall[,"Launch.Att.per.90"] <- (overall[,"Launch.Att"]/overall$MP)*90
  overall[,"Launch.Att.per.Pass"] <- (overall[,"Launch.Att"]/overall["Pass.Att"])
  overall[,"Launch.Att.per.opPass"] <- (overall[,"Launch.Att"]/overall["opPass.Att"])
  overall[,"Through.Comp.Pct"] <- overall[,"Through.Comp"]/overall[,"Through.Att"]
  overall[,"Through.Att.per.90"] <- (overall[,"Through.Att"]/overall$MP)*90
  overall[,"Through.Att.per.Pass"] <- (overall[,"Through.Att"]/overall["Pass.Att"])
  overall[,"Through.Att.per.opPass"] <- (overall[,"Through.Att"]/overall["opPass.Att"])
  overall[,"Throw.In.Comp.Pct"] <- overall[,"Throw.In.Comp"]/overall[,"Throw.In.Att"]
  overall[,"Throw.In.Att.per.90"] <- (overall[,"Throw.In.Att"]/overall$MP)*90
  overall[,"Throw.In.Att.per.Pass"] <- (overall[,"Throw.In.Att"]/overall["Pass.Att"])
  overall[,"CK.Effectiveness"] <- overall[,"Corner.Kicks.Completed"]/overall[,"Corner.Kicks.Taken"]
  overall[,"FK.Pass.Comp.Pct"] <- overall[,"FK.Pass.Comp"]/overall[,"FK.Pass.Att"]
  overall[,"Pass.Comp.Pct"] <- overall[,"Pass.Comp"]/overall[,"Pass.Att"]
  overall[,"opPass.Comp.Pct"] <- overall[,"opPass.Comp"]/overall[,"opPass.Att"]
  overall[,"rFreq.Pass.Fwd"] <- overall[,"fwPass.Att"]/(overall[,"fwPass.Att"]+overall[,"sPass.Att"]+overall[,"bPass.Att"])
  overall[,"rFreq.Pass.Side"] <- overall[,"sPass.Att"]/(overall[,"fwPass.Att"]+overall[,"sPass.Att"]+overall[,"bPass.Att"])
  overall[,"rFreq.Pass.Back"] <- overall[,"bPass.Att"]/(overall[,"fwPass.Att"]+overall[,"sPass.Att"]+overall[,"bPass.Att"])
  overall[,"fwPass.Comp.Pct"] <- overall[,"fwPass.Comp"]/overall[,"fwPass.Att"]
  overall[,"sPass.Comp.Pct"] <- overall[,"sPass.Comp"]/overall[,"sPass.Att"]
  overall[,"bPass.Comp.Pct"] <- overall[,"bPass.Comp"]/overall[,"bPass.Att"]
  overall[,"Pct.Pass.Pressd"] <- overall[,"PPass.Att"]/overall[,"Pass.Att"]
  overall[,"PPass.Comp.Pct"] <- overall[,"PPass.Comp"]/overall[,"PPass.Att"]
  overall[,"Pct.opPass.Pressd"] <- overall[,"opPPass.Att"]/overall[,"opPass.Att"]
  overall[,"opPPass.Comp.Pct"] <- overall[,"opPPass.Comp"]/overall[,"opPPass.Att"]
  overall[,"rFreq.PPass.Fwd"] <- overall[,"fwPPass.Att"]/(overall[,"fwPPass.Att"]+overall[,"sPPass.Att"]+overall[,"bPPass.Att"])
  overall[,"rFreq.PPass.Side"] <- overall[,"sPPass.Att"]/(overall[,"fwPPass.Att"]+overall[,"sPPass.Att"]+overall[,"bPPass.Att"])
  overall[,"rFreq.PPass.Back"] <- overall[,"bPPass.Att"]/(overall[,"fwPPass.Att"]+overall[,"sPPass.Att"]+overall[,"bPPass.Att"])
  overall[,"fwPPass.Comp.Pct"] <- overall[,"fwPPass.Comp"]/overall[,"fwPPass.Att"]
  overall[,"sPPass.Comp.Pct"] <- overall[,"sPPass.Comp"]/overall[,"sPPass.Att"]
  overall[,"bPPass.Comp.Pct"] <- overall[,"bPPass.Comp"]/overall[,"bPPass.Att"]
  overall[,"rFreq.opPass.Fwd"] <- overall[,"fwopPass.Att"]/(overall[,"fwopPass.Att"]+overall[,"sopPass.Att"]+overall[,"bopPass.Att"])
  overall[,"rFreq.opPass.Side"] <- overall[,"sopPass.Att"]/(overall[,"fwopPass.Att"]+overall[,"sopPass.Att"]+overall[,"bopPass.Att"])
  overall[,"rFreq.opPass.Back"] <- overall[,"bopPass.Att"]/(overall[,"fwopPass.Att"]+overall[,"sopPass.Att"]+overall[,"bopPass.Att"])
  overall[,"fwopPass.Comp.Pct"] <- overall[,"fwopPass.Comp"]/overall[,"fwopPass.Att"]
  overall[,"sopPass.Comp.Pct"] <- overall[,"sopPass.Comp"]/overall[,"sopPass.Att"]
  overall[,"bopPass.Comp.Pct"] <- overall[,"bopPass.Comp"]/overall[,"bopPass.Att"]
  overall[,"TO.Win.Pct"] <- overall[,"TO.Won"]/overall[,"Take.Ons"]
  overall[,"AD.Win.Pct"] <- overall[,"AD.Won"]/overall[,"Aerial.Duels"]
  overall[,"Int.per.90"] <- (overall[,"Interceptions"]/overall$MP)*90
  overall[,"GperSOG"] <- overall[,"Goals.Allowed"]/overall[,"SOG.Faced"]
  overall[,"GperBCSOG"] <- overall[,"BC.Goals.Allowed"]/overall[,"BC.SOG.Faced"]
  overall[,"Distribution.Success"] <- overall[,"All.Pass.Comp"]/overall[,"All.Pass.Att"]
  overall[,"Throw.Comp.Pct"] <- overall[,"Throw.Comp"]/overall[,"Throw.Att"]
  overall[,"Drop.Kick.Comp.Pct"] <- overall[,"Drop.Kick.Comp"]/overall[,"Drop.Kick.Att"]
  overall[,"GK.FK.Comp.Pct"] <- overall[,"GK.FK.Comp"]/overall[,"GK.FK.Att"]
  overall
}

##Takes a match list, rbinds them, and includes only rows for specific teams
rbindTeamMatches <- function(match_list, team) {
  d <- do.call("rbind", match_list)
  d <- d[d[,"Team"] == team,]
}

##For when you just want one team, given an "overall" data frame, culls it down
##to "team"
subsetOverall <- function(team, table) {
  table <- table[table[,"Team"] == team,]
  table
}