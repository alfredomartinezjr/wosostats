#R function that reads from the database, given a competition, and computes one overall table
# for the competition
library(RCurl)
database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
database <- read.csv(textConnection(database), stringsAsFactors = FALSE)


#Set "competition" as the competition slug that is in the database
#competition <- competitionsluggoeshere
getCompetitionMatches <- function(competition.slug, round=NA) {
  if(competition.slug == "database"){
    matches <- database[!is.na(database[,"match.csv.link"]),"stats.csv.link"]
    names <- database[!is.na(database[,"match.csv.link"]),"matchup"]
  } else {
    if (!is.na(round)){
      database <- database[database[,"round"]==round,]
    }
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
rbindCompetitionMatches <- function(match_list) {
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
  overall[,"Shots.per.90"] <- (overall$Shots/overall$MP)*90
  overall[,"Shot.Accuracy"] <- (overall$Goals + overall$Shot.GK.Stop + overall$Shot.Def.Stop)/
    (overall$Goals + overall$Shot.GK.Stop + overall$Shot.Def.Stop + overall$Shot.Miss)
  overall[,"Pct.Shots.Pressed"] <- overall[,"Shot.Pressed"]/overall[,"Shots"]
  overall[,"Assists.per.90"] <- (overall$Assists/overall$MP)*90
  overall[,"Key.Passes.per.90"] <- (overall[,"Key.Passes"]/overall$MP)*90
  overall[,"Big.Chances.per.90"] <- (overall[,"Big.Chances"]/overall$MP)*90
  overall[,"BC.Conversion.Pct"] <- overall[,"BC.Scored"]/overall[,"Big.Chances"]
  overall[,"Pass.Att.per.90"] <- (overall[,"Pass.Att"]/overall$MP)*90
  overall[,"Pass.Comp.Pct"] <- overall[,"Pass.Comp"]/overall[,"Pass.Att"]
  overall[,"opPass.Att.per.90"] <- (overall[,"opPass.Att"]/overall$MP)*90
  overall[,"opPass.Comp.Pct"] <- overall[,"opPass.Comp"]/overall[,"opPass.Att"]
  overall[,"opPPass.Att.per.90"] <- (overall[,"opPPass.Att"]/overall$MP)*90
  overall[,"opPPass.Comp.Pct"] <- overall[,"opPPass.Comp"]/overall[,"opPPass.Att"]
  overall[,"Pct.opPass.Pressed"] <- overall[,"opPPass.Att"]/overall[,"opPass.Att"]
  overall[,"rFreq.Pass.Fwd"] <- overall[,"fwPass.Att"]/(overall[,"fwPass.Att"]+overall[,"sPass.Att"]+overall[,"bPass.Att"])
  overall[,"rFreq.Pass.Side"] <- overall[,"sPass.Att"]/(overall[,"fwPass.Att"]+overall[,"sPass.Att"]+overall[,"bPass.Att"])
  overall[,"rFreq.Pass.Back"] <- overall[,"bPass.Att"]/(overall[,"fwPass.Att"]+overall[,"sPass.Att"]+overall[,"bPass.Att"])
  overall[,"fwPass.Att.per.90"] <- (overall[,"fwPass.Att"]/overall$MP)*90
  overall[,"fwPass.Comp.Pct"] <- overall[,"fwPass.Comp"]/overall[,"fwPass.Att"]
  overall[,"sPass.Att.per.90"] <- (overall[,"sPass.Att"]/overall$MP)*90
  overall[,"sPass.Comp.Pct"] <- overall[,"sPass.Comp"]/overall[,"sPass.Att"]
  overall[,"bPass.Att.per.90"] <- (overall[,"bPass.Att"]/overall$MP)*90
  overall[,"bPass.Comp.Pct"] <- overall[,"bPass.Comp"]/overall[,"bPass.Att"]
  overall[,"rFreq.A3.Passes"] <- overall[,"A3Pass.Att"]/(overall[,"A3Pass.Att"]+overall[,"M3Pass.Att"]+overall[,"D3Pass.Att"])
  overall[,"rFreq.M3.Passes"] <- overall[,"M3Pass.Att"]/(overall[,"A3Pass.Att"]+overall[,"M3Pass.Att"]+overall[,"D3Pass.Att"])
  overall[,"rFreq.D3.Passes"] <- overall[,"D3Pass.Att"]/(overall[,"A3Pass.Att"]+overall[,"M3Pass.Att"]+overall[,"D3Pass.Att"])
  overall[,"A3Pass.Att.per.90"] <- (overall[,"A3Pass.Att"]/overall$MP)*90
  overall[,"A3Pass.Comp.Pct"] <- overall[,"A3Pass.Comp"]/overall[,"A3Pass.Att"]
  overall[,"M3Pass.Att.per.90"] <- (overall[,"M3Pass.Att"]/overall$MP)*90
  overall[,"M3Pass.Comp.Pct"] <- overall[,"M3Pass.Comp"]/overall[,"M3Pass.Att"]
  overall[,"D3Pass.Att.per.90"] <- (overall[,"D3Pass.Att"]/overall$MP)*90
  overall[,"D3Pass.Comp.Pct"] <- overall[,"D3Pass.Comp"]/overall[,"D3Pass.Att"]
  overall[,"rFreq.A3.opPasses"] <- overall[,"A3opPass.Att"]/(overall[,"A3opPass.Att"]+overall[,"M3opPass.Att"]+overall[,"D3opPass.Att"])
  overall[,"rFreq.M3.opPasses"] <- overall[,"M3opPass.Att"]/(overall[,"A3opPass.Att"]+overall[,"M3opPass.Att"]+overall[,"D3opPass.Att"])
  overall[,"rFreq.D3.opPasses"] <- overall[,"D3opPass.Att"]/(overall[,"A3opPass.Att"]+overall[,"M3opPass.Att"]+overall[,"D3opPass.Att"])
  overall[,"A3opPass.Att.per.90"] <- (overall[,"A3opPass.Att"]/overall$MP)*90
  overall[,"A3opPass.Comp.Pct"] <- overall[,"A3opPass.Comp"]/overall[,"A3opPass.Att"]
  overall[,"M3opPass.Att.per.90"] <- (overall[,"M3opPass.Att"]/overall$MP)*90
  overall[,"M3opPass.Comp.Pct"] <- overall[,"M3opPass.Comp"]/overall[,"M3opPass.Att"]
  overall[,"D3opPass.Att.per.90"] <- (overall[,"D3opPass.Att"]/overall$MP)*90
  overall[,"D3opPass.Comp.Pct"] <- overall[,"D3opPass.Comp"]/overall[,"D3opPass.Att"]
  overall[,"rFreq.opPass.Fwd"] <- overall[,"fwopPass.Att"]/(overall[,"fwopPass.Att"]+overall[,"sopPass.Att"]+overall[,"bopPass.Att"])
  overall[,"rFreq.opPass.Side"] <- overall[,"sopPass.Att"]/(overall[,"fwopPass.Att"]+overall[,"sopPass.Att"]+overall[,"bopPass.Att"])
  overall[,"rFreq.opPass.Back"] <- overall[,"bopPass.Att"]/(overall[,"fwopPass.Att"]+overall[,"sopPass.Att"]+overall[,"bopPass.Att"])
  overall[,"fwopPass.Att.per.90"] <- (overall[,"fwopPass.Att"]/overall$MP)*90
  overall[,"fwopPass.Comp.Pct"] <- overall[,"fwopPass.Comp"]/overall[,"fwopPass.Att"]
  overall[,"sopPass.Att.per.90"] <- (overall[,"sopPass.Att"]/overall$MP)*90
  overall[,"sopPass.Comp.Pct"] <- overall[,"sopPass.Comp"]/overall[,"sopPass.Att"]
  overall[,"bopPass.Att.per.90"] <- (overall[,"bopPass.Att"]/overall$MP)*90
  overall[,"bopPass.Comp.Pct"] <- overall[,"bopPass.Comp"]/overall[,"bopPass.Att"]
  overall[,"Pct.Passes.Pressed"] <- overall[,"PPass.Att"]/overall[,"Pass.Att"]
  overall[,"PPass.Att.per.90"] <- (overall[,"PPass.Att"]/overall$MP)*90
  overall[,"PPass.Comp.Pct"] <- overall[,"PPass.Comp"]/overall[,"PPass.Att"]
  overall[,"rFreq.PPass.Fwd"] <- overall[,"fwPPass.Att"]/(overall[,"fwPPass.Att"]+overall[,"sPPass.Att"]+overall[,"bPPass.Att"])
  overall[,"rFreq.PPass.Side"] <- overall[,"sPPass.Att"]/(overall[,"fwPPass.Att"]+overall[,"sPPass.Att"]+overall[,"bPPass.Att"])
  overall[,"rFreq.PPass.Back"] <- overall[,"bPPass.Att"]/(overall[,"fwPPass.Att"]+overall[,"sPPass.Att"]+overall[,"bPPass.Att"])
  overall[,"fwPPass.Att.per.90"] <- (overall[,"fwPPass.Att"]/overall$MP)*90
  overall[,"fwPPass.Comp.Pct"] <- overall[,"fwPPass.Comp"]/overall[,"fwPPass.Att"]
  overall[,"sPPass.Att.per.90"] <- (overall[,"sPPass.Att"]/overall$MP)*90
  overall[,"sPPass.Comp.Pct"] <- overall[,"sPPass.Comp"]/overall[,"sPPass.Att"]
  overall[,"bPPass.Att.per.90"] <- (overall[,"bPPass.Att"]/overall$MP)*90
  overall[,"bPPass.Comp.Pct"] <- overall[,"bPPass.Comp"]/overall[,"bPPass.Att"]
  overall[,"Cross.Att.per.90"] <- (overall[,"Cross.Att"]/overall$MP)*90
  overall[,"Cross.Att.per.opPass"] <- (overall[,"Cross.Att"]/overall["opPass.Att"])
  overall[,"Cross.Comp.Pct"] <- overall[,"Cross.Comp"]/overall[,"Cross.Att"]
  overall[,"Launch.Att.per.90"] <- (overall[,"Launch.Att"]/overall$MP)*90
  overall[,"Launch.Att.per.Pass"] <- (overall[,"Launch.Att"]/overall["Pass.Att"])
  overall[,"Launch.Comp.Pct"] <- overall[,"Launch.Comp"]/overall[,"Launch.Att"]
  overall[,"Through.Att.per.90"] <- (overall[,"Through.Att"]/overall$MP)*90
  overall[,"Through.Att.per.Pass"] <- (overall[,"Through.Att"]/overall["Pass.Att"])
  overall[,"Through.Att.per.opPass"] <- (overall[,"Through.Att"]/overall["opPass.Att"])
  overall[,"Through.Comp.Pct"] <- overall[,"Through.Comp"]/overall[,"Through.Att"]
  overall[,"Throw.In.Att.per.90"] <- (overall[,"Throw.In.Att"]/overall$MP)*90
  overall[,"Throw.In.Att.per.Pass"] <- (overall[,"Throw.In.Att"]/overall["Pass.Att"])
  overall[,"Throw.In.Comp.Pct"] <- overall[,"Throw.In.Comp"]/overall[,"Throw.In.Att"]
  overall[,"CK.Comp.Pct"] <- overall[,"Corner.Kicks.Completed"]/overall[,"Corner.Kicks.Taken"]
  overall[,"FK.Pass.Comp.Pct"] <- overall[,"FK.Pass.Comp"]/overall[,"FK.Pass.Att"]
  overall[,"Take.Ons.per.90"] <- (overall[,"Take.Ons"]/overall$MP)*90
  overall[,"TO.Win.Pct"] <- overall[,"TO.Won"]/overall[,"Take.Ons"]
  overall[,"Poss.Disrupted.per.90"] <- (overall[,"All.Possessions.Disrupted"]/overall$MP)*90
  overall[,"Aerial.Duels.per.90"] <- (overall[,"Aerial.Duels"]/overall$MP)*90
  overall[,"AD.Win.Pct"] <- overall[,"AD.Won"]/overall[,"Aerial.Duels"]
  overall[,"Opp.Poss.Disrupted.per.90"] <- (overall[,"All.Opp.Poss.Disrupted"]/overall$MP)*90
  overall[,"Dribbled.per.90"] <- (overall[,"Dribbled"]/overall$MP)*90
  overall[,"Recoveries.per.90"] <- (overall[,"Recoveries"]/overall$MP)*90
  overall[,"Int.per.90"] <- (overall[,"Interceptions"]/overall$MP)*90
  overall[,"Blocks.per.90"] <- (overall[,"Blocks"]/overall$MP)*90
  overall[,"Clearances.per.90"] <- (overall[,"Clearances"]/overall$MP)*90
  overall[,"GperSOG"] <- overall[,"Goals.Allowed"]/overall[,"GK.SOG.Faced"]
  overall[,"GperBCSOG"] <- overall[,"BC.Goals.Allowed"]/overall[,"BC.SOG.Faced"]
  overall[,"HB.Win.Pct"] <- overall[,"HB.Won"]/overall[,"High.Balls.Faced"]
  overall[,"GK.Overall.Pass.Comp.Pct"] <- overall[,"GK.Overall.Pass.Comp"]/overall[,"GK.Overall.Pass.Att"]
  overall[,"GK.Throw.Comp.Pct"] <- overall[,"GK.Throw.Comp"]/overall[,"GK.Throw.Att"]
  overall[,"GK.Drop.Kick.Comp.Pct"] <- overall[,"GK.Drop.Kick.Comp"]/overall[,"GK.Drop.Kick.Att"]
  overall[,"GKFK.Comp.Pct"] <- overall[,"GKFK.Comp"]/overall[,"GKFK.Att"]
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