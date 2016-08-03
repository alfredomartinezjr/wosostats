require(readxl)
require(RCurl)
require(plyr)
require(dplyr)

#Download the USA-FRA SheBelieves Cup 2016 Excel file at this link: https://github.com/amj2012/wosostats/blob/master/source/excel/shebelieves-cup-2016/shebelieves-cup-2016-usa-fra-030616.xlsx
#Make sure the USA-FRA Excel file is in your working directory

##TIDY THE EXCEL FILE---------
#Running the following runs the Excel file through the tidy-excel.R code
#This code cleans up the match spreadsheet (it always looks ugly) and turns
#it into something that can be read by the code below that generates the
#location-based passing stats
#Set match.file <- "shebelieves-cup-2016-usa-fra-030616.xlsx"
if (exists("match.file")) {
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/tidy-excel.R")
}

##OR JUST SOURCE THE CSV FILE----------
#Running the following just sources the match spreadsheet in csv format directly
#from the GitHub repo
#set matchURL <- "https://raw.githubusercontent.com/amj2012/wosostats/master/source/csv/international-friendlies-2016/2016-international-friendly-usa-irl-012316.csv"
if (exists("matchURL")) {
  df <- getURL(matchURL)
  df <- read.csv(textConnection(df), stringsAsFactors = FALSE)
}

##CREATE COLUMNS IN STATS TABLE WITH PLAYER INFO---------
#Now we create the stats table!
#Here, we create a data frame for the "poss.player" column, and another for the
#"def.player" column. Then, we merge them into one, which is the beginning of the
#stats table, which for now will just be called "players"
players <- rbind(data.frame(Player=unique(df$poss.player), Team=NA, GP=NA, MP=NA, GS=NA),
                 data.frame(Player=unique(df$def.player), Team=NA, GP=NA, MP=NA, GS=NA))
players <- players[!is.na(players[,"Player"]),]
players <- unique(players[,])
#Here, we use the df data frame (this is the larger match spreadsheet you created with
#tidy-excel.R) to fill data in the "players" data frame such as each player's team
#and how many minutes they played.
matchlength <- length(unique(df$time))
substitutions <- df[grepl("substitution",df[,"poss.action"]),]
stats.tab.row <- 1
while (stats.tab.row <= nrow(players)) {
  player <- as.character(players[stats.tab.row,"Player"])
  #if they don't appear in the substitutions data frame, the player played the entire match
  if (nrow(substitutions[substitutions[,"poss.player"] %in% player,]) == 0) {
    players[stats.tab.row,"GP"] <- 1
    players[stats.tab.row,"MP"] <- matchlength
    players[stats.tab.row,"GS"] <- 1
  } else if (nrow(substitutions[substitutions[,"poss.player"] %in% player,]) > 0) {
    #check if she was a starter, based on if she was only ever substituted on
    if (grepl("substitution.on", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==FALSE) {
      #if she was a starter who was subbed off, get the length of unique values for vector df[,"time] 
      #up to where she got subbed off
      eventnum <- substitutions[substitutions[,"poss.player"] == player,"event"]
      lastminute <- grep(eventnum, df[,"event"])
      minutesplayed <- length(unique(df[1:lastminute,"time"]))
      players[stats.tab.row,"GP"] <- 1
      players[stats.tab.row,"MP"] <- minutesplayed
      players[stats.tab.row,"GS"] <- 1
    } else
      #if she wasn't a starter and got subbed on and wasn't also later subbed off
      if ((grepl("substitution.on", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==TRUE)
          & (grepl("substitution.off", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==FALSE)) {
        #if she wasn't a starter, got subbed on, and was never subbed off, get the length of unique
        #values for vector df[,"time] from when she got subbed on to when she got subbed off
        eventnum <- substitutions[substitutions[,"poss.player"] == player,"event"]
        firstminute <- grep(eventnum, df[,"event"])[1]
        minutesplayed <- length(unique(df[firstminute:nrow(df),"time"]))
        players[stats.tab.row,"GP"] <- 1
        players[stats.tab.row,"MP"] <- minutesplayed
        players[stats.tab.row,"GS"] <- 0
      } else
        #if she wasn't a starter, got subbed on, and was later subbed off
        if ((grepl("substitution.on", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==TRUE)
            & (grepl("substitution.off", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==TRUE)) {
          #if she wasn't a starter, got subbed on, and as later subbed off, get the length of unique
          #values for vector df[,"time] from when she got subbed on to when she got subbed off
          eventnum <- substitutions[substitutions[,"poss.player"] == player,"event"]
          firstminute <- grep(eventnum[1], df[,"event"])
          lastminute <- grep(eventnum[2], df[,"event"])
          minutesplayed <- length(unique(df[firstminute:lastminute,"time"]))
          players[stats.tab.row,"GP"] <- 1
          players[stats.tab.row,"MP"] <- minutesplayed
          players[stats.tab.row,"GS"] <- 0
        }
  }
  stats.tab.row <- stats.tab.row + 1
}
stats.tab.row <- 1
while (stats.tab.row <= nrow(players)) {
  player <- as.character(players[stats.tab.row,"Player"])
  playerteam <- unique(df[df[,"poss.player"] == player & !is.na(df[,"poss.player"]),"poss.team"])
  if(length(playerteam) == 0) {
    playerteam <- unique(df[df[,"def.player"] == player & !is.na(df[,"def.player"]),"def.team"])
  }
  players[stats.tab.row,"Team"] <- playerteam
  stats.tab.row <- stats.tab.row + 1
}
rm(eventnum,matchlength,player, playerteam, stats.tab.row, firstminute, 
   lastminute, minutesplayed, substitutions)

##CREATES COLUMNS WITH OVERALL PASSING STATS----------
#Will need the following functions
source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/functions.R")
t <- addMultiColumnsForQualifiers(patterns=c("pressured"="pressured", "challenged"="challenged", "forward.pass"="^passes.f", "sideways.pass"="^passes.s", "backward.pass"="^passes.b"),
                                  pattern_locations = c("def.action","def.action", "poss.action", "poss.action", "poss.action"),
                                  ogdf = df, ndf= createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                                        "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", df))
t <- addColumnForMultiQualifiers(c("pressured"="yes","challenged"="yes"), newcol = "pressed",
                                 df = t, exp="OR")
t2 <- createPassingTable(t, extra="pass.att.per.90")
names(t2) <- c("Player","Pass Comp", "Pass Att","Pass Att per 90", "Pass Comp Pct")
stats <- merge(players, t2, by=1, all=TRUE)
rm(t,t2,players)
stats$`Pass Att per 90` <- (stats$`Pass Att`/stats$MP)*90

##CREATES COLUMNS WITH LOCATION-BASED OPEN PLAY PASSING STATS FOR EACH THIRD OF THE FIELD----------
#Passing stats, without dead ball scenarios (GKs, GK throws, GK drop kicks,FKs, CKs, throw ins)
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick|goal.kick", patternLocation = "play.type", ogdf = df, invert = TRUE,
                           ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                        "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", df))
t <- t[t[,"opPass"]=="yes",]
#creates column for each third (def, mid, attack)
t <- addMultiColumnsForQualifiers(patterns = c("A3"="A3L|AL|A3C|AC|A3R|AR|A18|A6",
                                               "M3"="AM3L|AML|AM3C|AMC|AM3R|AMR|DM3L|DML|DM3C|DMC|DM3R|DMR",
                                               "D3"="D3L|DL|D3C|DC|D3R|DR|D18|D6"),
                                  ogdf = df,ndf = t,
                                  pattern_locations = c("poss.location","poss.location","poss.location"))
##Creates blank table with columns for direction distribution
locationdist <- createTable(c("rFreq A3 opPasses", "rFreq M3 opPasses", "rFreq D3 opPasses"), "A3", t)
##Create a table for completions, attempts, and comp pct for A3 passes
a3tab <- createPassingTable(t[t[,"A3"] == "yes",], extra="a3opPass.att.per.90")
names(a3tab) <- c("Player","A3opPass.Comp", "A3opPass.Att", "A3opPass.Att.per.90", "A3opPass.Comp.Pct")
passlocation <- merge(locationdist, a3tab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for M3 passes
m3tab <- createPassingTable(t[t[,"M3"] == "yes",], extra="m3opPass.att.per.90")
names(m3tab) <- c("Player","M3opPass.Comp", "M3opPass.Att", "M3opPass.Att.per.90", "M3opPass.Comp.Pct")
passlocation <- merge(passlocation, m3tab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for D3 passes
d3tab <- createPassingTable(t[t[,"D3"] == "yes",], extra="d3opPass.att.per.90")
names(d3tab) <- c("Player","D3opPass.Comp", "D3opPass.Att", "D3opPass.Att.per.90", "D3opPass.Comp.Pct")
passlocation <- merge(passlocation, d3tab, by="Player", all=TRUE)
##Calculate location distribution
passlocation[is.na(passlocation)] <- 0
passlocation$rFreq.A3.opPasses <- passlocation$A3opPass.Att/rowSums(passlocation[,c("A3opPass.Att", "M3opPass.Att", "D3opPass.Att")])
passlocation$rFreq.M3.opPasses <- passlocation$M3opPass.Att/rowSums(passlocation[,c("A3opPass.Att", "M3opPass.Att", "D3opPass.Att")])
passlocation$rFreq.D3.opPasses <- passlocation$D3opPass.Att/rowSums(passlocation[,c("A3opPass.Att", "M3opPass.Att", "D3opPass.Att")])
stats <- merge(stats, passlocation, by="Player", all=TRUE)
rm(t,locationdist, a3tab,m3tab, d3tab,passlocation)
stats$`A3opPass.Att.per.90` <- (stats$`A3opPass.Att`/stats$MP)*90
stats$`M3opPass.Att.per.90` <- (stats$`M3opPass.Att`/stats$MP)*90
stats$`D3opPass.Att.per.90` <- (stats$`D3opPass.Att`/stats$MP)*90

##CREATES COLUMNS WITH LOCATION-BASED OPEN PLAY PASSING STATS FOR EACH ZONE OF THE FIELD----------
#Passing stats, without dead ball scenarios (GKs, GK throws, GK drop kicks,FKs, CKs, throw ins)
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick|goal.kick", patternLocation = "play.type", ogdf = df, invert = TRUE,
                           ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                        "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", df))
t <- t[t[,"opPass"]=="yes",]
#creates column for each third (def, mid, attack)
t <- addMultiColumnsForQualifiers(patterns = c("D6"="D6", "D18"="D18", "DL"="D3L|DL", "DC"="D3C|DC","DR"="D3R|DR", 
                                               "DML"="DM3L|DML", "DMC"="DM3C|DMC", "DMR"="DM3R|DMR", "AML"="AM3L|AML",
                                               "AMC"="AM3C|AMC", "AMR"="AM3R|AMR", "AL"="A3L|AL", "AC"="A3C|AC", "AR"="A3R|AR",
                                               "A18"="A18", "A6"="A6"),
                                  ogdf = df,ndf = t,
                                  pattern_locations = c("poss.location","poss.location","poss.location","poss.location",
                                                        "poss.location","poss.location","poss.location", "poss.location",
                                                        "poss.location","poss.location","poss.location","poss.location",
                                                        "poss.location","poss.location","poss.location","poss.location"))
##Creates blank table with columns for direction distribution
zones <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML",
           "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
for(i in zones) {
  tab <- createPassingTable(t[t[,i] == "yes",])
  names(tab) <- c("Player", paste0(i,"opPass.Comp"), paste0(i,"opPass.Att"), paste0(i,"opPass.Comp.Pct"))
  if(exists("passlocation")){
    passlocation <- merge(passlocation, tab, by="Player", all=TRUE)
  } else {
    passlocation <- tab
  }
}
stats <- merge(stats, passlocation, by="Player", all=TRUE)
rm(t, tab ,passlocation, i, zones)

##CREATES CSV FILE FOR STATS TABLE
stats[is.na(stats)] <- 0
names(stats) <- gsub(" ",".", names(stats))
createcsv <- function(name) {
  write.csv(stats, file=paste0(name,"-passing-stats.csv"), row.names = FALSE)
}