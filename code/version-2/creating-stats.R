#SOURCING---------------
library(plyr)
library(dplyr)
library(RCurl)
#locationofmatchcsv is the location on your computer or URL for the csv file with the logged match actions
#locationofmatchcsv must be a string
if(!exists("d")){
  d <- getURL(matchURL)
  d <- read.csv(textConnection(d), stringsAsFactors = FALSE)
}
## About offline mode:
### Trying to do work on a plane & don't want to pay $8 for Wi-Fi? Stuck in a train tunnel?
### Assign "offline" to  online_mode and, assuming you've got the GitHub repo duplicated in
### your working directory, you can just read the files instead of going online.
### Otherwise, if online_mode hasn't been created yet, you'll just source the "functions.R"
### file from the GitHub site
if(!exists("online_mode")){
  source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/functions.R")
} else if(exists("online_mode") && online_mode == "offline"){
  source("~/wosostats/code/version-2/functions.R")
}

#MINUTES PLAYED & META DATA----------
##Adds missing columns (if they're missing)
if(!("xG" %in% colnames(d))) (d$xG <- NA)
## Gets data frame that binds data frames of every player who shows up in "poss.player" and "def.player" column
players <- rbind(data.frame(Player=unique(d$poss.player), Team=NA, GP=NA, MP=NA, GS=NA),data.frame(Player=unique(d$def.player), Team=NA, GP=NA, MP=NA, GS=NA))
players <- players[!is.na(players[,"Player"]),]
players <- unique(players[,])
matchlength <- length(unique(d$time))
substitutions <- d[grepl("substitution",d[,"poss.action"]),]
x <- 1
while (x <= nrow(players)) {
  player <- as.character(players[x,"Player"])
  #if they don't appear in the substitutions data frame, the player played the entire match
  if (nrow(substitutions[substitutions[,"poss.player"] %in% player,]) == 0) {
    players[x,"GP"] <- 1
    players[x,"MP"] <- matchlength
    players[x,"GS"] <- 1
  } else if (nrow(substitutions[substitutions[,"poss.player"] %in% player,]) > 0) {
    #check if she was a starter, based on if she was only ever substituted on
    if (grepl("substitution.on", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==FALSE) {
      #if she was a starter who was subbed off, get the length of unique values for vector d[,"time] 
      #up to where she got subbed off
      e <- substitutions[substitutions[,"poss.player"] == player,"event"]
      lastminute <- grep(e, d[,"event"])
      minutesplayed <- length(unique(d[1:lastminute,"time"]))
      players[x,"GP"] <- 1
      players[x,"MP"] <- minutesplayed
      players[x,"GS"] <- 1
    } else
      #if she wasn't a starter and got subbed on and wasn't also later subbed off
      if ((grepl("substitution.on", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==TRUE)
          & (grepl("substitution.off", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==FALSE)) {
        #if she wasn't a starter, got subbed on, and was never subbed off, get the length of unique
        #values for vector d[,"time] from when she got subbed on to when she got subbed off
        e <- substitutions[substitutions[,"poss.player"] == player,"event"]
        firstminute <- grep(e, d[,"event"])[1]
        minutesplayed <- length(unique(d[firstminute:nrow(d),"time"]))
        players[x,"GP"] <- 1
        players[x,"MP"] <- minutesplayed
        players[x,"GS"] <- 0
      } else
        #if she wasn't a starter, got subbed on, and was later subbed off
        if ((grepl("substitution.on", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==TRUE)
            & (grepl("substitution.off", paste(substitutions[substitutions[,"poss.player"] == player,"poss.action"],collapse="|"))==TRUE)) {
          #if she wasn't a starter, got subbed on, and as later subbed off, get the length of unique
          #values for vector d[,"time] from when she got subbed on to when she got subbed off
          e <- substitutions[substitutions[,"poss.player"] == player,"event"]
          firstminute <- grep(e[1], d[,"event"])
          lastminute <- grep(e[2], d[,"event"])
          minutesplayed <- length(unique(d[firstminute:lastminute,"time"]))
          players[x,"GP"] <- 1
          players[x,"MP"] <- minutesplayed
          players[x,"GS"] <- 0
        }
  }
  x <- x + 1
}

rm(firstminute, lastminute, minutesplayed)
## Set team name
x <- 1
while (x <= nrow(players)) {
  player <- as.character(players[x,"Player"])
  playerteam <- unique(d[d[,"poss.player"] == player & !is.na(d[,"poss.player"]),"poss.team"])
  if(length(playerteam) == 0) {
    playerteam <- unique(d[d[,"def.player"] == player & !is.na(d[,"def.player"]),"def.team"])
  }
  players[x,"Team"] <- playerteam
  x <- x + 1
}

#SHOTS---------------
## Creates table for players pased on types of shots.
t <- createTable(c("shots.scored", "shots", "shots.per.90", "accuracy", "shots.stopped.by.gk", "shots.stopped.by.def", "shots.missed") ,"poss.action", d)
## Add column adding all shot attempts
t$shots <- t$shots.scored + t$shots.stopped.by.gk + t$shots.stopped.by.def + t$shots.missed
## Add column for accuracy
t$accuracy <- (t$shots.scored + t$shots.stopped.by.gk + t$shots.stopped.by.def)/
  (t$shots.scored + t$shots.stopped.by.gk + t$shots.stopped.by.def + t$shots.missed)
## Change names to be more readable
names(t) <- c("Player","Goals","Shots","Shots per 90","Shot Accuracy","Shot GK Stop", "Shot Def Stop", "Shot Miss")
all <- merge(players, t, by="Player", all=TRUE)
## Calculate "per 90" stats
all$`Shots per 90` <- (all$Shots/all$MP)*90
rm(t, players)

#EXPECTED GOALS (xG)----------
t <- ddply(d[!is.na(d[,"xG"]),c("poss.player", "xG")], .(poss.player), summarise, xG=sum(xG))
names(t) <- c("Player","xG")
all <- merge(all, t, by=c("Player"), all=TRUE)
rm(t)

#SHOTS UNDER PRESSURE---------------
t <- addMultiColumnsForQualifiers(patterns = c("pressured"="pressure", "challenged"="challenge"), 
                                  pattern_locations = c("def.action", "def.action"), ogdf = d,
                                  ndf = createCleanDataFrame(c("shots", "accuracy", "shots.scored", "shots.stopped.by.gk", 
                                                         "shots.stopped.by.def", "shots.missed") ,"poss.action", d))
t <- addColumnForMultiQualifiers(c("pressured"="yes","challenged"="yes"), newcol = "pressed",
                                 df = t, exp="OR")
## Create table with a column for shots under and not under pressure
t2 <- createTable(c("yes", "pct", "total", "no"), "pressed", t)
## Add "total" and "pct" values
t2$total <- t2$yes + t2$no
t2$pct <- t2$yes/t2$total
# rename
t2 <- t2[,1:3]
names(t2) <- c("Player","Shot Pressed", "Pct Shots Pressed")
all <- merge(all, t2, by="Player", all=TRUE)
rm(t,t2)

#SHOT LOCATION---------------
t <- createCleanDataFrame(c("shots", "accuracy", "shots.scored", "shots.stopped.by.gk", "shots.stopped.by.def", "shots.missed") ,
                          "poss.action", d)
## Creates table for shot.location
t2 <- createTable(c("A6", "A18", "A3L", "A3C", "A3R", "Beyond","AM3L", "AM3C", 
                    "AM3R", "DM3L", "DM3C", "DM3R", "D3L", "D3C", "D3R", 
                    "D18", "D6"), "poss.location", t)
## Add everything beyond the attacking 3rd and put it in the "Beyond" column
t2$beyond <- rowSums(t2[,8:18])
## Get rid of all columns after the "Beyond" column to save space
t2 <- t2[,1:7]
names(t2) <- c("Player", "A6 Shots", "A18 Shots", "A3L Shots", "A3C Shots", "A3R Shots", "Far Shots")
all <- merge(all, t2, by="Player", all=TRUE)
rm(t,t2)

#ASSISTS---------------
t <- addMultiColumnsForQualifiers(c("assists"="assist", "key.passes"="key.pass|second.assist", "second.assists"="second.assist"),
                                  pattern_locations = c("poss.notes", "poss.notes", "poss.notes"),
                                  ogdf = d, ndf = createDataFrame(c("passes.f.c", "passes.f", 
                                                                         "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
# accounts for second.assists being counted as assists
t[t[,"assists"] == "yes" & t[,"second.assists"]=="yes","assists"] <- "no"
t <- addColumnForMultiQualifiers(newcol="key.assists", pattern=c("assists"="yes", "key.passes"="yes"), df = t, exp = "AND")
##Create tables for each column (five different ones)
t1 <- createTable(c("yes"),"assists",t)
names(t1) <- c("Player", "Assists")
t2 <- createTable(c("yes"),"key.passes",t)
names(t2) <- c("Player", "Key Passes")
t6 <- merge(t1, t2, by="Player", all=TRUE)
t3 <- createTable(c("yes"),"key.assists",t)
names(t3) <- c("Player", "Key Assists")
t6 <- merge(t6, t3, by="Player", all=TRUE)
t4 <- createTable(c("yes"),"second.assists",t)
names(t4) <- c("Player", "Second Assists")
t6 <- merge(t6, t4, by="Player", all=TRUE)
##Sort by "assists" and "second.assists"
t6[is.na(t6)] <- 0
t6$`Assists per 90` <- numeric(nrow(t6))
t6$`Key Passes per 90` <- numeric(nrow(t6))
all <- merge(all, t6, by="Player", all=TRUE)
rm(t, t1, t2, t3, t4, t6)
##Calculate "per 90" fields
all$`Assists per 90` <- (all$Assists/all$MP)*90
all$`Key Passes per 90` <- (all$`Key Passes`/all$MP)*90

#BIG CHANCES---------------
t <- createTable(c("big.chances", "big.chances.per.90", "big.chances.scored", "big.chances.conversion","big.chances.shot.on.goal", "big.chances.shot.missed", 
                   "big.chances.dispossessed","big.chances.created", "big.chances.lost"),"poss.notes", d)
t$big.chances <- t$big.chances.scored + t$big.chances.dispossessed + t$big.chances.shot.on.goal + t$big.chances.shot.missed + t$big.chances.lost
t$big.chances.conversion <- t$big.chances.scored/t$big.chances
## Sort by "big.chances" and "big.chances.scored"
names(t) <- c("Player","Big Chances","Big Chances per 90", "BC Scored", "BC Conversion Pct","BC SOG", "BC Shot Miss", "BC Dispossess", "BC Created", "BC Lost")
all <- merge(all, t, by="Player", all=TRUE)
rm(t)
#Calculate "per 90" fields
all$`Big Chances per 90` <- (all$`Big Chances`/all$MP)*90

#OVERALL PASSING---------------
t <- addMultiColumnsForQualifiers(patterns=c("pressured"="pressured", "challenged"="challenged", "forward.pass"="^passes.f", "sideways.pass"="^passes.s", "backward.pass"="^passes.b"),
                                  pattern_locations = c("def.action","def.action", "poss.action", "poss.action", "poss.action"),
                                  ogdf = d, ndf= createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                                        "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t <- addColumnForMultiQualifiers(c("pressured"="yes","challenged"="yes"), newcol = "pressed",
                                 df = t, exp="OR")
t2 <- createPassingTable(t, extra="pass.att.per.90")
names(t2) <- c("Player","Pass Comp", "Pass Att","Pass Att per 90", "Pass Comp Pct")
all <- merge(all, t2, by=1, all=TRUE)
rm(t,t2)
all$`Pass Att per 90` <- (all$`Pass Att`/all$MP)*90

#OPEN PLAY PASSING----------
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick", patternLocation = "play.type", ogdf = d, 
                           ndf = createDataFrame(c("passes.f.c", "passes.f", 
                                                   "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d),
                           invert = TRUE)
##create open play passing table
t <- createPassingTable(t[t[,"opPass"]=="yes",], extra="oppass.att.per.90")
names(t) <- c("Player","opPass Comp", "opPass Att","opPass Att per 90" , "opPass Comp Pct")
all <- merge(all, t, by=1, all=TRUE)
rm(t)
all$`opPass Att per 90` <- (all$`opPass Att`/all$MP)*90

#OPEN PLAY PASSING UNDER PRESSURE----------
## Add qualifiers
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick", 
                           patternLocation = "play.type", ogdf = d, invert = TRUE,
                           ndf = createDataFrame(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t <- addMultiColumnsForQualifiers(patterns = c("pressured"="pressure","challenged"="challenge"),
                                  pattern_locations = c("def.action", "def.action"), ogdf = d,
                                  ndf = t[t[,"opPass"]=="yes",])
t <- addColumnForMultiQualifiers(newcol = "pressed", df = t, exp = "OR",
                                 pattern = c("pressured"="yes","challenged"="yes"))
## Create table for open play passes under pressure
t2 <- createTable(c("Pct of Passes", "yes", "no"), "pressed", t)
t2[,"Pct.of.Passes"] <- t2$yes/(t2$yes + t2$no)
t3 <- merge(createPassingTable(t[t[,"pressed"] == "yes",], extra="opppass.att.per.90"), t2[1:2], by="Player", all=TRUE)
names(t3) <- c("Player", "opPPass Comp", "opPPass Att", "opPPass Att per 90","opPPass Comp Pct", 
               "Pct opPass Pressed")
all <- merge(all, t3, by="Player", all=TRUE)
rm(t, t2, t3)
all$`opPPass Att per 90` <- (all$`opPPass Att`/all$MP)*90

#OVERALL PASSING BY DIRECTION---------------
t <- addMultiColumnsForQualifiers(patterns = c("forward.pass"="^passes.f","sideways.pass"="^passes.s","backward.pass"="^passes.b"),
                             pattern_locations = c("poss.action","poss.action","poss.action"),ogdf = d,
                             ndf = createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
##Creates blank table with columns for direction distribution
directiondist <- createTable(c("rFreq Pass Fwd", "rFreq Pass Side", "rFreq Pass Back"), "forward.pass", t)
##Create a table for completions, attempts, and comp pct for FORWARD passes
fwdtab <- createPassingTable(t[t[,"forward.pass"] == "yes",],extra="fwpass.att.per.90")
names(fwdtab) <- c("Player","fwPass.Comp", "fwPass.Att", "fwPass.Att.per.90", "fwPass.Comp.Pct")
passdirection <- merge(directiondist, fwdtab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for SIDEWAYS passes
sidetab <- createPassingTable(t[t[,"sideways.pass"] == "yes",],extra="fwpass.att.per.90")
names(sidetab) <- c("Player","sPass.Comp", "sPass.Att","sPass.Att.per.90" , "sPass.Comp.Pct")
passdirection <- merge(passdirection, sidetab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for BACKWARDS passes
backtab <- createPassingTable(t[t[,"backward.pass"] == "yes",],extra="fwpass.att.per.90")
names(backtab) <- c("Player","bPass.Comp", "bPass.Att","bPass.Att.per.90" , "bPass.Comp.Pct")
passdirection <- merge(passdirection, backtab, by="Player", all=TRUE)
##Calculate direction distribution
passdirection[is.na(passdirection)] <- 0
passdirection$rFreq.Pass.Fwd <- passdirection$fwPass.Att/rowSums(passdirection[,c("fwPass.Att", "sPass.Att", "bPass.Att")])
passdirection$rFreq.Pass.Side <- passdirection$sPass.Att/rowSums(passdirection[,c("fwPass.Att", "sPass.Att", "bPass.Att")])
passdirection$rFreq.Pass.Back <- passdirection$bPass.Att/rowSums(passdirection[,c("fwPass.Att", "sPass.Att", "bPass.Att")])
all <- merge(all, passdirection, by="Player", all=TRUE)
rm(directiondist, fwdtab,sidetab, backtab,passdirection)
all$`fwPass.Att.per.90` <- (all$`fwPass.Att`/all$MP)*90
all$`sPass.Att.per.90` <- (all$`sPass.Att`/all$MP)*90
all$`bPass.Att.per.90` <- (all$`bPass.Att`/all$MP)*90

#OVERALL PASSING BY LOCATION---------
#creates column for each third (def, mid, attack)
t <- addMultiColumnsForQualifiers(patterns = c("A3"="A3L|AL|A3C|AC|A3R|AR|A18|A6",
                                                "M3"="AM3L|AML|AM3C|AMC|AM3R|AMR|DM3L|DML|DM3C|DMC|DM3R|DMR",
                                                "D3"="D3L|DL|D3C|DC|D3R|DR|D18|D6"),
                                  pattern_locations = c("poss.location","poss.location","poss.location"),ogdf = d,
                                  ndf = createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
##Creates blank table with columns for direction distribution
locationdist <- createTable(c("rFreq A3 Passes", "rFreq M3 Passes", "rFreq D3 Passes"), "A3", t)
##Create a table for completions, attempts, and comp pct for A3 passes
a3tab <- createPassingTable(t[t[,"A3"] == "yes",], extra="a3pass.att.per.90")
names(a3tab) <- c("Player","A3Pass.Comp", "A3Pass.Att", "A3Pass.Att.per.90", "A3Pass.Comp.Pct")
passlocation <- merge(locationdist, a3tab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for M3 passes
m3tab <- createPassingTable(t[t[,"M3"] == "yes",], extra="m3pass.att.per.90")
names(m3tab) <- c("Player","M3Pass.Comp", "M3Pass.Att", "M3Pass.Att.per.90", "M3Pass.Comp.Pct")
passlocation <- merge(passlocation, m3tab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for D3 passes
d3tab <- createPassingTable(t[t[,"D3"] == "yes",], extra="d3pass.att.per.90")
names(d3tab) <- c("Player","D3Pass.Comp", "D3Pass.Att", "D3Pass.Att.per.90", "D3Pass.Comp.Pct")
passlocation <- merge(passlocation, d3tab, by="Player", all=TRUE)
##Calculate location distribution
passlocation[is.na(passlocation)] <- 0
passlocation$rFreq.A3.Passes <- passlocation$A3Pass.Att/rowSums(passlocation[,c("A3Pass.Att", "M3Pass.Att", "D3Pass.Att")])
passlocation$rFreq.M3.Passes <- passlocation$M3Pass.Att/rowSums(passlocation[,c("A3Pass.Att", "M3Pass.Att", "D3Pass.Att")])
passlocation$rFreq.D3.Passes <- passlocation$D3Pass.Att/rowSums(passlocation[,c("A3Pass.Att", "M3Pass.Att", "D3Pass.Att")])
all <- merge(all, passlocation, by="Player", all=TRUE)
rm(locationdist, a3tab,m3tab, d3tab,passlocation)
all$`A3Pass.Att.per.90` <- (all$`A3Pass.Att`/all$MP)*90
all$`M3Pass.Att.per.90` <- (all$`M3Pass.Att`/all$MP)*90
all$`D3Pass.Att.per.90` <- (all$`D3Pass.Att`/all$MP)*90

#OPEN PLAY PASSING BY LOCATION----------
#Passing stats, without dead ball scenarios (GKs, GK throws, GK drop kicks,FKs, CKs, throw ins)
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick", patternLocation = "play.type", ogdf = d, invert = TRUE,
                           ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                        "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t <- t[t[,"opPass"]=="yes",]
#creates column for each third (def, mid, attack)
t <- addMultiColumnsForQualifiers(patterns = c("A3"="A3L|AL|A3C|AC|A3R|AR|A18|A6",
                                               "M3"="AM3L|AML|AM3C|AMC|AM3R|AMR|DM3L|DML|DM3C|DMC|DM3R|DMR",
                                               "D3"="D3L|DL|D3C|DC|D3R|DR|D18|D6"),
                                  ogdf = d,ndf = t,
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
all <- merge(all, passlocation, by="Player", all=TRUE)
rm(locationdist, a3tab,m3tab, d3tab,passlocation)
all$`A3opPass.Att.per.90` <- (all$`A3opPass.Att`/all$MP)*90
all$`M3opPass.Att.per.90` <- (all$`M3opPass.Att`/all$MP)*90
all$`D3opPass.Att.per.90` <- (all$`D3opPass.Att`/all$MP)*90

#COMPLETED PASSES BY ORIGIN & DESTINATION----------
t <- addColumnForQualifier("completed", pattern="passes.*.c", patternLocation = "poss.action", ogdf = d, 
                           ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                        "passes.s.c", "passes.s", "passes.b.c", "passes.b", "movement"), "poss.action", d))
t <- t[t[,"completed"]=="yes",]
t[,"pass.range"] <- NA
x <- 1
while (x <= nrow(t)){
  #passes from defensive 3rd to defensive 3rd
  if (grepl("D6|D18|D3",t[x,"poss.location"]) && grepl("D6|D18|D3",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.D3toD3"
  }
  #passes from defensive 3rd to middle 3rd
  if (grepl("D6|D18|D3",t[x,"poss.location"]) && grepl("M",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.D3toM3"
  }
  #passes from defensive 3rd to attacking 3rd
  if (grepl("D6|D18|D3",t[x,"poss.location"]) && grepl("A6|A18|A3",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.D3toA3"
  }
  #passes from middle 3rd to defensive 3rd
  if (grepl("M",t[x,"poss.location"]) && grepl("D6|D18|D3",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.M3toD3"
  }
  #passes from middle 3rd to middle 3rd
  if (grepl("M",t[x,"poss.location"]) && grepl("M",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.M3toM3"
  }
  #passes from middle 3rd to attacking 3rd
  if (grepl("M",t[x,"poss.location"]) && grepl("A6|A18|A3",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.M3toA3"
  }
  #passes from attacking 3rd to defensive 3rd
  if (grepl("A6|A18|A3",t[x,"poss.location"]) && grepl("D6|D18|D3",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.A3toD3"
  }
  #passes from attacking 3rd to middle 3rd
  if (grepl("A6|A18|A3",t[x,"poss.location"]) && grepl("M",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.A3toM3"
  }
  #passes from attacking 3rd to attacking 3rd
  if (grepl("A6|A18|A3",t[x,"poss.location"]) && grepl("A6|A18|A3",t[x,"poss.play.destination"])) {
    t[x,"pass.range"] <- "Pass.Comp.A3toA3"
  }
  x <- x + 1
}
t2 <- createTable(c("Pass.Comp.D3toD3", "Pass.Comp.D3toM3", "Pass.Comp.D3toA3", "Pass.Comp.M3toD3", "Pass.Comp.M3toM3", 
                    "Pass.Comp.M3toA3", "Pass.Comp.A3toD3", "Pass.Comp.A3toM3", "Pass.Comp.A3toA3"),"pass.range", t)
all <- merge(all, t2, by="Player", all=TRUE)
rm(t,t2)

#OPEN PLAY PASSING BY DIRECTION----------
#Passing stats, without dead ball scenarios (GKs, GK throws, GK drop kicks,FKs, CKs, throw ins)
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick|goal.kick", patternLocation = "play.type", ogdf = d, 
                              ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                      "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d),
                              invert = TRUE)
##add qualifiers for forward, sideways, and backward pass attemps & completions
t <- addMultiColumnsForQualifiers(pattern_locations=c("poss.action", "poss.action", "poss.action", "def.action", "def.action"),
                                  patterns=c("forward.pass"="^passes.f","sideways.pass"="^passes.s",
                                               "backward.pass"="^passes.b", "pressured"="pressured", "challenged"="challenged"),
                                  ogdf = d, ndf = t[t[,"opPass"]=="yes",])
##add qualifiers for pressed passes
t <- addColumnForMultiQualifiers(newcol = "pressed", df = t, exp = "OR",
                                 pattern = c("pressured"="yes","challenged"="yes"))
##Creates blank table with columns for direction distribution
directiondist <- createTable(c("rFreq opPass Fwd", "rFreq opPass Side", "rFreq opPass Back", "yes", "no"), "pressed", t)[,1:4]
##Create a table for completions, attempts, and comp pct for FORWARD passes
fwdtab <- createPassingTable(t[t[,"forward.pass"] == "yes",], extra="fwoppass.att.per.90")
names(fwdtab) <- c("Player", "fwopPass.Comp", "fwopPass.Att", "fwopPass.Att.per.90", "fwopPass.Comp.Pct")
passdirection <- merge(directiondist, fwdtab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for SIDEWAYS passes
sidetab <- createPassingTable(t[t[,"sideways.pass"] == "yes",], extra="soppass.att.per.90")
names(sidetab) <- c("Player", "sopPass.Comp", "sopPass.Att", "sopPass.Att.per.90", "sopPass.Comp.Pct")
passdirection <- merge(passdirection, sidetab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for BACKWARDS passes
backtab <- createPassingTable(t[t[,"backward.pass"] == "yes",], extra="boppass.att.per.90")
names(backtab) <- c("Player", "bopPass.Comp", "bopPass.Att", "bopPass.Att.per.90", "bopPass.Comp.Pct")
passdirection <- merge(passdirection, backtab, by="Player", all=TRUE)
##Calculate direction distribution
passdirection[is.na(passdirection)] <- 0
passdirection$rFreq.opPass.Fwd <- passdirection$fwopPass.Att/rowSums(passdirection[,c("fwopPass.Att", "sopPass.Att", "bopPass.Att")])
passdirection$rFreq.opPass.Side <- passdirection$sopPass.Att/rowSums(passdirection[,c("fwopPass.Att", "sopPass.Att", "bopPass.Att")])
passdirection$rFreq.opPass.Back <- passdirection$bopPass.Att/rowSums(passdirection[,c("fwopPass.Att", "sopPass.Att", "bopPass.Att")])
all <- merge(all, passdirection, by="Player", all=TRUE)
rm(backtab, sidetab, directiondist, fwdtab, passdirection)
all$`fwopPass.Att.per.90` <- (all$`fwopPass.Att`/all$MP)*90
all$`sopPass.Att.per.90` <- (all$`sopPass.Att`/all$MP)*90
all$`bopPass.Att.per.90` <- (all$`bopPass.Att`/all$MP)*90

#PASSING UNDER PRESSURE---------------
t <- addMultiColumnsForQualifiers(patterns=c("pressured"="pressure", "challenged"="challenge"),
                                  pattern_locations = c("def.action", "def.action"),
                                  ogdf = d, 
                                  ndf = createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", 
                                                               "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t <- addColumnForMultiQualifiers(newcol = "pressed", df = t, exp = "OR",
                                 pattern = c("pressured"="yes","challenged"="yes"))
t2 <- createPassingTable(t[t[,"pressed"] == "yes",], extra="ppass.att.per.90")
names(t2) <- c("Player","PPass Completed", "PPass Attempts", "PPass Att per 90", "PPass Comp Pct")
## Create a table comparing how many passes were under pressure
t3 <- createTable(c("Pct of Passes", "yes", "no"), "pressed", t)
t3[,"Pct.of.Passes"] <- t3$yes/(t3$yes + t3$no)
## Exclude rows with 0% passes under pressure
t3 <- t3[t3[,"Pct.of.Passes"] != 0,]
t4 <- merge(t2,t3, by="Player", all=TRUE)
t4 <- t4[,c("Player", "Pct.of.Passes", "PPass Completed", "PPass Attempts", "PPass Att per 90", "PPass Comp Pct")]
names(t4) <- c("Player", "Pct Passes Pressed", "PPass Comp", "PPass Att", "PPass Att per 90", "PPass Comp Pct")
all <- merge(all, t4, by="Player", all=TRUE)
rm(t, t2, t3, t4)
all$`PPass Att per 90` <- (all$`PPass Att`/all$MP)*90

#PASSING UNDER PRESSURE BY DIRECTION---------------
t <- createCleanDataFrame(c("passes.f.c", "passes.f", 
                            "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d)
##add qualifiers
t <- addMultiColumnsForQualifiers(patterns = c("pressured"="pressure", "challenged"= "challenge", "forward.pass"="^passes.f",
                                               "sideways.pass"="^passes.s","backward.pass"="^passes.b"), 
                                  pattern_locations = c("def.action","def.action","poss.action","poss.action","poss.action"),
                                  ogdf = d, 
                                  ndf = createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", 
                                                               "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t <- addColumnForMultiQualifiers(newcol = "pressed", df = t, exp = "OR",
                                 pattern = c("pressured"="yes","challenged"="yes"))
##Creates blank table with columns for direction distribution
directiondist <- createTable(c("rFreq PPass Fwd", "rFreq PPass Side", "rFreq PPass Back", "yes"), "pressed", t)[,1:4]
##Create a table for completions, attempts, and comp pct for FORWARD passes
fwdtab <- createPassingTable(t[t[,"forward.pass"] == "yes" & t[,"pressed"] == "yes",], extra="fwppass.att.per.90")
names(fwdtab) <- c("Player","fwPPass.Comp", "fwPPass.Att", "fwPPass.Att.per.90", "fwPPass.Comp.Pct")
passdirection <- merge(directiondist, fwdtab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for SIDEWAYS passes
sidetab <- createPassingTable(t[t[,"sideways.pass"] == "yes" & t[,"pressed"] == "yes",], extra="sppass.att.per.90")
names(sidetab) <- c("Player", "sPPass.Comp", "sPPass.Att", "sPPass.Att.per.90", "sPPass.Comp.Pct")
passdirection <- merge(passdirection, sidetab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for BACKWARDS passes
backtab <- createPassingTable(t[t[,"backward.pass"] == "yes" & t[,"pressed"] == "yes",], extra="bppass.att.per.90")
names(backtab) <- c("Player", "bPPass.Comp", "bPPass.Att", "bPPass.Att.per.90", "bPPass.Comp.Pct")
passdirection <- merge(passdirection, backtab, by="Player", all=TRUE)
##Calculate direction distribution
passdirection[is.na(passdirection)] <- 0
passdirection$rFreq.PPass.Fwd <- passdirection$fwPPass.Att/rowSums(passdirection[,c("fwPPass.Att", "sPPass.Att", "bPPass.Att")])
passdirection$rFreq.PPass.Side <- passdirection$sPPass.Att/rowSums(passdirection[,c("fwPPass.Att", "sPPass.Att", "bPPass.Att")])
passdirection$rFreq.PPass.Back <- passdirection$bPPass.Att/rowSums(passdirection[,c("fwPPass.Att", "sPPass.Att", "bPPass.Att")])
all <- merge(all, passdirection, by="Player", all=TRUE)
rm(t, passdirection, backtab, sidetab, fwdtab, directiondist)
all$`fwPPass.Att.per.90` <- (all$`fwPPass.Att`/all$MP)*90
all$`sPPass.Att.per.90` <- (all$`sPPass.Att`/all$MP)*90
all$`bPPass.Att.per.90` <- (all$`bPPass.Att`/all$MP)*90

#CROSSES---------------
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick|goal.kick", patternLocation = "play.type", ogdf = d, invert = TRUE,
                           ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                        "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t <- t[t[,"opPass"]=="yes",]
t <- createDataFrame(c("corner.crosses", "deep.crosses"), "play.type", createDataFrame(c("passes.f.c", "passes.f", 
                                                                                         "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", t))
## Fills in blanks with info from cell above it
t <- fillBlanks(t)
## Then, exclude anything that marks a stoppage in time
t <- t[t[,"poss.action"] != "playcutoffbybroadcast",]
t <- t[grep("corner.crosses|deep.crosses", t[,"play.type"]),]
## Create table with columns for completed, blocked, and missed crosses
t2 <- createPassingTable(t, extra = c("cross.att.per.90","cross.att.per.oppass"))
## Create table with columns for corner and deep crosses
t3 <- createTable(c("corner.crosses", "deep.crosses"), "play.type", t)
# Merge the two sets of columns
t4 <- merge(t2, t3, by="Player", all=TRUE)
names(t4) <- c("Player","Cross Comp", "Cross Att", "Cross Att per 90", "Cross Att per opPass","Cross Comp Pct", "Corner Crosses", "Deep Crosses")
all <- merge(all, t4, by=1, all=TRUE)
rm(t, t2, t3, t4)
#Calculate "per 90" & "per pass" fields
all$`Cross Att per 90` <- (all$`Cross Att`/all$MP)*90
all$`Cross Att per opPass` <- (all$`Cross Att`/all$`opPass Att`)

#LAUNCH BALLS---------------
t <- addColumnForQualifier(newcol="launch", pattern = "launch|gk.drop.kick", patternLocation = "play.type",
                           ogdf = d, ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                             "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
##Keep only rows with "yes" in "launch" column
t <- t[t[,"launch"]=="yes",]
t2 <- createPassingTable(t, extra = c("launch.att.per.90", "launch.att.per.pass"))
names(t2) <- c("Player","Launch Comp", "Launch Att", "Launch Att per 90", "Launch Att per Pass", "Launch Comp Pct")
all <- merge(all, t2, by=1, all=TRUE)
rm(t, t2)
#Calculate "per 90" columns
all$`Launch Att per 90` <- (all$`Launch Att`/all$MP)*90
all$`Launch Att per Pass` <- (all$`Launch Att`/all$`Pass Att`)

#THROUGH BALLS---------------
t <- createCleanDataFrame(c("through"), "play.type", createDataFrame(c("passes.f.c", "passes.f", 
                                                                       "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
## Create table
t2 <- createPassingTable(t, extra= c("through.att.per.90", "through.att.per.pass", "through.att.per.oppass"))
names(t2) <- c("Player","Through Comp", "Through Att", "Through Att per 90", "Through Att per Pass", "Through Att per opPass", "Through Comp Pct")
all <- merge(all, t2, by=1, all=TRUE)
rm(t2, t)
#Calculate "per 90" columns
all$`Through Att per 90` <- (all$`Through Att`/all$MP)*90
all$`Through Att per Pass` <- (all$`Through Att`/all$`Pass Att`)
all$`Through Att per opPass` <- (all$`Through Att`/all$`opPass Att`)

#THROW INS---------------
##Create blank "throwin" column, to be filled with "yes" or "no"
t <- addColumnForQualifier(newcol = "throwin", pattern = c("throw.in"), patternLocation = "play.type",
                           ogdf = d, ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                             "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t2 <- t[t[,"throwin"]=="yes",]
## Create table
t2 <- createPassingTable(t2, extra= c("throw.in.att.per.90", "throw.in.att.per.pass"))
names(t2) <- c("Player", "Throw In Comp", "Throw In Att", "Throw In Att per 90", "Throw In Att per Pass", "Throw In Comp Pct")
all <- merge(all, t2, by=1, all=TRUE)
rm(t2)
#Calculate "per 90" columns
all$`Throw In Att per 90` <- (all$`Throw In Att`/all$MP)*90
all$`Throw In Att per Pass` <- (all$`Throw In Att`/all$`Pass Att`)

#CORNER KICKS----------
t <- addMultiColumnsForQualifiers(ogdf=d, ndf=createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                                   "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d),
                                     patterns = c("cornerkick"="corner.kick", "assist"="^assist", "keypass"="key.pass|^second.assist"), 
                                     pattern_locations = c("play.type", "poss.notes", "poss.notes"))
t <- t[t[,"cornerkick"]=="yes",]
t2 <- createPassingTable(t)
names(t2) <- c("Player","Corner Kicks Completed", "Corner Kicks Taken", "CK Comp Pct")
## Create table for assists and key passes
t3 <- createTable(c("yes"), "assist", t)
names(t3)[2] <- "CK Assist"
t4 <- createTable(c("yes"), "keypass", t)
names(t4)[2] <- "CK Key Pass"
## Merge tables
t5 <- merge(t2, t3, by="Player", all=TRUE)
t5 <- merge(t5, t4, by="Player", all=TRUE)
all <- merge(all, t5, by="Player", all=TRUE)
rm(t, t2, t3, t4, t5)

#FREE KICKS----------
t <- addMultiColumnsForQualifiers(ogdf=d, ndf=createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", 
                                                                     "passes.b.c", "passes.b", "shots.stopped.by.gk", 
                                                                     "shots.stopped.by.def", "shots.blocked", "shots.missed", 
                                                                     "shots.scored"), "poss.action", d),
                                  patterns = c("fk"="free.kick", "assist"="^assist", "keypass"="key.pass|^second.assist", "shot"="shot", "scored"="scored"), 
                                  pattern_locations = c("play.type", "poss.notes", "poss.notes", "poss.action", "poss.action"))
t <- t[t[,"fk"]=="yes",]
t2 <- createPassingTable(t)
names(t2) <- c("Player", "FK Pass Comp", "FK Pass Att", "FK Pass Comp Pct")
## Create table for assists, key passes, shots, and goals
t3 <- createTable(c("yes"), "assist", t)
names(t3)[2] <- "FK Assist"
t4 <- createTable(c("yes"), "keypass", t)
names(t4)[2] <- "FK Key Pass"
t5 <- createTable(c("yes"), "shot", t)
names(t5)[2] <- "FK Shot"
t6 <- createTable(c("yes"), "scored", t)
names(t6)[2] <- "FK Scored"
## Merge tables
t7 <- merge(t2, t3, by="Player", all=TRUE)
t7 <- merge(t7, t4, by="Player", all=TRUE)
t7 <- merge(t7, t5, by="Player", all=TRUE)
t7 <- merge(t7, t6, by="Player", all=TRUE)
t7$Free.Kicks.Taken <- rowSums(t7[,c("FK Pass Att", "FK Shot")], na.rm=TRUE)
names(t7)[9] <- "Free Kicks Taken"
all <- merge(all, t7, by="Player", all=TRUE)
rm(t, t2, t3, t4, t5, t6, t7)

#TAKE ONS---------------
t <- createTable(c("Take Ons", "take.ons.per.90", "take.on.won","take.on.lost", "Take On Win Pct",  "dispossessed", "lost.touch", "All Possessions Disrupted", "Poss Disrupted per 90"), "poss.action", 
                 createCleanDataFrame(c("take.on.won", "take.on.lost", "dispossessed", "lost.touch"),"poss.action", d))
## Fill in blank columns & rename
t[,"Take.Ons"] <- t[,"take.on.won"] + t[,"take.on.lost"]
t[,"Take.On.Win.Pct"] <- t[,"take.on.won"]/t[,"Take.Ons"]
t[,"All.Possessions.Disrupted"] <- t[,"take.on.lost"] + t[,"dispossessed"] + t[,"lost.touch"]
names(t) <- c("Player", "Take Ons","Take Ons per 90" , "TO Won", "TO Lost", "TO Win Pct", "Dispossessed", "Lost Touches", "All Possessions Disrupted", "Poss Disrupted per 90")
all <- merge(all, t, by=1, all=TRUE)
rm(t)
all$`Take Ons per 90` <- (all$`Take Ons`/all$MP)*90
all$`Poss Disrupted per 90` <- (all$`All Possessions Disrupted`/all$MP)*90

#AERIAL DUELS---------------
t <- createDataFrame(c("aerial.won", "aerial.lost"), "poss.action", d)
t2 <- t[,c("event", "time", "poss.position", "poss.team", "poss.player", "poss.action", "poss.location")]
names(t2) <- c("event", "time", "position", "team", "poss.player", "player.event", "location")
t3 <- t[,c("event", "time","def.position", "def.team", "def.player", "def.action", "def.location")]
names(t3) <- c("event", "time", "position", "team", "poss.player", "player.event", "location")
t4 <- rbind(t2,t3)
t5 <- createTable(c("Aerial Duels", "aerial.duels.per.90", "aerial.won", "aerial.lost", "Success Pct"), "player.event", t4)
## Fill in blank columns, sort, & rename
t5[,"Aerial.Duels"] <- t5[,"aerial.won"] + t5[,"aerial.lost"]
t5[,"Success.Pct"] <- t5[,"aerial.won"]/t5[,"Aerial.Duels"]
names(t5) <- c("Player","Aerial Duels", "Aerial Duels per 90", "AD Won", "AD Lost", "AD Win Pct")
all <- merge(all, t5, by=1, all=TRUE)
rm(t, t2,t3,t4,t5)
all$`Aerial Duels per 90` <- (all$`Aerial Duels`/all$MP)*90

#TACKLES & PRESSURE---------------
t <- createDataFrame(c("dispossessed", "tackles.ball.away", "tackles.ball.won", "tackles.ball","dribbled.tackles.missed", 
                       "dribbled.out.run","dribbled.turned", "pressured", "challenged"), "def.action", d)
t <- t[,c("event","time","def.position","def.team","def.player","def.action","def.location","def.player.disciplinary","def.notes")]
names(t) <- c("event", "time", "position" ,"team", "poss.player", "player.event", "location", 
              "def.player.disciplinary", "def.notes")
t2 <- createTable(c("tackles","dispossessed", "all.opp.poss.disrupted", "opp.poss.disrupted.per.90", "dribbled","dribbled.per.90", "pressured", "challenged", 
                    "tackles.ball.away", "tackles.ball.won", "tackles.ball",
                    "dribbled.tackles.missed", "dribbled.out.run","dribbled.turned"), "player.event", t)
## Fill in blank columns, get rid of excess columns, and rename
t2$tackles <- t2$tackles.ball.away + t2$tackles.ball.won + t2$tackles.ball
t2$dribbled <- t2$dribbled.tackles.missed + t2$dribbled.out.run + t2$dribbled.turned
t2$all.opp.poss.disrupted <- t2$tackles + t2$dispossessed
t2 <- t2[,1:9]
names(t2) <- c("Player","Tackles", "Dispossesses", "All Opp Poss Disrupted", "Opp Poss Disrupted per 90","Dribbled", "Dribbled per 90", "Press Opp", "Challenge Opp")
all <- merge(all, t2, by=1, all=TRUE)
rm(t,t2)
all$`Opp Poss Disrupted per 90` <- (all$`All Opp Poss Disrupted`/all$MP)*90
all$`Dribbled per 90` <- (all$`Dribbled`/all$MP)*90

#RECOVERIES---------------
t <- createDataFrame(c("recoveries"), "poss.action", d)
t <- t[grep("recoveries",t[,"poss.action"]),]
##Create columns that will mark if an event is a defensive or possessing recovery
t$def <- NA
t$poss <- NA
x <- 1
while (x <= length(unique(t$event))) {
  if (is.na((d[d[,"event"] == (unique(t$event)[x] - 1),"poss.team"] == t[t[,"event"] == unique(t$event)[x],"poss.team"])[1])){
    t[t[,"event"] == unique(t$event)[x],"def"] <- NA
    t[t[,"event"] == unique(t$event)[x],"poss"] <- NA
  }
  else if ((d[d[,"event"] == (unique(t$event)[x] - 1),"poss.team"] == t[t[,"event"] == unique(t$event)[x],"poss.team"])[1]) {
    t[t[,"event"] == unique(t$event)[x],"def"] <- "no"
    t[t[,"event"] == unique(t$event)[x],"poss"] <- "yes"
  } else if ((d[d[,"event"] == (unique(t$event)[x] - 1),"poss.team"] != t[t[,"event"] == unique(t$event)[x],"poss.team"])[1]) {
    t[t[,"event"] == unique(t$event)[x],"def"] <- "yes"
    t[t[,"event"] == unique(t$event)[x],"poss"] <- "no"
  }
  x <- x + 1
}
t <- t[t[,"poss.action"] != "playcutoffbybroadcast",]
##Create table for overall recoveries
t2 <- data.frame(unclass(table(t$poss.player, t$poss.action)))
t2 <- cbind(Player=rownames(t2), t2)
t2$recoveries.per.90 <- NA
names(t2) <- c("Player", "Recoveries", "Recoveries per 90")
##Create table for defensive recoveries
t3 <- t[t[,"def"] == "yes",]
t3 <- data.frame(unclass(table(t3$poss.player, t3$poss.action)))
t3 <- cbind(Player=rownames(t3), t3)
names(t3) <- c("Player", "Def Recoveries")
t5 <- merge(t2, t3, by="Player", all=TRUE)
##Create table for possessing recoveries
t4 <- t[t[,"def"] == "no",]
t4 <- data.frame(unclass(table(t4$poss.player, t4$poss.action)))
t4 <- cbind(Player=rownames(t4), t4)
names(t4) <- c("Player", "Poss Recoveries")
t5 <- merge(t5, t4, by="Player", all=TRUE)
all <- merge(all, t5, by=1, all=TRUE)
rm(t2,t3,t4,t5)
all$`Recoveries per 90` <- (all$Recoveries/all$MP)*90

#INTERCEPTIONS, BLOCKS, CLEARANCES, BALL SHIELDS----------
t <- createDataFrame(c("interceptions","clearances", "ball.shield", "blocks"), "def.action", d)
t <- t[,c("event","time", "def.position","def.team","def.player","def.action","def.location", "def.player.disciplinary","def.notes")]
names(t) <- c("event", "time", "position","team", "poss.player", "player.event", "location", 
              "def.player.disciplinary", "def.notes")
t2 <- createTable(c("interceptions", "interceptions.per.90","interceptions.per.op.pass","blocks", "blocks.per.90", "clearances", "clearances.per.90", "ball.shield"), "player.event", t)
names(t2) <- c("Player","Interceptions","Int per 90","Int per OP Pass", "Blocks","Blocks per 90", "Clearances", "Clearances per 90", "Ball Shields")
all <- merge(all, t2, by=1, all=TRUE)
rm(t2)
all$`Int per 90` <- (all$Interceptions/all$MP)*90
all$`Blocks per 90` <- (all$Blocks/all$MP)*90
all$`Clearances per 90` <- (all$Clearances/all$MP)*90

#DISCIPLINARY---------
#For disciplinary actions that show up the def.notes column
t <- d[,c("event","time","def.position", "def.team", "def.player", "def.action",
          "def.location", "gk.ball.stop", "gk.s.o.g.attempt", "poss.player.disciplinary", 
          "poss.notes", "def.player.disciplinary", "def.notes" )]
t <- addMultiColumnsForQualifiers(patterns=c("offside.calls"="offside.calls","fouls.won"="fouls.won", "fouls.conceded"="fouls.conceded","yellow.cards"="yellow.cards", 
                                             "red.cards"="red.cards", "penalties.won"="penalties.won","penalties.conceded"="penalties.conceded"),
                                  pattern_locations=c("def.action","def.player.disciplinary","def.player.disciplinary","def.player.disciplinary",
                                                      "def.player.disciplinary","def.player.disciplinary","def.player.disciplinary"),
                                  ogdf=d, ndf=t)
names(t) <- c("event","time","position", "team", "poss.player", "def.action", 
              "def.location", "gk.ball.stop", "gk.s.o.g.attempt", "poss.player.disciplinary", 
              "poss.notes", "def.player.disciplinary", "def.notes", "offside.calls", "fouls.won",
              "fouls.conceded",  "yellow.cards", "red.cards", "penalties.won", "penalties.conceded")
t2 <- createTable(c("yes"),"offside.calls",t)
names(t2) <- c("Player", "dOffsides")
t3 <- createTable(c("yes"),"fouls.won",t)
names(t3) <- c("Player", "dFouls Won")
t4 <- createTable(c("yes"),"fouls.conceded",t)
names(t4) <- c("Player", "dFouls Conceded")
t5 <- createTable(c("yes"),"yellow.cards",t)
names(t5) <- c("Player", "dYellow Cards")
t6 <- createTable(c("yes"),"red.cards",t)
names(t6) <- c("Player", "dRed Cards")
t7 <- createTable(c("yes"),"penalties.won",t)
names(t7) <- c("Player", "dPenalties Won")
t8 <- createTable(c("yes"),"penalties.conceded",t)
names(t8) <- c("Player", "dPenalties Conceded")
def <- merge(t2, t3, by="Player", all=TRUE)
def <- merge(def, t4, by="Player", all=TRUE)
def <- merge(def, t5, by="Player", all=TRUE)
def <- merge(def, t6, by="Player", all=TRUE)
def <- merge(def, t7, by="Player", all=TRUE)
def <- merge(def, t8, by="Player", all=TRUE)
rm(t,t2,t3,t4,t5,t6,t7,t8)
#For disciplinary actions that show up the poss.notes column
t <- addMultiColumnsForQualifiers(patterns=c("offside.calls"="offside.calls","fouls.won"="fouls.won", "fouls.conceded"="fouls.conceded","yellow.cards"="yellow.cards", 
                                             "red.cards"="red.cards", "penalties.won"="penalties.won","penalties.conceded"="penalties.conceded"),
                                  pattern_locations=c("poss.action","poss.player.disciplinary","poss.player.disciplinary","poss.player.disciplinary",
                                                      "poss.player.disciplinary","poss.player.disciplinary","poss.player.disciplinary"),
                                  ogdf=d, ndf=d)
t2 <- createTable(c("yes"),"offside.calls",t)
names(t2) <- c("Player", "pOffsides")
t3 <- createTable(c("yes"),"fouls.won",t)
names(t3) <- c("Player", "pFouls Won")
t4 <- createTable(c("yes"),"fouls.conceded",t)
names(t4) <- c("Player", "pFouls Conceded")
t5 <- createTable(c("yes"),"yellow.cards",t)
names(t5) <- c("Player", "pYellow Cards")
t6 <- createTable(c("yes"),"red.cards",t)
names(t6) <- c("Player", "pRed Cards")
t7 <- createTable(c("yes"),"penalties.won",t)
names(t7) <- c("Player", "pPenalties Won")
t8 <- createTable(c("yes"),"penalties.conceded",t)
names(t8) <- c("Player", "pPenalties Conceded")
poss <- merge(t2, t3, by="Player", all=TRUE)
poss <- merge(poss, t4, by="Player", all=TRUE)
poss <- merge(poss, t5, by="Player", all=TRUE)
poss <- merge(poss, t6, by="Player", all=TRUE)
poss <- merge(poss, t7, by="Player", all=TRUE)
poss <- merge(poss, t8, by="Player", all=TRUE)
rm(t,t2,t3,t4,t5,t6,t7,t8)
#Sum common columns
comb <- merge(poss,def, by="Player", all=TRUE)
comb[,"Offsides"] <- rowSums(comb[,c("pOffsides","dOffsides")], na.rm = TRUE)
comb[,"Fouls Won"] <- rowSums(comb[,c("pFouls Won","dFouls Won")], na.rm = TRUE)
comb[,"Fouls Conceded"] <- rowSums(comb[,c("pFouls Conceded","dFouls Conceded")], na.rm = TRUE)
comb[,"Yellow Cards"] <- rowSums(comb[,c("pYellow Cards","dYellow Cards")], na.rm = TRUE)
comb[,"Red Cards"] <- rowSums(comb[,c("pRed Cards","dRed Cards")], na.rm = TRUE)
comb[,"Penalties Won"] <- rowSums(comb[,c("pPenalties Won","dPenalties Won")], na.rm = TRUE)
comb[,"Penalties Conceded"] <- rowSums(comb[,c("pPenalties Conceded","dPenalties Conceded")], na.rm = TRUE)
comb <- comb[,c("Player", "Offsides", "Fouls Won", "Fouls Conceded",  "Yellow Cards", 
                "Red Cards", "Penalties Won", "Penalties Conceded")]
all <- merge(all, comb, by="Player", all=TRUE)
rm(comb,def,poss)

#ERRORS & BIG CHANCE STOPS----------
#For errors and big stops that show up the def.notes column
t <- d[,c("event","time","def.position", "def.team", "def.player", "def.action",
                                      "def.location", "gk.ball.stop", "gk.s.o.g.attempt", "poss.player.disciplinary", 
                                      "poss.notes", "def.player.disciplinary", "def.notes" )]
t <- addMultiColumnsForQualifiers(patterns=c("e.to.goals"="errors.to.goals","og"="own.goals", "e.to.big.chances"="errors.to.big.chances","bc.stopped"="big.chances.stopped"),
                                  pattern_locations=c("def.notes","def.notes","def.notes","def.notes"),
                                  ogdf=d, ndf=t)
names(t) <- c("event","time","position", "team", "poss.player", "def.action", 
              "def.location", "gk.ball.stop", "gk.s.o.g.attempt", 
              "poss.player.disciplinary", "poss.notes",
              "def.player.disciplinary", "def.notes", "errors.to.goals","own.goals", "errors.to.big.chances",  "big.chances.stopped")
t2 <- createTable(c("yes"),"errors.to.goals",t)
names(t2) <- c("Player", "E to Goals")
t3 <- createTable(c("yes"),"own.goals",t)
names(t3) <- c("Player", "OG")
t4 <- createTable(c("yes"),"errors.to.big.chances",t)
names(t4) <- c("Player", "E to Big Chances")
t5 <- createTable(c("yes"),"big.chances.stopped",t)
names(t5) <- c("Player", "BC Stopped")
def <- merge(t2, t3, by="Player", all=TRUE)
def <- merge(def, t4, by="Player", all=TRUE)
def <- merge(def, t5, by="Player", all=TRUE)
rm(t,t2,t3,t4,t5)
#For errors and big stops that show up the poss.notes column
t <- addMultiColumnsForQualifiers(patterns=c("errors.to.goals"="errors.to.goals","og"="own.goals", "e.to.big.chances"="errors.to.big.chances","bc.stopped"="big.chances.stopped"),
                                  pattern_locations=c("poss.notes","poss.notes","poss.notes","poss.notes"),
                                  ogdf=d, ndf=d)
t2 <- createTable(c("yes"),"errors.to.goals",t)
names(t2) <- c("Player", "E to Goals")
t3 <- createTable(c("yes"),"og",t)
names(t3) <- c("Player", "OG")
t4 <- createTable(c("yes"),"e.to.big.chances",t)
names(t4) <- c("Player", "E to Big Chances")
t5 <- createTable(c("yes"),"bc.stopped",t)
names(t5) <- c("Player", "BC Stopped")
poss <- merge(t2, t3, by="Player", all=TRUE)
poss <- merge(poss, t4, by="Player", all=TRUE)
poss <- merge(poss, t5, by="Player", all=TRUE)
comb <- rbind(def,poss)
all <- merge(all, comb, by="Player", all=TRUE)
rm(def,poss,comb,t,t2,t3,t4,t5)

#GK SHOTS ON GOAL FACED----------
t <- createDataFrame(c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"), "def.action", d)
##transforms data frame to move defensive actions into the "possessing action" columns
t <- t[grep("gk", t[,"def.action"]),c("event","time","def.position", "def.team", "def.player", "def.action",
                                      "def.location", "gk.ball.stop", "gk.s.o.g.attempt", "poss.player.disciplinary", 
                                      "poss.notes", "def.player.disciplinary", "def.notes" )]
t <- addColumnForQualifier("saves", "gk.s.o.g.stop","def.action", d, t)
t <- addColumnForQualifier("goals.allowed", "gk.s.o.g.scored", "def.action", d, t)
names(t) <- c("event","time","position", "team", "poss.player", "def.action", 
              "def.location", "gk.ball.stop", "gk.s.o.g.attempt", 
              "poss.player.disciplinary", "poss.notes",
              "def.player.disciplinary", "def.notes", "saves", "goals.allowed")
## Create table for saves, fill in blank columns, cut down excess columns
t2 <- createTable(c("saves", "yes", "no"), "saves", t)
t2$saves <- t2$yes
t2 <- t2[,c("Player","saves", "no")]
t2 <- t2[,1:2]
## Create table for goals conceded, fill in blank columns, cut down excess columns
t3 <- createTable(c("goals.allowed", "yes", "no"), "goals.allowed", t)
t3$goals.allowed <- t3$yes
t3 <- t3[,c("Player","goals.allowed", "no")]
t3 <- t3[,1:2]
## Create table for shots on goal faced
t4 <- createTable(c("sog.faced","gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"), "def.action", t)
t4$sog.faced <- rowSums(t4[,3:5])
t4 <- t4[,1:2]
## Merge tables
t5 <- merge(t2, t3, by="Player", all=TRUE)
t5 <- merge(t5, t4, by="Player", all=TRUE)
## Create column for Goals per SOG
t5$GperSOG <- t5$goals.allowed/t5$sog.faced
names(t5) <- c("Player","Saves", "Goals Allowed", "GK SOG Faced", "GperSOG")
rm(t2,t3,t4)
all <- merge(all, t5, by=1, all=TRUE)
rm(t5)

#GK BIG CHANCES SOG FACED----------
t <- addMultiColumnsForQualifiers(patterns=c("BC.SOG.Faced"="big.chances.scored|big.chances.shot.on.goal","BC.Goals.Allowed"="big.chances.scored","GK.Stop"="gk.s.o.g.stop"),
                                     pattern_locations = c("poss.notes", "poss.notes","def.action"),
                                     ogdf = d, ndf = createDataFrame(c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"), "def.action", d))
t <- addColumnForMultiQualifiers(newcol = "BC.Saves", df = t, exp = "AND", 
                                    pattern = c("BC.SOG.Faced"="yes", "GK.Stop"="yes"))
#Shift def columns to be poss columns, to be readable by createTable function
t <- t[grep("gk", t[,"def.action"]),c("event","time","def.position", "def.team", "def.player", "def.action",
                                      "def.location", "gk.ball.stop", "gk.s.o.g.attempt", "poss.player.disciplinary", 
                                      "poss.notes", "def.player.disciplinary", "def.notes", "BC.SOG.Faced", "BC.Saves", "BC.Goals.Allowed")]
names(t) <- c("event","time","position", "team", "poss.player", "def.action", 
              "def.location", "gk.ball.stop", "gk.s.o.g.attempt", 
              "poss.player.disciplinary", "poss.notes",
              "def.player.disciplinary", "def.notes", "BC.SOG.Faced", "BC.Saves", "BC.Goals.Allowed")
#Create tables
t2 <- merge(createTable(c("yes", "no"), "BC.SOG.Faced", t)[,c("Player", "yes")], createTable(c("yes", "no"), "BC.Saves", t)[,c("Player", "yes")], by="Player", all=TRUE)
t2 <- merge(t2, createTable(c("yes", "no"), "BC.Goals.Allowed", t)[,c("Player", "yes")], by="Player", all=TRUE)
t2$GperBCSOG <- NA
names(t2) <- c("Player", "BC SOG Faced", "BC Saves", "BC Goals Allowed", "GperBCSOG")
t2$GperBCSOG <- t2[,"BC Goals Allowed"]/t2[,"BC SOG Faced"]
all <- merge(all, t2, by="Player", all=TRUE)
rm(t, t2)

#GK HIGH BALLS FACED----------
t <- createDataFrame("GK", "def.position", d)
#t <- createDataFrame(c("gk.high.balls.won","gk.high.balls.lost"), "def.action", d)
t <- addMultiColumnsForQualifiers(patterns = c("hbwon"="gk.high.balls.won", "hblost"="gk.high.balls.lost", "cross"="cross",
                                               "corner.kick"="corner.kick","free.kick"="free.kick", "caught"="caught",
                                               "punched.away"="punched.to.safety", "parried.away"="parried.to.safety",
                                               "collected"="collected","foul.won"="fouls.won"),
                                  pattern_locations = c("def.action", "def.action", "play.type","play.type",
                                                        "play.type",  "gk.ball.stop", "gk.ball.stop", "gk.ball.stop",
                                                        "gk.ball.stop", "gk.ball.stop"),
                                  ogdf = t, ndf = t)

## Only goalkeepers
t <- t[grep("[Gg][Kk]", t[,"def.position"]),]
t$poss.player <- t$def.player
## Create stats table for basic high balls stats
t2 <- merge(cbind("Player"=character(0), "High Balls"=numeric(0)), createTable(c("gk.high.balls.won", "gk.high.balls.lost", "gk.high.ball.win.pct"), "def.action", t), by="Player", all=TRUE)
names(t2) <- c("Player", "High Balls Faced", "HB Won", "HB Lost", "HB Win Pct")
t2$`High Balls Faced` <- t2$`HB Won` + t2$`HB Lost`
t2$`HB Win Pct` <- t2$`HB Won`/t2$`High Balls Faced`
## Create more detailed stats for high balls
t2 <- merge(t2, createTable(c("yes", "no"), "caught", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[6] <- "HB Caught"
t2 <- merge(t2, createTable(c("yes", "no"), "punched.away", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[7] <- "HB Punched"
t2 <- merge(t2, createTable(c("yes", "no"), "parried.away", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[8] <- "HB Parried"
t2 <- merge(t2, createTable(c("yes", "no"), "collected", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[9] <- "HB Collected"
t2 <- merge(t2, createTable(c("yes", "no"), "foul.won", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[10] <- "HB Fouls Won"
t2 <- merge(t2, createTable(c("yes", "no"), "cross", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[11] <- "Crosses Faced"
t2 <- merge(t2, createTable(c("yes", "no"), "hbwon", t[t["cross"]=="yes",]), by="Player", all=TRUE)
names(t2)[12:13] <- c("Crosses Won", "Crosses Lost")
t2 <- merge(t2, createTable(c("yes", "no"), "corner.kick", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[14] <- "Corner Kicks Faced"
t2 <- merge(t2, createTable(c("yes", "no"), "hbwon", t[t["corner.kick"]=="yes",]), by="Player", all=TRUE)
names(t2)[15:16] <- c("CKs Won", "CKs Lost")
t2 <- merge(t2, createTable(c("yes", "no"), "free.kick", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[17] <- "Free Kicks Faced"
t2 <- merge(t2, createTable(c("yes", "no"), "hbwon", t[t["free.kick"]=="yes",]), by="Player", all=TRUE)
names(t2)[18:19] <- c("FKs Won", "FKs Lost")
all <- merge(all, t2, by=1, all=TRUE)
rm(t, t2)

#GK SMOTHERS----------
t <- createDataFrame(c("gk.smothers.won", "gk.smothers.lost"), "def.action", d)
t <- t[grep("[Gg][Kk]", t[,"def.position"]),]
t$poss.player <- t$def.player
t <- createTable(c("gk.smothers.won", "gk.smothers.lost"), "def.action", t)
names(t) <- c("Player","Smothers Won", "Smothers Lost")
smothers <- t
rm(t)
all <- merge(all, smothers, by=1, all=TRUE)
rm(smothers)

#GK DISTRIBUTION----------
#Create clean data frame with only goalkeeper passing events
t <- addMultiColumnsForQualifiers(patterns=c("gkthrow"="gk.throw", "gkdropkick"="gk.drop.kick","gkfk"="goal.kick|free.kick"),
                                     pattern_locations = c("play.type","play.type","play.type"),
                                     ogdf = d, ndf = createDataFrame(c("passes.f.c", "passes.f", 
                                                                       "passes.s.c", "passes.s", "passes.b.c", "passes.b"), 
                                                                     "poss.action", 
                                                                     d[grep("[Gg][Kk]", d[,"poss.position"]),]))
##create overall GK passing table
t2 <- createPassingTable(t)
names(t2) <- c("Player","GK Overall Pass Comp", "GK Overall Pass Att", "GK Overall Pass Comp Pct")
#create GK throws passing table
t3 <- createPassingTable(t[t[,"gkthrow"]=="yes",])
names(t3) <- c("Player","GK Throw Comp", "GK Throw Att", "GK Throw Comp Pct")
#create GK drop kick passing table
t4 <- createPassingTable(t[t[,"gkdropkick"]=="yes",])
names(t4) <- c("Player","GK Drop Kick Comp", "GK Drop Kick Att", "GK Drop Kick Comp Pct")
#create GK goal kick + free kick passing table
t5 <- createPassingTable(t[t[,"gkfk"]=="yes",])
names(t5) <- c("Player","GKFK Comp", "GKFK Att", "GKFK Comp Pct")
#Merge tables together
t6 <- merge(t2, t3, by="Player", all=TRUE)
t6 <- merge(t6, t4, by="Player", all=TRUE)
t6 <- merge(t6, t5, by="Player", all=TRUE)
all <- merge(all, t6, by=1, all=TRUE)
rm(t, t2, t3, t4, t5, t6)

#CLEANING UP TABLE----------
all[is.na(all)] <- 0
names(all) <- gsub(" ",".", names(all))
rm(x)
rm(d, substitutions, e, matchlength, playerteam)
if(exists("matchURL")){
  rm(matchURL)
}
rm(addColumnForMultiQualifiers,addMultiColumnsForQualifiers,addColumnForQualifier, createCleanDataFrame, createDataFrame, createTable, fillBlanks, createPassingTable, player)
