#SOURCING---------------
library(plyr)
library(dplyr)
library(RCurl)
#locationofmatchcsv is the location on your computer or URL for the csv file with the logged match actions
#locationofmatchcsv must be a string
d <- getURL(matchURL)
d <- read.csv(textConnection(d), stringsAsFactors = FALSE)
source("https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/functions.R")

#MINUTES PLAYED & META DATA----------
## Gets data frame that binds data frames of every player who shows up in "poss.player" and "def.player" column
players <- rbind(data.frame(Player=unique(d$poss.player), Team=NA, MP=NA,GS=NA),data.frame(Player=unique(d$def.player), Team=NA, MP=NA,GS=NA))
players <- players[!is.na(players[,"Player"]),]
players <- unique(players[,])
matchlength <- length(unique(d$time))
substitutions <- d[grepl("substitution",d[,"poss.action"]),]
x <- 1
while (x <= nrow(players)) {
  player <- as.character(players[x,"Player"])
  #if they don't appear in the substitutions data frame, the player played the entire match
  if (nrow(substitutions[substitutions[,"poss.player"] %in% player,]) == 0) {
    players[x,"MP"] <- matchlength
    players[x,"GS"] <- 1
  } else if (nrow(substitutions[substitutions[,"poss.player"] %in% player,]) > 0) {
    #check if she was a starter, based on if she was only ever substituted on
    if ((substitutions[substitutions[,"poss.player"] == player,"poss.action"] %in% "substitution.on") == FALSE) {
      #if she was a starter who was subbed off, get the length of unique values for vector d[,"time] 
      #up to where she got subbed off
      e <- substitutions[substitutions[,"poss.player"] == player,"event"]
      lastminute <- grep(e, d[,"event"])
      minutesplayed <- length(unique(d[1:lastminute,"time"]))
      players[x,"MP"] <- minutesplayed
      players[x,"GS"] <- 1
    } else
      #if she wasn't a starter and got subbed on and wasn't also later subbed off
      if ((substitutions[substitutions[,"poss.player"] == player,"poss.action"] %in% "substitution.on")
          & !(substitutions[substitutions[,"poss.player"] == player,"poss.action"] %in% "substitution.off")){
        #if she wasn't a starter, got subbed on, and was never subbed off, get the length of unique
        #values for vector d[,"time] from when she got subbed on to when she got subbed off
        e <- substitutions[substitutions[,"poss.player"] == player,"event"]
        firstminute <- grep(e, d[,"event"])
        minutesplayed <- length(unique(d[firstminute:nrow(d),"time"]))
        players[x,"MP"] <- minutesplayed
        players[x,"GS"] <- 0
      } else
        #if she wasn't a starter, got subbed on, and was later subbed off
        if ((substitutions[substitutions[,"poss.player"] == player,"poss.action"] %in% "substitution.on")
            & (substitutions[substitutions[,"poss.player"] == player,"poss.action"] %in% "substitution.off")) {
          #if she wasn't a starter, got subbed on, and as later subbed off, get the length of unique
          #values for vector d[,"time] from when she got subbed on to when she got subbed off
          e <- substitutions[substitutions[,"poss.player"] == player,"event"]
          firstminute <- grep(e[1], d[,"event"])
          lastminute <- grep(e[2], d[,"event"])
          minutesplayed <- length(unique(d[firstminute:lastminute,"time"]))
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
names(t2) <- c("Player","Shot Pressd", "Pct Shots Pressd")
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
t <- addMultiColumnsForQualifiers(c("assists"="^assist", "key.passes"="key.pass|^second.assist", "second.assists"="^second.assist"),
                                  pattern_locations = c("poss.notes", "poss.notes", "poss.notes"),
                                  ogdf = d, ndf = createDataFrame(c("passes.f.c", "passes.f", 
                                                                         "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t <- addColumnForMultiQualifiers(newcol="key.assists", pattern=c("assists"="yes", "key.passes"="yes"), df = t, exp = "AND")
##Create tables for each column (five different ones)
t1 <- createTable(c("yes"),"assists",t)
names(t1) <- c("Player", "Assists")
t2 <- createTable(c("yes", "key.passes.to.goals"),"key.passes",t)
names(t2) <- c("Player", "Key Passes", "KP to Goal")
t6 <- merge(t1, t2, by="Player", all=TRUE)
t3 <- createTable(c("yes"),"key.assists",t)
names(t3) <- c("Player", "Key Assists")
t6 <- merge(t6, t3, by="Player", all=TRUE)
t4 <- createTable(c("yes"),"second.assists",t)
names(t4) <- c("Player", "Second Assists")
t6 <- merge(t6, t4, by="Player", all=TRUE)
##Sort by "assists" and "second.assists"
t6[is.na(t6)] <- 0
t6[,"KP Goal"] <- (t6[,"Key Assists"] + t6[,"Second Assists"])/t6[,"Key Passes"]
t6$`Assists per 90` <- numeric(nrow(t6))
t6$`Key Passes per 90` <- numeric(nrow(t6))
t6$`Assists per Pass` <- numeric(nrow(t6))
t6$`Key Passes per Pass` <- numeric(nrow(t6))
all <-merge(all, t6, by="Player", all=TRUE)
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
names(t) <- c("Player","Big Chances","Big Chances per 90", "BC Scored", "BC to Goal","BC SOG", "BC Shot Miss", "BC Dispossess", "BC Created", "BC Lost")
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
t2 <- createPassingTable(t)
names(t2) <- c("Player","Pass Comp", "Pass Att", "Pass Comp Pct")
all <- merge(all, t2, by=1, all=TRUE)
rm(t2)

#OPEN PLAY PASSING----------
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick", patternLocation = "play.type", ogdf = d, 
                           ndf = createDataFrame(c("passes.f.c", "passes.f", 
                                                   "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d),
                           invert = TRUE)
##create open play passing table
t <- createPassingTable(t[t[,"opPass"]=="yes",])
names(t) <- c("Player","opPass Comp", "opPass Att", "opPass Comp Pct")
all <- merge(all, t, by=1, all=TRUE)
rm(t)

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
t3 <- merge(createPassingTable(t[t[,"pressed"] == "yes",]), t2[1:2], by="Player", all=TRUE)
names(t3) <- c("Player", "opPPass Comp", "opPPass Att", "opPPass Comp Pct", 
               "Pct opPass Pressd")
all <- merge(all, t3, by="Player", all=TRUE)
rm(t, t2, t3)

#OVERALL PASSING BY DIRECTION---------------
t <- addMultiColumnsForQualifiers(patterns = c("forward.pass"="^passes.f","sideways.pass"="^passes.s","backward.pass"="^passes.b"),
                             pattern_locations = c("poss.action","poss.action","poss.action"),ogdf = d,
                             ndf = createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
##Creates blank table with columns for direction distribution
directiondist <- createTable(c("rFreq Pass Fwd", "rFreq Pass Side", "rFreq Pass Back"), "forward.pass", t)
##Create a table for completions, attempts, and comp pct for FORWARD passes
fwdtab <- createPassingTable(t[t[,"forward.pass"] == "yes",])
names(fwdtab) <- c("Player","fwPass.Comp", "fwPass.Att", "fwPass.Comp.Pct")
passdirection <- merge(directiondist, fwdtab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for SIDEWAYS passes
sidetab <- createPassingTable(t[t[,"sideways.pass"] == "yes",])
names(sidetab) <- c("Player","sPass.Comp", "sPass.Att", "sPass.Comp.Pct")
passdirection <- merge(passdirection, sidetab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for BACKWARDS passes
backtab <- createPassingTable(t[t[,"backward.pass"] == "yes",])
names(backtab) <- c("Player","bPass.Comp", "bPass.Att", "bPass.Comp.Pct")
passdirection <- merge(passdirection, backtab, by="Player", all=TRUE)
##Calculate direction distribution
passdirection[is.na(passdirection)] <- 0
passdirection$rFreq.Pass.Fwd <- passdirection$fwPass.Att/rowSums(passdirection[,c("fwPass.Att", "sPass.Att", "bPass.Att")])
passdirection$rFreq.Pass.Side <- passdirection$sPass.Att/rowSums(passdirection[,c("fwPass.Att", "sPass.Att", "bPass.Att")])
passdirection$rFreq.Pass.Back <- passdirection$bPass.Att/rowSums(passdirection[,c("fwPass.Att", "sPass.Att", "bPass.Att")])
all <- merge(all, passdirection, by="Player", all=TRUE)
rm(directiondist, fwdtab,sidetab, backtab,passdirection)

#OPEN PLAY PASSING BY DIRECTION----------
#Passing stats, without dead ball scenarios (GKs, GK throws, GK drop kicks,FKs, CKs, throw ins)
t <- addColumnForQualifier("opPass", pattern="throw|gk|corner.kick|free.kick", patternLocation = "play.type", ogdf = d, 
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
fwdtab <- createPassingTable(t[t[,"forward.pass"] == "yes",])
names(fwdtab) <- c("Player", "fwopPass.Comp", "fwopPass.Att", "fwopPass.Comp.Pct")
passdirection <- merge(directiondist, fwdtab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for SIDEWAYS passes
sidetab <- createPassingTable(t[t[,"sideways.pass"] == "yes",])
names(sidetab) <- c("Player", "sopPass.Comp", "sopPass.Att", "sopPass.Comp.Pct")
passdirection <- merge(passdirection, sidetab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for BACKWARDS passes
backtab <- createPassingTable(t[t[,"backward.pass"] == "yes",])
names(backtab) <- c("Player", "bopPass.Comp", "bopPass.Att", "bopPass.Comp.Pct")
passdirection <- merge(passdirection, backtab, by="Player", all=TRUE)
##Calculate direction distribution
passdirection[is.na(passdirection)] <- 0
passdirection$rFreq.opPass.Fwd <- passdirection$fwopPass.Att/rowSums(passdirection[,c("fwopPass.Att", "sopPass.Att", "bopPass.Att")])
passdirection$rFreq.opPass.Side <- passdirection$sopPass.Att/rowSums(passdirection[,c("fwopPass.Att", "sopPass.Att", "bopPass.Att")])
passdirection$rFreq.opPass.Back <- passdirection$bopPass.Att/rowSums(passdirection[,c("fwopPass.Att", "sopPass.Att", "bopPass.Att")])
all <- merge(all, passdirection, by="Player", all=TRUE)
rm(backtab, sidetab, directiondist, fwdtab, passdirection)

#PASSING UNDER PRESSURE---------------
t <- addMultiColumnsForQualifiers(patterns=c("pressured"="pressure", "challenged"="challenge"),
                                  pattern_locations = c("def.action", "def.action"),
                                  ogdf = d, 
                                  ndf = createCleanDataFrame(c("passes.f.c", "passes.f", "passes.s.c", 
                                                               "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
t <- addColumnForMultiQualifiers(newcol = "pressed", df = t, exp = "OR",
                                 pattern = c("pressured"="yes","challenged"="yes"))
t2 <- createPassingTable(t[t[,"pressed"] == "yes",])
names(t2) <- c("Player","Completed", "Attempts", "Comp Pct")
## Create a table comparing how many passes were under pressure
t3 <- createTable(c("Pct of Passes", "yes", "no"), "pressed", t)
t3[,"Pct.of.Passes"] <- t3$yes/(t3$yes + t3$no)
## Exclude rows with 0% passes under pressure
t3 <- t3[t3[,"Pct.of.Passes"] != 0,]
t4 <- merge(t2,t3, by="Player", all=TRUE)
t4 <- t4[,c("Player","Pct.of.Passes", "Completed", "Attempts", "Comp Pct")]
names(t4) <- c("Player", "Pct Pass Pressd", "PPass Comp", "PPass Att", 
               "PPass Comp Pct")
all <- merge(all, t4, by="Player", all=TRUE)
rm(t, t2, t3, t4)

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
fwdtab <- createPassingTable(t[t[,"forward.pass"] == "yes" & t[,"pressed"] == "yes",])
names(fwdtab) <- c("Player","fwPPass.Comp", "fwPPass.Att", "fwPPass.Comp.Pct")
passdirection <- merge(directiondist, fwdtab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for SIDEWAYS passes
sidetab <- createPassingTable(t[t[,"sideways.pass"] == "yes" & t[,"pressed"] == "yes",])
names(sidetab) <- c("Player", "sPPass.Comp", "sPPass.Att", "sPPass.Comp.Pct")
passdirection <- merge(passdirection, sidetab, by="Player", all=TRUE)
##Create a table for completions, attempts, and comp pct for BACKWARDS passes
backtab <- createPassingTable(t[t[,"backward.pass"] == "yes" & t[,"pressed"] == "yes",])
names(backtab) <- c("Player", "bPPass.Comp", "bPPass.Att", "bPPass.Comp.Pct")
passdirection <- merge(passdirection, backtab, by="Player", all=TRUE)
##Calculate direction distribution
passdirection[is.na(passdirection)] <- 0
passdirection$rFreq.PPass.Fwd <- passdirection$fwPPass.Att/rowSums(passdirection[,c("fwPPass.Att", "sPPass.Att", "bPPass.Att")])
passdirection$rFreq.PPass.Side <- passdirection$sPPass.Att/rowSums(passdirection[,c("fwPPass.Att", "sPPass.Att", "bPPass.Att")])
passdirection$rFreq.PPass.Back <- passdirection$bPPass.Att/rowSums(passdirection[,c("fwPPass.Att", "sPPass.Att", "bPPass.Att")])
all <- merge(all, passdirection, by="Player", all=TRUE)
rm(t, passdirection, backtab, sidetab, fwdtab, directiondist)

#CROSSES---------------
t <- createDataFrame(c("corner.crosses", "deep.crosses"), "play.type", createDataFrame(c("passes.f.c", "passes.f", 
                                                                                         "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
## Fills in blanks with info from cell above it
t <- fillBlanks(t)
## Then, exclude anything that marks a stoppage in time
t <- t[t[,"poss.action"] != "playcutoffbybroadcast",]
t <- t[grep("corner.crosses|deep.crosses", t[,"play.type"]),]
## Create table with columns for completed, blocked, and missed crosses
t2 <- createPassingTable(t, extra = c("cross.att.per.90","cross.att.per.pass","cross.att.per.oppass"))
## Create table with columns for corner and deep crosses
t3 <- createTable(c("corner.crosses", "deep.crosses"), "play.type", t)
# Merge the two sets of columns
t4 <- merge(t2, t3, by="Player", all=TRUE)
names(t4) <- c("Player","Cross Comp", "Cross Att", "Cross Att per 90", "Cross Att per Pass", "Cross Att per opPass","Cross Comp Pct", "Corner Crosses", "Deep Crosses")
all <- merge(all, t4, by=1, all=TRUE)
rm(t, t2, t3, t4)
#Calculate "per 90" & "per pass" fields
all$`Cross Att per 90` <- (all$`Cross Att`/all$MP)*90
all$`Cross Att per Pass` <- (all$`Cross Att`/all$`Pass Att`)
all$`Cross Att per opPass` <- (all$`Cross Att`/all$`opPass Att`)

#LAUNCH BALLS---------------
t <- addColumnForQualifier(newcol="launch", pattern = "launch|gk.drop.kick", patternLocation = "play.type",
                           ogdf = d, ndf = createCleanDataFrame(c("passes.f.c", "passes.f", 
                                                             "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d))
##Keep only rows with "yes" in "launch" column
t <- t[t[,"launch"]=="yes",]
t2 <- createPassingTable(t, extra = c("launch.att.per.90", "launch.att.per.pass", "launch.att.per.oppass"))
names(t2) <- c("Player","Launch Comp", "Launch Att", "Launch Att per 90", "Launch Att per Pass", "Launch Att per opPass", "Launch Comp Pct")
all <- merge(all, t2, by=1, all=TRUE)
rm(t, t2)
#Calculate "per 90" columns
all$`Launch Att per 90` <- (all$`Launch Att`/all$MP)*90
all$`Launch Att per Pass` <- (all$`Launch Att`/all$`Pass Att`)
all$`Launch Att per opPass` <- (all$`Launch Att`/all$`opPass Att`)

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
names(t2) <- c("Player","Corner Kicks Completed", "Corner Kicks Taken", "CK Effectiveness")
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
t <- createTable(c("take.on.won", "Take On Success" ,"Take Ons", "take.on.lost", "dispossessed", "lost.touch"), "poss.action", 
                 createCleanDataFrame(c("take.on.won", "take.on.lost", "dispossessed", "lost.touch"),"poss.action", d))
## Fill in blank columns & rename
t[,"Take.Ons"] <- t[,"take.on.won"] + t[,"take.on.lost"]
t[,"Take.On.Success"] <- t[,"take.on.won"]/t[,"Take.Ons"]
names(t) <- c("Player","TO Won", "TO Win Pct", "Take Ons", "TO Lost", "Dispossessed", "Lost Touches")
all <- merge(all, t, by=1, all=TRUE)
rm(t)

#AERIAL DUELS---------------
t <- createDataFrame(c("aerial.won", "aerial.lost"), "poss.action", d)
t2 <- t[,c("event", "time", "poss.position", "poss.team", "poss.player", "poss.action", "poss.location")]
names(t2) <- c("event", "time", "position", "team", "poss.player", "player.event", "location")
t3 <- t[,c("event", "time","def.position", "def.team", "def.player", "def.action", "def.location")]
names(t3) <- c("event", "time", "position", "team", "poss.player", "player.event", "location")
t4 <- rbind(t2,t3)
t5 <- createTable(c("aerial.won", "Success Pct", "Aerial Duels", "aerial.lost"), "player.event", t4)
## Fill in blank columns, sort, & rename
t5[,"Aerial.Duels"] <- t5[,"aerial.won"] + t5[,"aerial.lost"]
t5[,"Success.Pct"] <- t5[,"aerial.won"]/t5[,"Aerial.Duels"]
t5 <- t5[order(-t5[,"aerial.won"], -t5[,"Success.Pct"], t5[,"Aerial.Duels"]),]
names(t5) <- c("Player","AD Won", "AD Win Pct", "Aerial Duels", "AD Lost")
all <- merge(all, t5, by=1, all=TRUE)
rm(t, t2,t3,t4,t5)

#TACKLES & PRESSURE---------------
t <- createDataFrame(c("dispossessed", "tackles.ball.away", "tackles.ball.won", "dribbled.tackles.missed", 
                       "dribbled.out.run","dribbled.turned", "pressured", "challenged"), "def.action", d)
t <- t[,c("event","time","def.position","def.team","def.player","def.action","def.location","def.player.disciplinary","def.notes")]
names(t) <- c("event", "time", "position" ,"team", "poss.player", "player.event", "location", 
              "def.player.disciplinary", "def.notes")
t2 <- createTable(c("tackles","dispossessed", "dribbled", "pressured", "challenged", 
                    "tackles.ball.away", "tackles.ball.won",
                    "dribbled.tackles.missed", "dribbled.out.run","dribbled.turned"), "player.event", t)
## Fill in blank columns, get rid of excess columns, and rename
t2$tackles <- t2$tackles.ball.away + t2$tackles.ball.won
t2$dribbled <- t2$dribbled.tackles.missed + t2$dribbled.out.run + t2$dribbled.turned
t2 <- t2[,1:6]
t2 <- t2[order(-t2$tackles, -t2$dispossessed, t2$dribbled, t2$pressured, t2$challenged),]
names(t2) <- c("Player","Tackles", "Dispossesses", "Dribbled", "Press Opp", "Challenge Opp")
tackles <- t2

all <- merge(all, tackles, by=1, all=TRUE)
rm(tackles)

#RECOVERIES---------------
t <- createDataFrame(c("recoveries"), "poss.action", d)
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
##Create table for overall recoveries
t2 <- data.frame(unclass(table(t$poss.player, t$poss.action)))
t2 <- cbind(Player=rownames(t2), t2)
names(t2) <- c("Player", "Recoveries")
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

#INTERCEPTIONS, BLOCKS, CLEARANCES, BALL SHIELDS----------
t <- createDataFrame(c("interceptions","clearances", "ball.shield", "blocks"), "def.action", d)
t <- t[,c("event","time", "def.position","def.team","def.player","def.action","def.location", "def.player.disciplinary","def.notes")]
names(t) <- c("event", "time", "position","team", "poss.player", "player.event", "location", 
              "def.player.disciplinary", "def.notes")
t2 <- createTable(c("interceptions", "interceptions.per.90","interceptions.per.op.pass","blocks", "clearances", "ball.shield"), "player.event", t)
names(t2) <- c("Player","Interceptions","Int per 90","Int per OP Pass", "Blocks","Clearances", "Balls Shields")
all <- merge(all, t2, by=1, all=TRUE)
rm(t2)
all$`Int per 90` <- (all$Interceptions/all$MP)*90

#ERRORS & BIG CHANCE STOPS

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
names(t5) <- c("Player","Saves", "Goals Allowed", "SOG Faced", "GperSOG")
rm(t2,t3,t4)
all <- merge(all, t5, by=1, all=TRUE)
rm(t5)

#GK BIG CHANCES SOG FACED----------
t <- createDataFrame(c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"), "def.action", d)
##Create blank "BC SOG Faced", "BC Saves" & "BC Goals Allowed" columns
t$BC.SOG.Faced <- NA
t$BC.Saves <- NA
t$BC.Goals.Allowed <- NA
##Fill in "BC SOG Faced" column based on if a big chance SOG is logged in "poss.notes"
x <- 1
while (x <= length(unique(t$event))) {
  #Checks if an event has a big chance SOG
  if (grepl("big.chances.scored|big.chances.shot.on.goal",
            paste(unlist(strsplit(t[t[,"event"] == unique(t$event)[x],"poss.notes"], ","), 
                         recursive=TRUE), 
                  sep="", 
                  collapse=" "))) {
    t[t[,"event"] == unique(t$event)[x],"BC.SOG.Faced"] <- "yes"
  } else {
    t[t[,"event"] == unique(t$event)[x],"BC.SOG.Faced"] <- "no"
  }
  #Checks if an event is a big chance saved
  if (grepl("gk.s.o.g.stop",
           paste(unlist(strsplit(t[t[,"event"] == unique(t$event)[x],"def.action"], ","), 
                        recursive=TRUE), 
                 sep="", 
                 collapse=" ")) && 
      grepl("yes",
            paste(unlist(strsplit(t[t[,"event"] == unique(t$event)[x],"BC.SOG.Faced"], ","), 
                         recursive=TRUE), 
                  sep="", 
                  collapse=" "))) {
    t[t[,"event"] == unique(t$event)[x],"BC.Saves"] <- "yes"
  } else {
    t[t[,"event"] == unique(t$event)[x],"BC.Saves"] <- "no"
  }
  #Checks if an event has a big chance scored
  if (grepl("big.chances.scored",
            paste(unlist(strsplit(t[t[,"event"] == unique(t$event)[x],"poss.notes"], ","), 
                         recursive=TRUE), 
                  sep="", 
                  collapse=" "))) {
    t[t[,"event"] == unique(t$event)[x],"BC.Goals.Allowed"] <- "yes"
  } else {
    t[t[,"event"] == unique(t$event)[x],"BC.Goals.Allowed"] <- "no"
  }
  x <- x + 1
}
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
t <- createDataFrame(c("gk.high.balls.won","gk.high.balls.lost"), "def.action", d)
## Add column for whether a high ball was won
t <- addColumnForQualifier("hbwon", "gk.high.balls.won", "def.action", d, t)
## Add column for whether a high ball was lost
t <- addColumnForQualifier("hblost", "gk.high.balls.lost", "def.action", d, t)
## Add column for whether an event is a cross
t <- addColumnForQualifier("cross", "cross", "play.type", d, t)
## Add column for whether an event is a corner kick
t <- addColumnForQualifier("corner.kick", "corner.kick", "play.type", d, t)
## Add column for whether an event is a free kick
t <- addColumnForQualifier("free.kick", "free.kick", "play.type", d, t)
## Add columns for how high balls were won
t <- addColumnForQualifier("caught", "caught", "gk.ball.stop", d, t)
t <- addColumnForQualifier("punched.away", "punched.to.safety", "gk.ball.stop", d, t)
t <- addColumnForQualifier("parried.away", "parried.to.safety", "gk.ball.stop", d, t)
t <- addColumnForQualifier("collected", "collected", "gk.ball.stop", d, t)
t <- addColumnForQualifier("foul.won", "fouls.won", "gk.ball.stop", d, t)
## Only goalkeepers
t <- t[grep("[Gg][Kk]", t[,"def.position"]),]
t$poss.player <- t$def.player
## Create table
t2 <- merge(cbind("Player"=character(0), "High Balls"=numeric(0)), createTable(c("gk.high.balls.won", "gk.high.balls.lost"), "def.action", t), by="Player", all=TRUE)
names(t2) <- c("Player", "High Balls", "HB Won", "HB Lost")
t2$`High Balls` <- t2$`HB Won` + t2$`HB Lost`
t2 <- merge(t2, createTable(c("yes", "no"), "caught", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[5] <- "HB Caught"
t2 <- merge(t2, createTable(c("yes", "no"), "punched.away", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[6] <- "HB Punched"
t2 <- merge(t2, createTable(c("yes", "no"), "parried.away", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[7] <- "HB Parried"
t2 <- merge(t2, createTable(c("yes", "no"), "collected", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[8] <- "HB Collected"
t2 <- merge(t2, createTable(c("yes", "no"), "foul.won", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[9] <- "HB Fouls Won"


t2 <- merge(t2, createTable(c("yes", "no"), "cross", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[10] <- "Crosses"
t2 <- merge(t2, createTable(c("yes", "no"), "hbwon", t[t["cross"]=="yes",]), by="Player", all=TRUE)
names(t2)[11:12] <- c("Crosses Won", "Crosses Lost")
t2 <- merge(t2, createTable(c("yes", "no"), "corner.kick", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[13] <- "Corner Kicks"
t2 <- merge(t2, createTable(c("yes", "no"), "hbwon", t[t["corner.kick"]=="yes",]), by="Player", all=TRUE)
names(t2)[14:15] <- c("CKs Won", "CKs Lost")
t2 <- merge(t2, createTable(c("yes", "no"), "free.kick", t)[,c("Player", "yes")], by="Player", all=TRUE)
names(t2)[16] <- "Free Kicks"
t2 <- merge(t2, createTable(c("yes", "no"), "hbwon", t[t["free.kick"]=="yes",]), by="Player", all=TRUE)
names(t2)[17:18] <- c("FKs Won", "FKs Lost")

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
t <- createDataFrame(c("passes.f.c", "passes.f", 
                            "passes.s.c", "passes.s", "passes.b.c", "passes.b"), 
                          "poss.action", 
                          d[grep("[Gg][Kk]", d[,"poss.position"]),])
##Create blank "gkthrow", "gkdropkick", and "gkfk" columns
t$gkthrow <- NA
t$gkdropkick <- NA
t$gkfk <- NA
x <- 1
while (x <= length(unique(t$event))) {
  if (grepl("gk.throw",
            paste(unlist(strsplit(t[t[,"event"] == unique(t$event)[x],"play.type"], ","), 
                         recursive=TRUE), 
                  sep="", 
                  collapse=" "))) {
    t[t[,"event"] == unique(t$event)[x],"gkthrow"] <- "throw"
  } else {
    t[t[,"event"] == unique(t$event)[x],"gkthrow"] <- "no"
  }
  if (grepl("gk.drop.kick",
            paste(unlist(strsplit(t[t[,"event"] == unique(t$event)[x],"play.type"], ","), 
                         recursive=TRUE), 
                  sep="", 
                  collapse=" "))) {
    t[t[,"event"] == unique(t$event)[x],"gkdropkick"] <- "dropkick"
  } else {
    t[t[,"event"] == unique(t$event)[x],"gkdropkick"] <- "no"
  }
  if (grepl("goal.kick|free.kick",
            paste(unlist(strsplit(t[t[,"event"] == unique(t$event)[x],"play.type"], ","), 
                         recursive=TRUE), 
                  sep="", 
                  collapse=" "))) {
    t[t[,"event"] == unique(t$event)[x],"gkfk"] <- "goalfreekick"
  } else {
    t[t[,"event"] == unique(t$event)[x],"gkfk"] <- "no"
  }
  x <- x + 1
}
##create overall GK passing table
t2 <- createTable(c("completed", "pct", "attempts" , "passes.f.c", "passes.f", 
                    "passes.s.c", "passes.s", "passes.b.c", 
                    "passes.b"), "poss.action", t)
t2$attempts <- rowSums(t2[,c(5:10)])
t2$completed <- rowSums(t2[,c(5,7,9)])
t2$pct <- t2$completed/t2$attempts
t2 <- t2[,1:4]
names(t2) <- c("Player","All Pass Comp", "Distribution Success", "All Pass Att")
#create GK throws passing table
t3 <- createTable(c("completed", "pct", "attempts" , "passes.f.c", "passes.f", 
                    "passes.s.c", "passes.s", "passes.b.c", 
                    "passes.b"), "poss.action", t[t[,"gkthrow"]=="throw",])
t3$attempts <- rowSums(t3[,c(5:10)])
t3$completed <- rowSums(t3[,c(5,7,9)])
t3$pct <- t3$completed/t3$attempts
t3 <- t3[,1:4]
names(t3) <- c("Player","Throw Comp", "Throw Comp Pct", "Throw Att")
#create GK drop kick passing table
t4 <- createTable(c("completed", "pct", "attempts" , "passes.f.c", "passes.f", 
                    "passes.s.c", "passes.s", "passes.b.c", 
                    "passes.b"), "poss.action", t[t[,"gkdropkick"]=="dropkick",])
t4$attempts <- rowSums(t4[,c(5:10)])
t4$completed <- rowSums(t4[,c(5,7,9)])
t4$pct <- t4$completed/t4$attempts
t4 <- t4[,1:4]
names(t4) <- c("Player","Drop Kick Comp", "Drop Kick Comp Pct", "Drop Kick Att")
#create GK goal kick + free kick passing table
t5 <- createTable(c("completed", "pct", "attempts" , "passes.f.c", "passes.f", 
                    "passes.s.c", "passes.s", "passes.b.c", 
                    "passes.b"), "poss.action", t[t[,"gkfk"]=="goalfreekick",])
t5$attempts <- rowSums(t5[,c(5:10)])
t5$completed <- rowSums(t5[,c(5,7,9)])
t5$pct <- t5$completed/t5$attempts
t5 <- t5[,1:4]
names(t5) <- c("Player","GK+FK Comp", "GK+FK Comp Pct", "GK+FK Att")
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
rm(d, substitutions, e, matchlength, playerteam, matchURL)
rm(addColumnForQualifier, createCleanDataFrame, createDataFrame, createTable, fillBlanks, createPassingTable, player)
