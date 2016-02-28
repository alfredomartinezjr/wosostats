library(plyr)
library(dplyr)
library(RCurl)
#locationofmatchcsv is the location on your computer or URL for the csv file with the logged match actions
#locationofmatchcsv must be a string
d <- getURL(matchURL)
d <- read.csv(textConnection(d), stringsAsFactors = FALSE)


#FUNCTIONS---------------
# 1.
## Function that takes a pattern and a column name
## The pattern will be the factors as well
createTable <- function(pattern, col, df) {
  ## Get event number for all events that have that pattern in the specified column
  e <- df[df[,col] %in% c(pattern),"event"]
  e <- paste0("^", e, "$")
  
  ## Go back to the original data frame and get all rows with an "event" number 
  ## matching a value in the "e" vector
  d2 <- df[grep(paste(e,collapse="|"), df[,"event"]),]
  
  ## Include only the rows with the pattern events (excludes defensive plays that added rows to an event)
  d2 <- d2[d2[,col] %in% c(pattern),]
  
  ### Set factors, in case all events specified in the pattern don't show up, so that they show up in the table
  d2[,col] <- factor(as.character(d2[,col]), levels=c(pattern))
  
  ## Create the table
  t <- table(d2$poss.player, d2[,col])
  t <- data.frame(unclass(t))
  t <- cbind(Player=rownames(t), t)
  rownames(t) <- NULL
  t
}

# 2.
## Function that creates a data frame of only events that fit a certain pattern
createDataFrame <- function(pattern, col, df) {
  ## Get event number for all events that have that pattern in the specified column
  e <- df[df[,col] %in% c(pattern),"event"]
  
  ## Add a "^" and "$" to each event value
  e <- paste0("^", e, "$")
  
  ## Go back to original data frame and get all rows with an "event" value from e
  df[grep(paste(e, collapse = "|"), df[,"event"]),]
}

# 3.
## Function that fills in blanks with values from above in specified columns that
## relate to the possessing player
fillBlanks <- function(df) {
  x <- 1
  while (x <= nrow(df)) {
    if (is.na(df[x,"poss.player"])) {
      df[x,c("poss.position", "poss.team", "poss.player", "poss.action", 
             "poss.location", "poss.play.destination", "play.type", 
             "poss.player.disciplinary", "poss.notes")] <- df[x-1,c("poss.position", "poss.team", 
                                                                    "poss.player", "poss.action",
                                                                    "poss.location", "poss.play.destination", 
                                                                    "play.type", "poss.player.disciplinary", 
                                                                    "poss.notes")] 
      x <- x + 1
    } else {
      x <- x + 1
    }
  }
  df
}

# 4.
## Adds column that fills in yes/no values based on qualifiers
addColumnForQualifier <- function (newcol, pattern, patternLocation, ogdf, ndf) {
  ndf[,newcol] <- NA
  x <- 1
  while (x <= nrow(ndf)) {
    e <- ndf[x,"event"]
    v <- ogdf[ogdf[,"event"] == e,patternLocation]
    if (grepl(pattern, paste(v, collapse = "|")) == TRUE) {
      ndf[x,newcol] = "yes"
    } else {
      ndf[x,newcol] = "no"
    }
    x <- x + 1
  }
  ndf
}

# 5.
## Fills in blanks and then gets rid of duplicates. Is poss-focused
createCleanDataFrame <- function(pattern, col, df) {
  t <- createDataFrame(pattern, col, df)
  ## Fills in blanks with info from cell above it
  ## Then, exclude anything that marks a stoppage in time
  t <- fillBlanks(t)
  t <- t[t[,"poss.action"] != "playcutoffbybroadcast",]
  
  ## Create the data.frame from which we will create the final table
  ## Takes only unique instances of "event" numbers
  t <- t[!duplicated(t$event),c("event", "time","poss.position", "poss.team", "poss.player", "poss.action", 
                                "poss.location", "poss.play.destination", "play.type", 
                                "poss.player.disciplinary", "poss.notes")]
  t
}

#MINUTES PLAYED & META DATA----------
## Gets data frame that binds data frames of every player who shows up in "poss.player" and "def.player" column
players <- rbind(data.frame(Player=unique(d$poss.player), Team=NA, MP=NA,GS=NA),data.frame(Player=unique(d$def.player), Team=NA, MP=NA,GS=NA))
players <- players[!is.na(players[,"Player"]),]
players <- players[unique(players[,"Player"]),]
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
## Set team name
x <- 1
while (x <= nrow(players)) {
  player <- as.character(players[x,"Player"])
  playerteam <- unique(d[d[,"poss.player"] == player & !is.na(d[,"poss.player"]),"poss.team"])
  players[x,"Team"] <- playerteam
  x <- x + 1
}


#SHOTS---------------
## Creates table for players pased on types of shots.
t <- createTable(c("shots", "accuracy", "shots.scored", "shots.stopped.by.gk", "shots.stopped.by.def", "shots.missed") ,"poss.action", d)
## Add column adding all shot attempts
t$shots <- t$shots.scored + t$shots.stopped.by.gk + t$shots.stopped.by.def + t$shots.missed
## Add column for accuracy
t$accuracy <- (t$shots.scored + t$shots.stopped.by.gk + t$shots.stopped.by.def)/
  (t$shots.scored + t$shots.stopped.by.gk + t$shots.stopped.by.def + t$shots.missed)
##Sort by "shots" and "accuracy"
t <- t[order(-t$shots, -t$accuracy),]
## Change names to be more readable
names(t) <- c("Player","Shots","Shot Accuracy","Goals Scored","Shots Stopped by GK", "Shots Stopped by Def", "Shots Missed")
shots <- t
print(shots, digits=2)

all <- merge(players, shots, by="Player", all=TRUE)

#SHOTS UNDER PRESSURE---------------
t <- createCleanDataFrame(c("shots", "accuracy", "shots.scored", "shots.stopped.by.gk", 
                            "shots.stopped.by.def", "shots.missed") ,"poss.action", d)
## Adds column for whether shot is "pressured" or "challenged"
t <- addColumnForQualifier("pressured", "pressured", "def.action", d, t)
t <- addColumnForQualifier("challenged", "challenged", "def.action", d, t)
t$pressed <- NA
x <- 1
while(x <= nrow(t)) {
  if (t[x,"pressured"] == "yes" | t[x,"challenged"] == "yes") {
    t[x,"pressed"] <- "yes"
    x <- x + 1
  } else {
    t[x,"pressed"] <- "no"
    x <- x + 1
  }
}
## Create table with a column for shots under and not under pressure
t2 <- createTable(c("total", "pct", "yes", "no"), "pressed", t)
## Add "total" and "pct" values
t2$total <- t2$yes + t2$no
t2$pct <- t2$yes/t2$total
# rename and print
t2 <- t2[,c(1,2:4)]
t2 <- t2[order(-t2$yes),]
names(t2) <- c("Player","Pct of Shots Under Pressure", "Shots Under Pressure", "Shots Not Under Pressure")
shotspressure <- t2
print(t2, digits=2)

all <- merge(all, shotspressure, by="Player", all=TRUE)

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
names(t2) <- c("Player", "Shots from A6", "Shots from A18", "Shots from A3L", "Shots from A3C", "Shots from A3R", "Shots from Beyond")
shotlocation <- t2
print(t2, digits=2)

all <- merge(all, shotlocation, by="Player", all=TRUE)

#ASSISTS---------------
t <- createTable(c("key.passes","assists", "second.assists", "unscored.key.passes"), "poss.notes", d)
## Add column for all key passes
t$key.passes <- t$assists + t$second.assists + t$unscored.key.passes
##Sort by "assists" and "second.assists"
t <- t[order(-t$assists, -t$second.assists),]
## Change names to be more readable
names(t) <- c("Player","All Key Passes","Assists","Second Assists", "Unscored Key Passes")
assists <- t
print(t, digits=2)

all <-merge(all, assists, by="Player", all=TRUE)

#BIG CHANCES---------------
t <- createTable(c("big.chances", "big.chances.scored", "big.chances.shot.on.goal", "big.chances.shot.missed", 
                   "big.chances.dispossessed"),"poss.notes", d)
t$big.chances <- t$big.chances.scored + t$big.chances.dispossessed + t$big.chances.shot.on.goal + t$big.chances.shot.missed
## Sort by "big.chances" and "big.chances.scored"
t <- t[order(-t$big.chances, -t$big.chances.scored),]
names(t) <- c("Player","Big Chances","BC Scored", "BC SOG", "BC Missed", "BC Dispossessed")
bigchances <- t
print(t, digits=2)

all <- merge(all, bigchances, by="Player", all=TRUE)

#CROSSES---------------
t <- createCleanDataFrame(c("corner.crosses", "deep.crosses"), "play.type", d)

## Create table with columns for completed, blocked, and missed crosses
t2 <- createTable(c("completed", "pct", "attempts", "passes.f.c", "passes.f", 
                    "passes.s.c", "passes.s"), "poss.action", t)
## Calculate empty columns
t2$completed <- t2$passes.f.c + t2$passes.s.c
t2$attempts <- rowSums(t2[,5:8])
t2$pct <- t2$completed/t2$attempts
t2 <- t2[,1:4]
## Create table with columns for corner and deep crosses
t3 <- createTable(c("corner.crosses", "deep.crosses"), "play.type", t)
# Merge the two sets of columns
t4 <- cbind(t2, t3)
t4 <- t4[,c(1:4,6,7)]
t4 <- t4[order(-t4$completed, -t4$pct, t4$attempts),]
names(t4) <- c("Player","Crosses Completed", "Cross Comp Pct", "Cross Attempts", "Crosses from Corner", "Crosses from Far")
crosses <- t4
print(t4, digits=2)

all <- merge(all, crosses, by=1, all=TRUE)

#THROUGH BALS---------------
t <- createCleanDataFrame(c("through"), "play.type", d)
## Create table
t2 <- createTable(c("completed", "pct", "attempts", "passes.f.c", "passes.f"), "poss.action", t)
## Calculate empty columns
t2$completed <- t2$passes.f.c
t2$attempts <- rowSums(t2[,5:6])
t2$pct <- t2$completed/t2$attempts
t2 <- t2[,1:4]
t2 <- t2[order(-t2$completed, -t2$pct, t2$attempts),]
names(t2) <- c("Player","Through Balls Completed", "Through Ball Pct", "Through Ball Attempts")
through <- t2
print(t2, digits=2)

all <- merge(all, through, by=1, all=TRUE)

#OVERALL PASSING---------------
t <- createCleanDataFrame(c("passes.f.c", "passes.f", 
                            "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", d)
t <- addColumnForQualifier("pressured", "pressured", "def.action", d, t)
t <- addColumnForQualifier("challenged", "challenged", "def.action", d, t)
t$pressed <- NA
x <- 1
while(x <= nrow(t)) {
  if (t[x,"pressured"] == "yes" | t[x,"challenged"] == "yes") {
    t[x,"pressed"] <- "yes"
    x <- x + 1
  } else {
    t[x,"pressed"] <- "no"
    x <- x + 1
  }
}
t2 <- createTable(c("completed", "pct", "attempts" , "passes.f.c", "passes.f", 
                    "passes.s.c", "passes.s", "passes.b.c", 
                    "passes.b"), "poss.action", t)
## Calculate blank columns & then get rid of excess
t2$attempts <- rowSums(t2[,c(5:10)])
t2$completed <- rowSums(t2[,c(5,7,9)])
t2$pct <- t2$completed/t2$attempts
t2 <- t2[,1:4]
# Order
t2 <- t2[order(-t2$pct, -t2$completed),]
names(t2) <- c("Player","Passes Completed", "Pass Comp Pct", "Pass Attempts")
overallpassing <- t2
print(t2, digits=2)

all <- merge(all, overallpassing, by=1, all=TRUE)

#PASSING UNDER PRESSURE---------------
## Include only passing attempts under pressure
t2 <- t[t[,"pressed"] == "yes",]
## Create table
t2 <- createTable(c("Completed", "Comp Pct","Attempts", "passes.f.c", "passes.f", 
                    "passes.s.c", "passes.s", "passes.b.c", 
                    "passes.b"), "poss.action", t2)
## Fill in blank columns and sort
t2[,"Completed"] <- rowSums(t2[,c("passes.f.c","passes.s.c","passes.b.c")])
t2[,"Attempts"] <- rowSums(t2[,5:10])
t2[,"Comp.Pct"] <- t2[,"Completed"]/t2[,"Attempts"]
t2 <- t2[,1:4]
## Create a table comparing how many passes were under pressure
t3 <- createTable(c("Pct of Passes", "yes", "no"), "pressed", t)
## Add "total" and "pct" values
t3[,"Pct.of.Passes"] <- t3$yes/(t3$yes + t3$no)
## Exclude rows with 0% passes under pressure
t3 <- t3[t3[,"Pct.of.Passes"] != 0,]
## Merge columns
t4 <- cbind(t2,t3)
t4 <- t4[,c(1:4,6:8)]
t4 <- t4[order(-t4[,"Comp.Pct"],-t4[,"Completed"],  -t4[,"Pct.of.Passes"]),]
t4 <- t4[,c("Player","Pct.of.Passes", "Completed", "Comp.Pct","Attempts")]
names(t4) <- c("Player", "Pct of Passes Under Pressure", "Passes Completed Under Pressure", 
               "Pass Comp Pct Under Pressure", "Pass Attempts Under Pressure")
passespressure <- t4
print(t4, digits=2)

all <- merge(all, passespressure, by="Player", all=TRUE)

#TAKE ONS---------------
t <- createCleanDataFrame(c("take.on.won", "take.on.lost", "dispossessed", "lost.touch"),"poss.action", d)
t2 <- createTable(c("take.on.won", "Take On Success" ,"Take Ons", "take.on.lost", "dispossessed", "lost.touch"), "poss.action", t)
## Fill in blank columns & rename
t2[,"Take.Ons"] <- t2[,"take.on.won"] + t2[,"take.on.lost"]
t2[,"Take.On.Success"] <- t2[,"take.on.won"]/t2[,"Take.Ons"]
t2 <- t2[order(-t2[,"take.on.won"], -t2[,"Take.On.Success"]),]
names(t2) <- c("Player","Take Ons Won", "Take On Win Pct", "Take On Attempts", "Take Ons Lost", "Dispossessed by Opp", "Lost Touches")
takeons <- t2
print(t2, digits=2)

all <- merge(all, takeons, by=1, all=TRUE)

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
t5 <- t5[,1:4]
names(t5) <- c("Player","Aerial Duels Won", "Aerial Duels Win Pct", "Total Aerial Duels")
aerialduels <- t5
print(t5, digits=2)
rm(t2,t3,t4,t5)

all <- merge(all, aerialduels, by=1, all=TRUE)

#TACKLES & PRESSURE---------------
t <- createDataFrame(c("dispossess.ball.shielded", "dispossess.steal", "dispossess.lost.touch", 
                       "tackles.ball.away", "tackles.ball.won", "dribbled.tackles.missed", 
                       "dribbled.out.run","dribbled.turned", "pressured", "challenged"), "def.action", d)
t <- t[,c("event","time","def.position","def.team","def.player","def.action","def.location","def.player.disciplinary","def.notes")]
names(t) <- c("event", "time", "position" ,"team", "poss.player", "player.event", "location", 
              "def.player.disciplinary", "def.notes")
t2 <- createTable(c("tackles", "dribbled", "pressured", "challenged", "dispossessed", 
                    "dispossess.ball.shielded", "dispossess.steal",
                    "dispossess.lost.touch", "tackles.ball.away", "tackles.ball.won",
                    "dribbled.tackles.missed", "dribbled.out.run","dribbled.turned"), "player.event", t)
## Fill in blank columns, get rid of excess columns, and rename
t2$tackles <- t2$tackles.ball.away + t2$tackles.ball.won
t2$dribbled <- t2$dribbled.tackles.missed + t2$dribbled.out.run + t2$dribbled.turned
t2$dispossessed <- t2$dispossess.ball.shielded + t2$dispossess.steal + t2$dispossess.lost.touch
t2 <- t2[,1:6]
t2 <- t2[order(-t2$tackles, t2$dribbled, t2$pressured, t2$challenged),]
names(t2) <- c("Player","Tackles", "Dribbled by Opp", "Pressured Opp", "Challenged Opp", "Dispossessed Opp")
tackles <- t2
print(t2, digits=2)

all <- merge(all, tackles, by=1, all=TRUE)

#RECOVERIES---------------
t <- createDataFrame(c("recoveries"), "poss.action", d)
t2 <- data.frame(unclass(table(t$poss.player, t$poss.action)))
t2 <- cbind(Player=rownames(t2), t2)
rownames(t2) <- NULL
names(t2) <- c("Player","Recoveries")
recoveries <- t2
print(t2, digits=2)
rm(t2)

all <- merge(all, recoveries, by=1, all=TRUE)

#INTERCEPTIONS, BLOCKS, CLEARANCES, BALL SHIELDS----------
t <- createDataFrame(c("interceptions","clearances", "ball.shield", "blocks"), "def.action", d)
t <- t[,c("event","time", "def.position","def.team","def.player","def.action","def.location", "def.player.disciplinary","def.notes")]
names(t) <- c("event", "time", "position","team", "poss.player", "player.event", "location", 
              "def.player.disciplinary", "def.notes")
t2 <- createTable(c("interceptions", "blocks", "clearances", "ball.shield"), "player.event", t)
t2 <- t2[order(-t2$interceptions,-t2$blocks, -t2$clearances),]
names(t2) <- c("Player","Interceptions","Blocks","Clearances", "Balls Shielded")
interceptions <- t2
print(t2, digits=2)
rm(t2)

all <- merge(all, interceptions, by=1, all=TRUE)

#GK SHOTS ON GOAL FACED----------
t <- createDataFrame(c("gk.s.o.g.stop", "gk.s.o.g.def.stop","gk.s.o.g.scored"), "def.action", d)
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
## Create table for goals conceded, fill in blank columns, cut down excess columns
t3 <- createTable(c("goals.allowed", "yes", "no"), "goals.allowed", t)
t3$goals.allowed <- t3$yes
t3 <- t3[,c("Player","goals.allowed", "no")]
## Merge tables
t4 <- cbind(t2, t3)
t4 <- t4[,c(1,2,5)]
names(t4) <- c("Player","Saves", "Goals Allowed")
gksogfaced <- t4
print(t4, digits=2)
rm(t2,t3,t4)

all <- merge(all, gksogfaced, by=1, all=TRUE)

#GK HIGH BALLS FACED----------
t <- createDataFrame(c("gk.high.balls.won","gk.high.balls.lost"), "def.action", d)
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
t2 <- cbind(createTable(c("gk.high.balls.won", "gk.high.balls.lost"), "def.action", t),
            createTable(c("yes", "no"), "caught", t),
            createTable(c("yes", "no"), "punched.away", t),
            createTable(c("yes", "no"), "parried.away", t),            
            createTable(c("yes", "no"), "collected", t),
            createTable(c("yes", "no"), "foul.won", t),   
            createTable(c("yes", "no"), "cross", t), 
            createTable(c("yes", "no"), "corner.kick", t), 
            createTable(c("yes", "no"), "free.kick", t))
t2 <- t2[,c(1,2,3,5,8,11,14,17,20,23,26)]
names(t2) <- c("Player","High Balls Won", "High Balls Lost", "High Balls Caught", "High Balls Punched Away", "High Balls Parried", "High Balls Collected", "High Ball Fouls Won", "Crosses High Balls Won", "Corner Kick High Balls Won", "Free Kick High Balls Won")
gkhighballs <- t2
print(t2, digits=2)
rm(t2)

all <- merge(all, gkhighballs, by=1, all=TRUE)

#GK SMOTHERS----------
t <- createDataFrame(c("gk.smothers.won", "gk.smothers.lost"), "def.action", d)
t <- t[grep("[Gg][Kk]", t[,"def.position"]),]
t$poss.player <- t$def.player
t <- createTable(c("gk.smothers.won", "gk.smothers.lost"), "def.action", t)
names(t) <- c("Player","Smothers Won", "Smothers Lost")
smothers <- t
print(t, digits=2)

all <- merge(all, smothers, by=1, all=TRUE)

#CLEANING UP TABLE----------
all[is.na(all)] <- 0
