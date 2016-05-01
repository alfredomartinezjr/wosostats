## Functions for reading and tidying up a raw Excel file of match stats
## and turning it into a data.frame in your environment
##
## Save the file into your working directory by running read.csv(df, file="filenamegoeshere.csv", row.names=FALSE)
##READING EXCEL FILE----------
## Install if necessary
require(xlsx)
## IMPORTANT: "match" must be set as a string value, or this won't work
## Might take a while to create. Takes about two minutes. Hold tight.
## The Excel file must be in the working directory
df <- read.xlsx(match, sheetName = "match")

##CHANGE COLUMN CLASSES----------
### Changes factors of select columns
df$event <- as.numeric(as.character(df[,"event"]))
df$poss.action <- as.character(df[,"poss.action"])
df$play.type <- as.character(df[,"play.type"])
df$def.action <- as.character(df[,"def.action"])
df$poss.player.disciplinary <- as.character(df[,"poss.player.disciplinary"])
df$poss.notes <- as.character(df[,"poss.notes"])
df$def.player.disciplinary <- as.character(df[,"def.player.disciplinary"])
df$poss.player <- as.character(df$poss.player)
df$def.player <- as.character(df$def.player)

##CLEAN UP----------
###Gets rid of NA columns
df <- df[,!grepl("^NA",names(df))]

###Gets rid of any blank rows after the match has ended
df <- df[1:max(grep("end.of.match", df[,"poss.action"])),]

###Fill in missing time data
####This ASSUMES that, if there are blanks, then the first row
####where a minute appears is the first event for that minute.
x <- grep("kickoff", df[,"poss.action"])
####in case there's more than one "kickoff" (incorrectly) logged
if(length(x) > 1) (x <- x[1])
while (x <= nrow(df)) {
  #checks if time is blank
  if (df[x,"time"] == "-" | is.na(df[x,"time"])) {
    #if time is blank, set it as previous value
    df[x,"time"] <- df[x-1, "time"]
  }
  x <- x + 1
}

###Re-calculates event values
####convert all blanks and "-"s to NAs
x <- 1
while (x <= nrow(df)) {
  if (df[x,"event"] == "-" | is.na(df[x,"event"]) | df[x,"event"] == " ") {
    df[x,"event"] <- NA
  }
  x <- x + 1
}

####sets x as row below kickoff
x <- grep("kickoff", df[,"poss.action"])
if(length(x) > 1) (x <- x[1])
x <- x + 1

####checks if "poss.player" is NA, "-", or " " and sets appropriate event value
while (x <= nrow(df)) {
  if(df[x,"poss.player"] == "-" | is.na(df[x,"poss.player"]) | df[x,"poss.player"] == " "){
    #sets event value as previous row's event value
    df[x,"event"] <- df[x-1,"event"]
  } else {
    #sets event value as 1 plus previous row's event value
    df[x,"event"] <- df[x-1,"event"]+1
  }
  x <- x + 1
}

###Gets rid of player numbers and leading/trailing whitespace in "poss.player" and "def.player" values
x <- 1
while (x <= nrow(df)) {
  poss.string <- df[x,"poss.player"]
  def.string <- df[x,"def.player"]
  df[x,"poss.player"] <- trimws(strsplit(as.character(poss.string)," \\(")[[1]][1])
  df[x,"def.player"] <- trimws(strsplit(as.character(def.string)," \\(")[[1]][1])
  x <- x + 1
}


##METADATA----------
### Creates a meta data frame of all columns from row 1 to row before kickoff
ref <- df[1:(grep("kickoff", df[,"poss.action"])[1]-1),]
### Creates a vector for the "home" team and the "away" team, excluding possible NA values
teams <- as.character(unique(ref$poss.team))
teams <- teams[!is.na(teams) & !(teams=="-") & !(teams==" ") & !(teams=="")]
hometeam <- teams[1]
awayteam <- teams[2]
### home team should always be listed first
homedata <- ref[ref[,"poss.team"]==hometeam,c("poss.position","poss.team", "poss.player")]
awaydata <- ref[ref[,"poss.team"]==awayteam,c("poss.position","poss.team", "poss.player")]
## Create data frame with opposites of each location
posslocations <- c("A6", "A18", "A3L", "A3C", "A3R", "AM3L", "AM3C", 
                   "AM3R", "DM3L", "DM3C", "DM3R", "D3L", "D3C", "D3R", 
                   "D18", "D6", "AL", "AC", "AR", "AML", "AMC", 
                   "AMR", "DML", "DMC", "DMR", "DL", "DC", "DR")
deflocations <- c("D6", "D18", "D3R", "D3C", "D3L", "DM3R", "DM3C",
                  "DM3L", "AM3R", "AM3C", "AM3L", "A3R", "A3C", "A3L", 
                  "A18", "A6", "DR", "DC", "DL", "DMR", "DMC",
                  "DML", "AMR", "AMC", "AML", "AR", "AC", "AL")
opposites <- data.frame(posslocations, deflocations)

##CHECK FOR INCORRECT DATA--------
###Checks if a player's name has certain letters in upper case (this messes with how stats are computed)
x <- grep("kickoff", df[,"poss.action"])
if(length(x) > 1) (x <- x[1])
x <- x + 1
####creates vector of players, excluding blanks
players <- as.character(unique(ref$poss.player))
players <- players[!is.na(players) & !(players=="-") & !(players==" ") & !(players=="")]
while (x <= nrow(df)) {
  #checks poss.player
  if(!is.na(df[x,"poss.player"])){
    y <- 1
    while (y <= length(players)) {
      if (((tolower(df[x,"poss.player"])==tolower(players[y]))) & (df[x,"poss.player"] != players[y])) {
        df[x,"poss.player"] <- players[y]
      }
      y <- y + 1
    }
  }
  #checks def.player
  if(!is.na(df[x,"def.player"])){
    z <- 1
    while (z <= length(players)) {
      if (((tolower(df[x,"def.player"])==tolower(players[z]))) & (df[x,"def.player"] != players[z])) {
        df[x,"def.player"] <- players[z]
      }
      z <- z + 1
    }
  }
  x <- x + 1
}

##CALCULATE MISSING PLAYER DATA---------
### Deletes metadata from df & converts "-", " ", and blank values to NAs
df <- df[grep("kickoff", df[,"poss.action"])[1]:nrow(df),]
df[(df) == "-"] <- NA
df[(df) == " "] <- NA
df[(df) == ""] <- NA
### Checks if a "poss.player" and "def.player" value is for a certain team, and then assigns the team value appropriately
df[grepl(paste(paste0("^", homedata$poss.player, "$"), collapse ="|"), df[,"poss.player"]), "poss.team"] <- hometeam 
df[grepl(paste(paste0("^", awaydata$poss.player, "$"), collapse ="|"), df[,"poss.player"]), "poss.team"] <- awayteam
df[grepl(paste(paste0("^", homedata$poss.player, "$"), collapse ="|"), df[,"def.player"]), "def.team"] <- hometeam 
df[grepl(paste(paste0("^", awaydata$poss.player, "$"), collapse ="|"), df[,"def.player"]), "def.team"] <- awayteam
### Checks if a "poss.player" and "def.player" value is for a certain position, and then assigns the position value appropriately
df[grepl(paste(paste0("^", ref[ref[,"poss.position"]=="GK","poss.player"], "$"), collapse ="|"), df[,"poss.player"]), "poss.position"] <- "GK" 
df[grepl(paste(paste0("^", ref[ref[,"poss.position"]=="D","poss.player"], "$"), collapse ="|"), df[,"poss.player"]), "poss.position"] <- "D" 
df[grepl(paste(paste0("^", ref[ref[,"poss.position"]=="M","poss.player"], "$"), collapse ="|"), df[,"poss.player"]), "poss.position"] <- "M" 
df[grepl(paste(paste0("^", ref[ref[,"poss.position"]=="F","poss.player"], "$"), collapse ="|"), df[,"poss.player"]), "poss.position"] <- "F" 
df[grepl(paste(paste0("^", ref[ref[,"def.position"]=="GK","def.player"], "$"), collapse ="|"), df[,"def.player"]), "def.position"] <- "GK" 
df[grepl(paste(paste0("^", ref[ref[,"poss.position"]=="D","def.player"], "$"), collapse ="|"), df[,"def.player"]), "def.position"] <- "D" 
df[grepl(paste(paste0("^", ref[ref[,"poss.position"]=="M","def.player"], "$"), collapse ="|"), df[,"def.player"]), "def.position"] <- "M" 
df[grepl(paste(paste0("^", ref[ref[,"poss.position"]=="F","def.player"], "$"), collapse ="|"), df[,"def.player"]), "def.position"] <- "F"

rm(ref)

##INVERTIBLE FUNCTION----------
##Function to determine if an action's location is invertible based on the
##location of certain opposing players' action
actionIsInvertible <- function(action, col) {
  grepl("pressure|challenge|aerial|tackle|dispossess|dribble|pass|move|take|shots",df[action, col])
}

##CONVERT SHORTCUTS----------
x <- 1
while (x <= nrow(df)) {
  ##Convert "poss.action" shortcuts
  if (grepl("^sgk", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "shots.stopped.by.gk"
  }
  if (grepl("^sdef", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "shots.stopped.by.def"
  }
  if (grepl("^sb", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "shots.blocked"
  }
  if (grepl("^sc", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "shots.scored"
  }
  if (grepl("^sm", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "shots.missed"
  }
  if (grepl("^pf", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "passes.f"
  }
  if (grepl("^ps", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "passes.s"
  }
  if (grepl("^pb", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "passes.b"
  }
  if (grepl("^m", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "movement"
  }
  if (grepl("^tkw", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "take.on.won"
  }
  if (grepl("^tkl", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "take.on.lost"
  }
  if (grepl("^d", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "dispossessed"
  }
  if (grepl("^lt", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "lost.touch"
  }
  if (grepl("^aw", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "aerial.won"
  }
  if (grepl("^al", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "aerial.lost"
  }
  if (grepl("^r", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "recoveries"
  }
  if (grepl("^bs", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "ball.shield"
  }
  if (grepl("^cl", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "clearances"
  }
  if (grepl("^playcutoff", df[x,"poss.action"])) {
    df[x,"poss.action"] <- "playcutoffbybroadcast"
  }
  ##Convert "play.type" shortcuts
  if (grepl("^th ", df[x,"play.type"])) {
    df[x,"play.type"] <- "through"
  }
  if (grepl("^lay", df[x,"play.type"])) {
    df[x,"play.type"] <- "lay.off"
  }
  if (grepl("^cc", df[x,"play.type"])) {
    df[x,"play.type"] <- "corner.crosses"
  }
  if (grepl("^dc", df[x,"play.type"])) {
    df[x,"play.type"] <- "deep.crosses"
  }
  if (grepl("^s", df[x,"play.type"])) {
    df[x,"play.type"] <- "switch"
  }
  if (grepl("^lay", df[x,"play.type"])) {
    df[x,"play.type"] <- "lay.off"
  }
  if (grepl("^flick", df[x,"play.type"])) {
    df[x,"play.type"] <- "flick.on"
  }
  if (grepl("^ti", df[x,"play.type"])) {
    df[x,"play.type"] <- "throw.in"
  }
  if (grepl("^fk", df[x,"play.type"])) {
    df[x,"play.type"] <- "free.kick"
  }
  if (grepl("^h", df[x,"play.type"])) {
    df[x,"play.type"] <- "headed"
  }
  if (grepl("^ck", df[x,"play.type"])) {
    df[x,"play.type"] <- "corner.kick"
  }
  if (grepl("^gk$|^gk ", df[x,"play.type"])) {
    df[x,"play.type"] <- "goal.kick"
  }
  if (grepl("^gkt", df[x,"play.type"])) {
    df[x,"play.type"] <- "gk.throws"
  }
  if (grepl("^gkdk", df[x,"play.type"])) {
    df[x,"play.type"] <- "gk.drop.kick"
  }
  if (grepl("^pk", df[x,"play.type"])) {
    df[x,"play.type"] <- "penalty.kick"
  }
  ##Convert "def.action" shortcuts
  if (grepl("^dbs", df[x,"def.action"])) {
    df[x,"def.action"] <- "ball.shield"
  }
  if (grepl("^bs", df[x,"def.action"])) {
    df[x,"def.action"] <- "ball.shield"
  }
  if (grepl("^dis", df[x,"def.action"])) {
    df[x,"def.action"] <- "dispossessed"
  }
  if (grepl("^ds", df[x,"def.action"])) {
    df[x,"def.action"] <- "dispossessed"
  }
  if (grepl("^dlt", df[x,"def.action"])) {
    df[x,"def.action"] <- "dispossessed"
  }
  if (grepl("^tb", df[x,"def.action"])) {
    df[x,"def.action"] <- "tackles.ball"
  }
  if (grepl("^tba", df[x,"def.action"])) {
    df[x,"def.action"] <- "tackles.ball.away"
  }
  if (grepl("^tbw", df[x,"def.action"])) {
    df[x,"def.action"] <- "tackles.ball.won"
  }
  if (grepl("^dtm", df[x,"def.action"])) {
    df[x,"def.action"] <- "dribbled.tackles.missed"
  }
  if (grepl("^dor", df[x,"def.action"])) {
    df[x,"def.action"] <- "dribbled.out.run"
  }
  if (grepl("^dt", df[x,"def.action"])) {
    df[x,"def.action"] <- "dribbled.turned"
  }
  if (grepl("^p", df[x,"def.action"])) {
    df[x,"def.action"] <- "pressured"
  }
  if (grepl("^ch", df[x,"def.action"])) {
    df[x,"def.action"] <- "challenged"
  }
  if (grepl("^bl", df[x,"def.action"])) {
    df[x,"def.action"] <- "blocks"
  }
  if (grepl("^int", df[x,"def.action"])) {
    df[x,"def.action"] <- "interceptions"
  }
  if (grepl("^bd", df[x,"def.action"])) {
    df[x,"def.action"] <- "ball.shield"
  }
  if (grepl("^cl", df[x,"def.action"])) {
    df[x,"def.action"] <- "clearances"
  }
  if (grepl("^aw", df[x,"def.action"])) {
    df[x,"def.action"] <- "aerial.won"
  }
  if (grepl("^al", df[x,"def.action"])) {
    df[x,"def.action"] <- "aerial.lost"
  }
  ##Convert "poss.player.disciplinary" shortcuts
  if (grepl("^fw", df[x,"poss.player.disciplinary"])) {
    df[x,"poss.player.disciplinary"] <- "fouls.won"
  }
  if (grepl("^fc", df[x,"poss.player.disciplinary"])) {
    df[x,"poss.player.disciplinary"] <- "fouls.conceded"
  }
  ##Convert "poss.notes" shortcuts
  if (grepl("^keep.poss|^kept.poss", df[x,"poss.notes"])) {
    df[x,"poss.notes"] <- "out.of.bounds.keep.poss"
  }
  if (grepl("^lost.poss|^lose", df[x,"poss.notes"])) {
    df[x,"poss.notes"] <- "out.of.bounds.lost.poss"
  }
  ##Convert "def.player.disciplinary" shortcuts
  if (grepl("^fw", df[x,"def.player.disciplinary"])) {
    df[x,"def.player.disciplinary"] <- "fouls.won"
  }
  if (grepl("^fc", df[x,"def.player.disciplinary"])) {
    df[x,"def.player.disciplinary"] <- "fouls.conceded"
  }
  x <- x + 1
}

##FILLS IN BLANK DEF.LOCATION CELLS----------
## Goes down the entire data.frame, row by row, and fills in blank "def.location" cells
cantDetermine <- c()
x <- 1
while (x <= nrow(df)) {
  ## checks if "def.location" is NA for actions that can have their location determined
  ## based on the inverse of certain actions from opposing players
  if (is.na(df[x,"def.location"])) {
    col <- "def.action"
    if (actionIsInvertible(x, col)) {
      ## Check if "poss.location is filled in
      if(!is.na(df[x, "poss.location"])) {
        # find location of poss.player
        location <- df[df[,"event"] == df[x,"event"],"poss.location"][1]
        # assign the opposite as "def.location"
        df[x,"def.location"] <- opposites[as.character(opposites[,"posslocations"]) == as.character(location),"deflocations"]
      }
      ## if "poss.location" is an NA, we can't determine the blank "def.location" value
      else if (is.na(df[x, "poss.location"])) {
        paste(x, "has an NA poss.location value")
      }
    } 
    ## checks if "def.location" is blank for interceptions, which can have its location
    ## determined based on location of next action, which is by definition by the intercepting
    ## player at the location of the interception
    else if (grepl("interceptions", df[x,"def.action"])) {
      # find location of next poss.player
      e <- df[x,"event"][1]
      ne <- e + 1
      location <- df[df[,"event"] == df[ne,"event"],"poss.location"][1]
      # assign it as the "def.location"
      df[x,"def.location"] <- location
    }
    ## Otherwise, NA values "def.location" can't be determined
    else {
      if(!is.na(df[x,"def.action"])){
        cantDetermine <- c(cantDetermine, df[x,"event"])
      }
    }
  }
  x <- x + 1
}
print("The following events have blank def.location")
cantDetermine

##FILLS IN BLANK POSS.LOCATION CELLS & DETERMINE COMPLETED PASSES----------
df$poss.action <- as.character(df$poss.action)
e <- 1
while (e <= max(df$event, na.rm = TRUE)) {
  # get row for "poss.action" for "event"
  row <- grep(e,df[,"event"])[1]
  # get event value and row for "poss.action" for next event
  nextevent <- e + 1
  nextrow <- grep(nextevent,df[,"event"])[1]
  # checks these conditions which must be fulfilled for the pass attempt to be a completed pass
  if(
    # checks if the event is a pass attempt
    grepl("pass", df[df[,"event"] == e,"poss.action"][1]) &&
    
    # checks if the "poss.play.destination" value is blank and needs to be filled
    #don't remember why I put this in and don't think it's necessary
    #is.na(df[row,"poss.play.destination"]) &&
    
    # checks if the next event isn't a stop in play or break in broadcast
    # these instances should have the "poss.play.destination" value filled in anyways
    !grepl("playcutoffbybroadcast|offside|stoppage|
           substitution|halftime|fulltime|end.of", df[nextrow,"poss.action"]) &&
    
    # checks if next event isn't a lost aerial duel
    !grepl("aerial.lost", df[nextrow, "poss.action"]) &&
    
    # checks if next event, which shouldn't be a lost aerial duel, has the same team as the possessing team
    df[row,"poss.team"] == df[nextrow,"poss.team"] &&
    
    # if the above conditions are satisfied, check "def.action" to make sure it does not
    # include defensive actions that would still indicate an unsuccessful pass attempt
    !grepl("interceptions|blocks|clearances|shield|high.balls.won|smothers.won|loose.balls.won", df[df[,"event"] == e,"def.action"])
    )
    # if the previous test is passed, then it's a completed pass! Now, to determine the destination of the pass
    {
    # use location from the next event as
    # the poss.play.destination value
    df[row,"poss.play.destination"] <- df[nextrow, "poss.location"]
    
    # one last thing, add a ".c" to the end of the "poss.action" value to signify that it's a completed pass
    string <- df[row,"poss.action"]
    df[row,"poss.action"] <- paste0(string, ".c")
    
    # move on to the next event
    e <- e + 1
  }
    # if the previous test is not passed, then the event is not a completed pass
    # move on to the next event
  else {
    e <- e + 1
  }
}

##FILLS IN BLANK POSS.PLAY.DESTINATION CELLS--------
#For when defensive action can be used to determine "poss.play.destination"
#df$poss.play.destination <- as.character(df$poss.action)
#e <- 1
#while (e <= length(unique(df$event))){
#  #check if is a nonblank poss action of a certain type with blank poss play dest. value
#  if (!is.na(df[df[,"event"]==e,"poss.action"][1]) & is.na(df[df[,"event"]==e,"poss.play.destination"][1])){
#    #check if any def actions are of a certain type
#    if(grepl("interception|blocks|clearances|ball.shield",
#             paste(unlist(strsplit(df[df[,"event"] == e,"def,action"], ","),
#                          recursive=TRUE), sep="", collapse=" "))) {
#      #if ball shield present, set this as the destination
#    }
#  }
#}


rm(opposites, deflocations, location, awayteam, hometeam, posslocations, homedata, awaydata, teams)
