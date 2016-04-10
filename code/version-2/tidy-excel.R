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

##FINDS METADATA----------
## Delete all rows before kickoff and saves them in a separate data frame "head"
### Gets the row number where the match starts, stores it in x
start <- grep("kickoff", df[,"poss.action"])
### Creates a data frame of all columns from row 1 to row x, for reference
ref <- df[1:(start-1),]
### Then, deletes everything before kickoff from the df data.frame
df <- df[start:nrow(df),]
### Changes select column factors to integers
df$event <- as.integer(as.character(df[,"event"]))
### Changes select column factors to characters
df$poss.action <- as.character(df[,"poss.action"])
df$play.type <- as.character(df[,"play.type"])
df$def.action <- as.character(df[,"def.action"])
df$poss.player.disciplinary <- as.character(df[,"poss.player.disciplinary"])
df$poss.notes <- as.character(df[,"poss.notes"])
df$def.player.disciplinary <- as.character(df[,"def.player.disciplinary"])
### Gets rid of all those "-" and "" and turn them into NAs
df[(df) == "-"] <- NA
df[(df) == ""] <- NA
rm(start)
## There are a lot of blank spaces where the team acronym should be. The code below fills them in
### Creates a vector for the "home" team and the "away" team
teams <- as.character(unique(ref$poss.team))
hometeam <- teams[1]
awayteam <- teams[2]
### Creates two different vectors from each team's list of players
homeplayers <- as.character(ref[ref[,"poss.team"] == hometeam,"poss.player"])
awayplayers <- as.character(ref[ref[,"poss.team"] == awayteam,"poss.player"])
### Uses the above vectors & values to check df rows where "poss.team" and "def.team"
### is blank, and fills in the appropriate value based on what the player name is in 
### "poss.player" or "def.player"
df[grepl(paste(paste0("^", homeplayers, "$"), collapse ="|"), df[,"poss.player"]), "poss.team"] <- hometeam 
df[grepl(paste(paste0("^", awayplayers, "$"), collapse ="|"), df[,"poss.player"]), "poss.team"] <- awayteam
df[grepl(paste(paste0("^", homeplayers, "$"), collapse ="|"), df[,"def.player"]), "def.team"] <- hometeam 
df[grepl(paste(paste0("^", awayplayers, "$"), collapse ="|"), df[,"def.player"]), "def.team"] <- awayteam
rm(ref)
## Create data frame with opposites of each location
posslocations <- c("A6", "A18", "A3L", "A3C", "A3R", "AM3L", "AM3C", 
                   "AM3R", "DM3L", "DM3C", "DM3R", "D3L", "D3C", "D3R", 
                   "D18", "D6")
deflocations <- c("D6", "D18", "D3R", "D3C", "D3L", "DM3R", "DM3C",
                  "DM3L", "AM3R", "AM3C", "AM3L", "A3R", "A3C", "A3L", 
                  "A18", "A6")
opposites <- data.frame(posslocations, deflocations)

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
  if (grepl("^th", df[x,"play.type"])) {
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
  if (grepl("^ds", df[x,"def.action"])) {
    df[x,"def.action"] <- "dispossessed"
  }
  if (grepl("^dlt", df[x,"def.action"])) {
    df[x,"def.action"] <- "dispossessed"
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
      print(paste0(x, "'s defensive location can't be determined. poss.location is NA"))
    }
  }
  x <- x + 1
}

##FILLS IN BLANK POSS.LOCATION CELLS----------
df$poss.action <- as.character(df$poss.action)
e <- 1
while (e <= max(df$event)) {
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
    is.na(df[row,"poss.play.destination"]) &&
    
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

rm(opposites, awayplayers, homeplayers, deflocations, awayteam, hometeam, posslocations, teams)
