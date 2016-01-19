## Functions for reading and tidying up a raw Excel file of match stats
## and turning it into a data.frame in your environment

## Install if necessary
require(xlsx)
## IMPORTANT: "match" must be set as a string value, or this won't work
## Might take a while to create. Takes about two minutes. Hold tight.
df <- read.xlsx(match, sheetName = "match")
## Deletes all rows before kickoff and saves them in a separate data frame "head"
### Gets the row number where the match starts, stores it in x
start <- grep("kickoff", df[,"poss.action"])
### Creates a data frame of all columns from row 1 to row x, for reference
ref <- df[1:(start-1),]
### Then, deletes everything before kickoff from the df data.frame
df <- df[start:nrow(df),]
## Changes select column factors to integers
df$event <- as.integer(as.character(df[,"event"]))
## Gets rid of all those "-"'s and turn them into NAs
df[(df) == "-"] <- NA

rm(start)

## There are a lot of blank spaces where the team acronym should be. The code below fills them in
## Creates a vector for the "home" team and the "away" team
teams <- as.character(unique(ref$poss.team))
hometeam <- teams[1]
awayteam <- teams[2]
## Creates two different vectors from each team's list of players
homeplayers <- as.character(ref[ref[,"poss.team"] == hometeam,"poss.player"])
awayplayers <- as.character(ref[ref[,"poss.team"] == awayteam,"poss.player"])
## Uses the above vectors & values to check df rows where "poss.team" and "def.team"
## is blank, and fills in the appropriate value based on what the player name is in 
## "poss.player" or "def.player"
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

## Goes down the entire data.frame, row by row, and fills in blank "def.location" cells
x <- 1
while (x <= nrow(df)) {
  ## checks if "def.location" is blank for "pressured" events
  if (is.na(df[x,"def.location"]) && grepl("pressured", df[x,"def.action"])) {
    # find location of poss.player
    location <- df[df[,"event"] == df[x,"event"],"poss.location"][1]
    # assign the opposite as "def.location"
    df[x,"def.location"] <- opposites[as.character(opposites[,"posslocations"]) == as.character(location),"deflocations"]
    x <- x + 1
  } 
  ## checks if "def.location" is blank for "challenged" events
  else if (is.na(df[x,"def.location"]) && grepl("challenged", df[x,"def.action"])) {
    # find location of poss.player
    location <- df[df[,"event"] == df[x,"event"],"poss.location"][1]
    # assign the opposite as "def.location"
    df[x,"def.location"] <- opposites[as.character(opposites[,"posslocations"]) == as.character(location),"deflocations"]
    x <- x + 1
  } 
  ## checks if "def.location" is blank for aerial duels
  else if(is.na(df[x,"def.location"]) && grepl("aerial", df[x,"def.action"])) {
    # find location of poss.player
    location <- df[df[,"event"] == df[x,"event"],"poss.location"][1]
    # assign the opposite as "def.location"
    df[x,"def.location"] <- opposites[as.character(opposites[,"posslocations"]) == as.character(location),"deflocations"]
    x <- x + 1    
  } 
  ## checks if "def.location" is blank for tackles, dribbles, or dispossessions
  else if(is.na(df[x,"def.location"]) && grepl("tackle|dispossess|dribble", df[x,"def.action"])) {
    # find location of poss.player
    location <- df[df[,"event"] == df[x,"event"],"poss.location"][1]
    # assign the opposite as "def.location"
    df[x,"def.location"] <- opposites[as.character(opposites[,"posslocations"]) == as.character(location),"deflocations"]
    x <- x + 1    
  }
  ## checks if "def.location" is blank for interceptions
  else if(is.na(df[x,"def.location"]) && grepl("interceptions", df[x,"def.action"])){
    # find location of next poss.player
    e <- df[x,"event"][1]
    ne <- e + 1
    location <- df[df[,"event"] == df[ne,"event"],"poss.location"][1]
    # assign it as the "def.location"
    df[x,"def.location"] <- location
    x <- x + 1
  }
  else {
    x <- x + 1
  }
}

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
    ){
    # if the previous test is passed, then it's a completed pass!
    # now we take the location from the next event and use that as
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