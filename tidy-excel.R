## Functions for reading and tidying up a raw Excel file of match stats
## Install if necessary
require(xlsx)
## Takes string "matchlocation", which should be the raw GitHub URL of 
## the match stats Excel spreadsheet you want to analyze, and uses that string
## to locate the Excel spreadsheet in GitHub and writes is as data.frame "d"
## IMPORTANT: "matchlocation" must be set as a string value, or this won't work
##
## Might take a while to create. Takes about two minutes. Hold tight.
d <- read.xlsx(matchlocation, sheetName = "match")

## Delete all rows before kickoff, but be sure to save them in a separate data frame for reference
### Gets the row number where the match starts, store in x
x <- grep("kickoff", d[,"poss.player.event"])
### Create data frame of all columns from row 1 to row x, for reference
ref <- d[1:(x-1),]
### Now, delete everything before kickoff from the d data.frame
df <- d[x:nrow(d),]
## Fill in any blanks in 
df$homecount <- as.integer(as.character(df[,"homecount"]))
df$awaycount <- as.integer(as.character(df[,"awaycount"]))
df$home.score <- as.integer(as.character(df[,"home.score"]))
df$away.score <- as.integer(as.character(df[,"away.score"]))
df$event <- as.integer(as.character(df[,"event"]))

## Get rid of all those "-"'s and turn them into blank spaces NA
df[(df) == "-"] <- NA

## Fill in any rows with a "poss.player" value that don't have a "poss.team" value
## Do the same for any rows with a "def.player" value that don't have a "def.team" value
## First, create a vector for the "home" team and the "away" team
teams <- as.character(unique(ref$poss.team))
hometeam <- teams[1]
awayteam <- teams[2]
## Then create a vector for each team's list of players
homeplayers <- as.character(ref[ref[,"poss.team"] == hometeam,"poss.player"])
awayplayers <- as.character(ref[ref[,"poss.team"] == awayteam,"poss.player"])
## Now use these vectors & values to check the "poss.player" columns in df
## that have blank values in "poss.team" and fill in the appropriate value
df[grepl(paste(paste0("^", homeplayers, "$"), collapse ="|"), df[,"poss.player"]), "poss.team"] <- hometeam 
df[grepl(paste(paste0("^", awayplayers, "$"), collapse ="|"), df[,"poss.player"]), "poss.team"] <- awayteam
df[grepl(paste(paste0("^", homeplayers, "$"), collapse ="|"), df[,"def.player"]), "def.team"] <- hometeam 
df[grepl(paste(paste0("^", awayplayers, "$"), collapse ="|"), df[,"def.player"]), "def.team"] <- awayteam

## Get rid of instances where "poss.player" is an NA but has a "poss.team" value.
df[is.na(df[,"poss.player"]),"poss.team"] <- NA
df[is.na(df[,"poss.team"]),"poss.player"] <- NA

# now the fun parts

## Create data frame with opposites of each location
posslocations <- c("OPP.6.YD", "OPP.18.YD", "OPP.3RD.L", "OPP.3RD.C",
                               "OPP.3RD.R", "OPP.MID.3RD.L", "OPP.MID.3RD.C", 
                               "OPP.MID.3RD.R", "DEF.MID.3RD.L", "DEF.MID.3RD.C",
                               "DEF.MID.3RD.R", "DEF.3RD.L", "DEF.3RD.C", "DEF.3RD.R",
                               "DEF.18.YD", "DEF.6.YD")
deflocations <- c("DEF.6.YD", "DEF.18.YD", "DEF.3RD.R", "DEF.3RD.C", 
                               "DEF.3RD.L", "DEF.MID.3RD.R", "DEF.MID.3RD.C", "DEF.MID.3RD.L",
                               "OPP.MID.3RD.R", "OPP.MID.3RD.C", "OPP.MID.3RD.L", "OPP.3RD.R", 
                               "OPP.3RD.C", "OPP.3RD.L", "OPP.18.YD","OPP.6.YD")
opposites <- data.frame(posslocations, deflocations)

## Fill in "def.event" for blank "def.events" events
x <- 1
while (x <= nrow(df)) {
  ## checks if "def.location" is blank for "pressured.opp" events
  if (is.na(df[x,"def.location"]) && grepl("pressured", df[x,"def.event"])) {
    # find location of poss.player
    location <- df[df[,"event"] == df[x,"event"],"poss.location"][1]
    # assign the opposite as "def.location"
    df[x,"def.location"] <- opposites[as.character(opposites[,"posslocations"]) == as.character(location),"deflocations"]
    x <- x + 1
  } 
  ## checks if "def.location" is blank for "aerial.challenges"
  else if(is.na(df[x,"def.location"]) && grepl("aerial.challenges", df[x,"def.event"])) {
    # find location of poss.player
    location <- df[df[,"event"] == df[x,"event"],"poss.location"][1]
    # assign the opposite as "def.location"
    df[x,"def.location"] <- opposites[as.character(opposites[,"posslocations"]) == as.character(location),"deflocations"]
    x <- x + 1    
  } 
  ## checks if "def.location" is blank for tackles, dribbles, or dispossessions
  else if(is.na(df[x,"def.location"]) && grepl("tackle|dispossess|dribble", df[x,"def.event"])) {
    # find location of poss.player
    location <- df[df[,"event"] == df[x,"event"],"poss.location"][1]
    # assign the opposite as "def.location"
    df[x,"def.location"] <- opposites[as.character(opposites[,"posslocations"]) == as.character(location),"deflocations"]
    x <- x + 1    
  }
  ## checks if "def.location" is blank for blocked.passes
  else {
    x <- x + 1
  }
}

## Fill in "poss.destination" for passes
x <- 1
while(x <= nrow(df)) {
  # checks if "poss.destination" is blank for completed passes
  if (is.na(df[x,"poss.destination"]) && grepl("^passes...c$", df[x,"poss.player.event"])) {
    #find number of next event that happens after the completed pass
    e <- df[x,"event"] + 1
    #get vector of locations of that next event, in case there were 
    # multiple defensive plays associated with that event
    location <- df[df[,"event"] == e,"poss.location"][1]
    ## Assign "poss.destination as "location"
    df[x,"poss.destination"] <- location
    x <- x + 1
  } 
  # checks if "poss.destination" is blank for blocked passes
  else if (is.na(df[x,"poss.destination"]) && grepl("^passes...b$", df[x,"poss.player.event"])) {
    v <- df[df[,"event"] == df[x,"event"],c("def.event", "def.location")]
    df[x,"poss.destination"] <- v[v[,"def.event"] == "passes.blocked","def.location"]
    x <- x + 1
  } else {
    x <- x + 1
  }
}