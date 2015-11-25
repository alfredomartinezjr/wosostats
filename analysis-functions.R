## The following are functions you'll need to create the different tables
## for the different types of match stats and, ultimately, the R Markdown
## document that has these functions all over it.

# 1.
## Function that takes a pattern and a column name
## The pattern will be the factors as well
createTable <- function(pattern, col, df) {
  ## Get event number for all events that have that pattern in the specified column
  e <- df[df[,col] %in% c(pattern),"event"]
  e <- paste0("^", e, "$")
  
  ## Go back to the original data frame and get all rows that is an "event" number that 
  ## matches a value in the "e" vector
  d2 <- df[grep(paste(e,collapse="|"), df[,"event"]),]
  
  ## Take out all rows that are just there to signify a broadcast interruption
  d2e <- d2[d2[,"poss.player"] != "playcutoffbybroadcast",]
  
  ## Include only the rows with the pattern events (excludes defensive plays that added rows to an event)
  d2e <- d2e[d2e[,col] %in% c(pattern),]
  
  ### Set factors, in case all events specified in the pattern don't show up, so that they show up in the table
  d2e[,col] <- factor(as.character(d2e[,col]), levels=c(pattern))
  
  ## Create the table
  t <- table(d2e$poss.player, d2e[,col])
  t <- data.frame(unclass(t))
  t
}

# 2.
## Function that creates a data frame of only events that fit a certain pattern
createDataFrame <- function(pattern, col, df) {
  ## Get event number for all events that have that pattern in the specified column
  e <- df[df[,col] %in% c(pattern),"event"]
  
  ## Add a "^" and "$" to each event name
  e <- paste0("^", e, "$")
  
  ## Go back to original data frame and get all rows with an "event" value from e
  df[grep(paste(e, collapse = "|"), df[,"event"]),]
}

# 3.
## Function that fills in blanks with values from above in specified columns
## If this doesn't work, check to see if columns 7 through 13 have changed
## May want to just plug in the names manually, instead of using "7:13"
fillBlanks <- function(df) {
  x <- 1
  while (x <= nrow(df)) {
    if (df[x,"poss.player"] == "") {
      df[x,7:13] <- df[x-1,7:13] 
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
  t <- t[t[,"poss.player"] != "playcutoffbybroadcast",]
  
  ## Create the data.frame from which we will create the final table
  ## Takes only unique instances of "event" numbers
  t <- t[!duplicated(t$event),c("event", "time", "poss.team", "poss.player" ,"poss.player.event", "poss.location", "ball.type", "poss.notes")]
  t
}