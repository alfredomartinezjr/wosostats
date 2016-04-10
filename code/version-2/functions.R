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
    #Fill in all player info
    if (is.na(df[x,"poss.player"])) {
      df[x,c("poss.position", "poss.team", "poss.player", "poss.action", 
             "poss.location", "poss.play.destination")] <- df[x-1,c("poss.position", "poss.team", 
                                                                    "poss.player", "poss.action",
                                                                    "poss.location", "poss.play.destination")] 
    }
    x <- x + 1
  }
  df
}

# 4.
## Adds column that fills in yes/no values based on qualifiers
addColumnForQualifier <- function (newcol, pattern, patternLocation, ogdf, ndf, invert=FALSE) {
  ## By default, if a condition is satisfied then the row is filled in with "yes"
  ## set "invert" as TRUE if you'd like to flip this
  if (invert == TRUE){
    success = "no"
    failure = "yes"
  } else { #default
    success = "yes"
    failure = "no"
  }
  ndf[,newcol] <- NA
  x <- 1
  while (x <= nrow(ndf)) {
    e <- ndf[x,"event"]
    v <- ogdf[ogdf[,"event"] == e,patternLocation]
    if (grepl(pattern, paste(v, collapse = "|")) == TRUE) {
      ndf[x,newcol] = success
    } else {
      ndf[x,newcol] = failure
    }
    x <- x + 1
  }
  ndf
}

# 5.
## Adds a column for qualifiers across multiple columns
## "patterns" should be a vector where each element's name is the name of each new column to
## be created, and the element is the pattern we're looking for in that instance of
## "pattern_locations"
addMultiColumnsForQualifiers <- function(patterns, pattern_locations, ogdf, ndf) {
  #creates a new column for each qualifier in "patterns"
  for(i in 1:length(patterns)) {
    ndf[,names(patterns[i])] <- NA
    ndf <- addColumnForQualifier(names(patterns[i]), patterns[i], pattern_locations[i], ogdf, ndf)
  }
  ndf
}

# 6.
## Adds column that looks for multiple qualifiers across multiple columns
## "patterns" here will have each element be a column's pattern, and the element's name
## will be the name of the column to be searched
## "exp" is the type of expression. Such as, is it an OR, AND search
addColumnForMultiQualifiers <- function(newcol, pattern, df, exp, invert=FALSE) {
  if (exp == "OR") {
    df[,newcol] <- "no"
    #then condition is satisfied if any of the columns are TRUE
    x <- 1
    while(x <= nrow(df)) {
      #subsets df to only the row in question
      subsetcol <- df[x,names(pattern)]
      y <- 1
      #goes through each column to find a TRUE
      while(y <= ncol(subsetcol)){
        if (subsetcol[,names(pattern[y])] == pattern[y]) {
          df[x,newcol] <- "yes"
        }
        y <- y + 1
      }
      x <- x + 1
    }
  } else if (exp == "AND") {
    #then condition is satisfied if all of the columns are TRUE
    df[,newcol] <- "yes"
    x <- 1
    while(x <= nrow(df)) {
      #subsets df to only the row in question
      subsetcol <- df[x,names(pattern)]
      y <- 1
      #goes through each column to find a FALSE
      while(y <= ncol(subsetcol)){
        if (subsetcol[,names(pattern[y])] != pattern[y]) {
          df[x,newcol] <- "no"
        }
        y <- y + 1
      }
      x <- x + 1
    }
  }
  df
}

# 7.
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


# 8. 
## Create passing table
createPassingTable <- function(df, extra=NA){
  if (is.na(extra[1])){
    s <- createTable(c("completed", "attempts", "pct",  "passes.f.c", "passes.f", 
                       "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", df)
  } else {
    s <- createTable(c("completed", "attempts", extra, "pct",  "passes.f.c", "passes.f", 
                       "passes.s.c", "passes.s", "passes.b.c", "passes.b"), "poss.action", df)
  }
  ## Calculate empty columns
  s$completed <- s$passes.f.c + s$passes.s.c + s$passes.b.c
  s$attempts <- rowSums(s[,c("passes.f.c", "passes.f", 
                             "passes.s.c", "passes.s", "passes.b.c", "passes.b")])
  s$pct <- s$completed/s$attempts
  if (is.na(extra[1])) {
    s <- s[,c("Player", "completed", "attempts", "pct")]
  } else {
    s <- s[,c("Player", "completed", "attempts", extra, "pct")]
  }
  s
}

#7. Create multiple tables
## useful for when you're going to need to create a lot of tables for
## given qualifiers, and then merge them all into one table
