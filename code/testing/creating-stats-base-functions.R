## The following are functions you'll need to create the different tables
## for the different types of match stats and, ultimately, the R Markdown
## document that has these functions all over it.

## Function that takes a pattern and a column name
## The pattern will be the factors as well

createStatsTable <- function(pattern, target_col, source_df, new_sumcol = NA, new_divcol = list(), 
                             stat_names = NA, drop_cols = NA, team = "poss") {
  # Set factors, in case all events specified in the pattern don't show up, so that they show up in the table
  source_df <- source_df[!(grepl("end.of.match|stoppage.in.play|halftime|playcutoff|kickoff",source_df[,"poss.action"])),]
  source_df[,target_col] <- factor(as.character(source_df[,target_col]), levels=c(pattern))
  ## Create the table
  if(team == "poss") {
    stats_table <- table(paste(source_df$poss.team, source_df$poss.number, source_df$poss.player, sep="_"), source_df[,target_col])
    stats_table <- data.frame(unclass(stats_table))
    stats_table <- cbind(Player=sapply(rownames(stats_table),function(x) strsplit(x,"_")[[1]][3]), 
                         Team = sapply(rownames(stats_table),function(x) strsplit(x,"_")[[1]][1]),
                         Number = sapply(rownames(stats_table),function(x) strsplit(x,"_")[[1]][2]),
                         stats_table)
  } else if (team == "def") {
    stats_table <- table(paste(source_df$def.team, source_df$def.number, source_df$def.player, sep="_"), source_df[,target_col])
    stats_table <- data.frame(unclass(stats_table))
    stats_table <- cbind(Player=sapply(rownames(stats_table),function(x) strsplit(x,"_")[[1]][3]), 
                         Team = sapply(rownames(stats_table),function(x) strsplit(x,"_")[[1]][1]),
                         Number = sapply(rownames(stats_table),function(x) strsplit(x,"_")[[1]][2]),
                         stats_table)
  }
  if(nrow(source_df)==0){
    stats_table <- cbind(data.frame(Player=character(),Team=character(),Number=integer()),stats_table)
  }
  rownames(stats_table) <- NULL
  
  if(!(length(new_sumcol) == 1 && is.na(new_sumcol))){
    stats_table[,new_sumcol$name] <- rowSums(stats_table[,grep(new_sumcol$summed,names(stats_table))])
  }
  if(length(new_divcol) > 0) {
    if(length(new_divcol$numerator) == 1 && length(new_divcol$denominator) == 1) {
      stats_table[,new_divcol$name] <- stats_table[,new_divcol$numerator]/stats_table[,new_divcol$denominator]
    } else if(length(new_divcol$numerator) > 1 && length(new_divcol$denominator) == 1) {
      stats_table[,new_divcol$name] <- rowSums(stats_table[,new_divcol$numerator])/stats_table[,new_divcol$denominator]
    } else if(length(new_divcol$numerator) > 1 && length(new_divcol$denominator) > 1) {
      stats_table[,new_divcol$name] <- rowSums(stats_table[,new_divcol$numerator])/rowSums(stats_table[,new_divcol$denominator])
    } else if(length(new_divcol$numerator) == 1 && length(new_divcol$denominator) > 1) {
      stats_table[,new_divcol$name] <- stats_table[,new_divcol$numerator]/rowSums(stats_table[,new_divcol$denominator])
    }
  }
  if(!(length(stat_names) == 1 && is.na(stat_names))) {
    colnames(stats_table) <- c(colnames(stats_table[1:(ncol(stats_table)-length(stat_names))]),stat_names)
  }
  if(!(length(drop_cols) == 1 && is.na(drop_cols))) {
    stats_table <- stats_table[!names(stats_table) %in% drop_cols]
  }
  
  stats_table
}

## Create passing table
createPassingTable <- function(source_df, extra=character(), stat_names = character()){
  pass_table <- createStatsTable(c("completed", "attempts", "pct",  "passes.f.c", "passes.f",
                                   "passes.s.c", "passes.s", "passes.b.c", "passes.b",extra), "poss.action", source_df)
  ## Calculate empty columns
  pass_table$completed <- pass_table$passes.f.c + pass_table$passes.s.c + pass_table$passes.b.c
  pass_table$attempts <- rowSums(pass_table[,c("passes.f.c", "passes.f", 
                                               "passes.s.c", "passes.s", "passes.b.c", "passes.b")])
  pass_table$pct <- pass_table$completed/pass_table$attempts
  pass_table <- pass_table[,-grep("passes\\.[fbs]",names(pass_table))]
  
  if(length(stat_names) > 1) {
    colnames(pass_table) <- c(colnames(pass_table[1:(ncol(pass_table)-length(stat_names))]),stat_names)
  }
  
  pass_table
}

createLocationStatsTable <- function(pattern, target_col, source_df,stat_names) {
  locationZones <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML",
                     "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
  for(zone in locationZones) {
    zone_table <- createStatsTable(pattern, target_col, source_df = source_df[source_df[,zone] == TRUE,])
    
    colnames(zone_table) <- c(colnames(zone_table[1:(ncol(zone_table)-length(pattern))]),paste(zone,stat_names))
    
    if(exists("stats_table")){
      stats_table <- merge(zone_table, stats_table, by=c("Player","Team","Number"), all=TRUE)
    } else {
      stats_table <- zone_table
    }
  }
  stats_table
}



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
    if (is.na(df[x,"poss.player"]) & is.na(df[x,"poss.action"])) {
      df[x,c("poss.position", "poss.team", "poss.number" ,"poss.player", "poss.action", 
             "poss.location", "poss.play.destination")] <- df[x-1,c("poss.position", "poss.team", "poss.number", 
                                                                    "poss.player", "poss.action",
                                                                    "poss.location", "poss.play.destination")] 
    }
    x <- x + 1
  }
  df
}

## Fills in blanks and then gets rid of duplicates. Is poss-focused
createCleanDataFrame <- function(pattern, col, df, keep_cols = character(0)) {
  t <- createDataFrame(pattern, col, df)
  ## Fills in blanks with info from cell above it
  ## Then, exclude anything that marks a stoppage in time
  t <- fillBlanks(t)
  t <- t[!grepl("end.of.match|stoppage.in.play|halftime|playcutoff|kickoff",t[,"poss.action"]),]
  
  ## Create the data.frame from which we will create the final table
  ## Takes only unique instances of "event" numbers
  t <- t[!duplicated(t$event),c("event", "time", "poss.team", "poss.player", "poss.number","poss.position","poss.action", 
                                "poss.location", "poss.play.destination", "play.type", 
                                "poss.player.disciplinary", "poss.notes", keep_cols)]
  t
}

## Adds column that fills in yes/no values based on qualifiers


addColumnForQualifier <- function(newcol, pattern, patternLocation, ogdf, ndf, invert=FALSE) {
  newcol_vec <- logical(nrow(ndf))
  for(match_sheet_row in 1:length(newcol_vec)) {
    match_sheet_vec <- ogdf[ogdf[,"event"] == ndf[match_sheet_row,"event"],patternLocation]
    if(invert) {
      if (!(TRUE %in% grepl(pattern, match_sheet_vec))) {
        newcol_vec[match_sheet_row] <- TRUE
      }
    } else {
      if (TRUE %in% grepl(pattern, match_sheet_vec)) {
        newcol_vec[match_sheet_row] <- TRUE
      }
    }
  }
  ndf[,newcol] <- newcol_vec
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
    ndf <- addColumnForQualifier(names(patterns[i]), patterns[i], pattern_locations[i], ogdf, ndf)
  }
  ndf
}

# 6.
## Adds column that looks for multiple qualifiers across multiple columns
## "patterns" here will have each element be a column's pattern, and the element's name
## will be the name of the column to be searched
## "exp" is the type of expression. Such as, is it an OR, AND search


addColumnForMultiQualifiers <- function(newcol, pattern, source_df, exp, invert=FALSE) {
  newcol_vec <- logical(nrow(source_df))
  if(nrow(source_df) > 0) {
    for(match_sheet_row in 1:nrow(source_df)){
      #subsets source_df to only the row in question
      subsetcol <- source_df[match_sheet_row,names(pattern)]
      #goes through each column to find a TRUE
      if (exp == "OR" & (TRUE %in% (pattern == subsetcol))) {
        newcol_vec[match_sheet_row] <- TRUE
      } else if (exp == "AND" & !(FALSE %in% (pattern == subsetcol))) {
        newcol_vec[match_sheet_row] <- TRUE
      }
    }  
  }
  source_df[,newcol] <- newcol_vec
  source_df
}