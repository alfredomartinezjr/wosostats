## The following are functions you'll need to create the different tables
## for the different types of match stats and, ultimately, the R Markdown
## document that has these functions all over it.

## Function that takes a pattern and a column name
## The pattern will be the factors as well

createStatsTable <- function(pattern=character(), target_col=character(), 
                             source_df, new_sumcol = NA, new_divcol = list(), 
                             stat_names = NA, drop_cols = NA, team = "poss",
                             isPassing=FALSE, extra_col_names=character(),
                             location="none") {
  source_df <- source_df[!(grepl("end.of.match|stoppage.in.play|halftime|playcutoff|kickoff", source_df[, "poss.action"])), ]
  if(isPassing) {
    pattern <- c("completed", "attempts", "pct",  "passes.f.c", "passes.f",
                 "passes.s.c", "passes.s", "passes.b.c", "passes.b", extra_col_names)
    target_col <- "poss.action"
  }
  # Set factors, in case all events specified in the pattern don't show up, so that they show up in the table
  source_df[,target_col] <- factor(as.character(source_df[,target_col]), levels=c(pattern))
  ## Create the table
  if(location=="none"){
    if(team == "poss") {
      stats_table <- table(paste(source_df$poss.team, source_df$poss.number, source_df$poss.player, sep="_"), source_df[, target_col])
    } else if (team == "def") {
      stats_table <- table(paste(source_df$def.team, source_df$def.number, source_df$def.player, sep="_"), source_df[, target_col])
    }
    stats_table <- data.frame(unclass(stats_table))
    stats_table <- cbind(Player=sapply(rownames(stats_table),function(x) strsplit(x, "_")[[1]][3]), 
                         Team = sapply(rownames(stats_table),function(x) strsplit(x, "_")[[1]][1]),
                         Number = sapply(rownames(stats_table),function(x) strsplit(x, "_")[[1]][2]),
                         stats_table)
    if(nrow(source_df)==0){
      stats_table <- cbind(data.frame(Player=character(),Team=character(),Number=integer()),stats_table)
    }
    rownames(stats_table) <- NULL
    # calculate passing stats, if isPassing is TRUE
    if(isPassing){
      ## Calculate empty columns
      stats_table$completed <- stats_table$passes.f.c + stats_table$passes.s.c + stats_table$passes.b.c
      stats_table$attempts <- rowSums(stats_table[,c("passes.f.c", "passes.f", 
                                                     "passes.s.c", "passes.s", "passes.b.c", "passes.b")])
      stats_table$pct <- stats_table$completed/stats_table$attempts
      stats_table <- stats_table[,-grep("passes\\.[fbs]",names(stats_table))]
    }
    # calculate summing and division called in arguments
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
    # set column names and drop anything that's not needed
    if(!(length(stat_names) == 1 && is.na(stat_names))) {
      colnames(stats_table) <- c(colnames(stats_table[1:(ncol(stats_table)-length(stat_names))]),stat_names)
    }
    if(!(length(drop_cols) == 1 && is.na(drop_cols))) {
      stats_table <- stats_table[!names(stats_table) %in% drop_cols]
    }
  } else if(location=="zones" | location=="thirds") {
    if(location=="zones"){
      locations <- c("D6", "D18", "DL", "DC","DR", "DML", "DMC", "DMR", "AML", "AMC", "AMR", "AL", "AC", "AR", "A18", "A6")
      if(team=="def"){
        opposites <- c("A6", "A18", "AR", "AC", "AL", "AMR", "AMC", "AML", "DMR", "DMC", "DML", "DR", "DC", "DL", "D18", "D6")
      }
    } else if(location=="thirds"){
      locations <- c("D3", "M3", "A3")
      if(team=="def"){
        opposites <- c("A3", "M3", "D3")
      }
    } else if(location=="wings"){
      locations <- c("L3", "C3", "R3")
      if(team=="def") {
        opposites <- c("R3", "C3", "R3")
      }
    }
    for(index in locations) {
      if(team == "poss") {
        location_df <- source_df[source_df[,index] == TRUE,]
        location_table <- table(paste(location_df$poss.team, location_df$poss.number, location_df$poss.player, sep="_"), location_df[,target_col])
      } else if (team == "def") {
        location_df <- source_df[source_df[,opposites[grep(index, locations)]] == TRUE,]
        location_table <- table(paste(location_df$def.team, location_df$def.number, location_df$def.player, sep="_"), location_df[,target_col])
      }
      location_table <- data.frame(unclass(location_table))
      location_table <- cbind(Player=sapply(rownames(location_table),function(x) strsplit(x,"_")[[1]][3]), 
                              Team = sapply(rownames(location_table),function(x) strsplit(x,"_")[[1]][1]),
                              Number = sapply(rownames(location_table),function(x) strsplit(x,"_")[[1]][2]),
                              location_table)
      if(nrow(location_df)==0){
        location_table <- cbind(data.frame(Player=character(),Team=character(),Number=integer()),location_table)
      }
      rownames(location_table) <- NULL
      # calculate passing stats, if isPassing is TRUE
      if(isPassing){
        ## Calculate empty columns
        location_table$completed <- location_table$passes.f.c + location_table$passes.s.c + location_table$passes.b.c
        location_table$attempts <- rowSums(location_table[,c("passes.f.c", "passes.f", 
                                                       "passes.s.c", "passes.s", "passes.b.c", "passes.b")])
        location_table$pct <- location_table$completed/location_table$attempts
        location_table <- location_table[,-grep("passes\\.[fbs]",names(location_table))]
      }
      # calculate summing and division called in arguments
      if(!(length(new_sumcol) == 1 && is.na(new_sumcol))){
        location_table[,new_sumcol$name] <- rowSums(location_table[,grep(new_sumcol$summed,names(location_table))])
      }
      if(length(new_divcol) > 0) {
        if(length(new_divcol$numerator) == 1 && length(new_divcol$denominator) == 1) {
          location_table[,new_divcol$name] <- location_table[,new_divcol$numerator]/location_table[,new_divcol$denominator]
        } else if(length(new_divcol$numerator) > 1 && length(new_divcol$denominator) == 1) {
          location_table[,new_divcol$name] <- rowSums(location_table[,new_divcol$numerator])/location_table[,new_divcol$denominator]
        } else if(length(new_divcol$numerator) > 1 && length(new_divcol$denominator) > 1) {
          location_table[,new_divcol$name] <- rowSums(location_table[,new_divcol$numerator])/rowSums(location_table[,new_divcol$denominator])
        } else if(length(new_divcol$numerator) == 1 && length(new_divcol$denominator) > 1) {
          location_table[,new_divcol$name] <- location_table[,new_divcol$numerator]/rowSums(location_table[,new_divcol$denominator])
        }
      }

      colnames(location_table) <- c(colnames(location_table[1:(ncol(location_table)-length(stat_names))]),paste(index,stat_names))
      if(!(length(drop_cols) == 1 && is.na(drop_cols))) {
        location_table <- location_table[!names(location_table) %in% drop_cols]
      }
      
      if(exists("stats_table")){
        stats_table <- merge(location_table, stats_table, by=c("Player","Team","Number"), all=TRUE)
      } else {
        stats_table <- location_table
      }
    }
  }
  
  stats_table
}

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
createSubset <- function(pattern, col, source_df = match_sheet, clean = FALSE) {
  # Function that creates a subset of a source_df data frame based on certain patterns in a given column
  #events <- source_df[source_df[, col] %in% c(pattern), "event"] # all events with pattern in specified column
  events <- source_df[grepl(pattern, source_df[, col]), "event"] # all events with pattern in specified column
  events <- paste0("^", events, "$") # adds "^" and "$" to each event value
  source_df <- source_df[grep(paste(events, collapse = "|"), source_df[,"event"]),] # subsets to rows with event number
  if(clean == TRUE) {
    source_df <- fillBlanks(source_df)
    source_df <- source_df[!grepl("end.of.match|stoppage.in.play|halftime|playcutoff|kickoff", source_df[, "poss.action"]), ]
    source_df <- source_df[!duplicated(source_df$event),]
  }
  source_df
}
## Adds column that fills in yes/no values based on qualifiers
addColumnForQualifier <- function(newcol, pattern, patternLocation, ogdf, ndf, invert=FALSE) {
  newcol_vec <- logical(nrow(ndf))
  if(nrow(ndf) > 0){
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
  if(length(pattern_locations) == 1){
    pattern_locations <- rep(pattern_locations, length(patterns))
  }
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