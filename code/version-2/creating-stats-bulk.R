library(RCurl)
database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
database <- read.csv(textConnection(database), stringsAsFactors = FALSE)

#This just pulls every match in the database. This could be a long vector
matches <- database[,"match.csv.link"]


#Create empty list
stats_list <- vector("list", 0)

#Runs the creating-stars.R code through each match in "matches"
#Returns a list of data.frames
for (i in matches) {
  matchURL <- i
  source("creating-stats.R")
  stats_list[[i]] <- all
}

#Writes csv files in bulk into whatever directory you're in
#for (i in 1:length(stats_list)) {
#  file_name <- strsplit(matches[i], "/")[[1]][[length(strsplit(matches[i], "/")[[1]])]]
#  write.csv(stats_list[[i]], file=file_name, row.names = FALSE)
#}