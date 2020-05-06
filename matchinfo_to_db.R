library(readr)
library(dplyr)

fpath <- "database.csv"
dat <- read_csv(fpath)
names(dat) <- gsub("\\.","_",names(dat))
dat <- select(dat, -c("year", "month", "day", "home_team",
                      "away_team", "rmd_link", "match_csv_link", 
                      "stats_csv_link", "home_FIFA_rank", 
                      "away_FIFA_rank"))
match_id <- c(100001:(100001+nrow(dat)-1))
dat <- cbind(match_id, dat)
write_csv(dat, "source/temp_database/matches.csv", na="")
