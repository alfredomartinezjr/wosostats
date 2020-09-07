# WoSo Stats
This GitHub repository is for the WoSo Stats project, dedicated to collecting, analyzing, and distributing data about women's soccer from around the world. In this repository you'll find a variety of code, files, and documents relevant to our work.

Most of you are probably here because you want the advanced stats. To get right to it, run the following in R, which will create a `tbl_all` data frame in your working directory with a large collection of stats for each player in each game in the database.
``` r
source("https://raw.githubusercontent.com/alfredomartinezjr/wosostats/master/calc_stats.R")
```

You can also directly download the table [here](https://raw.githubusercontent.com/alfredomartinezjr/wosostats/master/data/tbl_all.csv). And you can view and interact with the stats in [the WoSo Stats Shiny app](https://amj2012.shinyapps.io/wosostats/). A glossary for what the columns in the `tbl_all` table represent are in the [Glossary](https://wosostats.com/glossary/). 

The database is made up a variety of tables, listed below, that are sourced from Excel match logs by running the `lineups_to_db.R` and `excel_to_db.R` scripts. Details of what the various values across the database represent, and how the matches were logged, can be found in the ["How matches are logged"](https://wosostats.com/how-matches-are-logged/) section of the WoSo Stats website.
* [lineups](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/lineups.csv)
* [rosters](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/rosters.csv)
* [matches](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/matches.csv)
* [events](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/events.csv)
* [event_type](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/event_type.csv)
* [poss_notes](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/poss_notes.csv)
* [defending](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/defending.csv)
* [def_notes](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/def_notes.csv)
* [poss_discipline](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/poss_discipline.csv)
* [def_discipline](https://wosostats-data-database-public.s3-us-west-1.amazonaws.com/def_discipline.csv)

The Excel match logs can be found in [a public Google Drive folder](https://drive.google.com/drive/folders/13-8Ws14GougTk_FZBv4k-VUCaRyml1hj?usp=sharing). 

Currently, our biggest database is for 2016 USWNT matches and for the 2016 NWSL season. While this project is still being occasionally worked on, matches are not being logged as regularly as in the past and it is more likely that this project will try to log and archive data from historical matches (pre-2016) as advanced stats have recently been tracked more regularly for certain women's soccer competitions.

The main Twitter account for this project where you'll find the most up-to-date info on recent news, stats, and overall developments is [@WoSoStats](https://twitter.com/wosostats). The website is at www.wosostats.com. Any questions about the project or how to join can be directed to the Twitter account or to Alfredo at wosostats.team@gmail.com.
