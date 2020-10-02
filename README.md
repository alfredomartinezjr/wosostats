# WoSo Stats
`WoSo Stats` is an R package for the WoSo Stats project, dedicated to collecting, analyzing, and distributing data about women's soccer from around the world. The functions in the R package allow you to access and analyze the database.

This package is currently in the very earliest stages of development. However, it can be installed like so:
``` r
devtools::install_github("alfredomartinezjr/wosostats")
```

More documentation for the R package is pending.

The database is made up a variety of tables, listed below, that are sourced from Excel match logs.Details of what the various values across the database represent, and how the matches were logged, can be found in the ["How matches are logged"](https://wosostats.com/how-matches-are-logged/) section of the WoSo Stats website. The R package provides functions for accessing all of these tables.
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

The WoSo Stats database largely has data for 2016 USWNT matches and for the 2016 NWSL season. While this project is still being occasionally worked on, matches are not being logged as regularly as in the past and it is more likely that this project will try to log and archive data from historical matches (pre-2016) as advanced stats have recently been tracked more regularly for certain women's soccer competitions.

The main Twitter account for this project where you'll find the most up-to-date info on recent news, stats, and overall developments is [@WoSoStats](https://twitter.com/wosostats). The website is at www.wosostats.com. Any questions about the project or how to join can be directed to the Twitter account or to Alfredo at wosostats.team@gmail.com.
