# WoSo Stats
This GitHub repository is for the WoSo Stats project, dedicated to collecting, analyzing, and distributing data about women's soccer from around the world. In this repository you'll find a variety of code, files, and documents relevant to our work.

Most of you are probably here because you want the advanced stats. To get right to it, run the following in R, which will create a `tbl_all` data frame in your working directory with a large collection of stats for each player in each game in the database. You can also directly download the table [here](https://raw.githubusercontent.com/alfredomartinezjr/wosostats/master/stats/tbl_all.csv), for now, as this is not the intended final destination for this. Ideally it'll be hosted somewhere on the WoSo Stats site.
``` r
source("https://raw.githubusercontent.com/alfredomartinezjr/wosostats/master/calc_stats.R")
```

[The WoSo Stats Shiny app](https://amj2012.shinyapps.io/wosostats/) is an interactive web app, running on Shiny, that allows you to browse through the stats in the database.

Most of what you see in [the Shiny app](https://amj2012.shinyapps.io/wosostats/) is sourced from the [stats](https://github.com/amj2012/woso-stats/tree/master/stats) directory, although this due to change soon. The data in the stats directory, while essentially the same as the tbl_all table described above, is based on old code, is made up of over a hundred tables instead of one (albeit large) table, and will not be updated from now on. Soon, the data the Shiny app represents will be the tbl_all table.

The [shiny](https://github.com/amj2012/wosostats/tree/master/shiny/wosostats) directory is where to find the R code behind the WoSo Stats Shiny app, which can be found at [https://amj2012.shinyapps.io/wosostats](https://amj2012.shinyapps.io/wosostats/).

The [resources](https://github.com/amj2012/woso-stats/tree/master/resources) directory is where to find necessary and helpful resources for how to log stats on your own and definitions for what is being measured and analyzed. While matches are not currently being actively logged, this is still useful if you want to go through and see how matches are logged and definitions for the stats in the tbl_all table.

The [source](https://github.com/amj2012/woso-stats/tree/master/source) directory is where to find all the match spreadsheets that have been logged. The raw Excel match logs are in the excel directory. The temp_database directory is a database of tables based on the Excel files, which are a result of running the lineups_to_db.R and excel_to_db.R scripts. That last directory is not meant to be a permanent location for that database, hence the name, and the plan is to also move it off of Github and somewhere onto the WoSo Stats site.

Currently, our biggest database is for 2016 USWNT matches and for the 2016 NWSL season. While this project is still being occasionally worked on, matches are not being logged as regularly as in the past and it is more likely that this project will try to log and archive data from historical matches (pre-2016) as advanced stats have recently been tracked more regularly for certain women's soccer competitions.

The main Twitter account for this project where you'll find the most up-to-date info on recent news, stats, and overall developments is [@WoSoStats](https://twitter.com/wosostats). The website is at www.wosostats.com. Any questions about the project or how to join can be directed to the Twitter account or to Alfredo at wosostats.team@gmail.com.
