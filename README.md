# WoSo Stats
This GitHub repository is for the WoSo Stats project, dedicated to collecting, analyzing, and distributing data about women's soccer from around the world. In this repository you'll find a variety of code, files, and documents relevant to our work.

Most of you are probably here because you want the advanced stats. To get right to it, run this in R:
``` r
source("calc_stats.R")
```

The main Twitter account for this project where you'll find the most up-to-date info on recent news, stats, and overall developments is [@WoSoStats](https://twitter.com/wosostats). The website is at www.wosostats.com. Any questions about the project or how to join can be directed to the Twitter account or to Alfredo at wosostats.team@gmail.com.

To see just some of the data that has been tracked, view [the WoSo Stats Shiny app](https://amj2012.shinyapps.io/wosostats/). There you can see stats for players. Currently, our biggest database is for 2016 USWNT matches and for the 2016 NWSL season. While this project is still being occasionally worked on, matches are not being logged as regularly as in the past and it is more likely that this project will try to log and archive data from historical matches (pre-2016) as advanced stats have recently been tracked more regularly for certain women's soccer competitions.

The [stats](https://github.com/amj2012/woso-stats/tree/master/stats) folder is where to find the csv files for all the stats tables that the Shiny app is currently reading. Most of what you see in [the Shiny app](https://amj2012.shinyapps.io/wosostats/) is sourced from this folder, although this due to change soon.

The [code](https://github.com/amj2012/woso-stats/tree/master/code) folder is where to find the R code that is used to process and analyze the source Excel and csv files. "Version-2" is the most recent code that is currently being used, while "Version-1" is a folder of deprecated code from an earlier stage in this project.

The [resources](https://github.com/amj2012/woso-stats/tree/master/resources) folder is where to find necessary and helpful resources for how to log stats on your own (please help! volunteers needed!) and definitions for what is being measured and analyzed.

The [shiny](https://github.com/amj2012/wosostats/tree/master/shiny/wosostats) folder is where to find the R code behind the WoSo Stats Shiny app, which can be found at [https://amj2012.shinyapps.io/wosostats](https://amj2012.shinyapps.io/wosostats/).

The [source](https://github.com/amj2012/woso-stats/tree/master/source) folder is where to find all the match spreadsheets that have been logged, in Excel and csv format.

The [database.csv](https://github.com/amj2012/wosostats/blob/master/database.csv) file is the spreadsheet with every match that has been logged. This is also the list of matches that are represented in the WoSo Stats Shiny app. 

The [stats.csv](https://github.com/amj2012/wosostats/blob/master/stats.csv) file is used by the WoSo Stats Shiny app and is a list of the stats represented in that app.
