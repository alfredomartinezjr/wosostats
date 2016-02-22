So you want to log stats for matches on your own? You’re amazing! Here are the steps you to take. If you have questions, send me a DM/mention at [@WoSoStats](https://twitter.com/wosostats) or email me at alfredom790@gmail.com.

**IMPORTANT: If you decide to start logging match stats, IT IS NOT recommended that you log location data for events in the `poss.location`, `poss.play.destination`, or `def.location` columns, except for `poss.location` data for shots. Logging location data on top of everything else is VERY time-consuming and, while interesting, is not as important as getting a match logged and done, which can be logged for location data later.**

#Before you start logging the match stats
1. Download the match-stats-templae.xlsx Excel document from this link: https://github.com/amj2012/woso-stats/blob/master/resources/match-stats-template.xlsx?raw=true.  
  * Using Excel is highly recommended as the ability to format, auto-fill, auto-complete, and utilize formulas makes the job of logging stats way easier and faster.
  * To get an idea of what this spreadsheet should look like, I highly recommend to look over the Excel file for the NWSL SRFC-WAS semifinal, which can be found here https://github.com/amj2012/wosostats/blob/master/source/excel/nwsl-2015/2015-nwsl-semifinal-srfc-was-091315.xlsx, and look at it alongside the actual match which can be seen here: https://www.youtube.com/watch?v=_jdBcOVuEus. 
2. In the `poss.player` column, write in the last name of each player who played in the match as either a starter or a sub, starting with all the home team players. Do the same for the `def.player` column.
3. In the `poss.team` and `def.team` columns, write in the acronym of the team for each respective player. 
  * For club teams, refer to the acronyms.csv spreadsheet which can be found at: https://github.com/amj2012/woso-stats/blob/master/resources/abbreviations.csv.
  * For international teams, refer to the FIFA country codes
4. In the `poss.position` and `def.position` columns, write in the starting position for each respective player.
Don’t get too fancy. Just write in one of “GK,” “D,” “M,” or “F.”
5. Field dimensions can vary widely, so figure out where the borders of the middle third are, so you can tell if a player is in their attacking, middle, or defensive third. Try to also get a sense of where the left, center, and right thirds of the fields are. The left and right thirds will usually cut a few yards into the 18-yard box, and the center third will usually extend a few yards from the center circle. Width of the field can vary widely, so use your best judgment here.
  * It helps to use landmarks around the stadium to figure this out. For example, if the field has football lines or lawn stripes, you can use them to figure out the thirds of the field.
  * Or, you can also use the stands as landmarks.
  * Worst comes to worst, find the widest shot you can of the field, either during the match stream or from an image online, and literally use a ruler to figure out the thirds of the fields.
  * The breakdown of the field should be based on this: ![](http://i.imgur.com/EQLmpYp.png)
6. The template spreadsheet should be 1500 rows long. However, if the match you are logging requires more rows and ends up going into blank rows (beyond the ones filled in with “-” hyphens), then guesstimate how many more rows you’ll need and fill them all in with additional “-” hyphens (more on what those are about below).

#While logging match stats
1. Start logging stats in the row below the one with “kickoff” in the `poss.action` column.
2. Create a new row for the following types of events, to be typed in the `poss.action` exactly as it’s shown in the code span below. Refer to the action-definitions.doc file at [https://github.com/amj2012/woso-stats/blob/master/resources/definitions.md](https://github.com/amj2012/woso-stats/blob/master/resources/definitions.md) for in-depth definitions for each action.
  * **Shots stopped by the goalkeeper** - `shots.stopped.by.gk`
  * **Shots stopped by a defender** - `shots.stopped.by.def`
  * **Shots blocked by a defender** - `shots.blocked`
  * **Shots missed** - `shots.missed`
  * **Shots scored** - `shots.scored`
  * **Forward pass attempts** - `passes.f`
  * **Sideway pass attempts** - `passes.s`
  * **Backward pass attempts** - `passes.b`
  * **Movement into another zone** - `movement`
  * **Take ons won** - `take.on.won`
  * **Take ons lost** - `take.on.lost`
  * **Dispossessed of the ball** - `dispossessed`
  * **Lost touch** - `lost.touch`
  * **Aerial duels won** - `aerial.won`
  * **Aerial duels lost** - `aerial.lost`
  * **Recoveries** - `recoveries`
  * **Balls shielded** - `ball.shield`
  * **Clearances** - `clearances`
  * **Offside Calls** - `offside.calls`
  * **Substitutions** - `substitution.on` & `substitution.off`
  * There will be instances where play is either stopped or peskily cut off by the broadcast. There will also be breaks in play. These instances should be noted.
  * **Play cut off by broadcast** - `playcutoffbybroadcast`
  * **Halftime** - `halftime`
  * **Fulltime, but with extra time on the way** - `fulltime`
  * **End of first period of extra time** - `end.of.1.ET`
  * **End of second period of extra time** - `end.of.2.ET`
  * **End of the match** - `end.of.match`
  * **Other stoppages in play** - `stoppage.in.play`
3. For every single new action logged in the `poss.action` column, the value in that row’s `event` column should automatically increase by 1. 
  * If the `poss.action` column has a “-” denoting that there are additional defensive actions being credited to the event, then the value in the `event` column should remain the same. The idea is that a new event is only triggered by a new possessive action or something like a stoppage in play or break in broadcast.
4. For every single new action logged in the `poss.action` column, log the possessing player’s name in `poss.player` 
5. You do NOT have to define the event's location in `poss.location` for anything other than shots. If you decided to log the location of every event, it could take about as much as 50% longer to finish the entire match. It is recommended that you leave `poss.location` blank unless otherwise specified, or unless you know what you're getting yourself into.
  * In the `poss.location` column, the value must be one of the appropriate acronyms, shown in italics below. The location is relative to the player for which you are logging it (e.g. a player is in her 18-yard box if she is defending someone in possession of the ball in the opponent’s 18-yard box, and vice versa). 
    * **Opponent’s 6-yard box** - `A6`
    * **Opponent’s 18-yard box** - `A18`
    * **Attacking third, left wing** - `A3L`
    * **Attacking third, center field** - `A3C`
    * **Attacking third, right wing** - `A3R`
    * **Opponent’s half of middle third, left wing** - `AM3L`
    * **Opponent’s half of middle third, center field** - `AM3C`
    * **Opponent’s half of middle third, right wing** - `AM3R`
    * **Own half of middle third, left wing** - `DM3L`
    * **Own half of middle third, center field** - `DM3C`
    * **Own half of middle third, right wing** - `DM3R`
    * **Defensive third, left wing** - `D3L`
    * **Defensive third, center field** - `D3C`
    * **Defensive third, right wing** - `D3R`
    * **Own 18-yard box** - `D18`
    * **Own 6-yard box** - `D6`
5. The `poss.play.destination` column should only be filled in if the following event was cut off by a broadcast interruption or a stoppage in play. For example, if a player completes a pass, but the receiving player did not have a chance to create an event worth logging because the broadcast decided to interrupt the feed with a replay, then manually enter the destination of the action. Otherwise, there is no way of determining the destination.
6. The `play.type` column should be filled in for the following types of passing and shots actions written in italics below. Refer to the action-definitions.doc file at [https://github.com/amj2012/woso-stats/blob/master/resources/definitions.md](https://github.com/amj2012/woso-stats/blob/master/resources/definitions.md) for in-depth definitions for each qualifier.
  * **Corner crosses** - `corner.crosses`
  * **Deep crosses** - `deep.crosses`
  * **Switch** - `switch`
  * **Launch balls** - `launch`
  * **Through balls** - `through`
  * **Lay-off balls** - `lay.off`
  * **Flick-on balls** - `flick.on`
  * **Throw-ins** - `throw.in`
  * **Free kicks** - `free.kick`
  * **Headed balls** - `headed`
  * **Corner kicks** - `corner.kick`
  * **Goal kick** - `goal.kick`
  * **Goalkeeper throws** - `gk.throws`
  * **Goalkeeper drop kicks** - `gk.drop.kick`
  * **Penalty kicks** - `pk`
7. For each event in the `poss.action` column, log any of the following defensive actions in italics below in the `def.action` column. Refer to the action-definitions.doc file at [https://github.com/amj2012/woso-stats/blob/master/resources/definitions.md](https://github.com/amj2012/woso-stats/blob/master/resources/definitions.md) for in-depth definitions for each action.
  * **Dispossessing through a ball shield** - `dispossess.ball.shield`
  * **Dispossessing through a steal** - `dispossess.steal`
  * **Dispossessing a lost touch** - `dispossess.lost.touch`
  * **Tackling the ball away** - `tackles.ball.away`
  * **Tackling and winning the ball** - `tackles.ball.won`
  * **Dibbled by an opponent due to a missed tackle** - `dribbled.tackles.missed`
  * **Dribbled by an opponent due to being out-run** - `dribbled.out.run`
  * **Dribbled by an opponent due to being turned** - `dribbled.turned`
  * **Pressuring an opponent** - `pressured`
  * **Challenging an opponent** - `challenged`
  * **Blocks** - `blocks`
  * **Interceptions** - `interceptions`
  * **Balls shielded** - `ball.shield`
  * **Clearances** - `clearances`
  * **Aerial duels won** - `aerial.won`
  * **Aerial duels lost** - `aerial.lost`
  * Goalkeepers have their own types of defensive actions in the “def.event” column”
  * **Shots on goal stopped by a goalkeeper** - `gk.s.o.g.stop`
  * **Shots on goal stopped by a defender** - `gk.s.o.g.def.stop`
  * **Shots on goal scored** - `gk.s.o.g.scored`
  * **Shots missed** - `gk.shot.miss`
  * **High balls won by the goalkeeper** - `gk.high.balls.won`
  * **High balls lost by the goalkeeper** - `gk.high.balls.lost`
  * **Smothers won by the goalkeeper** - `gk.smothers.won`
  * **Smothers lost by the goalkeeper** - `gk.smothers.lost`
  * **Loose balls claimed by the goalkeeper** - `gk.loose.balls.won`
  * **Loose balls lost by the goalkeeper** - `gk.loose.balls.lost`
  * Create a new row if necessary when there are multiple defensive actions for one possessing event, but be sure to leave the `poss.action` column blank so that a new event number is not accidentally created
8. For each `def.action` value, fill in the corresponding player in the `def.player` column.
9. The `def.location` column should ONLY be manually filled in for the following defensive events and ONLY if you're additionally interested in location data:
  * **Blocks** - `blocks`
  * **Interceptions** - `interceptions`
  * **Balls shielded** - `ball.shield`
  * **Clearances** - `clearances`
  * All goalkeeper-specific events
10. For all goalkeeper defensive actions, except for missed shots and goals, you should further describe the action by filling in the `gk.ball.stop` column with the appropriate descriptor for what happened to the ball, to be written as it is shown in italics below:
  * **Caught** - `caught`
  * **Punched to safety** - `punched.to.safety`
  * **Punched to danger** - `punched.to.danger`
  * **Dropped** - `dropped`
  * **Missed the ball** - `missed.the.ball`**Collected** - `collected`
  * **Parried to safety** - `parried.to.safety`
  * **Parried to danger** - `parried.to.danger`
  * **Deflected to safety** - `deflected.to.safety`
  * **Deflected to danger** - `deflected.to.danger`
11. For goalkeeper defensive actions that are shots on goals, log the type of save attempt, regardless of whether it was successful, by filling in the `gk.s.o.g.attempt` column with one of the following values to be written as it is shown in italics below:
  * **Diving save** - `diving`
  * **Standing save** - `standing`
  * **Reaching save** - `reaching`
  * **Stooping save** - `stooping`
12. If a foul was committed, log for the possessing player in the `poss.player.disciplinary` column and for the defending player in the `def.player.disciplinary` column one of the following values to be written as it is shown in italics below:
  * **Fouls won** - `fouls.won`
  * **Fouls conceded** `fouls.conceded`
  * **Yellow cards** - `yellow.cards`
  * **Red cards** - `red.cards`
  * **Penalty kicks won** - `penalties.won`
  * **Penalty kicks conceded** - `penalties.conceded`
13. Certain possessing player actions need additional qualifiers, related to scoring opportunities or defensive mistakes, that should be added in the `poss.notes` column, to be written in as they are shown in italics below. In the event that more than one of these apply to the same event, add them all into the same cell but separate them by a comma:
  * **Big chances scored** - `big.chances.scored`
  * **Big chances shot on goal** - `big.chances.shot.on.goal`
  * **Big chances missed** - `big.chances.shot.missed`
  * **Big chances disspossessed** - `big.chances.dispossessed`
  * **Big chance created** - `big.chances.created`
  * **Assists** - `assists`
  * **Second assists** - `second.assists`
  * **Unscored key passes** - `unscored.key.passes`
  * **Ball goes out of bounds and possession is kept** - `out.of.bounds.keep.poss`
  * **Ball goes out of bounds and possession is lost** - `out.of.bounds.lost.poss`
  * **Errors leading to a goal for the opposition** - `errors.to.goals`
  * **Errors leading to an unscored big chance for the opposition** - `errors.to.big.chances`
14. Similarly, certain defending player actions need additional qualifiers, related to defensive accomplishments and mistakes, that should be added in the `def.notes` column, to be written in as they are shown in italics below.
  * **Big chances stopped** - `big.chances.stopped`
  * **Own goals allowed** - `own.goals`
  * **Errors leading to a goal for the opposition** - `errors.to.goals`
  * **Errors leading to an unscored big chance for the opposition** - `errors.to.big.chances`
15. When a new minute in play is reached, change the “time” column for the first event of each minute to the time in minutes. An event in the first 30 seconds is in minute 1, an event at 30:34 is in minute 31, and so on. 
For stoppage time, use a plus sign to denote how much stoppage time was added. For example, 2 minutes into stoppage time after 90 minutes should be written as “90+3”, NOT as “93.” The same goes with examples such as “45+3”, “120+1”, and so on.

#After logging match stats
1. Delete all unused rows with “-” hyphens below the row with the end.of.match action
2. Quickly browse and search through the spreadsheet and be on the lookout for anything that doesn’t look right.
3. Search and replace every “-” hyphen with a blank “” value (do not replace it with a space “ “ value).
  * If this affects any hyphenated player names, search through the name that was changed and replace it with the correct name that was affected.
4. Save the file as an .xlsx file. There are other types of Excel files, but DO NOT save them as those types, just as an .xlsx file.

#What’s next
1. Save the Excel file in the wo-so Github repository by doing one of the following:
  * IF you are comfortable with using GitHub, push the Excel file as an addition to the wo-so repository.
  * OR if you are not comfortable with using GitHub or if your have no idea what that the above paragraph was about, just email me the raw Excel file at alfredom790@gmail.com.
2. Turn the Excel file into the filled-in completed .csv file that will be used for the actual analyses, by doing one of the following:
  * IF you are comfortable with using R:
    * set the directory in which the Excel file is as the working directory
    * source the tidy-excel.R file in https://raw.githubusercontent.com/amj2012/wosostats/master/code/version-2/tidy-excel.R. This should create a data frame named df in your working environment.
    * save the df data frame as a .csv file in either your working directory or anywhere else you’d like
    * If you’re comfortable with GitHub, push the new .csv file as an addition to the appropriate folder in the wo-so depository. Otherwise, just email me the .csv file at alfredom790@gmail.com.
  * IF you are not comfortable with using R to create this .csv file yourself, just notify me via email and I’ll take care of it.
3. You’re done! Please do celebrate, as this is great data that can be analyzed and it surely takes a lot of work for each match.
4. See the match analysis for yourself by downloading the template.Rmd R Markdown file, and in the first chunk change *matchlocationgoeshere* to the file location of either the csv file in your computer (if you made the csv file yourself) or the raw URL of the csv file’s location in the GitHub repository. OR, I’ll just email you the match analysis myself as soon as I create it.

#The logic and reasoning behind this process
  * **What’s with all the “-” hyphens?**
    * The “-” hyphens are there to make it make use of Excel’s Autocomplete feature (more on what that is here) that, after typing in the first few characters of a value that has already been typed in that column. For example, if you type in “pa” the cell will have a drop-down menu appear below it with “passes.f”, “passes.b”, and “passes.s” as options you can click. Clicking on, for example, “passes.f” will automatically insert that value into the cell. Not having to manually type in “passes.f”, for example, will save you an incredible amount of time in the long run, HOWEVER this feature does not seem to work if the cells above the one you are editing are spaceless blanks. To remedy this in a way you can tell the “blank” cell is appropriately edited, without filing it in with something too distracting, I’ve chosen “-” hyphens.
  * **Why aren’t all cells that could have a value filled in?**
    * The R code I have written is able to use some logic to figure out, based on values around it, what that “blank” value should be. For example, if a defending player was pressuring an opponent, you can just leave her location as the “-” value. The R code that this Excel file will run through will take the location of the possessing player and use that to figure out the location of the defensive action. If the match was properly logged (and if the code doesn't run into a bug), these logical checks in the R code will fill in these values for you. This saves you hours.
  * **Why does this process take so long?**
    * Logging each action in chronological order unfortunately takes a while, but this is the fastest process I’ve been able to create that can return mountains of data. It takes close to half a day to log an entire match, but you could spend days, or even longer, poring through the data. I could make this process shorter by getting rid of things like location data or special types of passes, but then we’d lose out on valuable data that I really don't yet know how to log otherwise. I hope to explore some ideas on how to make this faster, and ideas are welcome.
    * **Why not log in location data for everything?**
    * Logging location data increases that amount of time it takes to log a match by about 50%. I believe it's better to get rich data without location data for 30 matches, and then add that location data later as part of a separate project, than to take longer to get those 30 matches done in the first place. It's just a matter of priorities. Besides, as the columns for location data are still there, it should be simple to go back and add location data for any match.
