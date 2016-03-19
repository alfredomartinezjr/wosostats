*This is a list of action definitions that are meant to inform how actions should be logged. It is meant to be used by people who are either logging stats on their own or would like to have a look at how they are being defined. If you would like to see definitions for stats that are calculated from these actions (such as in the Shiny app or on various R Markdown docs), you'll want to go to the stats-glossary.md page (coming soon)!*

Below is a list of definitions for values to be used when logging match actions in the match-stats-template.xlsx Excel document. These values are player actions (such as passes and shotsb), qualifiers to player actions that further describe the action (such as the type of pass or if a big chance was stopped), and location data. When logging actions, refer to this document when in doubt about what a certain value means and when to log it. Feedback is welcome and should be sent to alfredom790@gmail or @WoSoStats on Twitter.

#Table of Contents
* Possessing Player Actions - `poss.action`
* Play Type - `play.type`
* Defensive Player Actions - `def.action`
* Goalkeeper Actions - `gk.ball.stop` & `gk.s.o.g.attempt`
* Disciplinary Actions - `poss.player.disciplinary` & `def.player.disciplinary`
* Additonal Possessing Player Notes - `poss.notes`
* Additonal Defensive Player Notes - `def.notes`
* Location-based Notes - `poss.location`, `poss.play.destination`, & `def.location`

Each player action falls under one of nine different types of actions. In the match-stats-templace.xlsx Excel document, this looks like nine different columns.

A key part of this model is that during any given moment in a match there is a team in "possession" of the ball and a team "defending" the ball. As such, these nine different columns will fall into one of three different types of actions: 1) actions by a player from the team "in possession" of the ball, 2) actions by a player from the team "defending" against the ball, and 3) actions by a goalkeeper making a play on the ball.

Another key part of this model is that there are actions that can only be done by a team in posession of the ball, actions that can only be done by a team playing defense, and actions that denote a clear change in possession. The last point is crucial; there are certain defensive actions (such as an interception) that signal a clear change in possession to the defending team, certain possessive actions (such as a recovery by a team that was formerly defending that do the same, and certain actions in other columns (such as a ball that went out of bounds and goes to the other team) that also do the same. This "flow" to the game where one team is in possession of the ball, the other team is defending, and certain moments denote a change in possession are key to logging stats under this model.

Locgging location data is laborious and, unless requested or unless you really want to know it, it is only recommended that it be logged for shots. More on how to define the different locations of a soccer pitch will be covered in its section, "Location-based Notes".

For each definition, the long name will be shown in **bold like this** and the different ways you can actually write it in the Excel spreadsheet will be shown in `code span like this`, separated by a comma. You are not to write out the long name in the Excel spreadsheet, and it is highly recommended you use the shortest version possible, as using shortcuts will save you a significant amount of time. Shortcuts specified below do not need to be searched and replaced after the fact with anything, as they will be correctly logged by the R code used by the Shiny App. However, while you are free to use any other shortcuts you like (such as nicknames for players), know that those will have to be searched and replaced in order to be readable by the R code that has been created.

A GIF showing an example will be provided for a definition when appropriate and possible.

#Possessing Player Actions
Column name: `poss.action`

The actions below will be tracked under the `poss.player.action` column. As the name suggests, these are actions by a player on a team "in possession” of the ball. 

**Shots stopped by the goalkeeper** - `shots.stopped.by.gk`, `sog.gk` , `sgk`

A shot that would have been scored but for being stopped by a goalkeeper's save

![](http://i.imgur.com/SKaaerO.gif)

**Shots stopped by a defender** - `shots.stopped.by.def`, `sog.def`, `sdef`

A shot that would have been scored but for being blocked by the last defender behind the goalkeeper

![](http://i.imgur.com/1rI71JW.gif)

**Shots blocked by a defender** - `shots.blocked`, `sb`

A shot heading towards the goal that was blocked by a defender who had other defenders and/or the goalkeeper behind her.

**Shots missed** - `shots.missed`, `sm`

A shot that misses the goal or hits the post or crossbar.

![](http://i.imgur.com/Dp3hVaX.gif?1)

**Shots scored** - `shots.scored`, `sc`

A shot that goes into the goal. Easy!

**Forward pass attempts** - `passes.f`, `pf`

Forward pass attempts, regardless of whether the pass attempt was completed

**Sideway pass attempts** - `passes.s`, `ps`

Sideway pass attempts, regardless of whether the pass attempt was completed. For a pass to go sideways, it does not have to be at a perfect 90 angle; if it goes a little bit backwards or forwards but can be deemed to be going largely sideways, it should be logged as such. 

**Backward pass attempts** - `passes.b`, `pb`

Backward pass attempts, regardless of whether the pass attempt was completed 

**Movement into another zone** - `movement`, `m`

When a player moves from one "zone" (zone definitions are outlined in the "Location-based Data" section below) into another "zone." If a player consecutively moves from one zone into another, each instance should be tracked separately.

Example: [http://i.imgur.com/mYoa9eF.gif](http://i.imgur.com/mYoa9eF.gif)

In the GIF above, Leigh Ann Brown, the player in white and blue, moves with the ball from her defensive right third, into her defensive right middle third, into her opposing right middle third, to her opposing right third. This should be logged as three separate 'movement' events; one from defensive right third into defensive right middle third, one from defensive right middle third into opposing right middle third, and one from opposing right middle third into opposing right third.

The above example on an Excel spreadsheet would look like this, with each "movement" in it's own row in the poss.action column, triggering a new event each time:

![](http://i.imgur.com/vOolLGF.png)

**Take ons won** - `take.on.won`, `tow`

A take on is an intentional attempt by a player to get past her defender while maintaining possession of the ball. A take on is “won” if the player dribbles past a defender, turns a defender to create open space, or draws a foul. 

Example 1: [http://i.imgur.com/uTDOeay.gif](http://i.imgur.com/uTDOeay.gif)

Mandy Laddish, the player in white and blue, wins a take on against Jess Fishlock, in the yellow and purple, by dribbling past a missed tackle

Example 2:  [http://i.imgur.com/GXS3exL.gif](http://i.imgur.com/GXS3exL.gif)

Erika Tymrak, the player in white and blue, wins a take on against Kendall Fletcher, in the yellow and purple, by running right past her for a chance at a cross.

Example 3: [http://i.imgur.com/OVbQKZH.gif](http://i.imgur.com/OVbQKZH.gif)

Megan Rapinoe, the player in yellow and purple, wins a take on against Erika Tymrak, in the white and blue, by turning her and dribbling past her into the open midfield: 

**Take ons lost** - `take.on.lost`, `tol`

A take on is “lost” if a player intentionally attempts to get past her defender and ends up getting dispossessed by a tackle (regardless of who recovers the ball), a lost touch, a ball shield, or because she conceded a foul to a defender. A lost take-on is different from the "dispossessed" definition covered below because a "take on lost" was the result of an intential attempt to get past a defender.

Example: [http://i.imgur.com/n0ThErg.gif](http://i.imgur.com/n0ThErg.gif)

Different take on attempts can occur in succession and should be logged separately. Here, Kim Little, the player in yellow and purple, wins a take on against #6 Jen Buczkowski by dribbling through her tackle attempt, but Little proceeds to lose the following take on attempt against #3 Becca Moros, who steps up to tackle the ball away from her.

**Dispossessed of the ball** - `dispossessed`, `d`

A dispossession is when the player in possession of the ball loses the ball to a defender who stepped up to take away the ball, without the possessing player having attempted to a “take on” the defender. This is different from a lost take on in that encompasses moments other than "lost take ons" where a possessing player was dispossessed by a defender who was making an intentional attempt at dispossessing that possessing player.

This is meant to define moments such as a possessing player getting snuck up from behind. It is also meant to define moments where a possessing player isn't so much attempting to take on a defender as she is trying to just maintain possession in the face of a player challenging her off the ball.

Example 1: [http://i.imgur.com/wfXAGUx.gif](http://i.imgur.com/wfXAGUx.gif)

Keelin Winters, the player in yellow and purple who receives the pass, is dispossessed from behind by Jen Buczkowski, in the white and blue. Since it does not appear like Winters had a chance to attempt a take on, this is logged as a dispossession. This looks like this on the spreadsheet (certain columns have been hidden to better show this):

![](http://i.imgur.com/Qs8a6oO.png)

Example 2: [http://i.imgur.com/GWHVuk5.gif](http://i.imgur.com/GWHVuk5.gif)

Demi Stokes, the player in red who receives the pass, is immediatelly challenged for the ball by Tobin Heath, the player in white. Stokes, practically face to face with Heath, never seems to be taking on Heath so much as she is trying to keep the ball away. Since it doesn't look like a clear take on, and since it's clear that Heath was forcing a dispossession with a tackle, this is considered a "dispossessed" for Stokes. This looks like this on the spreadsheet (certain columns have been hidden to better show this):

![](http://i.imgur.com/NxF9P55.png)

**Lost touch** - `lost.touch`, `lt`

A lost touch is when a player loses possession of the ball due to a mistouch or heavy touch, without it being clear that she was attempting a shot, pass or taken; or it is when a player attempts to win possession of the ball and unsuccessfully knocks it away with a mistouch or bad touch.

This type of action is meant to be different from the ones defined above (lost take ons and "dispossessions") in that a "lost touch", as it should appear in this Excel spreadsheet, is meant to encompass bad touches. It is not meant to define unsuccessful intentional attempts to get past opponents, and it is not meant to define moments where a player, who wasn't attempting to take on anyone, was dispossessed because of a defender intentional attempt at taking the ball away. 

Put in other words, if the possessing player wasn't clearly attempting to take on a defender, and the defender who ended up with the ball didn't dispossess the possessing player as much as she actually just recovered a loose ball, then the possessing player's action should be considered a "lost.touch."

Example 1: [http://i.imgur.com/W4y6DOs.gif](http://i.imgur.com/W4y6DOs.gif)

Lindsey Horan, the player in white, attempts to trap a ball with her body but ends up knocking the ball away. She was not being challenged by any one from England, the team in red, and it is not clear that she was attempting a pass, so this is considered a "lost.touch." This looks like this on the spreadsheet (certain columns have been hidden to better show this):

![](http://i.imgur.com/hXESkEU.png)

Example 2: [http://i.imgur.com/hVcRbPJ.gif](http://i.imgur.com/hVcRbPJ.gif)

Mallory Pugh, the player in white, attemps to win a ball that has been knocked into her direction from an aerial duel, and she ends up mistouching it and glancing it into the direction of a player on the opposing team. It's not clear that this was meant to be a pass and Jordan Nobbs, the player in red from the opposing team running towards her from behind, was not physically engaged with her as Pugh was attempting to control the ball, so this is considered a "lost.touch." This looks like this on the spreadsheet (certain columns have been hidden to better show this):

![](http://i.imgur.com/JtSy0jH.png)

Example 3: [http://i.imgur.com/9CaWO5l.gif](http://i.imgur.com/9CaWO5l.gif)

Jess Fishlock, the player in yellow and purple, receives a pass from Kim Little but loses the ball to Maddy Laddish, in the white and blue, due to a bad first touch. Since it is not clear that Fishlock was attempting to take on Laddish with that touch, this is logged as a dispossession: 

**Aerial duels won** - `aerial.won`, `aw`

Aerial duels are when two players challenge for a 50/50 ball in the air. The first player to make contact with the ball is deemed to have won the aerial duel, regardless of where the ball ends up or who recovers it.

Passes that are also aerial duels should still be counted as aerial duels in a separate row as its own event. So, if a player challenges for a launched ball and heads it to an intended recipient, it should be logged as an `aerial.won` in one event and as a `passes.f/s/b` in the next event.

Example 1: [http://imgur.com/W4dC52H.gif](http://imgur.com/W4dC52H.gif)

Lindsey Horan, the player in white, launches a pass forward. Carli Lloyd, the player in white, and Carol Sanchez, the player in red, both go up into the air to challenge for the ball, and Lloyd wins the challenge by being the first to make contact with the ball. In addition, Lloyd also looks to clearly be, in that same motion, making a flick-on header pass forward to her teammate. In this case, Lloyd gets an "aerial.won" and a "passes.f". On the spreadsheet, this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/QQ9hosw.png)

**Aerial duels lost** - `aerial.lost`, `al`

A player is deemed to have lost an aerial duel if the player challenging her for the ball got to the ball first, regardless of where the ball ends up or who recovers it.

A lost aerial duel should be logged for both players when they both had a reasonable chance at winning the ball but ended up mistiming their jump.

Example 1: [http://i.imgur.com/EspvEYN.gif](http://i.imgur.com/EspvEYN.gif)

Mallory Pugh, the player in white, and Steph Houghton, the player in red, both have a resonable chance at winning this launched pass, but neither really manages to get off a good jump, due to running into each other, even though it was a winnable ball in the air for both of them. This goes down as an "aerial.lost" for the "possessing" player (due to the ball still being in "possession" of the team in white), and also an "aerial.lost" for the defending player. On the spreadsheet, this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/lK5EHAs.png)

**Recoveries** - `recoveries`, `r`

A recovery is when a player gets posession of a loose ball, regardless of which team was the one to previously have possession of the ball. A recovery should always be logged when a player wins possession of the ball after a scenario that creates a loose ball, such as (but not limited to) a block, an aerial duel, a clearance, or a tackle, a missed pass, or a dispossession. Recoveries are a way of noting how a team wins or maintains possession.

To reiterate, every moment where a player gains possession of a loose ball, regardless of which team last possessed the ball, must be logged as recovery for the recovering player.

Example 1: [http://i.imgur.com/v6B4AIK.gif](http://i.imgur.com/v6B4AIK.gif)

Mallory Pugh, the player in white, attempts to pass the ball forward but is blocked by a player from the opposing team. The ball is now a loose ball and the person who regains possession is Carli Lloyd, the player in white, who should then be logged with a recovery. On the spreadsheet, this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/PsAGVSv.png)

Example 2: [http://i.imgur.com/hwBHR4z.gif](http://i.imgur.com/hwBHR4z.gif)

Kelley O'Hara, the player in white, clears the ball away from the box. Her teammate, Carli Lloyd gets a bad touch on the ball and knocks it away, causing a loose ball scenario. Jordan Nobbs, the player in red, ends up gaining possession of the loose ball and gets credited with a recovery. On the spreadsheet, this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/IQZBj93.png)

Example 3: [http://i.imgur.com/ZBFFuH1.gif](http://i.imgur.com/ZBFFuH1.gif)

Toni Duggan, the player in red, attempts to pass the ball sideways but it ends up being incomplete and becoming a missed pass. A missed pass that doesn't go out of bounds is pretty much a loose ball scenario, and Mallory Pugh, the player in white, gains possession of it and gets credited with a recovery. On the spreadsheet, this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/4OIvG2y.png)

**Balls shielded** - `ball.shield`, `bs`

A ball shield in the "poss.action" column is when a possessing player, *who it should can be deemed to already have possession of the ball*, intentionally uses her body to shield the ball from a defender with the intention of keeping the ball in her team's possession. This action is meant to encompass moments where a player is trying to waste time by shielding the ball in the corner; when a player is trying to earn a throw-in or corner by getting a defender to knock the ball out of bounds; when a player shields a loose ball or pass for a teammate to pick up; or when a player shields a pass to turn a defender before touching the balll.

There is a "ball.shield" action in the "def.action" column, but that goes for a separate type of ball shields that gets credited to a player from a team defending the ball.

Example 1: [http://i.imgur.com/As8VvLs.gifv](http://i.imgur.com/As8VvLs.gifv)

Katrine Veje, the player in black and blue, receives a pass in the corner area and proceeds to attempt a cross which is blocked by Amanda Da Costa, the player in white. The blocked ball, which was last touched by Da Costa, the defender, is now a loose ball rolling out of bounds and Veje shields the ball away from Da Costa as it goes out of bounds for a corner kick. As the blocked ball is still technically, under this model, "under possession" by Veje's team, she gets credited with a "ball shield" in the "poss.action" column for keeping the ball under her team's possession. On the spreadsheet, this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/FxISiGC.png)

Example 2: [http://i.imgur.com/0dICuJ7.gifv](http://i.imgur.com/0dICuJ7.gifv)

Diana Matheson, the player in white, passes the ball forward to her teammate, Francisca Ordega. Before touching the ball, Ordega shields the ball away from her defender, Lauren Barnes, who is challenging her for the ball. Ordega successfully shields the pass and maintains possession of the ball for her team as she then runs forward towards the box.  On the spreadsheet, this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/Rk261Cz.png)

Example 3: [http://i.imgur.com/IPEyEHb.gifv](http://i.imgur.com/IPEyEHb.gifv)

Kim Little, the player in black and blue, dribbles the ball towards the corner flag while being challenged by Diana Matheson and Estelle Johnson, the players in white. While what happens next could be considered a "dispossessed" action, context and apparent intent is important; it is the 87th minute, Little's team has a 2-0 lead, and she appears to be more shielding the ball than actually trying to dribble it past her defenders. Matheson proceeds to gets a touch on the ball and knocks it out of bounds, keeping the ball in possession of Little's team and winning Little a "ball.shield" as it appears her intent was to shield the ball rather than beat her defenders. On the spreadsheet, this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/Oq3hEuQ.png)

**Clearances** - `clearances`, `cl`

A clearance is logged in the `poss.action` column when a player in possession of the ball intentionally kicks the ball away without an intended recipient. Intent, time on the ball, pressure from defenders, and the angle the player was facing when she kicked the ball all should be considered when deciding to consider something a "clearance" rather than a pass attempt.

**Offside Calls** - `offside.calls`, `os`

Logged when a player is called offside.

**Breaks in play or broadcast**

**Play cut off by broadcast** - `playcutoffbybroadcast`, `playcutoff`, `pco`

These are pesky instances when the broadcast of the game is cut off by something such as a replay or sideline interview. They can completely cut off your ability to log match stats and can affect how stats are analyzed if they aren't outright mentioned. They should be logged in the `poss.action` column.

On super-pesky instances when the broadcast cuts off right after a completed, impeding your ability to tell if the pass was completed or not and to who, log the last pass that could be seen that was completed as "passes.f.c", "passes.s.c", or "passes.b.c", depending on the direction of the pass. The "c" at the end indicates that the pass was completed. You only need add that "c" when a completed pass was the last action before a stoppage in play or broadcast interruption. 

The shory story for the reason for the "c", and why you don't have to add it to every single completed pass attempt in the game, is that the R code that reads the Excel file, which generates the match actions .csv file that is used to compute match stats, can tell if a pass attempt was completed based on other actions and qualifiers in the surrounding events and columns, except for these instances when play was stopped or cut off directly after a completed pass.

For example, observe this moment: [https://streamable.com/7o9o](https://streamable.com/7o9o). Ali Krieger, the player in white, completes a pass forward to her teammate Sam Mewis, and then the broadcast cuts to a shot of Hope Solo for a couple of seconds. When the broadcast, does return to the game, the ball is still in possession of the same team but is now at the feet of Whitney Engen. Since it is not 100% clear what happened while the camera was on Solo, on the spreadsheet this action would look like the following (certain columns have been hidden to better show this):

![](http://i.imgur.com/8Q3ZbAc.png)

**Substitutions** - `substitution.on` & `substitution.off` (OR `sub.f` & `sub.o`)

Note which players are being substituted on and off. 

The player who gets subbed off should have her substitution logged as occurring during her last minute of play. For example, if play is stopped at 50:35 (the 51st minute), Player A is subbed off, and play does not resume again until 51:10 (the 52nd minute), then Player A should have her substitution logged as having happened during minute 51.

The player who gets subbed on should have her substitution logged as occurring during her first minute of actual play. For example, to stay with the example in the previous paragraph, if play was stopped at 50:35 (the 51st minute), Player B was subbed on, and play does not resume again until 51:10 (the 52 minute), then Player B should have her substitution onto the pitch as having happened during minute 52.

This process for logging the minute of when a substitution happened is essential for correctly computing how many minutes a player actually played.

**Halftime** - `halftime`

**Fulltime, but with extra time on the way** - `fulltime`

This should be logged if there is extra time on the way.

**End of first period of extra time** - `end.of.1.ET`

**End of second period of extra time** - `end.of.2.ET`

**End of the match** - `end.of.match`

**Other stoppages in play** - `stoppage.in.play`

If not a substitution or end of play, any other instances that stop play, such as an injury, should be noted.


#Play Types
Column name: `play.type`

Certain shots and passes logged in the `poss.action` column will be of special types that will require additional qualifiers to be logged in this column. Sometimes more than one of these will apply, such as a lay off that was also headed, in which case a new row should be created (but leaving the 'poss.action' column blank so as not to create a new event by accidet), and the additional qualifier should be added in the new row.

To further show how multiple qualifiers can be added to the same action, below are two examples taken from an Excel spreadsheet for an actual match (certain columns have been hidden to better show the use of mutiple qualifiers).

Example 1: [http://i.imgur.com/hPSFSq9.gifv](http://i.imgur.com/hPSFSq9.gifv)

Karen Bardsley passes the ball forward, and the pass was both a goal kick and launched into the air. Note how the poss.action column for the row with the additional qualifier was left "blank" with a hyphen, which, based on a formula in the spreadsheet, ensures that the additional row is logged under the same event number (in this case, event 6).

![](http://i.imgur.com/JyNiAMu.png)

Example 2: [http://i.imgur.com/hPSFSq9.gifv](http://i.imgur.com/hPSFSq9.gifv)

Fara Williams, the player in red, receives a pass from her teammate Lucy Bronze and passes the ball forward, and the pass was both a header and a flick-on. Again, the poss.action column is left blank so as not to create a new event:

![](http://i.imgur.com/O1wl4ep.png)

**Corner crosses** - `corner.crosses`, `cc`

Crosses, a ball launched or driven from the left third or right third of the field into the box, that were also struck from within the attacking third.

**Deep crosses** - `deep.crosses`, `dc`

Crosses, a ball launched or driven from the left third or right third of the field into the box, that were also struck from beyond the attacking third.

**Switch** - `switch`, `s`

A long, high ball to an intended recipient across the field.

**Launch balls** - `launch`, `lau`

Also sometimes known as "long balls." Long, high balls into open space, not clearly towards any one intended recipient, or into a crowded area, or into open space for a teammate to run into. If they come from the left or right thirds of the field, they should be logged as crosses and NOT as a launch ball.

Example 1: [http://i.imgur.com/HgK2mXJ.gifv](http://i.imgur.com/HgK2mXJ.gifv)

Hope Solo, the goalkeeper in black, passes the ball forward by launching into the air to be contested in the midfield. In the spreadsheet, this moment would look like this (certain columns have been hidden to better show this):

![](http://i.imgur.com/1x6eyKT.png)

Example 2: [http://i.imgur.com/Ad7Pa60.gifv](http://i.imgur.com/Ad7Pa60.gifv)

Many goal kicks, passes taken by a goalkeeper after the ball has gone out of bounds, end up being launched passes, such as this one by Karen Bardsley, the goalkeeper in green. In the spreadsheet, this moment would look like this (certain columns have been hidden to better show this):

![](http://i.imgur.com/nZAjQFF.png)

**Through balls** - `through`, `th`

A pass, through the ground or air, that splits the defense by going between two defenders, or between a defender and the touchline, or around the defensive line, into open space, to meet a teammate at the end of her run.

Example 1: [http://i.imgur.com/rVWTJs3.gifv](http://i.imgur.com/rVWTJs3.gifv)

Alex Greenwood, the player in red, passes the ball forward, which goes between the defender, Kelley O'Hara, and the touchline into open space for her teammate, Toni Duggan, to receive. Regardless of the result of the pass, the attempt was a through ball and it should be logged as such. In the spreadsheet, this moment would look like this (certain columns have been hidden to better show this):

![](http://i.imgur.com/hxxI92P.png)

Example 2: [http://i.imgur.com/FK1fDSB.gifv](http://i.imgur.com/FK1fDSB.gifv)

Toni Duggan, the player in red, passes the ball forward, which attempts to meet her teammate at the end of her run in front of the defenders. In the spreadsheet, this moment would look like this (certain columns have been hidden to better show this):

![](http://i.imgur.com/ICPK2hk.png)

**Lay-off balls** - `lay.off`, `lay`

A one-touch pass back into the same general direction from where the ball came from. Does not necessarily have to be a pass back to the player who made the previous pass.

Example 1: [http://i.imgur.com/gXJMNcW.gifv](http://i.imgur.com/gXJMNcW.gifv)

Diana Matheson, the player in white, receives a pass from her teammate and in touch passes it back in the same direction to another teammate, Crystal Dunn. Despite the fact that the pass wasn't back to the same player, Matheson should get credited with what's the definition of a lay-off, a one-touch pass into the same general direction. In the spreadsheet, this moment would look like this (certain columns have been hidden to better show this):

![](http://i.imgur.com/Oqcwlvq.png)

Example 2: [http://i.imgur.com/QvNtzvr.gifv](http://i.imgur.com/QvNtzvr.gifv)

Kim Little, the player in black and blue, receives a pass from her teammate, Megan Rapinoe, and in one touch passes it back in the same general direction from where it came from to her teammate Jess Fishlock. In the spreadsheet, this moment would look like this (certain columns have been hidden to better show this):

![](http://i.imgur.com/YLktCyC.png)

**Flick-on balls** - `flick.on`, `flick`

A glancing pass into the same general direction from which it came from. Does not necessarily have to be a pass to the player who made the previous pass. A header can be a flick-on.

Example 1: [http://i.imgur.com/fB5auDp.gifv](http://i.imgur.com/fB5auDp.gifv)

Diana Matheson, the player in white, receives a pass from her teammate and flicks it on into the same direction with a glancing touch. In the spreadsheet, this moment would look like this (certain columns have been hidden to better show this):

![](http://i.imgur.com/I1eM2h3.png)

Example 2: [http://i.imgur.com/bPL1ReE.gifv](http://i.imgur.com/bPL1ReE.gifv)

Demi Stokes, the player in red, receives a throw-in from her teammate and heads it forward with a glancing touch into the same direction. This should be logged as a flick-on. In the spreadsheet, this moment would look like this (certain columns have been hidden to better show this):

![](http://i.imgur.com/ccdyrDc.png)

**Throw-ins** - `throw.in`, `ti`

**Free kicks** - `free.kick`, `fk`

A ball given to a team after a foul. This should be logged for both shots and passes.

**Headed balls** - `headed`, `h`

This should be logged for both shots and passes.

**Corner kicks** - `corner.kick`, `ck`

This should be logged for corner kicks that are launched into the box and for those that are just short passes.

**Goal kick** - `goal.kick`, `gk`

This is separate from a `gk.drop.kick` in that a goal kick happens after a stoppage in play.

**Goalkeeper throws** - `gk.throws`, `gkt`

**Goalkeeper drop kicks** - `gk.drop.kick`, `gkdk`

This is separate from a `goal.kick` in that a drop kick is after a goalkeeper wins the ball from open play.

**Penalty kicks** - `pk`

#Defensive Player Actions
Column name: `def.action`

Not everything in the 'poss.action' will have a reaction from the defending team that gets to be logged. The following defensive actions, when they happen, are to be logged in the `def.action` column within the same event as the possessing action to which the defender is acting upon.

**Dispossessing through a ball shield** - `dispossess.ball.shield`, `dbs`

When a defender steps in between a player of the possessing team and shields the ball away from her until another player from the defending team either recovers the ball or clears it away, or until the ball goes out of bounds.

**Dispossessing through a steal** - `dispossess.steal`, `ds`

When a defender wins the ball without the possessing player having had a chance to take on the player or get rid of the ball. Usually the result of a defender sneaking up on a player from behind.

**Dispossessing a lost touch** - `dispossess.lost.touch`, `dlt`

When a defender steps up to win a ball that the possessing player, usually under pressure, gives away due to a bad touch.

**Tackling the ball away** - `tackles.ball.away`, `tba`

When a defender challenges a possessing player, connects with the ball while making contact and engaging with the player, and successfully dispossesses the possessing player of the ball for someone else to recover. As opposed to a tackle where the tackler also wins the ball (described below), this should be logged for when the ball is tackled away but the person making the tackle isn't the one to immediately recover it in the act of making a tackle. A tackle should be logged regardless of which team ends up recovering the loose  ball. 

If the defending player who made the tackle doesn't win the ball then she should be credited with a `tackles.ball.away` regardless of which team's player ends up recovering the loose ball. 

**Tackling and winning the ball** - `tackles.ball.won`, `tbw`

When a defender challenges a possessing player, connects with the ball while making contact and engaging with the player, and successfully dispossesses the possessing player of the ball and also clearly wins possession of the ball in the same instance as making the tackle. Sometimes a defender will tackle the ball some distance away and then run up to it to recover it, which should be logged as a `tackles.ball.away` followed by a `recoveries`, as the tackle wasn't what immediately won the ball.

**Dibbled by an opponent due to a missed tackle** - `dribbled.tackles.missed`, `dbm`

When a defender goes in for a tackle, misses the ball, and the possessing player dribbles past the missed tackle.

**Dribbled by an opponent due to being out-run** - `dribbled.out.run`, `dot`

Also known as getting "burnt." When a defender has a possessing player dribble past her without clearly going in for a tackle and without getting turned.

**Dribbled by an opponent due to being turned** - `dribbled.turned`, `dt`

When a defender is turned and allows a possessing player to dribble past her. Usually the result of a defender getting caught going the wrong way due to a feint.

**Pressuring an opponent** - `pressured`, `p`

When a defender applies pressure onto a possessing player's pass, shot, movement into another zone, ball shield, or recovery by stepping up, running at the player, or staying close in front of her, all with the intent of hurrying up the possessing player's play or impeding the possessing player's chance at making a play. There can be more than one defender deemed to be pressuring an opponent (for example, being double-teamed).

**Challenging an opponent** - `challenged`, `ch`

Same as a `pressured` instance, except these are instances when a defender ends up making contact with a possessing player as she is making one of the aforementioned plays (pass, shot, movement into another zone, ball shield, recovery), further challening that possessing player's ability to make that play on the ball. Like with pressuring an opponent, there can be more than one defender deemed to be challeging an opponent (for example, being double-teamed).

**Blocks** - `blocks`, `bl`

When a defender blocks a pass or a shot and creates a loose ball situation that usually either goes out of bounds or is recovered by another player.

**Interceptions** - `interceptions`, `int`

When a defender blocks a pass (or sometimes a shot) and clearly wins possession of the ball. These should NOT be counted for missed passes that go into open area, too far away from its intended recipient, and were going to be won by the opposing team anyways. Interceptions should be logged for passes that, were it not for the intercepting defender, were going to meet its intended recipient at her location or at the end of her run.

**Balls shielded** - `ball.shield`, `bs`

When a defender attempts to shield a loose ball that was in possession of the opposing team.

**Clearances** - `clearances`, `cl`

When a defender intentionally kicks a ball away, without an intended recipient, that was played by the opposing team, without having clearly won possession of the ball. Otherwise, it would be considered an interception followed by a clearance logged in the `poss.action` column.

**Aerial duels won** - `aerial.won`, `aw`

Aerial duels are when two players challenge for a 50/50 ball in the air. The first player to make contact with the ball is deemed to have won the aerial duel, regardless of where the ball ends up or who recovers it.

**Aerial duels lost** - `aerial.lost`, `al`

A player is deemed to have lost an aerial duel if the player challenging her for the ball got to the ball first, regardless of where the ball ends up or who recovers it.

When both players challenge for the ball and neither wins the ball (i.e., they both mistime their jumps) but can reasonable be said to have had a chance to win the ball, then they both get credidet with an "aerial.lost."

**Shots on goal stopped by a goalkeeper** - `gk.s.o.g.stop`, `gksogstop`

Credited to a goalkeeper when a shot on goal is faced and stopped by the goalkeeper.

**Shots on goal stopped by a defender** - `gk.s.o.g.def.stop`, `gksogdefstop`

Credited to the last defender, behind the goalie, who stops a shot that would have been scored.

**Shots on goal scored** - `gk.s.o.g.scored`, `gksogscored`

Credited to the goalkeeper when a goal is scored.

**Shots missed** - `gk.shot.miss`, `gksmiss`

Credited to the goalkeeper when a shot is missed.

**High balls won by the goalkeeper** - `gk.high.balls.won`, `gkhbw`

When a goalkeeper faces a high ball, usually from a corner kick, cross, or launch ball, and wins it by either gaining possession of the ball or clearing the ball away.

**High balls lost by the goalkeeper** - `gk.high.balls.lost`, `gkhbw`

When a goalkeeper faces a high ball, usually from a corner kick, cross, or launch ball, and loses it by either mishandling it or completely missing the ball.

**Smothers won by the goalkeeper** - `gk.smothers.won`, `gksw`

When a goalkeeper faces a take on by an opposing player in the box and wins it by successfully coming out to either claim the ball or clear it to safety.

**Smothers lost by the goalkeeper** - `gk.smothers.lost`, `gksl`

When a goalkeeper faces a take on by an opposing player in the box and loses it by missing the ball, causing a foul, or mishandling the ball.

**Loose balls claimed by the goalkeeper** - `gk.loose.balls.won`, `gklbw`

When a goalkeeper successfully claims a loose ball.

**Loose balls lost by the goalkeeper** - `gk.loose.balls.lost`, `gklbl`

When a goalkeeper unsuccessfully comes out for a loose ball. Usually the result of mishandling the ball.

#Goalkeeper Ball Stops
Column name: `gk.ball.stop`

When a goalkeeper makes an attempt to stop a ball with any part of her body, whether it's a high ball, loose ball, or shot, the type of stop attempt should be logged in the `gk.ball.stop` column. The stop will be one of the below.

**Caught** - `caught`

**Punched to safety** - `punched.to.safety`, `punched.safety`

Punching a ball out of bounds counts as "to safety."

**Punched to danger** - `punched.to.danger`, `punched.danger`

When a goalkeeper punches the ball away, but close to the goal and at the feet of an opponent ready to make a play on the ball. 

**Dropped** - `dropped`

When a goalkeeper mishandles the ball and ultimately loses it.

**Missed the ball** - `missed.the.ball`, `missed.ball`

When a goalkeeper misses the ball which rolls or flies by.

**Collected** - `collected`

When a goalkeeper doesn't cleanly catch the ball but does handle it, usually with a bounce, and ultimately collect it.

**Parried to safety** - `parried.to.safety`, `parried.safety`

When a goalkeeper gets a glancing touch on the ball and deviates it into safety. Parrying a ball out of bounds counts as "to safety."

**Parried to danger** - `parried.to.danger`, `parried.danger`

When a goalkeeper gets a glancing touch on the ball and deviates it away, but close to the goal and at the feet of an opponent ready to make a play on the ball.

**Deflected to safety** - `deflected.to.safety`, `deflected.safety`

When a goalkeeper uses a part of her body other than her hands (such as her body or legs) to deviate the direction of the ball into safety. Deviating a ball out of bounds counts as "to safety."

**Deflected to danger** - `deflected.to.danger`, `deflected.danger`

When a goalkeeper uses a part of her body other than her hands (such as her body or legs) to deviate the direction of the ball, but it ends up close to goal and at the feet of an opponent ready to make a play on the ball.



#Goalkeeper save attempt
Column name: `gk.s.o.g.attempt`

When a goalkeeper makes a save attempt on a shot on goal with any part of her body, the type of save will be one of the ones below and should be logged in the `gk.s.o.g.attempt` column.

**Diving save** - `diving`

**Standing save** - `standing`

**Reaching save** - `reaching`

**Stooping save** - `stooping`

**No save attempt** - `none`

#Disciplinary notes
Column names: `poss.player.disciplinary` & `def.player.disciplinary`

When a player wins or concededes foul, card, and/or penaly, it should be logged for the possessing player in the `poss.player.disciplinary` column and for the defending player in the `def.player.disciplinary` column. Sometimes more than one of these will apply, such as penalty kick conceded that was also a yellow card, in which case a comma should separate the two in the same cell so it looks like `yellow.cards,penalties.conceded`.

**Fould won** - `fouls.won`, `fw`

**Fouls conceded** `fouls.conceded`, `fc`

**Yellow cards** - `yellow.cards`, `yc`

**Red cards** - `red.cards`, `rc`

**Penalty kicks won** - `penalties.won`, `pw`

**Penalty kicks conceded** - `penalties.conceded`, `pc`

#Additional Possessing Player Notes
Column name: `poss.notes`

Certain possessing player actions need additional qualifiers, related to scoring opportunities or defensive mistakes, that don't fit in any of the other aforementioned columns and should instead be be logged in the `poss.notes` column. Sometimes more than one of these will apply, such as a big chance that was shot and missed, and thus went out of bounds, in which case a comma should separate the two in the same cell so it looks like `big.chances.shot.missed,out.of.bounds.lost.poss`.

**Big chances scored** - `big.chances.scored`, `bcs`

A big chance is a clear-cut goal scoring opportunity where a possessing player is reasonably expected to score. These are usually one-on-one chances with the goalkeeper or very close range and generally unpressured shots.

When a big chance has occurred, log `big.chances.scored` if the possessing player scores.

**Big chances shot on goal** - `big.chances.shot.on.goal`, `bcsog`

When a big chance has occured, log `big.chances.shot.on.goal` if the possessing player gets a shot on goal but does not score.

**Big chances missed** - `big.chances.shot.missed`, `bcsm`

When a big chance has occured, log `big.chances.shot.missed` if the possessing player misses the shot.

**Big chances disspossessed** - `big.chances.dispossessed`, `bcd`

When a big chance has occured, log `big.chances.dispossessed` if the possessing player gets dispossessed before having a chance at a shot on goal

**Big chances created** - `big.chances.created`, `bcc`

To be noted for plays, usually via successful take ons or interceptions in dangerous areas, where a player creates a big chance by herself.

**Assists** - `assists`, `a`

**Second assists** - `second.assists`, `sa`

A pass that wasn't an assist that was still instrumental in creating a scored big chance, such as a through ball to an player in the box who lays it off for the goalscorer to shoot.

**Unscored key passes** - `unscored.key.passes`, `ukp`

A pass instrumental in creating a big chance that wasn't converted into a goal.

**Ball goes out of bounds and possession is kept** - `out.of.bounds.keep.poss`, `kept.poss.out.of.bounds`, `kpob`

**Ball goes out of bounds and possession is lost** - `out.of.bounds.lost.poss`, `kept.poss.out.of.bounds`, `lpob`

**Errors leading to a goal for the opposition** - `errors.to.goals`, `etg`

**Errors leading to an unscored big chance for the opposition** - `errors.to.big.chances`, `etbc`

#Additonal Defensive Player Notes
Column name: `def.notes`

Similary for defending players, certain defensive actions will also need additional qualifiers, related to defensive accomplishments and mistakes, that don't fit in any of the other aforementioned columns and should instead be be logged in the `def.notes` column.

**Big chances stopped** - `big.chances.stopped`, `bcc`

When a defending player or a goalkeeper stops a possessing player's big chance from being scored or shot, such as with a block, tackle, or save.

**Own goals allowed** - `own.goals`, `og`

**Errors leading to a goal for the opposition** - `errors.to.goals`, `etg`

**Errors leading to an unscored big chance for the opposition** - `errors.to.big.chances`, `etbc`


#Location-based Notes
Column names: `poss.location`, `poss.play.destination`, & `def.location`

Each `poss.action` will have a location on the pitch, which will either be manually logged or coded into the `poss.location` column. If it is a type of play with a destination, such as a pass or movement, the destination on the pitch will also either be manually logged or coded into the `poss.play.destination` column.

Similarly, for each `def.action`, a location on the pitch for the defensive action will be either manually logged or coded into the `def.location` column.

The acronyms used for each location are defined below. To better get an idea of how the pitch is split up, refer to this image: 

![](http://i.imgur.com/EQLmpYp.png)

**Opponent’s 6-yard box** - `A6`

**Opponent’s 18-yard box** - `A18`

**Attacking third, left wing** - `A3L`

**Attacking third, center field** - `A3C`

**Attacking third, right wing** - `A3R`

**Opponent’s half of middle third, left wing** - `AM3L`

**Opponent’s half of middle third, center field** - `AM3C`

**Opponent’s half of middle third, right wing** - `AM3R`

**Own half of middle third, left wing** - `DM3L`

**Own half of middle third, center field** - `DM3C`

**Own half of middle third, right wing** - `DM3R`

**Defensive third, left wing** - `D3L`

**Defensive third, center field** - `D3C`

**Defensive third, right wing** - `D3R`

**Own 18-yard box** - `D18`

**Own 6-yard box** - `D6`
