#Table of Contents
* Possessing Player Actions - `poss.action`
* Play Type - `play.type`
* Defensive Player Actions - `def.action`
* Goalkeeper Actions - `gk.ball.stop` & `gk.s.o.g.attempt`
* Disciplinary Actions - `poss.player.disciplinary` & `def.player.disciplinary`
* Other Notes - `poss.notes` & `def.notes`
* Location-based Notes - `poss.location`, `poss.play.destination`, & `def.location`

Below is a list of definitions for values to be used when logging match stats in the match-stats-template.xlsx Excel document. These values are player actions, such as passes and fouls, which take up most of this document, and location data, which merits its own section. When logging stats, refer to this document when in doubt about what a certain value means and when to log it. Feedback is welcome.

Each player action falls under one of nine different types of actions. In the match-stats-templace.xlsx Excel document, this looks like nine different columns.

A key part of this model is that during any given moment in a match there is a team in "possession" of the ball and a team "defending" the ball. As such, these nine different columns will fall into one of three different types of actions: 1) actions by a player from the team "in possession" of the ball, 2) actions by a player from the team "defending" against the ball, and 3) actions by a goalkeeper making a play on the ball.

Location data will be logged, either manually or coded after the fact, for three different instances, which will be discussed in the "Location-based Notes" section.

For each definition, the long name will be shown in **bold like this** and the logged name (the value that will actually be input into the spreadsheet) will be shown in `code span like this`. A GIF showing an example will be provided for a definition when appropriate and possible.

#Possessing Player Actions
Column name: `poss.player.action`

The actions below will be tracked under the `poss.player.action` column. As the name suggests, these are actions by a player on a team "in possession” of the ball. 

**Shots stopped by the goalkeeper** - `shots.stopped.by.gk`

A shot that would have been scored but for being stopped by a goalkeeper's save

![](http://i.imgur.com/SKaaerO.gif)

**Shots stopped by a defender** - `shots.stopped.by.def`

A shot that would have been scored but for being blocked the last defender behind the goalkeeper

![](http://i.imgur.com/1rI71JW.gif)

**Shots blocked by a defender** - `shots.blocked`

A shot heading towards the goal that was blocked by a defender who had other defenders and/or the goalkeeper behind her.

**Shots missed** - `shots.missed`

A shot that misses the goal or hits the post or crossbar.

![](http://i.imgur.com/Dp3hVaX.gif?1)

**Shots scored** - `shots.scored`

A shot that goes into the goal. Easy!

**Forward pass attempts** - `passes.f`

Forward pass attempts, regardless of whether the pass attempt was completed

**Sideway pass attempts** - `passes.s`

Sideway pass attempts, regardless of whether the pass attempt was completed 

**Backward pass attempts** - `passes.b`

Backward pass attempts, regardless of whether the pass attempt was completed 

**Movement into another zone** - `movement`
When a player moves from one "zone" (zone definitions are outlined in the "Location-based Data" section below) into another "zone." If a player consecutively moves from one zone into another, each instance should be tracked separately.

Example: [http://i.imgur.com/mYoa9eF.gif](http://i.imgur.com/mYoa9eF.gif)

In the GIF above, Leigh Ann Brown, the player in white and blue, moves with the ball from her defensive right third, to her defensive right middle third, to the opposing right middle third, to her opposing right third. This should be logged as three separate 'movement' events.

A take on is an attempt by a player to beat her defender and maintain possession of the ball. A take on is “won” if the player dribbles past a defender, turns a defender to create open space, or draws a foul. A take on is “lost” if the ball is tackled away
take.on.won
take.on.lost
Examples:
Mandy Laddish, the player in white and blue, wins a take on against Jess Fishlock, in the yellow and purple, by dribbling past a missed tackle: http://i.imgur.com/uTDOeay.gif
Erika Tymrak, the player in white and blue, wins a take on against Kendall Fletcher, in the yellow and purple, by running right past her for a chance at a cross: http://i.imgur.com/GXS3exL.gif
Megan Rapinoe, the player in yellow and purple, wins a take on against Erika Tymrak, in the white and blue, by turning her and dribbling past her into the open midfield: http://i.imgur.com/OVbQKZH.gif
Different take on attempts can occur in succession and should be logged separately. Here, Kim Little, the player in yellow and purple, wins a take on against #6 Jen Buczkowski by dribbling through her tackle attempt, but Little proceeds to lose the following take on attempt against #3 Becca Moros, who steps up to tackle the ball away from her: http://i.imgur.com/n0ThErg.gif

A dispossession is when the possessing player is dispossessed off the ball without attemping to “take on” her defender. This is meant to encompass moments such as a bad touch on the ball, being shielded away from ones own ball, or losing the ball to a defender who snuck up from behind.
dispossessed
Examples:
Jess Fishlock, the player in yellow and purple, receives a pass from Kim Little but loses the ball to Maddy Laddish, in the white and blue, due to a bad first touch. Since it is not clear that Fishlock was attempting to take on Laddish with that touch, this is logged as a dispossession: http://i.imgur.com/9CaWO5l.gif
Keelin Winters, the player in yellow and purple who receives the pass, is dispossessed from behind by Jen Buczkowski, in the white and blue. Since it does not appear like Winters did not have a chance to attempt a take on, this is logged as a dispossession: http://i.imgur.com/wfXAGUx.gif

Aerial duels are when two players challenge in the air for a 50/50 ball. 
aerial.challenges.won
aerial.challenges.lost

Defensive Plays
interceptions
def.recoveries
poss.recoveries
balls.shielded
clearances

Miscellaneous
playcutoffbybroadcast
offside.calls
