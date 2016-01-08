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
Column name: `poss.action`

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

In the GIF above, Leigh Ann Brown, the player in white and blue, moves with the ball from her defensive right third, into her defensive right middle third, into her opposing right middle third, to her opposing right third. This should be logged as three separate 'movement' events; one from defensive right third into defensive right middle third, one from defensive right middle third into opposing right middle third, and one from opposing right middle third into opposing right third.

**Take ons won** - `take.on.won`

A take on is an attempt by a player to get past her defender and maintain possession of the ball. A take on is “won” if the player dribbles past a defender, turns a defender to create open space, or draws a foul. 

Example 1: [http://i.imgur.com/uTDOeay.gif](http://i.imgur.com/uTDOeay.gif)

Mandy Laddish, the player in white and blue, wins a take on against Jess Fishlock, in the yellow and purple, by dribbling past a missed tackle

Example 2:  [http://i.imgur.com/GXS3exL.gif](http://i.imgur.com/GXS3exL.gif)

Erika Tymrak, the player in white and blue, wins a take on against Kendall Fletcher, in the yellow and purple, by running right past her for a chance at a cross.

Example 3: [http://i.imgur.com/OVbQKZH.gif](http://i.imgur.com/OVbQKZH.gif)

Megan Rapinoe, the player in yellow and purple, wins a take on against Erika Tymrak, in the white and blue, by turning her and dribbling past her into the open midfield: 

**Take ons lost** - `take.on.lost`

A take on is “lost” if the ball is tackled away, regardless of who recovers the ball or where it ends up, or if the player loses possession through a lost touch or a ball shield.

Example: [http://i.imgur.com/n0ThErg.gif](http://i.imgur.com/n0ThErg.gif)

Different take on attempts can occur in succession and should be logged separately. Here, Kim Little, the player in yellow and purple, wins a take on against #6 Jen Buczkowski by dribbling through her tackle attempt, but Little proceeds to lose the following take on attempt against #3 Becca Moros, who steps up to tackle the ball away from her.

**Dispossessed of the ball** - `dispossessed`

A dispossession is when the player in possession of the ball loses the ball to a defender without attemping to “take on” the defender. This is different from a lost take on in that it encompasses moments such as a bad touch on the ball or losing the ball to a defender who snuck up from behind.

Example 1: [http://i.imgur.com/9CaWO5l.gif](http://i.imgur.com/9CaWO5l.gif)

Jess Fishlock, the player in yellow and purple, receives a pass from Kim Little but loses the ball to Maddy Laddish, in the white and blue, due to a bad first touch. Since it is not clear that Fishlock was attempting to take on Laddish with that touch, this is logged as a dispossession: 

Example 2: [http://i.imgur.com/wfXAGUx.gif](http://i.imgur.com/wfXAGUx.gif)
Keelin Winters, the player in yellow and purple who receives the pass, is dispossessed from behind by Jen Buczkowski, in the white and blue. Since it does not appear like Winters had a chance to attempt a take on, this is logged as a dispossession.

**Aerial duels won** - `aerial.won`

Aerial duels are when two players challenge for a 50/50 ball in the air. In the `poss.action` column, The first player to make contact with the ball is deemed to have won the aerial duel, regardless of where the ball ends up or who recovers it.

**Aerial duels lost** - `aerial.lost`

A player is deemed to have lost an aerial duel if she

**Recoveries**

**Balls shielded**

**Clearances**

`playcutoffbybroadcast`

`offside.calls`

`stoppage.in.play`

`substitution.on`

`substitution.off`

`halftime`

`fulltime`

`end.of.1.ET`

`end.of.2.ET`

`end.of.match`

#Play Types
Column name: `play.type`

`corner.crosses`

`deep.crosses`

`switch`

`launch`

`through`

`lay.off`

`flick.on`

`throw.in`

`free.kick`

`headed`

`corner.kick`

`goal.kick`

`gk.throws`

`gk.drop.kick`

`pk`

#Defensive Player Actions
Column name: `def.action`

dispossess.ball.shield
dispossess.steal
dispossess.lost.touch
tackles.ball.away
tackles.ball.won
dribbled.tackles.missed
dribbled.out.run
dribbled.turned
pressured
challenged
blocks
interceptions
ball.shield
clearances
aerial.won
aerial.lost
gk.s.o.g.stop
gk.s.o.g.def.stop
gk.s.o.g.scored
gk.shot.miss
gk.high.balls.won
gk.high.balls.lost
gk.smothers.won
gk.smothers.lost
gk.loose.balls.won
gk.loose.balls.lost
