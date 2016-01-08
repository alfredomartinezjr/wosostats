#Table of Contents
* Possessing Player Actions - `poss.action`
* Play Type - `play.type`
* Defensive Player Actions - `def.action`
* Goalkeeper Actions - `gk.ball.stop` & `gk.s.o.g.attempt`
* Disciplinary Actions - `poss.player.disciplinary` & `def.player.disciplinary`
* Other Notes - `poss.notes` & `def.notes`
* Location-based Notes - `poss.location`, `poss.play.destination`, & `def.location`

#Possessing Player Actions
Column name: poss.player.action

A key part of the way this model tracks match stats is the idea that at any given moment a team or player for a team has “possession” of the ball, and the other team or a player from the other team is playing in “defense.” 

The actions below will be tracked under the “poss.player.action” column. As the name suggests, they are actions that correspond to the player who belongs to the team deemed to be in “possession” of the ball. 

There are the obvious types of actions, such as shots, pass attempts, take ons. There are also actions that result in the ball being lost such as being dispossessed or losing a bad touch on the ball.

Say a player takes on a defender, the defender tackles the ball away from her, and a second defender recovers the ensuing loose ball. The player who took on the defender is credited with a “take.on.lost” action in the “poss.player.action” column, the defender who tackled the ball away is credited with a “tackles.ball.away” action in the “def.event.action” column, AND the second defender who recovered the loose ball and wins possession for her team triggers a new event since the  “def.recoveries” action that gets credited to her falls is to be logged in the “poss.player.action” column.

##Shots
###Shots stopped by the goalkeeper
``*shots.stopped.by.gk*``

A shot that would have been scored but for being stopped by a goalkeeper's save

![](http://i.imgur.com/SKaaerO.gif)

Shots stopped by a defender
shots.stopped.by.def
A shot that would have been scored but for being blocked by a defender who was the last defender

![](http://i.imgur.com/1rI71JW.gif)

Shots blocked by a defender
shots.blocked.by.def
A shot heading towards the goal that was blocked by a defender who had other defenders or the goalkeeper behind her.
Shots missed
shots.missed
A shot that misses the goal or hits the post or crossbar.

![](http://i.imgur.com/Dp3hVaX.gif?1)

Shots scored
shots.scored
A shot that goes into the goal

Pass Attempts
Passes attempts, intentional balls played to another player, are broken down in this category by direction (forward, sideways, backwards) and result (completed, backwards, missed)

A completed pass attempt is a pass attempt that results in the intended recipient either possessing the ball or being put in a position where she is expected to have maintained possession of the ball.

A blocked pass attempt is a pass attempt that was prevented from reaching its intended recipient by a defender who got in the way of the pass attempt. This includes clearances and interceptions. If a pass was deflected but still reached the intended recipient, it does not count as a blocked pass.

A missed pass attempt is a pass attempt, unblocked by a defender, which missed the intended recipient.

Forward pass attempts completed
passes.f.c
Forward pass attempts blocked
passes.f.b
Forward pass attempts missed
passes.f.m
Sideway pass attempts completed
passes.s.c
Sideway pass attempts blocked
passes.s.b
Sideway pass attempts missed
passes.s.m
Sideway pass attempts missed
passes.b.c
Sideway pass attempts missed
passes.b.b
Sideway pass attempts missed
passes.b.m

Possession
Movement to another zone (for more on zones of a field, refer to the location how-to). Each movement from one zone to another should be tracked separately.
movement
Examples:
Leigh Ann Brown, the player in white and blue, moves with the ball from her defensive right third, to her defensive right middle third, to the opposing right middle third, to her opposing right third. This would be three separate movement events: http://i.imgur.com/mYoa9eF.gif

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
