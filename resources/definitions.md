*This is a list of action definitions that are meant to inform how actions should be logged. It is meant to be used by people who are either logging stats on their own or would like to have a look at how they are being defined. If you would like to see definitions for stats that are calculated from these actions (such as in the Shiny app or on various R Markdown docs), you'll want to go to the stats-glossary.md page!*

#Table of Contents
* Possessing Player Actions - `poss.action`
* Play Type - `play.type`
* Defensive Player Actions - `def.action`
* Goalkeeper Actions - `gk.ball.stop` & `gk.s.o.g.attempt`
* Disciplinary Actions - `poss.player.disciplinary` & `def.player.disciplinary`
* Additonal Possessing Player Notes - `poss.notes`
* Additonal Defensive Player Notes - `def.notes`
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

**Lost touch** - `lost.touch`

A lost touch is when a player loses possession of the ball, without having taken a shot or attempting to pass the ball, and without having been under pressure or having been challenged.

**Aerial duels won** - `aerial.won`

Aerial duels are when two players challenge for a 50/50 ball in the air. The first player to make contact with the ball is deemed to have won the aerial duel, regardless of where the ball ends up or who recovers it.

Passes that are also aerial duels should still be counted as aerial duels in a separate row as its own event. So, if a player challenges for a launched ball and heads it to an intended recipient, it should be logged as an `aerial.won` in one event and as a `passes.f/s/b` in the next event.

**Aerial duels lost** - `aerial.lost`

A player is deemed to have lost an aerial duel if the player challenging her for the ball got to the ball first, regardless of where the ball ends up or who recovers it.

**Recoveries** - `recoveries`

A recovery is when a player gets posession of a loose ball.  In the `poss.action' column, a recovery should be logged to a player if the last player to have possession of the ball was on the same team. A recovery usually happens after a player on the defending team interferes with the ball such as with (but not limited to) a block, a won aerial duel, a clearance, or a tackle. 

For a ball to change possession under this model, a player on the opposing team must have clear possession of the ball; otherwise, it's created a loose ball that is still technically under "possession" by the other team and is thus recovered when a player wins it back. Recoveries are a way of noting how a team wins or maintains possession.

**Balls shielded** - `ball.shield`

A ball shield is when a player successfully and intentionally uses her body to shield the ball from a defender as it goes out of play.

**Clearances** - `clearances`

A clearance is logged in the `poss.action` column when a player in possession of the ball intentionally kicks the ball away without an intended recipient.

**Offside Calls** - `offside.calls`

Logged when a player is called offside.

**Breaks in play or broadcast**

**Play cut off by broadcast** - `playcutoffbybroadcast`

These are pesky instances when the broadcast of the game is cut off by something such as a replay or sideline interview. They can completely cut off your ability to log match stats and can affect how stats are analyzed if they aren't outright mentioned. They should be logged in the `poss.action` column.

**Substitutions** - `substitution.on` & `substitution.off`

Note which players are being substituted on and off.

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

Certain shots and passes logged in the `poss.action` column will be of special types that should be further logged in this column. Sometimes more than one of these will apply, such as a lay off that was also headed, in which case a comma should separate the two in the same cell so it looks like `lay.off,headed`

**Corner crosses** - `corner.crosses`

Crosses, a ball launched from the left third or right third of the field into the box, that were also struck from within the attacking third. 

**Deep crosses** - `deep.crosses`

Crosses, a ball launched from the left third or right third of the field into the box, that were also struck from beyond the attacking third.

**Switch** - `switch`

A long, high ball to an intended recipient across the field

**Launch balls** - `launch`

Also sometimes known as "long balls." Long, high balls into open space, not clearly towards any one intended recipient, or into a crowded area. If they come from the left or right thirds of the field, they should be logged as crosses and NOT as a launch ball.

**Through balls** - `through`

A pass that splits the defense, into open space, to meet a teammate at the end of her run.

**Lay-off balls** - `lay.off`

A one-touch pass into the direction from where the ball came from.

**Flick-on balls** - `flick.on`

A glancing pass into the same general direction from which it came from.

**Throw-ins** - `throw.in`

**Free kicks** - `free.kick`

This should be logged for both shots and passes.

**Headed balls** - `headed`

This should be logged for both shots and passes.

**Corner kicks** - `corner.kick`

This should be logged for corner kicks that are launched into the box and for those that are just short passes.

**Goal kick** - `goal.kick`

This is separate from a `gk.drop.kick` in that a goal kick happens after a stoppage in play.

**Goalkeeper throws** - `gk.throws`

**Goalkeeper drop kicks** - `gk.drop.kick`

This is separate from a `goal.kick` in that a drop kick is after a goalkeeper wins the ball from open play.

**Penalty kicks** - `pk`


#Defensive Player Actions
Column name: `def.action`

Not everything in the 'poss.action' will have a reaction from the defending team that will be logged. The following defensive actions are to be logged in the `def.action` column for the corresponding event to which they are reacting.

**Dispossessing through a ball shield** - `dispossess.ball.shield`

When a defender steps in between a player of the possessing team and shields the ball away from her for either another player from the defending team to recover or until the ball goes out of bounds.

**Dispossessing through a steal** - `dispossess.steal`

When a defender wins the ball without the possessing player having had a chance to take on the player or get rid of the ball. Usually the result of a defender sneaking up on a player from behind.

**Dispossessing a lost touch** - `dispossess.lost.touch`

When a defender steps up to win a ball that the possessing player, usually under pressure, gives away due to a bad touch.

**Tackling the ball away** - `tackles.ball.away`

When a defender challenges a possessing player, connects with the ball, and successfully tackles the ball away for another player to recover. If the defending player who made the tackle doesn't win the ball then she should be credited with a `tackles.ball.away` regardless of which team's player ends up recovering the loose ball.

**Tackling and winning the ball** - `tackles.ball.won`

When a defender challenges a possessing player, connects with the ball, successfully tackles the ball away, and wins possession of the ball.

**Dibbled by an opponent due to a missed tackle** - `dribbled.tackles.missed`

When a defender goes in for a tackle, misses the ball, and the possessing player dribbles past the missed tackle.

**Dribbled by an opponent due to being out-run** - `dribbled.out.run`

Also known as getting "burnt." When a defender has a possessing player dribble past her without clearly going in for a tackle.

**Dribbled by an opponent due to being turned** - `dribbled.turned`

When a defender is turned and allows a possessing player to dribble past her. Usually the result of a defender getting caught going the wrong way due to a feint.

**Pressuring an opponent** - `pressured`

When a defender applies pressure onto a possessing player's pass, shot, movement into another zone, ball shield, or recovery by stepping up, running at the player, or staying close in front of her, all with the intent of hurrying up the possessing player's play or impeding the possessing player's chance at making a play. There can be more than one defender deemed to be pressuring an opponent (for example, being double-teamed).

**Challenging an opponent** - `challenged`

Same as a `pressured` instance, except these are instances when a defender ends up making contact with a possessing player as she is making one of the aforementioned plays (pass, shot, movement into another zone, ball shield, recovery), further challening that possessing player's ability to make that play on the ball. Like with pressuring an opponent, there can be more than one defender deemed to be challeging an opponent (for example, being double-teamed).

**Blocks** - `blocks`

When a defender blocks a pass or a shot and creates a loose ball situation that usually either goes out of bounds or is recovered by another player.

**Interceptions** - `interceptions`

When a defender blocks a pass (or sometimes a shot) and clearly wins possession of the ball. These should NOT be counted for missed passes that go into open area, too far away from its intended recipient, and were going to be won by the opposing team anyways. Interceptions should be logged for passes that, were it not for the intercepting defender, were going to meet its intended recipient at her standing location or at the end of her run.

**Balls shielded** - `ball.shield`

When a defender attempts to shields a loose ball that was in possession of the opposing team.

**Clearances** - `clearances`

When a defender intentionally kicks a ball away, without an intended recipient, that was played by the opposing team, without having clearly won possession of the ball. Otherwise, it would be considered an interception followed by a clearance logged in the `poss.action` column.

**Aerial duels won** - `aerial.won`

Aerial duels are when two players challenge for a 50/50 ball in the air. The first player to make contact with the ball is deemed to have won the aerial duel, regardless of where the ball ends up or who recovers it.

**Aerial duels lost** - `aerial.lost`

A player is deemed to have lost an aerial duel if the player challenging her for the ball got to the ball first, regardless of where the ball ends up or who recovers it.

When both players challenge for the ball and neither wins the ball (i.e., they both mistime their jumps) but can reasonable be said to have had a chance to win the ball, then they both get credidet with an "aerial.lost."

**Shots on goal stopped by a goalkeeper** - `gk.s.o.g.stop`

Credited to a goalkeeper when a shot on goal is faced and stopped by the goalkeeper.

**Shots on goal stopped by a defender** - `gk.s.o.g.def.stop`

Credited to the last defender, behind the goalie, who stops a shot that would have been scored.

**Shots on goal scored** - `gk.s.o.g.scored`

Credited to the goalkeeper when a goal is scored.

**Shots missed** - `gk.shot.miss`

Credited to the goalkeeper when a shot is missed.

**High balls won by the goalkeeper** - `gk.high.balls.won`

When a goalkeeper faces a high ball, usually from a corner kick, cross, or launch ball, and wins it by either gaining possession of the ball or clearing the ball away.

**High balls lost by the goalkeeper** - `gk.high.balls.lost`

When a goalkeeper faces a high ball, usually from a corner kick, cross, or launch ball, and loses it by either mishandling it or completely missing the ball.

**Smothers won by the goalkeeper** - `gk.smothers.won`

When a goalkeeper faces a take on by an opposing player in the box and wins it by successfully coming out to either claim the ball or clear it to safety.

**Smothers lost by the goalkeeper** - `gk.smothers.lost`

When a goalkeeper faces a take on by an opposing player in the box and loses it by missing the ball, causing a foul, or mishandling the ball.

**Loose balls claimed by the goalkeeper** - `gk.loose.balls.won`

When a goalkeeper successfully claims a loose ball.

**Loose balls lost by the goalkeeper** - `gk.loose.balls.lost`

When a goalkeeper unsuccessfully comes out for a loose ball. Usually the result of mishandling the ball.

#Goalkeeper Ball Stops
Column name: `gk.ball.stop`

When a goalkeeper makes an attempt to stop a ball with any part of her body, whether it's a high ball, loose ball, or shot, the type of stop attempt should be logged in the `gk.ball.stop` column. The stop will be one of the below.

**Caught** - `caught`

**Punched to safety** - `punched.to.safety`

Punching a ball out of bounds counts as "to safety."

**Punched to danger** - `punched.to.danger`

When a goalkeeper punches the ball away, but close to the goal and at the feet of an opponent ready to make a play on the ball. 

**Dropped** - `dropped`

When a goalkeeper mishandles the ball and ultimately loses it.

**Missed the ball** - `missed.the.ball`

When a goalkeeper misses the ball which rolls or flies by.

**Collected** - `collected`

When a goalkeeper doesn't cleanly catch the ball but does handle it, usually with a bounce, and ultimately collect it.

**Parried to safety** - `parried.to.safety`

When a goalkeeper gets a glancing touch on the ball and deviates it into safety. Parrying a ball out of bounds counts as "to safety."

**Parried to danger** - `parried.to.danger`

When a goalkeeper gets a glancing touch on the ball and deviates it away, but close to the goal and at the feet of an opponent ready to make a play on the ball.

**Deflected to safety** - `deflected.to.safety`

When a goalkeeper uses a part of her body other than her hands (such as her body or legs) to deviate the direction of the ball into safety. Deviating a ball out of bounds counts as "to safety."

**Deflected to danger** - `deflected.to.danger`

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

**Fould won** - `fouls.won`

**Fouls conceded** `fouls.conceded`

**Yellow cards** - `yellow.cards`

**Red cards** - `red.cards`

**Penalty kicks won** - `penalties.won`

**Penalty kicks conceded** - `penalties.conceded`

#Additional Possessing Player Notes
Column name: `poss.notes`

Certain possessing player actions need additional qualifiers, related to scoring opportunities or defensive mistakes, that don't fit in any of the other aforementioned columns and should instead be be logged in the `poss.notes` column. Sometimes more than one of these will apply, such as a big chance that was shot and missed, and thus went out of bounds, in which case a comma should separate the two in the same cell so it looks like `big.chances.shot.missed,out.of.bounds.lost.poss`.

**Big chances scored** - `big.chances.scored`

A big chance is a clear-cut goal scoring opportunity where a possessing player is reasonably expected to score. These are usually one-on-one chances with the goalkeeper or very close range and generally unpressured shots.

When a big chance has occurred, log `big.chances.scored` if the possessing player scores.

**Big chances shot on goal** - `big.chances.shot.on.goal`

When a big chance has occured, log `big.chances.shot.on.goal` if the possessing player gets a shot on goal but does not score.

**Big chances missed** - `big.chances.shot.missed`

When a big chance has occured, log `big.chances.shot.missed` if the possessing player misses the shot.

**Big chances disspossessed** - `big.chances.dispossessed`

When a big chance has occured, log `big.chances.dispossessed` if the possessing player gets dispossessed before having a chance at a shot on goal

**Big chances created** - `big.chances.created`

To be noted for plays, usually via successful take ons or interceptions in dangerous areas, where a player creates a big chance by herself.

**Assists** - `assists`

**Second assists** - `second.assists`

A pass that wasn't an assist that was still instrumental in creating a scored big chance, such as a through ball to an player in the box who lays it off for the goalscorer to shoot.

**Unscored key passes** - `unscored.key.passes`

A pass instrumental in creating a big chance that wasn't converted into a goal.

**Ball goes out of bounds and possession is kept** - `out.of.bounds.keep.poss`

**Ball goes out of bounds and possession is lost** - `out.of.bounds.lost.poss`

**Errors leading to a goal for the opposition** - `errors.to.goals`

**Errors leading to an unscored big chance for the opposition** - `errors.to.big.chances`

#Additonal Defensive Player Notes
Column name: `def.notes`

Similary for defending players, certain defensive actions will also need additional qualifiers, related to defensive accomplishments and mistakes, that don't fit in any of the other aforementioned columns and should instead be be logged in the `def.notes` column.

**Big chances stopped** - `big.chances.stopped`

When a defending player or a goalkeeper stops a possessing player's big chance from being scored or shot, such as with a block, tackle, or save.

**Own goals allowed** - `own.goals`

**Errors leading to a goal for the opposition** - `errors.to.goals`

**Errors leading to an unscored big chance for the opposition** - `errors.to.big.chances`


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
