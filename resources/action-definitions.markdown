#Possessing Player Actions
Column name: poss.player.action

A key part of the way this model tracks match stats is the idea that at any given moment a team or player for a team has “possession” of the ball, and the other team or a player from the other team is playing in “defense.” 

The actions below will be tracked under the “poss.player.action” column. As the name suggests, they are actions that correspond to the player who belongs to the team deemed to be in “possession” of the ball. 

There are the obvious types of actions, such as shots, pass attempts, take ons. There are also actions that result in the ball being lost such as being dispossessed or losing a bad touch on the ball.

Say a player takes on a defender, the defender tackles the ball away from her, and a second defender recovers the ensuing loose ball. The player who took on the defender is credited with a “take.on.lost” action in the “poss.player.action” column, the defender who tackled the ball away is credited with a “tackles.ball.away” action in the “def.event.action” column, AND the second defender who recovered the loose ball and wins possession for her team triggers a new event since the  “def.recoveries” action that gets credited to her falls is to be logged in the “poss.player.action” column.

Shots
Shots stopped by the goalkeeper
shots.stopped.by.gk
A shot that would have been scored but for being stopped by a goalkeeper's save

![](http://i.imgur.com/REK3TLU.gif)

Shots stopped by a defender
shots.stopped.by.def
A shot that would have been scored but for being blocked by a defender who was the last defender
http://i.imgur.com/sasP8Jn.gif
Shots blocked by a defender
shots.blocked.by.def
A shot heading towards the goal that was blocked by a defender who had other defenders or the goalkeeper behind her.
Shots missed
shots.missed
A shot that misses the goal or hits the post or crossbar.
http://i.imgur.com/e0BpEze.gif
Shots scored
shots.scored
A shot that goes into the goal
http://i.imgur.com/7aQaxMn.gif
