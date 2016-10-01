This document is meant to instruct and aid anyone who is adding location data to a match log that already has every match action logged. For adding location, it's largely a matter of adding in location data for certain actions as you watch a replay of the match.

#Before you start adding location
Make sure you understand how a soccer field is split up in this model and how the match you are logging is split up. In general, a player will either be in the center, left, or right part of the field, relative to the direction her team is going, and she will either be in the defensive, middle, or attacking third of the field, again, also relative to the direction her team is going.

Field dimensions can vary widely, so figure out where the borders of the middle third are, so you can tell if a player is in their attacking, middle, or defensive third. Try to also get a sense of where the left, center, and right thirds of the fields are. The left and right thirds will usually cut a few yards into the 18-yard box, and the center third will usually extend a few yards from the center circle. Width of the field can vary widely, so use your best judgment here.
  * The breakdown of the field should be based on this. This image, by the way, is for a team that is going left to right (as in, for a team whose goalie is in the left goal and they are trying to score in the right goal): ![](http://i.imgur.com/EQLmpYp.png)
    * More on what each acronym means is covered in the next section.
  * It helps to use landmarks around the stadium to figure this out. For example, if the field has football lines or lawn stripes, you can use them to figure out the thirds of the field. ![](http://i.imgur.com/atMDAJZ.png)
  * Or, you can also use the stands as landmarks.
  * Worst comes to worst, find the widest shot you can of the field, either during the match stream or from an image online, and literally use a ruler to figure out the thirds of the fields. ![](http://i.imgur.com/eA1YDtA.png)

#Understand what each action & qualifier means (and what their shortcuts are)
Read through the [definitions.md document](https://github.com/amj2012/wosostats/blob/master/resources/definitions.md) to understand what each action and qualifier is, and what their shortcuts are as most Excel files will have used shortcuts to save on time. This will help you understand what in the match is in the Excel file.

#Understand what the location acronyms mean
For location data, you will log one of the following acronyms, depending on the player's position, relative to which direction her team is going, on [a field split up like this.](http://i.imgur.com/EQLmpYp.png)
  * **Opponent’s 6-yard box** - `A6`
  * **Opponent’s 18-yard box** - `A18`
  * **Attacking third, left wing** - `AL`
  * **Attacking third, center field** - `AC`
  * **Attacking third, right wing** - `AR`
  * **Opponent’s half of middle third, left wing** - `AML`
  * **Opponent’s half of middle third, center field** - `AMC`
  * **Opponent’s half of middle third, right wing** - `AMR`
  * **Own half of middle third, left wing** - `DML`
  * **Own half of middle third, center field** - `DMC`
  * **Own half of middle third, right wing** - `DMR`
  * **Defensive third, left wing** - `DL`
  * **Defensive third, center field** - `DC`
  * **Defensive third, right wing** - `DR`
  * **Own 18-yard box** - `D18`
  * **Own 6-yard box** - `D6`

#While adding location data
You will be adding location data to the "poss.location", "poss.player.destination", and "def.location" columns, but not for every action. Here are the rules to follow for each column.
  * **For the "poss.location" column:** Add location data for every action.
  * **For the "poss.play.destination" column:** DO NOT add location data EXCEPT for successful passes or movement where the following action was cut off by either a stoppage in play (such as a player injury or halftime) or a cut in the broadcast. All other successful passes and movement will be computed by the R code that runs through an Excel file and fills in empty values, based details of subsequent events.
  * **For the "def.location" column:** DO NOT add location data EXCEPT for the following defensive actions (again, the reason is that these are events for which the R code can't automatically compute the location based on the location of surrounding actions):
    * **Blocks** - `blocks`
    * **Interceptions** - `interceptions`
    * **Balls shielded** - `ball.shield`
    * **Clearances** - `clearances`
    * All goalkeeper-specific events

