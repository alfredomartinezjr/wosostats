*This is a glossary for stats shown in [the WoSo Stats Shiny app](https://amj2012.shinyapps.io/wosostats/) and in [the GitHub repository](https://github.com/amj2012/wosostats/tree/master/analysis/csv). It is a work in progress and will change over time as more stats are added.*

*The definitions are in the order in which they appear in the stats table.*

*Send me a DM at @WoSoStats on Twitter if something needs fixing or further explanation.*

**MP** - Minutes played

**GS** - Games started

##Shots & Goals Stats

**Goals** - All goals scored

**Shots** - All shots taken and not blocked by a defender in a situation where the defender was the last defender preventing a goal. Simply put, shots that were missed or shot on goal.

**Shots per 908** - Shots per 90 minutes. Calculated by dividing all *Shots* by *MP*, and multiplying the product by 90. Is meant to account for differences in playing time.

**Shot Accuracy** - The percentage of all **Shots** that were shot on goal. Calculated by dividing all shots on goal (**Shot GK Stop** + **Shot Def Stop** + **Goals**) by all shots, excluding blocks (**Shot GK Stop** + **Shot Def Stop** + **Goals** + **Shot Miss**).

**Shot GK Stop** - Shots on goal stopped by the goalkeeper.

**Shot Def Stop** - Shots on goal stopped by the last defender that prevented what otherwise would have been a goal.

**Shot Miss** - Shots that miss the goal or hit either the crossbar or post.

**Shot Pressd** - Shots taken under pressure from a defender.

**Pct Shots Pressd** - The percentage of all shots that were taken under pressure.

**A6 Shots** - Shots taken from the 6-yard box.

**A18 Shots** - Shots taken from the 18-yard box.

**A3L Shots** - Shots taken from the left wing of the attacking 3rd.

**A3C Shots** - Shots taken from the center of the attacking 3rd.

**A3R Shots** - Shots taken from the right wing of the attacking 3rd.

**Far Shots** - Shots taken from beyond the attacking 3rd.

##Key Passes & Big Chances Stats

**Assists** - All completed passes made to the goalscorer, regardless of whether they were **Key Passes**.

**Key Passes** - All passes instrumental in creating a clear goal-scoring opportunity (**Big Chance**), regardless of whether it was converted into a goal.

**Key Assists** - Assists that were also key passes, instrumental in creating the converted goal-scoring opportunity.

**Second Assists** - Passes that weren't the last pass to the goalscorer but were instrumental in creating the converted goal-scoring opportunity.

**Assists per 90** - Assists per 90 minutes. Calculated by dividing all **Assists** by **MP**, and multiplying the product by 90. Is meant to account for differences in playing time.

**Key Passes per 90** - Key passes per 90 minutes. Calculated by dividing all **Key Passes** by **MP**, and multiplying the product by 90. Is meant to account for differences in playing time.

**Big Chances** - Clear-cut goal scoring opportunities where the possessing player is reasonably expected to score.

**Big Chances per 90** - Big chance per 90 minutes. Calculated by dividing all **Big Chances** by **MP**, and multiplying the product by 90. Is meant to account for differences in playing time.

**BC Scored** - Big chances that resulted in a goal.

**BC Conversion Pct** - Percent of all big chances that resulted in a goal.

**BC SOG** - Big chances that resulted in a shot on goal.

**BC Shot Miss** - Big chances that resulted in a missed shot.

**BC Dispossess** - Big chances that were dispossessed by a defending player.

**BC Created** - Instances where a possessing player creates a big chance for herself, such as via a take-on or interception.

**BC Lost** -  Big chances that were lost due to a lost or missed touch.

##Passing Stats

**Pass Comp** - All completed passes.

**Pass Att** - All attempted passes.

**Pass Comp Pct** - Percentage of all attempted passes that were completed. Calculated by dividing **Pass Comp** by **Pass Att**.

**opPass Comp** - All open play passes that were completed. Excludes throw-ins, free kick passes, goal kicks, and corner kick passes.

**opPass Att** - All open play passes that were attempted. Excludes throw-ins, free kick passes, goal kicks, and corner kick passes.

**opPass Comp Pct** - Percentage of all attempted open play passes (excludes throw-ins, free kick passes, goal kicks, and corner kick passes) that were completed. Calculated by dividing **opPass Comp** by **opPass Att**.

**opPPass Comp** - All open play passes that were completed under pressure. Excludes throw-ins, free kick passes, goal kicks, and corner kick passes.

**opPPass Att** - All open play passes that were attempted under pressure. Excludes throw-ins, free kick passes, goal kicks, and corner kick passes.

**opPPass Comp Pct** - Percentage of all open play passes attempted under pressure (excludes throw-ins, free kick passes, goal kicks, and corner kick passes) that were completed. Calculated by dividing **opPPass Comp** by **opPPass Att**.

**Pct opPass Pressd** - Percentage of all open play pass attempts that were taken under pressure.

**rFreq Pass Fwd** - Percentage of all passes that were forward passes.

**rFreq Pass Side** - Percentage of all passes that were sideway passes.

**rFreq Pass Back** - Percentage of all passes that were backward passes.

**fwPass Comp** - All completed forward passes.

**fwPass Att** - All attempted forward passes.

**fwPass Comp Pct** - Percentage of all attempted forward passes that were completed. Calculated by dividing **fwPass Comp** by **fwPass Att**.

**sPass Comp** - All completed sideway passes.

**sPass Att** - All attempted sideway passes.

**sPass Comp Pct** - Percentage of all sideway passes that were completed. Calculated by dividing **sPass Comp** by **sPass Att**.

**bPass Comp** - All completed backward passes.

**bPass Att** - All attempted backward passes.

**bPass Comp Pct** - Percentage of all backward passes that were completed. Calculated by dividing **bPass Comp** by **bPass Att**.

**Pass Comp D3toD3** - All completed passes that occured within the defensive 3rd.

**Pass Comp D3toM3** - All completed passes that went from the defensive 3rd to the middle 3rd.

**Pass Comp D3toA3** - All completed passes that went from the defensive 3rd to the attacking 3rd.

**Pass Comp M3toD3** - All completed passes that went from the middle 3rd to the defensive 3rd.

**Pass Comp M3toM3** - All completed passes that occured within the middle 3rd.

**Pass Comp M3toA3** - All completed passes that went from the middle 3rd to the attacking 3rd.

**Pass Comp A3toD3** - All completed passes that went from the attacking 3rd to the defensive 3rd.

**Pass Comp A3toM3** - All completed passes that went from the attacking 3rd to the middle 3rd.

**Pass Comp A3toA3** - All completed passes that occured within the attacking 3rd.

**rFreq opPass Fwd** - Percentage of all open play passes (excludes throw-ins, free kick passes, goal kicks, and corner kick passes) that were forward passes.

**rFreq opPass Side** - Percentage of all open play passes (excludes throw-ins, free kick passes, goal kicks, and corner kick passes) that were sideway passes.

**rFreq opPass Back** - Percentage of all open play passes (excludes throw-ins, free kick passes, goal kicks, and corner kick passes) that were backward passes.

**fwopPass Comp** - All open play forward passes that were completed.

**fwopPass Att** - All open play forward passes that were attempted.

**fwopPass Comp Pct** - Percentage of all open play forward passes that were completed. Calculated by dividing **fwopPass Comp** by **fwopPass Att**.

**sopPass Comp** - All open play sideway passes that were completed.

**sopPass Att** - All open play sideway passes that were attempted. 

**sopPass Comp Pct** - Percentage of all open play sideway passes that were completed. Calculated by dividing **sopPass Comp** by **sopPass Att**.

**bopPass Comp** - All open play backward passes that were completed.

**bopPass Att** - All open play backward passes that were attempted.

**bopPass Comp Pct** - Percentage of all open play backward passes that were completed. Calculated by dividing **bopPass Comp** by **bopPass Att**.

**Pct Pass Pressd** - Percentage of all passes that were taken under pressure.

**PPass Comp** - All passes attempted under pressure that were completed.

**PPass Att** - All passes attempted under pressure.

**PPass Comp Pct** - Percentage of all passes attempted under pressure that were completed. Calculated by dividing **PPass Comp** by **PPass Att**.

**rFreq PPass Fwd** - Percentage of all passes attempted under pressure that were forward passes.

**rFreq PPass Side** - Percentage of all passes attempted under pressure that were sideway passes.

**rFreq PPass Back** - Percentage of all passes attempted under pressure that were backward passes.

**fwPPass Comp** - Forward passes attempted under pressure that were completed.

**fwPPass Att** - Forward passes attempted under pressure.

**fwPPass Comp Pct** - Percentage of all forward passes attempted under pressure that were completed. Calculated by dividing **fwPPass Comp** by **fwPPass Att**.

**sPPass Comp** - Sideway passes attempted under pressure that were completed.

**sPPass Att** - Sideway passes attempted under pressure.

**sPPass Comp Pct** - Percentage of all sideway passes attempted under pressure that were completed. Calculated by dividing **sPPass Comp** by **sPPass Att**. 

**bPPass Comp** - Backward passes attempted under pressure that were completed.

**bPPass Att** - Backward passes attempted under pressure.

**bPPass Comp Pct** - Percentage of all backward passes attempted under pressure that were completed. Calculated by dividing **bPPass Comp** by **bPPass Att**. 

**Cross Comp** - Cross attempts that were completed.

**Cross Att** - Cross attempts.

**Cross Att per 90** - Crosses attempted per 90 minutes. Calculated by dividing **Cross Att** by **MP**, and multiplying the product by 90. Is meant to account for differences in playing time.

**Cross Att per opPass** - Percentage of all open play pass attempts that were crosses.

**Cross Comp Pct** - Percentage of all cross attempts that were completed.

**Corner Crosses** - Cross attempts that were from the attacking 3rd.

**Deep Crosses** - Cross attempts that were from beyond the attacking 3rd.

**Launch Comp** - Launched balls that were completed.

**Launch Att** - Launched balls that were attempted.

**Launch Att per 90** - Launch ball attempts per 90 minutes. Calculated by dividing **Launch Att** by **MP**, and multiplying the product by 90. Is meant to account for differences in playing time.

**Launch Att per Pass** - Percentage of all pass attempts that were launched balls.

**Launch Comp Pct** - Percentage of all launch ball attempts that were completed.

**Through Comp** - Through balls that were completed.

**Through Att** - Through balls that were attempted.

**Through Att per 90** - Through balls attempted per 90 minutes. Calculated by dividing **Through Att** by **MP**, and multiplying the product by 90. Is meant to account for differences in playing time.

**Through Att per Pass** - Percentage of all pass attempts that were through balls.

**Through Att per opPass** - Percentage of all open play pass attempts that were through balls.

**Through Comp Pct** - Percentage of all through ball attempts that were completed.

**Throw In Comp** - Throw ins that were completed.

**Throw In Att** - Throw ins attempted.

**Throw In Att per 90** - Throw ins attempted per 90 minutes. Calculated by dividing **Throw In Att** by **MP**, and multiplying the product by 90. Is meant to account for differences in playing time.

**Throw In Att per Pass** - Percentage of all pass attempts that were throw ins.

**Throw In Comp Pct** - Percentage of all throw in attempts that were completed.

**Corner Kicks Completed** - Corner kicks that connected with a teammate.

**Corner Kicks Taken** - Corner kicks that were taken.

**CK Effectiveness** - Percentage of all corner kicks that connected with a teammate.

**CK Assist** - Corner kicks that were assists.

**CK Key Pass** - Corner kicks that were key passes.

**Free Kicks Taken** - Free kicks taken.

**FK Pass Comp** - Free kicks that were pass attempts.

**FK Pass Att** - Free kicks that were completed passes.

**FK Pass Comp Pct** - Percentage of all free kick passes that were completed.

**FK Assist** - Free kicks that were assists.

**FK Key Pass** - Free kicks that were key passes.

**FK Shot** - Free kicks that were shots.

**FK Scored** - Free kicks that were directly scored.

**Take Ons** - Intentional attempts by a player to get past her defender.

##Possession Stats

**TO Won** - Take ons that were successful.

**TO Lost** - Take ons that were not successful.

**TO Win Pct** - Percentage of all take ons that were successful.

**Dispossessed** - All instances of a player getting intentionally dispossessed by a defender without having made a take on.

**Lost Touches** - All instances of a player losing possession due to a bad touch.

**All Possessions Disrupted** - All instances when a player loses a take-on, has a lost touch, or is dispossessed. Calculated by adding **TO Lost**, **Dispossessed**, and **Lost Touches**.

**Aerial Duels** - All instances of a player challenging an opposing player for a 50/50 ball in the air.

**AD Won** - All instances of a player being the first player to touch the ball in an aerial duel.

**AD Lost** - All instances of the opposing player being the first to touch the ball in an aerial duel.

**AD Win Pct** - Percentage of all aerial duels that were won.

##Defending Stats

**Tackles** - When a player challenges an opponent in possession of the ball, connects with the ball while making contact and engaging with the player, and successfully dispossesses the possessing player of the ball.

**Dispossesses** - When a player dispossesses the possessing player without the possessing player having had a chance to either take on the player or get rid of the ball.

**Dribbled** - When a player faces a successul take on from an opponent.

**Press Opp** - When a player pressure onto a possessing player's pass, shot, movement into another zone, ball shield, or recovery by stepping up, running at the player, or staying close in front of her, all with the intent of hurrying up the possessing player's play or impeding the possessing player's chance at making a play.

**Challenge Opp** - When a player applies pressure and makes contact with a possessing player as she attempts to make a play.

**Recoveries** - When a player wins possession of a loose ball, regardless of which team was the one to previously have possession of the ball.

**Def Recoveries** - When a player wins possession of a loose ball that was in possession of the opposing team.

**Poss Recoveries** - When a player wins possession of a loose ball that was in possession of her team.

**Interceptions** - Blocked passes or shots that prevented the intended recipient from getting the ball and clearly won possession of the ball. Recoveries of missed passes are not counted as interceptions.

**Int per 90** - Interceptions per 90 minutes. Calculated by dividing **Interceptions** by **MP**, and multiplying the product by 90. Is meant to account for differences in playing time.

**Int per OP Pass** - Percentage of all opponent pass attempts that were intercepted.

**Int in D3** - Interceptions in the defensive 3rd.

**Int in M3** - Interceptions in the middle 3rd.

**Int in A3** - Interceptions in the attacking 3rd.

**Blocks** - Blocked passes or shots that resulted in a loose ball or ball going out of bounds.

**Pass Blocks** - Blocked passes that resulted in a loose ball or ball going out of bounds.

**Shot Blocks** - Blocked shots that resulted in a loose ball or ball going out of bounds.

**Clearances** - Intentionally kicking the ball away without an intended recipient.

**Balls Shields** - Successfully shielding the ball from an opponent without touching the ball.

##Goalkeeper Stats
**GK Saves** - Shots on goal stopped by the goalkeeper.

**Goals Allowed** - Goals conceded by the goalkeeper.

**GK SOG Faced** - Shots on goal faced by the goalkeeper.

**GperSOG** - Percentage of all shots on goal that resulted in goals.

**BC Saves** - Big Chance shots on goal stopped by the goalkeeper.

**BC Goals Allowed** - Big Chance goals conceded by the goalkeeper.

**BC SOG Faced** - Big Chance shots on goal faced by the goalkeeper.

**GperBCSOG** - Percentage of all Big Chance shots on goal that resulted in goals.

**High Balls Faced** - When a goalkeeper goes up for a high ball, usually due to a cross, launched ball, corner kick, or free kick.

**HB Won** - When a goalkeeper goes up for a high ball and wins possession or clears it away.

**HB Lost** - When a goalkeeper goes up for a high ball and misses the ball or mishandles it.

**HB Caught** - When a goalkeeper wins a high ball by cleanly catching it.

**HB Punched** - When a goalkeeper wins a high ball by punching it away.

**HB Parried** - When a goalkeeper wins a high ball by parrying it away.

**HB Collected** - When a goalkeeper wins a high ball by knocking it to the ground and catching it on the bounce or collapsing onto it.

**HB Fouls Won** - When a goalkeeper wins a high ball by winning a foul.

**Crosses Faced** - When a goalkeeper goes up for a high ball that is also a cross.

**Crosses Won** - When a goalkeeper goes up for a high ball that is a cross and wins possession or clears it away.

**Crosses Lost** - When a goalkeeper goes up for a high ball that is a cross and misses the ball or mishandles it.

**Corner Kicks Faced** - When a goalkeeper goes up for a high ball that is also a corner kick.

**CKs Won** - When a goalkeeper goes up for a high ball that is a corner kick and wins possession or clears it away.

**CKs Lost** - When a goalkeeper goes up for a high ball that is a corner kick and misses the ball or mishandles it.

**Free Kicks Faced** - When a goalkeeper goes up for a high ball that is also a free kick.

**FKs Won** - When a goalkeeper goes up for a high ball that is a free kick and wins possession or clears it away.

**FKs Lost** - When a goalkeeper goes up for a high ball that is a free kick and misses the ball or mishandles it.

**Smothers Won** - When a goalkeeper faces a take on by an opposing player in the box and wins it by successfully coming out to either claim the ball or clear it to safety.

**Smothers Lost** - When a goalkeeper faces a take on by an opposing player in the box and loses it by missing the ball, causing a foul, or mishandling the ball.

**GK Overall Pass Comp** - All passes attempted by the goalkeeper

**GK Overall Pass Att** - All passes attempted by the goalkeeper

**GK Overall Pass Comp Pct** - Percentage of all passes attempted by the goalkeeper that were completed

**GK Throw Comp** - Thrown passes completed by the goalkeeper.

**GK Throw Att** - Thrown passes attempted by the goalkeeper.

**GK Throw Comp Pct** - Percentage of all thrown passes attempted by the goalkeeper that were completed.

**GK Drop Kick Comp** - Drop kick passes completed by the goalkeeper.

**GK Drop Kick Att** - Drop kick passes attempted by the goalkeeper.

**GK Drop Kick Comp Pct** - Percentage of all drop kick passes attempted by the goalkeeper that were completed.

**GKFK Comp** - Goal kick and free kick passes completed by the goalkeeper.

**GKFK Att** - Goal kick and free kick passes attempted by the goalkeeper.

**GKFK Comp Pct** - Percentage of all Goal kick and free kick passes attempted by the goalkeeper that were completed.
