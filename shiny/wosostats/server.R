library(RCurl)
meta <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
meta <- read.csv(textConnection(meta), stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  
  output$matches <- renderUI({
    league <- as.character(input$competition)
    matchups <- meta[meta[,"competition.slug"] == league,"matchup"]
    dates <- meta[meta[,"competition.slug"] == league,"date"]
    links <- meta[meta[,"competition.slug"] == league,"stats.csv.link"]
    names(links) <- paste(matchups, dates)
    selectInput("matchselection", "Select a match", links)
  })
  
  dataInput <- eventReactive(input$go, {
    d <- getURL(input$matchselection)
    d <- read.csv(textConnection(d), stringsAsFactors = FALSE)
    is.num <- sapply(d, is.numeric)
    d[is.num] <- lapply(d[is.num], round, 2)
    
    #Shots
    shots <- d[,c("Player","Team","MP","GS","Goals","Shots","Shot.Accuracy","SOG.GK.Stop","SOG.Def.Stop","Shots.Missed","Shots.Pressured","Pct.of.Shots.Pressured")]
    names(shots) <- gsub("\\."," ", names(shots))

    #Passing
    passing <- d[,c("Player", "Team", "MP", "Passes.Completed","Pass.Attempts", "Pass.Comp.Pct", "Pct.of.Passes.Under.Pressure",
           "Passes.Completed.Under.Pressure","Pass.Attempts.Under.Pressure","Pass.Comp.Pct.Under.Pressure")] 
    names(passing) <- gsub("\\."," ", names(passing))

    #Possession
    possession <- d[,c("Player", "Team", "MP", "Take.Ons.Won","Take.Ons.Lost", "Take.On.Attempts","Take.On.Win.Pct", "Dispossessed.by.Opp","Lost.Touches")] 
    names(possession) <- gsub("\\."," ", names(possession))
    
    #Player Defending
    playerdefending <- d[,c("Player","Team","MP","Tackles","Dribbled.by.Opp", "Pressured.Opp","Challenged.Opp","Dispossessed.Opp")]
    names(playerdefending) <- gsub("\\."," ", names(playerdefending))

    #Ball Defending
    balldefending <- d[,c("Player","Team","MP","Interceptions","Recoveries","Blocks","Clearances","Balls.Shielded")]
    names(balldefending) <- gsub("\\."," ", names(balldefending))

    
    #Goalkeeping
    goalkeeping <- d[,c("Player","Team","MP","Saves","Goals.Allowed","High.Balls.Won","High.Balls.Lost",
           "High.Balls.Caught","High.Balls.Punched.Away","High.Balls.Parried","High.Balls.Collected",
           "High.Ball.Fouls.Won","Crosses.High.Balls.Won","Corner.Kick.High.Balls.Won",
           "Free.Kick.High.Balls.Won","Smothers.Won","Smothers.Lost")]
    names(goalkeeping) <- gsub("\\."," ", names(goalkeeping))
    
    #Shot Location
    shotlocation <- d[,c("Player","Team","MP","A6.Shots","A18.Shots","A3L.Shots","A3C.Shots","A3R.Shots","Further.Shots")]
    names(shotlocation) <- gsub("\\."," ", names(shotlocation))
    
    #Big Chances
    bigchances <- d[,c("Player","Team","MP","Big.Chances","BC.Scored","BC.SOG","BC.Missed","BC.Dispossessed")]
    names(bigchances) <- gsub("\\."," ", names(bigchances))
    
    #Key Passes
    keypasses <- d[,c("Player","Team","MP","Assists","All.Key.Passes","Key.Passes.to.Goals","Key.Assists","Second.Assists","Unscored.Key.Passes")]
    names(keypasses) <- gsub("\\."," ", names(keypasses))
    
    #Crosses & Through Balls
    specialpassing <- d[,c("Player","Team","MP","Crosses.Completed","Cross.Attempts","Cross.Comp.Pct","Crosses.from.Corner","Crosses.from.Far",
           "Through.Balls.Completed","Through.Ball.Attempts","Through.Ball.Pct")]
    names(specialpassing) <- gsub("\\."," ", names(specialpassing))
    
    #Passes by Direction
    directionpassing <- d[,c("Player","Team","MP","Pct.of.Pass.Att.Fwd","Pct.of.Pass.Att.Side","Pct.of.Pass.Att.Back","Fwd.Pass.Comp",
           "Fwd.Pass.Att","Fwd.Pass.Comp.Pct","Side.Pass.Comp","Side.Pass.Att","Side.Pass.Comp.Pct",
           "Back.Pass.Comp","Back.Pass.Att","Back.Pass.Comp.Pct")]
    names(directionpassing) <- gsub("\\."," ", names(directionpassing))
    names(directionpassing) <- c("Player","Team","MP","Pct Fwd Passes","Pct Side Passes","Pct Back Passes","Fwd Passes Comp",
                                   "Fwd Passes Att","Fwd Passes Comp Pct","Side Passes Comp","Side Passes Att","Side Passes Comp Pct",
                                   "Back Passes Comp","Back Passes Att","Back Passes Comp Pct")
    
    #Pressured Passes by Direction
    directionpressuredpassing <- d[,c("Player","Team","MP","Pct.of.Pressed.Pass.Att.Fwd","Pct.of.Pressed.Pass.Att.Side","Pct.of.Pressed.Pass.Att.Back",
           "Fwd.Pressed.Pass.Comp","Fwd.Pressed.Pass.Att","Fwd.Pressed.Pass.Comp.Pct","Side.Pressed.Pass.Comp",
           "Side.Pressed.Pass.Att","Side.Pressed.Pass.Comp.Pct","Back.Pressed.Pass.Comp","Back.Pressed.Pass.Att",
           "Back.Pressed.Pass.Comp.Pct")]
    names(directionpressuredpassing) <- gsub("\\."," ", names(directionpressuredpassing))
    
    list(d, shots, passing, possession, playerdefending, balldefending, goalkeeping, shotlocation, bigchances, keypasses, specialpassing, directionpassing, directionpressuredpassing)
  })
  
  output$shots <- renderDataTable(dataInput()[[2]], options = list(pageLength = 10))
  output$passing <- renderDataTable(dataInput()[[3]], options = list(pageLength = 10))
  output$possession <- renderDataTable(dataInput()[[4]], options = list(pageLength = 10))
  output$playerdefending <- renderDataTable(dataInput()[[5]], options = list(pageLength = 10))
  output$balldefending <- renderDataTable(dataInput()[[6]], options = list(pageLength = 10))
  output$goalkeeping <- renderDataTable(dataInput()[[7]], options = list(pageLength = 10))
  output$shotlocation <- renderDataTable(dataInput()[[8]], options = list(pageLength = 10))
  output$bigchances <- renderDataTable(dataInput()[[9]], options = list(pageLength = 10))
  output$keypasses <- renderDataTable(dataInput()[[10]], options = list(pageLength = 10))
  output$specialpassing <- renderDataTable(dataInput()[[11]], options = list(pageLength = 10))
  output$directionpassing <- renderDataTable(dataInput()[[12]], options = list(pageLength = 10))
  output$directionpressuredpassing <- renderDataTable(dataInput()[[13]], options = list(pageLength = 10))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("match-stats.csv")
    },
    content = function(con) {
      write.csv(dataInput()[[1]], con, row.names = FALSE)
    }
  )
})