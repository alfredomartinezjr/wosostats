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
    shots <- d[,c(1:4,5:13)]
    names(shots) <- gsub("\\."," ", names(shots))
    
    #Passing
    passing <- d[,c(1:4,37:43)]
    names(passing) <- gsub("\\."," ", names(passing))

    #Possession
    possession <- d[,c(1:4,44:52)]
    names(possession) <- gsub("\\."," ", names(possession))
    
    #Player Defending
    playerdefending <- d[,c(1:4,53:57)]
    names(playerdefending) <- gsub("\\."," ", names(playerdefending))
    
    #Ball Defending
    balldefending <- d[,c(1:4,58:62)]
    names(balldefending) <- gsub("\\."," ", names(balldefending))
    
    #Goalkeeping
    goalkeeping <- d[,c(1:4,63:76)]
    names(goalkeeping) <- gsub("\\."," ", names(goalkeeping))
    
    #Shot Location
    shotlocation <- d[,c(1:5,14:19)]
    names(shotlocation) <- gsub("\\."," ", names(shotlocation))
    
    #Big Chances
    bigchances <- d[,c(1:4,24:28)]
    names(bigchances) <- gsub("\\."," ", names(bigchances))
    
    #Key Passes
    keypasses <- d[,c(1:4,20:23)]
    names(keypasses) <- gsub("\\."," ", names(keypasses))
    
    #Crosses & Through Balls
    specialpassing <- d[,c(1:4,29:36)]
    names(specialpassing) <- gsub("\\."," ", names(specialpassing))
    
    list(d, shots, passing, possession, playerdefending, balldefending, goalkeeping, shotlocation, bigchances, keypasses, specialpassing)
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("match-stats.csv")
    },
    content = function(con) {
      write.csv(dataInput()[[1]], con, row.names = FALSE)
    }
  )
})