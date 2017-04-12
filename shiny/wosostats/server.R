library(RCurl)
library(dplyr)
database <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
database <- read.csv(textConnection(database), stringsAsFactors = FALSE)
stats <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/resources/shiny-stats.csv")
stats <- read.csv(textConnection(stats), stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  
  output$matches <- renderUI({
    league <- as.character(input$competition)
    matchups <- database[database[,"competition.slug"] == league,"matchup"]
    dates <- database[database[,"competition.slug"] == league,"date"]
    links <- database[database[,"competition.slug"] == league,"stats.csv.link"]
    names(links) <- paste(matchups, dates)
    selectInput("matchselection", "Select a match", links)
  })
  
  dataInput <- eventReactive(input$go, {
    d <- getURL(input$matchselection)
    d <- read.csv(textConnection(d), stringsAsFactors = FALSE)
    is.num <- sapply(d, is.numeric)
    d[is.num] <- lapply(d[is.num], round, 2)
    players <- length(d$Player)
    names(d) <- gsub("\\."," ", names(d))
    
    statstable <- as.data.frame(matrix(rep(0, length(stats$Acronym)), nrow = 1))
    names(statstable) <- c(stats$Acronym)
    statstable <- statstable[-1,]
    statstable$Player <- as.character(statstable$Player)
    statstable$Team <- as.character(statstable$Team)
    statstable <- bind_rows(statstable, d)
    statstable
  })
  
  output$shots <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Shots","Acronym"])], options = list(pageLength = 10))
  output$keypasses <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Key Passes","Acronym"])], options = list(pageLength = 10))
  output$bigchances <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Big Chances","Acronym"])], options = list(pageLength = 10))
  
  output$crosses <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Crosses","Acronym"])], options = list(pageLength = 10))
  output$launchballs <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Launch Balls","Acronym"])], options = list(pageLength = 10))
  output$throughballs <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Through Balls","Acronym"])], options = list(pageLength = 10))
  output$throwins <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Throw Ins","Acronym"])], options = list(pageLength = 10))
  output$cornerkicks <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Corner Kicks","Acronym"])], options = list(pageLength = 10))
  output$freekicks <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Free Kicks","Acronym"])], options = list(pageLength = 10))
  
  output$passing <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Overall Passing","Acronym"], stats[stats[,"Panel"]=="Adjusted Passing","Acronym"])], options = list(pageLength = 10))
  output$directionpassing <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Passing By Direction","Acronym"])], options = list(pageLength = 10))
  output$openplaydirectionpassing <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Open Play Passing By Direction","Acronym"])], options = list(pageLength = 10))
  output$passingpressure <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Passing Under Pressure","Acronym"], stats[stats[,"Panel"]=="Adjusted Passing Under Pressure","Acronym"])], options = list(pageLength = 10))
  output$directionpressuredpassing <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Passing Under Pressure by Direction","Acronym"])], options = list(pageLength = 10))
  
  output$possession <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Possession","Acronym"])], options = list(pageLength = 10))
  output$aerialduels <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Aerial Duels","Acronym"])], options = list(pageLength = 10))
  output$disciplinary <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Disciplinary","Acronym"])], options = list(pageLength = 10))
  
  output$playerdefending <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Player Defending","Acronym"])], options = list(pageLength = 10))
  output$balldefending <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Ball Defending","Acronym"])], options = list(pageLength = 10))
  output$defnotes <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="Errors & Big Chance Stops","Acronym"])], options = list(pageLength = 10))
  
  output$gksaves <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="GK Saves","Acronym"])], options = list(pageLength = 10))
  output$gkhighballs <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="GK High Balls","Acronym"])], options = list(pageLength = 10))
  output$gksetpieces <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="GK Set Pieces","Acronym"])], options = list(pageLength = 10))
  output$gkdistribution <- renderDataTable(dataInput()[,c("Player","Team","GP","GS","MP",stats[stats[,"Panel"]=="GK Distribution","Acronym"])], options = list(pageLength = 10))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("match-stats.csv")
    },
    content = function(con) {
      write.csv(dataInput(), con, row.names = FALSE)
    }
  )
})