library(shiny)
library(RCurl)
meta <- getURL("https://raw.githubusercontent.com/amj2012/wosostats/master/database.csv")
meta <- read.csv(textConnection(meta), stringsAsFactors = FALSE)
comp.choices <- unique(meta$competition.slug)
names(comp.choices) <- unique(meta$competition.string)

shinyUI(fluidPage(
  titlePanel("WoSo Stats"),
  sidebarLayout(
    sidebarPanel(
      div(helpText("Select your competition and match, then click on 'Go'"), style="font-size:12px"),
      div(radioButtons("competition",
                       label = "Select a competition",
                       choices = comp.choices), style="font-size:12px"),
      div(uiOutput("matches"), style="font-size:12px"),
      actionButton("go", "Go!"),
      br(),
      br(),
      br(),
      downloadButton("downloadData", "Download"),
      div(helpText("Download a csv file of all the stats for this match"), style="font-size:12px"),
      div(helpText(p("Send feedback & questions to wosostats.team@gmail.com or to", a("@WoSoStats on Twitter.", href="https://twitter.com/wosostats")), style="font-size:12px")),
      width = 3),
    mainPanel(
      div(navbarPage(
        title = NULL,
        header = p("There is a cap that shinyapps.io puts on how much total time this app can be used per month. Please, to save some headaches, if you don't need to use this app, close it! :) For a glossary of each of these stats, go", a("here.", href="https://github.com/amj2012/wosostats/blob/master/resources/stats-glossary.md"), "To learn more about how you can log stats for this project, read more ", a("here.", href="https://wosostats.wordpress.com/how-to-help")),
        tabPanel('Shots', dataTableOutput('shots')),
        tabPanel('Passing', dataTableOutput('passing')),
        tabPanel('Possession', dataTableOutput('possession')),
        tabPanel('Player Defending', dataTableOutput('playerdefending')),
        tabPanel('Ball Defending', dataTableOutput('balldefending')),
        tabPanel('GK Saves', dataTableOutput('gksaves')),
        navbarMenu("More",
                   tabPanel('Shot Location', dataTableOutput('shotlocation')),
                   tabPanel('Big Chances', dataTableOutput('bigchances')),
                   tabPanel('Key Passes', dataTableOutput('keypasses')),
                   tabPanel('Passing Under Pressure', dataTableOutput('passingpressure')),
                   tabPanel('Passing by Direction', dataTableOutput('directionpassing')),
                   tabPanel('Open Play Passing by Direction', dataTableOutput('openplaydirectionpassing')),
                   tabPanel('Passing Under Pressure by Direction',dataTableOutput('directionpressuredpassing')),
                   tabPanel('Passing by Location',dataTableOutput('locationpassing')),
                   tabPanel('Open Play Passing by Location',dataTableOutput('locationopenplaypassing')),
                   tabPanel('Passing Range',dataTableOutput('passingrange')),
                   tabPanel('Crosses', dataTableOutput('crosses')),
                   tabPanel('Launch Balls', dataTableOutput('launchballs')),
                   tabPanel('Through Balls', dataTableOutput('throughballs')),
                   tabPanel('Throw Ins', dataTableOutput('throwins')),
                   tabPanel('Corner Kicks', dataTableOutput('cornerkicks')),
                   tabPanel('Free Kicks', dataTableOutput('freekicks')),
                   tabPanel('Aerial Duels', dataTableOutput('aerialduels')),
                   tabPanel('Disciplinary', dataTableOutput('disciplinary')),
                   tabPanel('Errors & Big Chance Stops', dataTableOutput('defnotes')),
                   tabPanel('GK High Balls', dataTableOutput('gkhighballs')),
                   tabPanel('GK Set Pieces', dataTableOutput('gksetpieces')),
                   tabPanel('GK Distribution', dataTableOutput('gkdistribution'))
                   
        ))), style="font-size:12px")
  )
))