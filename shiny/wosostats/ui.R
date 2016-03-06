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
      div(helpText(p("Send feedback & questions to alfredom790@gmail.com or to", a("@WoSoStats on Twitter.", href="https://twitter.com/wosostats")), style="font-size:12px")),
      width = 3),
    mainPanel(
      div(navbarPage(
        title = NULL,
        header = p("There is a cap that shinyapps.io puts on how much total time this app can be used per month. Please, to save some headaches, if you don't need to use this app, close it! :) For a list of definitions, go", a("here", href="https://github.com/amj2012/wosostats/blob/master/resources/definitions.md")),
        tabPanel('Shots', dataTableOutput('shots')),
        tabPanel('Passing', dataTableOutput('passing')),
        tabPanel('Possession', dataTableOutput('possession')),
        tabPanel('Player Defending', dataTableOutput('playerdefending')),
        tabPanel('Ball Defending', dataTableOutput('balldefending')),
        tabPanel('Goalkeeping', dataTableOutput('goalkeeping')),
        navbarMenu("More",
                   tabPanel('Shot Location', dataTableOutput('shotlocation')),
                   tabPanel('Big Chances', dataTableOutput('bigchances')),
                   tabPanel('Key Passes', dataTableOutput('keypasses')),
                   tabPanel('Crosses & Through Balls', dataTableOutput('specialpassing')),
                   tabPanel('Passing by Direction', dataTableOutput('directionpassing')),
                   tabPanel('Passing Under Pressure by Direction', dataTableOutput('directionpressuredpassing'))
        ))), style="font-size:12px")
  )
))