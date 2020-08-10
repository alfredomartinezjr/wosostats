library(shiny)
library(tidyverse)
tbl_all <- read_csv("https://raw.githubusercontent.com/alfredomartinezjr/wosostats/master/stats/tbl_all.csv",
                    col_types = cols())
tbl_matches <- read_csv("https://raw.githubusercontent.com/alfredomartinezjr/wosostats/master/source/temp_database/matches.csv", 
                        col_types = cols())

ui <- fluidPage(
  titlePanel("WoSo Stats"),
  sidebarLayout(
    sidebarPanel(
      div(helpText("Select your competition and match, then click on 'Go'"), 
          style="font-size:12px"),
      div(radioButtons("competition",
                       label = "Select a competition",
                       choices = unique(tbl_matches$competition_string)), 
          style="font-size:12px"),
      div(uiOutput("matches"), 
          style="font-size:12px"),
      actionButton("go", "Go!"),
      br(),
      br(),
      br(),
      downloadButton("downloadData", "Download"),
      div(helpText("Download a csv file of all the stats for this match"), 
          style="font-size:12px"),
      div(helpText(p("Send feedback & questions to wosostats.team@gmail.com or to", 
                     a("@WoSoStats on Twitter.", href="https://twitter.com/wosostats")), 
                   style="font-size:12px")), width = 3),
    mainPanel(
      div(navbarPage(
        title = NULL,
        header = p("There is a cap that shinyapps.io puts on how much total time this app can be used per month. Please, to save some headaches, if you don't need to use this app, close it! :) For a glossary of each of these stats, go", a("here.", href="https://github.com/amj2012/wosostats/blob/master/resources/stats-glossary.md"), "To learn more about how you can log stats for this project, read more ", a("here.", href="https://wosostats.wordpress.com/how-to-help")),
        tabPanel('Shots', DT::dataTableOutput('shots')),
        tabPanel('Passing', DT::dataTableOutput('passing')),
        tabPanel('Possession', DT::dataTableOutput('possession')),
        tabPanel('Defending', DT::dataTableOutput('defending')),
        tabPanel('Goalkeeping', DT::dataTableOutput('goalkeeping')),
        navbarMenu("More",
                   tabPanel('Adv Passing', DT::dataTableOutput('adv_passing')),
                   tabPanel('Big Chances', DT::dataTableOutput('bigchances')),
                   tabPanel('Errors', DT::dataTableOutput('errors'))
        )
      )), style="font-size:12px")
  )
)

server <- function(input, output) {
  output$matches <- renderUI({
    matchups <- tbl_matches$matchup[tbl_matches$competition_string == input$competition]
    date <- tbl_matches$date[tbl_matches$competition_string == input$competition]
    matchids <- tbl_matches$match_id[tbl_matches$competition_string == input$competition]
    names(matchids) <- paste(matchups, date)
    selectInput("matchselection", "Select a match", matchids)
  })
  
  
  dataInput <- eventReactive( input$go, {
    tbl_all[tbl_all$match_id == input$matchselection,]
  })
  
  output$shots <- 
    DT::renderDataTable({
      DT::datatable(dataInput()[,c("player", "position", "team", "matchup", "date", 
                                   "goals", "shots", "shots_saved", "shots_missed", 
                                   "shots_blocked", "shots_pressed")],
                    options = list(pageLength = 10))
    })
  output$passing <- 
    DT::renderDataTable({
      DT::datatable(dataInput()[,c("player", "position", "team", "matchup", "date", 
                                   "pass_att", "pass_comp", "crosses", "crosses_comp",  
                                   "through_att", "through_comp", "throwin_att", 
                                   "throwin_comp", "ck_taken", "ck_comp", "fk_taken", 
                                   "fk_passatt", "fk_passcomp", "fk_shot", "fk_scored")], 
                    options = list(pageLength = 10))
    })
  output$adv_passing <- DT::renderDataTable({
    DT::datatable(dataInput()[,c("player", "position", "team", "matchup", "date", 
                                 "ppass_att", "ppass_comp", "op_pass_att", "op_pass_comp", 
                                 "op_ppass_att", "op_ppass_comp", "fw_pass_att", 
                                 "fw_pass_comp", "s_pass_att", "s_pass_comp", 
                                 "b_pass_att", "b_pass_comp", "fw_op_pass_att", 
                                 "fw_op_pass_comp", "s_op_pass_att", "s_op_pass_comp", 
                                 "b_op_pass_att", "b_op_pass_comp", "fw_ppass_att", 
                                 "fw_ppass_comp", "s_ppass_att", "s_ppass_comp", 
                                 "b_ppass_att", "b_ppass_comp")], 
                  options = list(pageLength = 10))
  })
    
  output$possession <- 
    DT::renderDataTable({
      DT::datatable(dataInput()[,c("player", "position", "team", "matchup", "date",
                                   "take_ons", "take_ons_won", "dispossessed", 
                                   "aerial_duels", "aerials_won", "recoveries")], 
                    options = list(pageLength = 10))
    })
  output$defending <- 
    DT::renderDataTable({
      DT::datatable(dataInput()[,c("player", "position", "team", "matchup", "date",
                                   "tackles", "dispossess_opp", "dribbled_byopp",
                                   "interceptions", "blocks", "pass_blocks", "shot_blocks",
                                   "clearances", "bc_stopped", "pressured_opp", 
                                   "challenged_opp", "ball_shields")], 
                    options = list(pageLength = 10))
    })
  output$goalkeeping <- 
    DT::renderDataTable({
      DT::datatable(dataInput()[,c("player", "position", "team", "matchup", "date",
                                   "gk_saves", "gk_goal_conceded", "gk_bigchances_conceded",
                                   "gk_bigchances_sog_faced", "gk_highballs",
                                   "gk_highballs_won")], 
                    options = list(pageLength = 10))
    })
  output$bigchances <- 
    DT::renderDataTable({
      DT::datatable(dataInput()[,c("player", "position", "team", "matchup", "date", 
                                   "assists", "key_passes", "key_assists",
                                   "second_assists", "big_chances", "bc_goals", 
                                   "bc_sog", "bc_smiss", "bc_dispossessed", 
                                   "bc_created", "bc_lost")], 
                    options = list(pageLength = 10))
    })
  output$errors <- 
    DT::renderDataTable({
      DT::datatable(dataInput()[,c("player", "position", "team", "matchup", "date",
                                   "err_togoals", "err_tobc", "own_goals")], 
                    options = list(pageLength = 10))
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("match_stats.csv")
    },
    content = function(con) {
      write_csv(dataInput(), con)
    }
  )
}

shinyApp(ui = ui, server = server)