library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Functions to query the API
query_api <- function(url) {
  response <- GET(url)
  fromJSON(content(response, as = "text"), flatten = TRUE)
}

list_all_sports <- function() {
  url <- "https://www.thesportsdb.com/api/v1/json/3/all_sports.php"
  query_api(url)$sports
}

list_leagues_in_country <- function(country) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/search_all_leagues.php?c=", country)
  query_api(url)$countries
}

search_team_by_name <- function(team_name) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/searchteams.php?t=", team_name)
  query_api(url)$teams
}

list_players_in_team <- function(team_id) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/lookup_all_players.php?id=", team_id)
  query_api(url)$player
}

get_event_details <- function(event_id) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/lookupevent.php?id=", event_id)
  query_api(url)$events
}

get_events_by_round <- function(league_id, round, season) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/eventsround.php?id=", league_id, "&r=", round, "&s=", season)
  query_api(url)$events
}

server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "league", choices = list_leagues_in_country("England")$strLeague)
    updateSelectInput(session, "explore_league", choices = list_leagues_in_country("England")$strLeague)
  })
  
  team_data <- reactiveVal(NULL)
  players_data <- reactiveVal(NULL)
  
  observeEvent(input$search_team, {
    team_data(search_team_by_name(input$team))
    updateSelectInput(session, "team_id", choices = team_data()$strTeam)
    updateSelectInput(session, "explore_team_id", choices = team_data()$strTeam)
  })
  
  observeEvent(input$get_players, {
    players_data(list_players_in_team(input$team_id))
    updateSelectInput(session, "variable_x", choices = names(players_data()))
    updateSelectInput(session, "variable_y", choices = names(players_data()))
  })
  
  output$team_data <- renderTable({
    head(team_data(), 10)
  })
  
  output$players_data <- renderTable({
    head(players_data(), 10)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("players_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(players_data(), file)
    }
  )
  
  plot_data <- eventReactive(input$plot_data, {
    req(input$variable_x, input$variable_y)
    ggplot(players_data(), aes_string(x = input$variable_x, y = input$variable_y)) +
      geom_point() +
      labs(title = paste(input$variable_y, "vs", input$variable_x))
  })
  
  output$plot <- renderPlot({
    plot_data()
  })
  
  output$summary <- renderTable({
    req(input$variable_x, input$variable_y)
    summary(players_data()[, c(input$variable_x, input$variable_y)])
  })
}