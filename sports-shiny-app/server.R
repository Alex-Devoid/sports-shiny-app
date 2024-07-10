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

list_teams_in_league <- function(league_name) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/search_all_teams.php?l=", URLencode(league_name))
  query_api(url)$teams
}

get_event_details <- function(event_id) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/lookupevent.php?id=", event_id)
  query_api(url)$events
}

get_events_by_round <- function(league_id, round, season) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/eventsround.php?id=", league_id, "&r=", round, "&s=", season)
  query_api(url)$events
}

truncate_text <- function(text, max_length = 100) {
  if (nchar(text) > max_length) {
    paste0(substr(text, 1, max_length), "...")
  } else {
    text
  }
}

server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "league", choices = list_leagues_in_country("England")$strLeague)
    updateSelectInput(session, "explore_league", choices = list_leagues_in_country("England")$strLeague)
  })
  
  team_data <- reactiveVal(NULL)
  players_data <- reactiveVal(NULL)
  
  observeEvent(input$league, {
    req(input$league)
    teams <- list_teams_in_league(input$league)
    if (!is.null(teams)) {
      updateSelectInput(session, "team_id", choices = setNames(teams$idTeam, teams$strTeam))
      updateSelectInput(session, "explore_team_id", choices = setNames(teams$idTeam, teams$strTeam))
    } else {
      updateSelectInput(session, "team_id", choices = NULL)
      updateSelectInput(session, "explore_team_id", choices = NULL)
    }
  })
  
  observeEvent(input$search_team, {
    teams <- search_team_by_name(input$team)
    team_data(teams)
    updateSelectInput(session, "team_id", choices = setNames(teams$idTeam, teams$strTeam))
    updateSelectInput(session, "explore_team_id", choices = setNames(teams$idTeam, teams$strTeam))
  })
  
  observeEvent(input$get_players, {
    players <- list_players_in_team(input$team_id)
    players_data(players)
    updateSelectInput(session, "variable_x", choices = names(players))
    updateSelectInput(session, "variable_y", choices = names(players))
    updateSelectInput(session, "categorical_variable", choices = names(players))
  })
  
  output$team_data <- renderTable({
    data <- head(team_data(), 10)
    if (!is.null(data) && input$truncate_descriptions) {
      data$strDescriptionEN <- sapply(data$strDescriptionEN, truncate_text)
    }
    data
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
    ggplot(players_data(), aes(x = .data[[input$variable_x]], y = .data[[input$variable_y]])) +
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
  
  output$contingency_table <- renderTable({
    req(input$categorical_variable)
    table(players_data()[[input$categorical_variable]])
  })
  
  output$numerical_summary <- renderTable({
    req(input$variable_x, input$categorical_variable)
    players_data() %>%
      group_by_at(input$categorical_variable) %>%
      summarise(across(all_of(input$variable_x), list(mean = mean, sd = sd, min = min, max = max), .names = "summary_{col}_{fn}"))
  })
  
  output$plot1 <- renderPlot({
    ggplot(players_data(), aes(x = .data[[input$variable_x]], y = .data[[input$variable_y]])) +
      geom_point() +
      labs(title = "Scatter Plot", x = input$variable_x, y = input$variable_y)
  })
  
  output$plot2 <- renderPlot({
    ggplot(players_data(), aes(x = .data[[input$variable_x]])) +
      geom_histogram(binwidth = 1) +
      labs(title = "Histogram", x = input$variable_x)
  })
  
  output$plot3 <- renderPlot({
    ggplot(players_data(), aes(x = .data[[input$variable_x]], fill = .data[[input$categorical_variable]])) +
      geom_bar(position = "dodge") +
      labs(title = "Bar Plot", x = input$variable_x)
  })
}