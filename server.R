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

list_all_countries <- function() {
  url <- "https://www.thesportsdb.com/api/v1/json/3/all_countries.php"
  query_api(url)$countries
}

list_all_sports <- function() {
  url <- "https://www.thesportsdb.com/api/v1/json/3/all_sports.php"
  query_api(url)$sports
}

list_leagues_in_country <- function(country) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/search_all_leagues.php?c=", country)
  query_api(url)$countrys
}

search_team_by_name <- function(team_name, sport) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/searchteams.php?t=", team_name)
  teams <- query_api(url)$teams
  if (!is.null(teams)) {
    teams <- teams[teams$strSport == sport, ]
  }
  teams
}

list_players_in_team <- function(team_id) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/lookup_all_players.php?id=", team_id)
  query_api(url)$player
}

list_teams_in_league <- function(league_name) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/search_all_teams.php?l=", URLencode(league_name))
  query_api(url)$teams
}

server <- function(input, output, session) {
  # Load initial data for countries and sports
  observe({
    countries <- list_all_countries()
    updateSelectInput(session, "country", choices = setNames(countries$name_en, countries$name_en))
    
    sports <- list_all_sports()
    updateSelectInput(session, "sport", choices = setNames(sports$strSport, sports$strSport))
  })
  
  # Update leagues based on selected country and sport
  observeEvent(input$country, {
    req(input$country)
    leagues <- list_leagues_in_country(input$country)
    updateSelectInput(session, "league", choices = leagues$strLeague)
  })
  
  team_data <- reactiveVal(NULL)
  players_data <- reactiveVal(NULL)
  
  observeEvent(input$league, {
    req(input$league)
    teams <- list_teams_in_league(input$league)
    if (!is.null(teams)) {
      updateSelectInput(session, "team_id", choices = setNames(teams$idTeam, teams$strTeam))
    } else {
      updateSelectInput(session, "team_id", choices = NULL)
    }
  })
  
  observeEvent(input$search_team, {
    teams <- search_team_by_name(input$team, input$sport)
    team_data(teams)
    updateSelectInput(session, "team_id", choices = setNames(teams$idTeam, teams$strTeam))
  })
  
  observeEvent(input$get_players, {
    players <- list_players_in_team(input$team_id)
    players_data(players)
    
    numeric_vars <- c("strWage", "strHeight", "strWeight")
    categorical_vars <- c("strNationality", "strPosition", "strTeam", "strSport", "strEthnicity", "strGender", "strSide", "strStatus")
    
    updateSelectInput(session, "variable_x", choices = numeric_vars)
    updateSelectInput(session, "variable_y", choices = numeric_vars)
    updateSelectInput(session, "categorical_variable", choices = categorical_vars)
    updateSelectInput(session, "facet_variable", choices = c("", categorical_vars))
  })
  
  output$team_data <- renderTable({
    data <- team_data()
    if (is.null(data)) return(NULL)
    if (input$truncate_descriptions) {
      data$strDescriptionEN <- substr(data$strDescriptionEN, 1, 100)
    }
    head(data, 10)
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
    plot_type <- input$plot_type
    facet_var <- input$facet_variable
    p <- ggplot(players_data(), aes_string(x = input$variable_x, y = input$variable_y))
    
    if (plot_type == "Scatter Plot") {
      p <- p + geom_point()
    } else if (plot_type == "Histogram") {
      p <- p + geom_histogram(binwidth = 1)
    } else if (plot_type == "Bar Plot") {
      p <- p + geom_bar(stat = "identity", position = "dodge")
    }
    
    if (facet_var != "") {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)))
    }
    
    p + labs(title = paste(input$variable_y, "vs", input$variable_x))
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
    ggplot(players_data(), aes_string(x = input$variable_x, y = input$variable_y)) +
      geom_point() +
      labs(title = "Scatter Plot", x = input$variable_x, y = input$variable_y)
  })
  
  output$plot2 <- renderPlot({
    ggplot(players_data(), aes_string(x = input$variable_x)) +
      geom_histogram(binwidth = 1) +
      labs(title = "Histogram", x = input$variable_x)
  })
  
  output$plot3 <- renderPlot({
    ggplot(players_data(), aes_string(x = input$variable_x, fill = input$categorical_variable)) +
      geom_bar(position = "dodge") +
      labs(title = "Bar Plot", x = input$variable_x)
  })
}