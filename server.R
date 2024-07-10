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

list_all_countries <- function() {
  url <- "https://www.thesportsdb.com/api/v1/json/3/all_countries.php"
  query_api(url)$countries
}

list_leagues_in_country_sport <- function(country, sport) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/search_all_leagues.php?c=", URLencode(country), "&s=", URLencode(sport))
  print(url)
  query_api(url)$countries
}

search_team_by_name <- function(team_name, sport) {
  url <- paste0("https://www.thesportsdb.com/api/v1/json/3/searchteams.php?t=", URLencode(team_name))
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

# List of European countries
european_countries <- c("England","Spain")

server <- function(input, output, session) {
  # Load initial data for sports and countries
  sports <- list_all_sports()
  countries <- list_all_countries()
  
  # Filter countries to only include European countries
  european_countries_list <- countries %>%
    filter(name_en %in% european_countries)
  
  updateSelectInput(session, "sport", choices = sports$strSport)
  updateSelectInput(session, "country", choices = european_countries_list$name_en)
  
  team_data <- reactiveVal(NULL)
  players_data <- reactiveVal(NULL)
  
  observeEvent(input$country, {
    req(input$country, input$sport)
    print(input$country)
    print(input$sport)
    leagues <- list_leagues_in_country_sport(input$country, input$sport)
    updateSelectInput(session, "league", choices = leagues$strLeague)
    updateSelectInput(session, "explore_league", choices = leagues$strLeague)
  })
  
  observeEvent(input$search_team, {
    teams <- search_team_by_name(input$team, input$sport)
    team_data(teams)
    updateSelectInput(session, "explore_team_id", choices = setNames(teams$idTeam, teams$strTeam))
  })
  
  observeEvent(input$league, {
    req(input$league)
    teams1 <- list_teams_in_league(input$league)
    if (!is.null(teams1)) {
      updateSelectInput(session, "team_id", choices = setNames(teams1$idTeam, teams1$strTeam))
    } else {
      updateSelectInput(session, "team_id", choices = NULL)
    }
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
}