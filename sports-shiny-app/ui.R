library(shiny)

ui <- navbarPage("Sports Data App",
                 tabPanel("About",
                          fluidPage(
                            titlePanel("About This App"),
                            sidebarLayout(
                              sidebarPanel(
                                img(src = "https://www.thesportsdb.com/images/sports/soccer.jpg", height = "200px")
                              ),
                              mainPanel(
                                h2("Purpose"),
                                p("This app allows users to query and summarize sports data from the SportsDB API. Users can explore data on various sports, leagues, teams, players, and events, and generate summaries and visualizations."),
                                h2("Data Source"),
                                p("The data is sourced from the ", a("SportsDB API", href = "https://www.thesportsdb.com/"), "."),
                                h2("App Tabs"),
                                p("The app consists of the following tabs:"),
                                tags$ul(
                                  tags$li("About: Information about the app."),
                                  tags$li("Data Download: Allows users to query and download data."),
                                  tags$li("Data Exploration: Enables users to explore and visualize the data.")
                                )
                              )
                            )
                          )),
                 tabPanel("Data Download",
                          fluidPage(
                            titlePanel("Data Download"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("sport", "Select Sport", choices = c("Soccer")),
                                selectInput("league", "Select League", choices = NULL),
                                textInput("team", "Search Team by Name"),
                                actionButton("search_team", "Search"),
                                selectInput("team_id", "Select Team", choices = NULL),
                                actionButton("get_players", "Get Players")
                              ),
                              mainPanel(
                                tableOutput("team_data"),
                                tableOutput("players_data"),
                                downloadButton("download_data", "Download Data")
                              )
                            )
                          )),
                 tabPanel("Data Exploration",
                          fluidPage(
                            titlePanel("Data Exploration"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("explore_sport", "Select Sport", choices = c("Soccer")),
                                selectInput("explore_league", "Select League", choices = NULL),
                                textInput("explore_team", "Search Team by Name"),
                                actionButton("explore_search_team", "Search"),
                                selectInput("explore_team_id", "Select Team", choices = NULL),
                                actionButton("explore_get_players", "Get Players"),
                                selectInput("variable_x", "X-axis Variable", choices = NULL),
                                selectInput("variable_y", "Y-axis Variable", choices = NULL),
                                actionButton("plot_data", "Plot Data")
                              ),
                              mainPanel(
                                plotOutput("plot"),
                                tableOutput("summary")
                              )
                            )
                          ))
)