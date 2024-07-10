library(shiny)

ui <- tagList(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  navbarPage("Sports Data App",
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
                      )
             ),
             tabPanel("Data Download",
                      fluidPage(
                        titlePanel("Data Download"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("country", "Select Country", choices = NULL),
                            selectInput("sport", "Select Sport", choices = NULL),
                            selectInput("league", "Select League", choices = NULL),
                            textInput("team", "Search Team by Name"),
                            actionButton("search_team", "Search"),
                            selectInput("team_id", "Select Team", choices = NULL),
                            actionButton("get_players", "Get Players"),
                            checkboxInput("truncate_descriptions", "Truncate Descriptions", value = TRUE)
                          ),
                          mainPanel(
                            downloadButton("download_data", "Download Data"),
                            tableOutput("team_data"),
                            tableOutput("players_data")
                          )
                        )
                      )
             ),
             tabPanel("Data Exploration",
                      fluidPage(
                        titlePanel("Data Exploration"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("variable_x", "X-axis Variable", choices = NULL, label = "Select the variable for the X-axis:"),
                            selectInput("variable_y", "Y-axis Variable", choices = NULL, label = "Select the variable for the Y-axis:"),
                            selectInput("categorical_variable", "Categorical Variable", choices = NULL, label = "Select a categorical variable for grouping or coloring:"),
                            selectInput("plot_type", "Plot Type", choices = c("Scatter Plot", "Histogram", "Bar Plot"), label = "Select the type of plot:"),
                            selectInput("facet_variable", "Facet by Variable", choices = NULL, label = "Select a variable to facet the plot by (optional):"),
                            actionButton("plot_data", "Plot Data"),
                            p('Please run "Get Players" first before plotting.')
                          ),
                          mainPanel(
                            plotOutput("plot"),
                            tableOutput("summary"),
                            tableOutput("contingency_table"),
                            tableOutput("numerical_summary"),
                            plotOutput("plot1"),
                            plotOutput("plot2"),
                            plotOutput("plot3")
                          )
                        )
                      )
             )
  )
)