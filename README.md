# Sports Data App

## Description
The Sports Data App is a Shiny application that allows users to query and summarize sports data from the SportsDB API. Users can explore data on various sports, leagues, teams, players, and events, and generate summaries and visualizations. The app is organized into three main tabs: About, Data Download, and Data Exploration.

## Packages Needed
To run this app, you need to install the following R packages:
- shiny
- httr
- jsonlite
- dplyr
- ggplot2

## Install Packages
You can install all the required packages using the following line of code:
```r
install.packages(c("shiny", "httr", "jsonlite", "dplyr", "ggplot2"))
```

To run from github enter: 
```
shiny::runGitHub("sports-shiny-app", "Alex-Devoid")
```