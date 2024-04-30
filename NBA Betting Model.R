library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
#setwd("/Users/jeevanbrar/Desktop/WoWP")

# Make data frame and skip one line for headers
data <- read.csv("NBA Team Data - Sheet1.csv", skip = 2)
teams <- c("BOS", "OKC", "DEN", "MIN", "LAC", "NYK", "DAL", "MIL", "CLE", "PHI", "ORL", "IND", "MIA")

Team1Name <- select.list(teams, title="Choose Team1")
Team2Name <- select.list(teams, title="Choose Team2")

#Team1Name <- "BOS"
#Team2Name <- "OKC"

# Function to retrieve Variables for both teams
get_team_ortg <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  ortg <- team_data$ORtg
  return(ortg)
}

get_team2_ortg <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  ortg <- team_data$ORtg
  return(ortg)
}

get_team1_Pace <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  Pace <- team_data$Pace
  return(Pace)
}

get_team2_Pace <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  Pace <- team_data$Pace
  return(Pace)
}

get_team1_EFS <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  EFS <- team_data$eFG.
  return(EFS)
}

get_team2_EFS <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  EFS <- team_data$eFG.
  return(EFS)
}

get_team1_DE <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  DE <- team_data$DRtg
  return(DE)
}

get_team2_DE <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  DE <- team_data$DRtg
  return(DE)
}

get_team1_TOR <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  TOR <- team_data$TOV..1
  return(TOR)
}

get_team2_TOR <- function(data, team_name) {
  team_data <- dplyr::filter(data, Team == team_name)
  TOR <- team_data$TOV..1
  return(TOR)
}

# Variable names
team1_ortg <- get_team_ortg(data, Team1Name)
team2_ortg <- get_team2_ortg(data, Team2Name)
team1_Pace <- get_team1_Pace(data, Team1Name)
team2_Pace <- get_team2_Pace(data, Team2Name)

team1_OE <- (team1_ortg/100)
team2_OE <- (team2_ortg/100)

team1_drtg <- get_team1_DE(data, Team1Name)
team2_drtg <- get_team2_DE(data,Team2Name)

team1DE <- team1_drtg/100
team2DE <- team2_drtg/100

EFGmean <- mean(data$eFG..1)

team1EFS <- get_team1_EFS(data, Team1Name)
team1RSP <- team1EFS/EFGmean
team2EFS <- get_team2_EFS(data, Team2Name)
team2RSP <- team2EFS/EFGmean

team1PPG <- (team1_ortg/100)*team1_Pace
team2PPG <- (team2_ortg/100)*team2_Pace

team1_TOR <- get_team1_TOR(data, Team1Name)
team1_ATOR <- 1-(team1_TOR/100)
team2_TOR <- get_team2_TOR(data, Team2Name)
team2_ATOR <- 1-(team2_TOR/100)

team1PAPG <- team1DE*team2_Pace
team2PAPG <- team2DE*team1_Pace

#Score Estimation Calculations

team1ROE <- ((team1_OE+team2DE)/2)
team2ROE <- ((team2_OE+team1DE)/2)

team1Score <- (team1ROE*((team1PPG+team2PAPG)/2)*team1RSP*team1_ATOR)
team2Score <- (team2ROE*((team2PPG+team1PAPG)/2)*team2RSP*team2_ATOR)

# statistical tests on scores

# Set parameters for the normal distribution
mean_error <- 3
sd_error <- 5

# Simulate scores with added random components
set.seed(team1Score+team2Score)
num_simulations <- 500
team1Scores <- replicate(num_simulations, {
  team1Score <- (team1ROE*((team1PPG+team2PAPG)/2)*team1RSP*team1_ATOR) + rnorm(1, mean = mean_error, sd = sd_error)
})

team2Scores <- replicate(num_simulations, {
  team2Score <- (team2ROE*((team2PPG+team1PAPG)/2)*team2RSP*team2_ATOR) + rnorm(1, mean = mean_error, sd = sd_error)
})

# Perform a paired t-test
t_test_results <- t.test(team1Scores, team2Scores, paired = TRUE, alternative = "two.sided")


# Betting recommendation based on the results
average_team1_score <- mean(team1Scores)
average_team2_score <- mean(team2Scores)

cat("\nBetting Recommendation:\n")
if(t_test_results$p.value < 0.05) {
  if(average_team1_score > average_team2_score) {
    cat("Bet on Team 1 (", Team1Name, ") as they are predicted to win significantly.\n")
  } else {
    cat("Bet on Team 2 (", Team2Name, ") as they are predicted to win significantly.\n")
  }
} else {
  cat("No bet recommended as there is no significant difference in the predicted scores.\n")
}

ui <- fluidPage(
  titlePanel("Simple Shiny App"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter your name:", value = "World")
    ),
    mainPanel(
      textOutput("greeting")
    )
  )
)
# Define server logic
server <- function(input, output) {
  output$greeting <- renderText({
    paste("Hello,", input$name, "!")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
