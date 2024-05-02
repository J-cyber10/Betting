library(shiny)
library(readr)
library(dplyr)
#setwd("/Users/jeevanbrar/Desktop/WoWP")
# Read the data
data <- read.csv("NBA Team Data - Sheet1.csv", skip = 2)

# List of teams
teams <- c("BOS", "OKC", "DEN", "MIN", "LAC", "NYK", "DAL", "MIL", "CLE", "PHI", "ORL", "IND", "MIA")

# Define UI
ui <- fluidPage(
  titlePanel("NBA Team Betting Recommendation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team1", "Select Team 1:", choices = teams),
      selectInput("team2", "Select Team 2:", choices = teams),
      numericInput("mean_error", "Mean Error:", value = 5),
      numericInput("sd_error", "Standard Error:", value = 3),
     # numericInput("num_simulations", "Number of Simulations:", value = ),
      actionButton("submit", "Get Recommendation"),
      actionButton("stop", "Stop App")
    ),
    mainPanel(
      h4("Betting Recommendation:"),
      textOutput("recommendation"),
      tableOutput("comparison_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$submit, {
    # Get selected teams
    team1 <- input$team1
    team2 <- input$team2
    
    mean_error <- input$mean_error
    sd_error <- input$sd_error
    # Function to retrieve Variables for both teams
    
    get_team_standard_deviation <- function(data, team_name) {
      team_data <- dplyr::filter(data, Team == team)
      
      # Calculate the standard deviation of scores
      sd_scores <- sd(team_data$team_scores)
      
      return(sd_scores)
    }
    
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
    team1_ortg <- get_team_ortg(data, team1)
    team2_ortg <- get_team2_ortg(data, team2)
    team1_Pace <- get_team1_Pace(data, team1)
    team2_Pace <- get_team2_Pace(data, team2)
    
    team1_OE <- (team1_ortg/100)
    team2_OE <- (team2_ortg/100)
    
    team1_drtg <- get_team1_DE(data, team1)
    team2_drtg <- get_team2_DE(data,team2)
    
    team1DE <- team1_drtg/100
    team2DE <- team2_drtg/100
    
    EFGmean <- mean(data$eFG..1)
    
    team1EFS <- get_team1_EFS(data, team1)
    team1RSP <- team1EFS/EFGmean
    team2EFS <- get_team2_EFS(data, team2)
    team2RSP <- team2EFS/EFGmean
    
    team1PPG <- (team1_ortg/100)*team1_Pace
    team2PPG <- (team2_ortg/100)*team2_Pace
    
    team1_TOR <- get_team1_TOR(data, team1)
    team1_ATOR <- 1-(team1_TOR/100)
    team2_TOR <- get_team2_TOR(data, team2)
    team2_ATOR <- 1-(team2_TOR/100)
    
    team1PAPG <- team1DE*team2_Pace
    team2PAPG <- team2DE*team1_Pace
    
    #Score Estimation Calculations
    
    team1ROE <- ((team1_OE+team2DE)/2)
    team2ROE <- ((team2_OE+team1DE)/2)
    
    team1Score <- (team1ROE*((team1PPG+team2PAPG)/2)*team1RSP*team1_ATOR)
    team2Score <- (team2ROE*((team2PPG+team1PAPG)/2)*team2RSP*team2_ATOR)
    
    # statistical tests on scores
    
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
    
    # Subset the first 10, 100, and 500 simulations for Team 1 and Team 2
    team1_scores_10 <- team1Scores[1:10]
    team1_scores_100 <- team1Scores[1:100]
    team1_scores_500 <- team1Scores[1:500]
    
    team2_scores_10 <- team2Scores[1:10]
    team2_scores_100 <- team2Scores[1:100]
    team2_scores_500 <- team2Scores[1:500]
    
    # Calculate statistics for each subset
    team1_stats_10 <- c(mean(team1_scores_10), sd(team1_scores_10), quantile(team1_scores_10, c(0.025, 0.975)))
    team1_stats_100 <- c(mean(team1_scores_100), sd(team1_scores_100), quantile(team1_scores_100, c(0.025, 0.975)))
    team1_stats_500 <- c(mean(team1_scores_500), sd(team1_scores_500), quantile(team1_scores_500, c(0.025, 0.975)))
    
    team2_stats_10 <- c(mean(team2_scores_10), sd(team2_scores_10), quantile(team2_scores_10, c(0.025, 0.975)))
    team2_stats_100 <- c(mean(team2_scores_100), sd(team2_scores_100), quantile(team2_scores_100, c(0.025, 0.975)))
    team2_stats_500 <- c(mean(team2_scores_500), sd(team2_scores_500), quantile(team2_scores_500, c(0.025, 0.975)))
    
    # Create comparison data frame
    
    
    # Display the comparison data frame
  
    
    # Display betting recommendation
    output$recommendation <- renderText({
      "\nBetting Recommendation:\n"
      if(t_test_results$p.value < 0.01 & abs(average_team1_score-average_team2_score) > 2.5) {
        if(average_team1_score > average_team2_score) {
          recommendation <- paste("Bet on Team 1 (", team1, ") as they are predicted to win significantly, by (",average_team1_score-average_team2_score,") points.\n")
        } else {
          recommendation <- paste("Bet on Team 2 (", team2, ") as they are predicted to win significantly, by (",average_team2_score-average_team1_score,") points.\n")
        }
      } else {
        recommendation <- paste("No bet recommended as there is no significant difference in the predicted scores, difference of (",abs(average_team2_score-average_team1_score),") points.\n")
      }

      statement <- paste(recommendation, "With a Standard Error of (", sd_error, ") and a Mean Error of (", mean_error, ").\n At p-value (",t_test_results$p.value,"). ")
    })
    output$comparison_table <- renderTable({
      # Extract predicted points and standard deviation of simulations for each team
      team1_predicted_points <- (average_team1_score * .92)-2
      team2_predicted_points <- (average_team2_score * .92)+2
      team1_sd_simulations <- sd(team1Scores)
      team2_sd_simulations <- sd(team2Scores)
      
      # Create a data frame for the comparison table
      comparison_data <- data.frame(
        Subset = c("10 Simulations", "100 Simulations", "500 Simulations"),
        Team1_Mean = c(team1_stats_10[1], team1_stats_100[1], team1_stats_500[1]),
        Team1_SD = c(team1_stats_10[2], team1_stats_100[2], team1_stats_500[2]),
        Team1_CI_Lower_Bound = c(team1_stats_10[3], team1_stats_100[3], team1_stats_500[3]),
        Team1_CI_Upper_Bound = c(team1_stats_10[4], team1_stats_100[4], team1_stats_500[4]),
        Team2_Mean = c(team2_stats_10[1], team2_stats_100[1], team2_stats_500[1]),
        Team2_SD = c(team2_stats_10[2], team2_stats_100[2], team2_stats_500[2]),
        Team2_CI_Lower_Bound = c(team2_stats_10[3], team2_stats_100[3], team2_stats_500[3]),
        Team2_CI_Upper_Bound = c(team2_stats_10[4], team2_stats_100[4], team2_stats_500[4])
      )
      
      return(comparison_data)
    })
  })

  observeEvent(input$stop, {
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
