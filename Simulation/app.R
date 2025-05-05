```{r}


# Prepare simulation data once
set.seed(42)

schedule <- data.frame(
  team = c("CMS", "La Verne", "ETBU", "Occidental", "Cal Lu", "Cal Tech", "Whittier",
           "Lewis and Clark", "Pacific Lutheran", "Chapman", "Redlands", 
           "MIT", "UW La Crosse", "Tufts"),
  win_prob = c(0.56, 0.55, 0.53, 0.85, 0.60, 0.95, 0.84,
               0.71, 0.73, 0.60, 0.74, 
               0.80, 0.54, 0.65),
  games = c(3, 4, 3, 3, 3, 3, 4,
            3, 2, 3, 3,
            2, 1, 3)
)

simulate_season <- function(schedule) {
  results <- schedule |>
    rowwise() |>
    mutate(
      wins = sum(runif(games) < win_prob),
      losses = games - wins
    ) |>
    ungroup()
  
  total_wins <- sum(results$wins)
  tibble(total_wins = total_wins)
}

n_simulations <- 1000
sim_results <- map_dfr(1:n_simulations, ~simulate_season(schedule))

# UI
ui <- fluidPage(
  titlePanel("Simulated Win Distribution"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("win_thresh", "Minimum Wins:",
                  min = min(sim_results$total_wins),
                  max = max(sim_results$total_wins),
                  value = 27,
                  step = 1)
    ),
    mainPanel(
      textOutput("percent_text"),
      plotOutput("win_plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$percent_text <- renderText({
    pct <- mean(sim_results$total_wins <= input$win_thresh) * 100
    paste0("Percentage of seasons with ", input$win_thresh, " wins or worse: ", round(pct, 2), "%")
  })
  
  output$win_plot <- renderPlot({
    ggplot(sim_results, aes(x = total_wins)) +
      geom_bar(fill = "steelblue", color = "black") +
      geom_vline(xintercept = input$win_thresh, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = "Distribution of Total Wins Over 1000 Simulated Seasons",
        x = "Total Wins",
        y = "Frequency"
      ) +
      theme_minimal()
  })
}

# Run app
shinyApp(ui = ui, server = server)

```