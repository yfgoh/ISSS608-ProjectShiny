


tabPanel("Influence of Oceanus Folk",
         tabsetPanel(
           tabPanel("Trajectory over Time",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("year_range_2a_time", "Filter by Year:", min = 1980, max = 2040,
                                    value = c(1980, 2040), step = 1, sep = "", animate = TRUE)
                      ),
                      mainPanel(
                        plotOutput("genreTrendPlot", height = "400px"),
                        htmlOutput("insight_2a")
                      )
                    )
           ),
           tabPanel("Outward Influence on other Genres",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("year_range_2a_genres", "Filter by Year:", min = 1980, max = 2040,
                                    value = c(1980, 2040), step = 1, sep = "", animate = TRUE)
                      ),
                      mainPanel(
                        plotOutput("influencedGenresPlot", height = "400px"),
                        tableOutput("topInfluencedArtists"),
                        htmlOutput("insight_2b")
                      )
                    )
           ),
           tabPanel("Outward Influence on other Artists",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("year_range_2a_artists", "Filter by Year:", min = 1980, max = 2040,
                                    value = c(1980, 2040), step = 1, sep = "", animate = TRUE)
                      ),
                      mainPanel(
                        plotOutput("evolvingOceanusPlot_2c1", height = "400px"),
                        htmlOutput("insight_2c1")
                      )
                    )
           ),
           tabPanel("Inward Influence from other Genre",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("year_range_2a_inward", "Filter by Year:", min = 1980, max = 2040,
                                    value = c(1980, 2040), step = 1, sep = "", animate = TRUE)
                      ),
                      mainPanel(
                        plotOutput("evolvingOceanusPlot_2c2", height = "400px"),
                        htmlOutput("insight_2c2")
                      )
                    )
           ),
           tabPanel("Evolution with Rise of Sailor Shift",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("year_range_2a_sailor", "Filter by Year:", min = 1980, max = 2040,
                                    value = c(1980, 2040), step = 1, sep = "", animate = TRUE)
                      ),
                      mainPanel(
                        plotOutput("evolvingOceanusPlot_2c3", height = "400px"),
                        htmlOutput("insight_2c3")
                      )
                    )
           )
         )
),





# Trajectory over Time
output$genreTrendPlot <- renderPlot({
  year_range <- input$year_range_2a_time
  # Placeholder example plot
  plot(1980:2040, rnorm(61), type = "l", col = "steelblue",
       xlab = "Year", ylab = "Genre Mentions", main = "Trajectory of Oceanus Folk Over Time")
  abline(v = year_range, col = "red", lty = 2)
})

output$insight_2a <- renderUI({
  HTML("<p><strong>Insight:</strong> Oceanus Folk saw a significant rise post-2024, aligning with Sailor Shift's breakout.</p>")
})

# Outward Influence on other Genres
output$influencedGenresPlot <- renderPlot({
  year_range <- input$year_range_2a_genres
  # Placeholder plot
  barplot(c(5, 15, 25), names.arg = c("Indie Pop", "Synthwave", "Desert Rock"), col = "#FF8C00",
          main = paste("Genres Influenced by Oceanus Folk:", year_range[1], "-", year_range[2]))
})

output$topInfluencedArtists <- renderTable({
  data.frame(
    Artist = c("Juno Rivers", "Maya Blue", "Echo Zenith"),
    Genre = c("Indie Pop", "Desert Rock", "Synthwave"),
    Influence_Score = c(0.92, 0.85, 0.81)
  )
})

output$insight_2b <- renderUI({
  HTML("<p><strong>Insight:</strong> Oceanus Folk has influenced crossover into Synthwave and Desert Rock genres after 2028.</p>")
})

# Outward Influence on other Artists
output$evolvingOceanusPlot_2c1 <- renderPlot({
  year_range <- input$year_range_2a_artists
  # Placeholder plot
  barplot(height = c(3, 8, 15, 22, 28), names.arg = 2026:2030, col = "#4682B4",
          main = "No. of Artists Influenced per Year")
  abline(v = year_range - 2025, col = "darkred", lty = 2)
})

output$insight_2c1 <- renderUI({
  HTML("<p><strong>Insight:</strong> Artist-level adoption of Oceanus Folk elements peaked in 2029.</p>")
})

# Inward Influence from other Genres
output$evolvingOceanusPlot_2c2 <- renderPlot({
  year_range <- input$year_range_2a_inward
  # Placeholder plot
  plot(1980:2040, sample(0:5, 61, replace = TRUE), type = "l", col = "purple",
       xlab = "Year", ylab = "Cross-Genre Influence Events",
       main = "Genres that Influenced Oceanus Folk")
  abline(v = year_range, col = "darkgreen", lty = 2)
})

output$insight_2c2 <- renderUI({
  HTML("<p><strong>Insight:</strong> Oceanus Folk was heavily influenced by Indie Folk between 2010 and 2020.</p>")
})

# Evolution with Rise of Sailor Shift
output$evolvingOceanusPlot_2c3 <- renderPlot({
  year_range <- input$year_range_2a_sailor
  # Placeholder line plot
  years <- 2000:2040
  popularity <- dnorm(years, mean = 2028, sd = 5) * 100
  plot(years, popularity, type = "l", lwd = 2, col = "#2C3E50",
       xlab = "Year", ylab = "Oceanus Folk Popularity Index",
       main = "Oceanus Folk's Rise with Sailor Shift")
  abline(v = c(2024, 2028), col = "orange", lty = 2)
  abline(v = year_range, col = "red", lty = 3)
})

output$insight_2c3 <- renderUI({
  HTML("<p><strong>Insight:</strong> Sailor Shift’s debut in 2024 and her hit album in 2028 coincided with rapid Oceanus Folk growth.</p>")
})
