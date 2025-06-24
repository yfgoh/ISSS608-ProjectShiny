#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(collapsibleTree)
library(visNetwork)
library(shinycssloaders)
library(bslib)

website_theme <- bs_theme(
  bootswatch = "minty",
  primary = "#2C3E50",
  secondary = "#E67E22",
  success = "#1ABC9C",
  base_font = font_google("Quicksand"),
  navbar_dark = TRUE
)

ui <- navbarPage(
  title = div(
    HTML("
    <div style='display: flex; flex-direction: column; justify-content: flex-end; margin-top: 8px; line-height: 1.2; color: #0077B6; font-weight: bold; font-size: 16px;'>
      <span>Oceanus Folk:</span>
      <span>Then-and-Now</span>
    </div>
  ")
  ),
  theme = website_theme,
  id = "main_tabs",
  
  tags$head(
    tags$style(HTML("
      .navbar-nav > .active > a,
      .navbar-nav > .active > a:hover,
      .navbar-nav > .active > a:focus {
        color: #2C3E50 !important;
        background-color: transparent !important;
        border-bottom: 2px solid #2C3E50 !important;
        font-weight: 600;
      }

      .navbar-nav > li > a:hover {
        background-color: transparent !important;
        border-bottom: 2px solid #aaa !important;
        color: #2C3E50 !important;
      }

      .navbar-nav > li > a {
        padding-top: 15px !important;
        padding-bottom: 10px !important;
      }
    "))
  ),
  
  tabPanel("Profile of Sailor's Career",
           sidebarLayout(
             sidebarPanel(
               textInput("artist", "Enter Artist", value = ""),
               sliderInput("year_range", "Filter by Year:", min = 1980, max = 2040,
                           value = c(1980, 2040), step = 1, sep = "", animate = TRUE),
               checkboxGroupInput("filter_genres", "Filter by Genre:", 
                                  choices = c("Oceanus Folk", "Indie Pop", "Indie Folk"))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Overview", 
                          withSpinner(collapsibleTreeOutput("influenceTree", width = "100%", height = "400px")),
                          tags$hr(),
                          htmlOutput("sailorBio")
                 ),
                 tabPanel("1a: Influenced By Sailor",
                          plotOutput("influencedByPlot", height = "400px"),
                          htmlOutput("insight_1a")
                 ),
                 tabPanel("1b: Collaborations",
                          visNetworkOutput("collabInfluenceNetwork", height = "500px"),
                          htmlOutput("insight_1b")
                 ),
                 tabPanel("1c: Impact on Oceanus Folk",
                          plotOutput("broadInfluencePlot", height = "400px"),
                          htmlOutput("insight_1c")
                 )
               )
             )
           )
  ),
  
  tabPanel("Influence of Oceanus Folk",
           sidebarLayout(
             sidebarPanel(
               textInput("artist", "Enter Artist", value = ""),
               sliderInput("year_range", "Filter by Year:", min = 1980, max = 2040,
                           value = c(1980, 2040), step = 1, sep = "", animate = TRUE),
               checkboxGroupInput("filter_genres", "Filter by Genre:", 
                                  choices = c("Oceanus Folk", "Indie Pop", "Indie Folk"))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("2a: Rise of Oceanus Folk's Influence", 
                          plotOutput("genreTrendPlot", height = "400px"),
                          htmlOutput("insight_2a")
                 ),
                 tabPanel("2b: Influenced Genres & Top Artists", 
                          plotOutput("influencedGenresPlot", height = "400px"),
                          tableOutput("topInfluencedArtists"),
                          htmlOutput("insight_2b")
                 ),
                 tabPanel("2c: Evolution of Oceanus Folk", 
                          plotOutput("evolvingOceanusPlot", height = "400px"),
                          htmlOutput("insight_2c")
                 )
               )
             )
           )
  ),
  
  tabPanel("Oceanus Folk's Rising Star",
           sidebarLayout(
             sidebarPanel(
               selectInput("selected_artists", "Select Artists to Compare:",
                           choices = c("Sailor Shift", "Maya Blue", "Juno Rivers"),
                           selected = c("Sailor Shift", "Maya Blue", "Juno Rivers"),
                           multiple = TRUE),
               sliderInput("year_range", "Filter by Year:", min = 2000, max = 2040,
                           value = c(2020, 2040), step = 1, sep = "", animate = TRUE),
               checkboxGroupInput("filter_genres", "Filter by Genre:", 
                                  choices = c("Oceanus Folk", "Indie Pop", "Indie Folk"))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("3a: Career Trajectories of 3 Artists", 
                          plotOutput("careerComparePlot", height = "400px"),
                          htmlOutput("insight_3a")
                 ),
                 tabPanel("3b: Prediction of Future Rising Stars", 
                          tableOutput("predictedStars"),
                          htmlOutput("insight_3b"),
                 )
               )
             )
           )
  )
)

server <- function(input, output, session) {
  
  output$influenceTree <- renderCollapsibleTree({
    collapsibleTree(
      df = data.frame(
        Genre = "Oceanus Folk", 
        Mentor = "Ivy Echoes", 
        Artist = "Sailor Shift"
      ),
      hierarchy = c("Genre", "Mentor", "Artist"),
      root = "Oceanus Folk"
    )
  })
  
  
  output$collabInfluenceNetwork <- renderVisNetwork({
    nodes <- data.frame(id = 1:4, label = c("Sailor", "Ivy Echos", "Node1", "Node2"))
    edges <- data.frame(from = c(1, 1, 2), to = c(2, 3, 4))
    visNetwork(nodes, edges)
  })
  
  output$yourPlotName <- renderPlot({
    plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE)
    text(1, 1, "To be added", cex = 1.5, col = "gray50")
  })
  
  output$insight_1a <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  output$insight_1b <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>. </p>")
  })
  
  output$insight_1c <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  # 2a: Rise of Oceanus Folk Influence
  
  output$insight_2a <- renderUI({
    HTML("<p><strong>Insight:</strong> < To be added>.</p>")
  })
  
  # 2b: Genres & Artists Influenced
  
  
  
  output$insight_2b <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  # 2c: Evolution of Oceanus Folk
  
  
  output$insight_2c <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  # 3a: Career trajectories of selected artists
  output$careerComparePlot <- renderPlot({
    years <- 2020:2030
    plot(years, c(5, 10, 20, 30, 45, 60, 70, 80, 85, 90, 92), type = "l", col = "#0073B7",
         ylim = c(0, 100), xlab = "Year", ylab = "No. of Notable Songs", lwd = 2,
         main = "No. of Notable Songs Over Time")
    lines(years, c(2, 4, 10, 18, 25, 30, 40, 55, 60, 70, 75), col = "#E67E22", lwd = 2)
    lines(years, c(1, 2, 4, 8, 12, 20, 28, 35, 45, 55, 60), col = "#1ABC9C", lwd = 2)
    legend("bottomright", legend = c("Sailor Shift", "Maya Blue", "Juno Rivers"),
           col = c("#0073B7", "#E67E22", "#1ABC9C"), lty = 1, lwd = 2)
  })
  
  output$insight_3a <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  # 3b: Predicted rising stars
  
  
  output$insight_3b <- renderUI({
    HTML("<p><strong>Prediction:</strong> <To be Added>.</p>")
  })
  
}

options(shiny.launch.browser = TRUE)
shinyApp(ui, server)