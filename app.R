# Load required libraries
library(shiny)
library(shinydashboard)
library(collapsibleTree)
library(visNetwork)
library(shinycssloaders)
library(bslib)
library(jsonlite)
library(dplyr)
library(stringr)
library(tibble)
library(tidygraph)
library(ggplot2)
library(forcats)
library(knitr)
library(kableExtra)
library(ggiraph)
library(ggraph)
library(tidyr)
library(igraph)
library(shinyjs)
library(plotly)
library(networkD3)
library(htmlwidgets)
library(fmsb)
library(broom)
library(purrr)

source("data_prep.R")  # Load your reactive function
source("Question1_Server.R") # Load Q1 server
source("Question2_Server.R") # Load Q2 server
source("Question3_Server.R") # Load Q3 server
source("Question3_a_Server.R") # Load Q3a server
source("Question3_b_Server.R") # Load Q3a server
source("Question3_b_table_Server.R") # Load Q3a server

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
    HTML("<div style='display: flex; flex-direction: column; justify-content: flex-end; margin-top: 8px; line-height: 1.2; color: #0077B6; font-weight: bold; font-size: 16px;'>
      <span>Oceanus Folk:</span>
      <span>Then-and-Now</span>
    </div>")
  ),
  windowTitle = "Oceanus Folk: Then-and-Now",  
  theme = website_theme,
  id = "main_tabs",
  
  tags$head(
    tags$style(HTML(".navbar-nav > .active > a,
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
      }"))
  ),
  ############################### Question 1 #######################################
  tabPanel("Profile of Sailor's Career",
           tabsetPanel(
             tabPanel("Sailor's Work", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("node_type_filter", "Filter Node Types:",
                                      choices = c("Song", "Album", "Person", "MusicalGroup"),
                                      selected = c("Song", "Album"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By","Member Of"),
                                      selected = c("Creator Of", "Influenced By"), multiple = TRUE)
                        ),
                        mainPanel(
                          br(),
                          h5("Hover your mouse over the nodes below to explore Sailor Shift's work"),
                          withSpinner(girafeOutput("sailorWorkPlot", width = "100%", height = "600px")),
                          tags$hr(),
                          htmlOutput("sailorBio"),
                          tags$hr(),
                          h5("See the table below for more details"),
                          DT::dataTableOutput("filteredNodeTable")
                        )
                      )
             ),
             ######################## Question 1a ##############################
             tabPanel("Primary Influences",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("node_type_filter", "Filter Node Types:",
                                      choices = c("Song", "Album", "MusicalGroup", "Person"),
                                      selected = c("Song", "Album", "MusicalGroup", "Person"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By", "Member Of"),
                                      selected = c("Creator Of", "Influenced By", "Member Of"), multiple = TRUE)
                        ),
                        mainPanel(
                          br(),
                          h4("Who has Sailor Shift been most influenced by over time?"),
                          h5("The visualisation shows all Persons and Musical Groups that have influenced Sailor Shift's work."),
                          withSpinner(girafeOutput("influencedByPlot", width = "100%", height = "600px")),
                          tags$hr(),
                          htmlOutput("insight_1a")
                        )
                      )
             ),
             ######################## Question 1b ##############################
             tabPanel("Collaborations & her Influences",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("node_type_filter", "Filter Node Types:",
                                      choices = c("Song", "Album", "MusicalGroup", "Person"),
                                      selected = c("Song", "Album", "MusicalGroup","Person"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By", "Member Of"),
                                      selected = c("Creator Of", "Influenced By", "Member Of"), multiple = TRUE)
                        ),
                        mainPanel(
                          br(),
                          h4("Who has Sailor Shift collaborated with and directly or indirectly influenced?"),
                          h5("Hover your mouse over the nodes below to learn more about her collaborators"),
                          withSpinner(girafeOutput("collabInfluenceNetwork", width = "100%", height = "600px")),
                          tags$hr(),
                          htmlOutput("insight_1b")
                        )
                      )
             ),
             ######################## Question 1c ##############################
             tabPanel("Impact on Oceanus Folk Collaborators",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("degree_sep", "Degree of Separation:", min = 1, max = 13,
                                      value = 13, step = 1, sep = "", animate = animationOptions(interval = 2000, loop = FALSE)),
                          checkboxInput(
                            inputId = "include_infinite",
                            label = "Include Infinite (Unreachable) Nodes",
                            value = TRUE
                          ),
                          helpText("Adjust the degree of separation to uncover how artists are interconnected across the network"),
                        ),
                        mainPanel(
                          br(),
                          h4("How has Sailor Shift influenced collaborators of the broader Oceanus Folk community?"),
                          withSpinner(girafeOutput("broadInfluencePlot", width = "100%", height = "600px")),
                          tags$hr(),
                          htmlOutput("insight_1c")
                        )
                      )
             )
           )
  ),
  
  ############################### Question 2 #######################################
  ############ Tab 1
  tabPanel(
    "Influence of Oceanus Folk",
    tabsetPanel(
      ################## Tab 1 ##################
      tabPanel(
        "Trajectory over Time",
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              "year_range_2a",
              "Year:",
              min = 1992,
              max = 2040,
              value = c(1992, 2040),
              step = 1,
              round = TRUE,
              sep = "",
              width = "100%",
              animate = FALSE
            ),
            helpText("This dual slider lets you explore the influence over the selected periods.")
          ),
          mainPanel(
            fluidRow(
              column(12, withSpinner(plotlyOutput("plot_facet_counts", height = "300px"))),
              column(12, withSpinner(plotlyOutput("plot_combined_2a", height = "600px")))
            ),
            tags$hr(),
            htmlOutput("insight_2afinal")
          )
        )
      ),
      
      ############ Tab 2
      
      tabPanel(
        title = "Outward Influence on other Genres",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "selected_genre",
              "Select Genre:",
              choices = c("All", sort(unique(genre_influence_stats$song_genre))),
              selected = "All",
              width = "100%"
            ),
            helpText("Select a genre to view how Oceanus Folk has influenced it.")
          ),
          
          mainPanel(
            # Row 1: Interpretation text (on top)
            fluidRow(column(
              width = 12,
              h4("Sankey Diagram: Top Influenced Genre"),
              helpText(
                "To determine which genres have been most influenced by Oceanus Folk, all songs and albums were identified. Then, the music (Songs/Albums) that influenced them were obtained to calculate the frequency and percentage of Oceanus Folk's influence across different musical genre. This analysis reveals the genres that show the strongest impact from Oceanus Folk's musical style."
              )
            )),
            br(),
            # Row 2: Sankey diagram (below text)
            fluidRow(column(
              width = 12,
              sankeyNetworkOutput("genreSankey", height = "400px")
            )),
            
            br(),
            
            fluidRow(column(
              width = 12,
              DT::dataTableOutput("genreTable"),
              helpText("Legend: Total_Music = Total no. of music under the genre."),
              helpText("Oceanus_Influence = Number of influenced music by Oceanus Folk."),
              helpText("Total_Influenced = Number of influenced music in the genre."),
              helpText("Perc_Oceanus = Percentage of music influenced by Oceanus Folk.")
            ))
          )
        )),
      
      ############ Tab 3
      
      tabPanel(
        "Outward Influence on other Artists",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "selected_artist",
              "Select Artist:",
              choices = c("All", sort(unique(creator_influenced_by_stats$creator_name))),
              selected = "All",
              width = "100%"
            ),
            helpText("Select an artist to view how much they have been influenced by Oceanus Folk.")
          ),
          
          mainPanel(
            # Row 1: Interpretation text
            fluidRow(column(
              width = 12,
              h4("Sankey Diagram: Top Influenced Artists"),
              helpText(
                "To identify the top artists most influenced by Oceanus Folk, all artists (persons or musical groups) who either (a) created Oceanus Folk music (songs/albums) or (b) were influenced by the genre were identified."
              )
            )),
            br(),
            # Row 2: Sankey diagram
            fluidRow(column(
              width = 12,
              sankeyNetworkOutput("artistSankey", height = "600px")
            )),
            
            br(),
            
            # Row 3: Table
            fluidRow(column(
              width = 12,
              DT::dataTableOutput("artistInfluenceTable")
            ))
          )
        )
      ),
      ############ Tab 4
      
      tabPanel(
        title = "Inward Influence from other Genres",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "selected_inward_influence_genre",
              "Select Genre:",
              choices = c("All", sort(unique(genre_influence_stats$song_genre))),
              selected = "All",
              width = "100%"
            ),
            helpText("Select a genre to view how it has influenced Oceanus Folk.")
          ),
          
          mainPanel(
            # Row 1: Interpretation text (on top)
            fluidRow(column(
              width = 12,
              h4("Sankey Diagram: Genres that Influenced Oceanus Folk"),
              helpText(
                "To determine which genres have most influenced Oceanus Folk, all Oceanus Folk songs/albums were analyzed along with the genres of songs that influenced them. This reveals the external genres that shaped Oceanus Folk's evolution."
              )
            )),
            br(),
            # Row 2: Sankey diagram
            fluidRow(column(
              width = 12,
              sankeyNetworkOutput("influencerSankey", height = "400px")
            )),
            
            br(),
            
            # Row 3: Table
            fluidRow(column(
              width = 12,
              DT::dataTableOutput("influencerGenreTable")
            ))
          )
        )),
      
      ############ Tab 5
      
      tabPanel(
        "Evolution with Rise of Sailor Shift",
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              inputId = "entropy_max_year",
              label = "Year",
              min = 1992,
              max = 2040,
              value = 2040,
              step = 1,
              sep = "",
              animate = animationOptions(interval = 100, loop = FALSE)
            ),
            helpText(
              "This animated slider lets you explore how genre entropy evolved over time."
            )
          ),
          mainPanel(
            plotlyOutput("entropyPlot", height = "400px"),
            htmlOutput("entropy_description") 
          )
        )
      )  # end of tabPanel Tab 5
    )    # ← CLOSE the tabsetPanel HERE!
  ), 
  
  ############################### Question 3 #######################################
  tabPanel("Oceanus Folk's Rising Star",
           tabsetPanel(
             ######################## Question 3 Table ############################
             tabPanel("Artist's Star Factor",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("filter_genres_3_t", "Filter by Genre:",
                                      choices = all_genres,
                                      selected = all_genres, multiple = TRUE),
                          selectizeInput("artist_3_t_1", "Select Artist 1 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          selectizeInput("artist_3_t_2", "Select Artist 2 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          selectizeInput("artist_3_t_3", "Select Artist 3 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          sliderInput("year_range_3_t", "Filter by Year:", min = 1992, max = 2040,
                                      value = c(1992, 2040), step = 1, round = TRUE, sep = "", width = "100%", animate = TRUE)
                        ),
                        mainPanel(
                          # First: Radar Plots
                          fluidRow(
                            column(width = 4,
                                   withSpinner(plotOutput("predictedStars_3_radar_1", height = "350px"))
                            ),
                            column(width = 4,
                                   withSpinner(plotOutput("predictedStars_3_radar_2", height = "350px"))
                            ),
                            column(width = 4,
                                   withSpinner(plotOutput("predictedStars_3_radar_3", height = "350px"))
                            )
                          ),
                          tags$hr(),
                          
                          # Then: Table
                          h5("Artists Ranked by Star Factor"),
                          withSpinner(DT::dataTableOutput("predictedStars_3_table")),
                          tags$hr(),
                          
                          # Insights
                          htmlOutput("insight_3_t")
                        )
                      )
             ),
             ######################## Question 3a ##############################
             tabPanel("Career Trajectories & Influence Comparison",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("filter_genres_3_a", "Filter by Genre:",
                                      choices = all_genres,
                                      selected = all_genres, multiple = TRUE),
                          selectizeInput("artist_3_a_1", "Select Artist 1 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          selectizeInput("artist_3_a_2", "Select Artist 2 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          selectizeInput("artist_3_a_3", "Select Artist 3 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE)
                        ),
                        mainPanel(
                          fluidRow(
                            column(width = 6,
                                   h5("Music (Song/Album) Releases"),
                                   withSpinner(plotlyOutput("predictedStars_3a_1", height = "340px"))
                            ),
                            column(width = 6,
                                   h5("Notable Music (Song/Album) Releases"),
                                   withSpinner(plotlyOutput("predictedStars_3a_2", height = "340px"))
                            )
                          ),
                          fluidRow(
                            column(width = 6,
                                   h5("New Artist Influences & Collaborations"),
                                   withSpinner(plotlyOutput("predictedStars_3a_3", height = "340px"))
                            ),
                            column(width = 6,
                                   h5("Influenced Music"),
                                   withSpinner(plotlyOutput("predictedStars_3a_4", height = "340px"))
                            )
                          ),
                          tags$hr(),
                          htmlOutput("insight_3a")
                        )
                      )
             ),
             
             ######################## Question 3b ##############################
             tabPanel("Emerging Stars of Oceanus Folk",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("filter_genres_3_b", "Select Genre:",
                                      choices = all_genres,
                                      selected = "Oceanus Folk", multiple = FALSE),
                          selectizeInput("artist_3_b_1", "Select Artist 1 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          selectizeInput("artist_3_b_2", "Select Artist 2 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          selectizeInput("artist_3_b_3", "Select Artist 3 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          sliderInput("year_range_3_b", "Filter by Year:", min = 1992, max = 2040,
                                      value = c(2020, 2040), step = 1, round = TRUE, sep = "", width = "100%", animate = TRUE)
                        ),
                        mainPanel(  # Everything goes inside mainPanel
                          tabsetPanel(
                            tabPanel("Star Factor",
                                     uiOutput("dynamic_title_3b"),
                                     withSpinner(DT::dataTableOutput("predictedStars_3_b")),
                                     tags$hr(),
                                     htmlOutput("insight_3b")
                            ),
                            tabPanel("Artists' Details",
                                     fluidRow(
                                       column(width = 6,
                                              h5("Music (Song/Album) Releases"),
                                              withSpinner(plotlyOutput("predictedStars_3b_1", height = "340px"))
                                       ),
                                       column(width = 6,
                                              h5("Notable Music (Song/Album) Releases"),
                                              withSpinner(plotlyOutput("predictedStars_3b_2", height = "340px"))
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 6,
                                              h5("New Artist Influences & Collaborations"),
                                              withSpinner(plotlyOutput("predictedStars_3b_3", height = "340px"))
                                       ),
                                       column(width = 6,
                                              h5("Influenced Music"),
                                              withSpinner(plotlyOutput("predictedStars_3b_4", height = "340px"))
                                       )
                                     )
                            )
                          )
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
  
  ######################################## Question 1 ###################################
  
  Question1_Server(input, output, session)  
  
  
  ######################################## Question 2 ###################################
  
  Question2_Server(input, output, session)
  
  ############################### Question 3 Table ##################################
  
  Question3_Server(input, output, session)
  
  Question3_a_Server(input, output, session)
  
  Question3_b_Server(input, output, session)
  
  Question3_b_table_Server(input, output, session)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$insight_1a <- renderUI({
    HTML("
    <h4>Key observations:</h4>
    <ul>
      <li>Most other artists and groups have only produced a single work that influenced one of Sailor Shift's creations</li>
      <li><strong>Wei Zhao stands out</strong> as having influenced Sailor Shift multiple times:</li>
      <ul>
        <li>Produced <em>Susurros de Passion</em> which influenced her album <em>Salty Dreams</em></li>
        <li>Composed <em>Silent Steps in the Forest's Embrace</em> which influenced her song <em>Moon Over the Tide</em></li>
      </ul>
    </ul>
    
    <p>Use the interactive visualization to explore these influence relationships in more detail.</p>
    ")
  })
  
  output$insight_1b <- renderUI({
    HTML("
    <ul>
      <li>Sailor Shift has collaborated with a wide variety of artists throughout the years.
      </li>
      <li>However, it is worth noting that Sailor Shift has not directly or indirectly influenced anyone, since none of her music has influenced others.
        <ul>
          <li>For example, no Songs/Albums has referenced her Songs or Albums.</li>
        </ul>
      </li>
    </ul>
  ")
  })
  
  output$insight_1c <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  output$insight_2a <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  output$insight_2b <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  output$insight_2c <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
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
  
  output$insight_3b <- renderUI({
    HTML("<p><strong>Prediction:</strong> <To be Added>.</p>")
  })
}

shinyApp(ui, server)