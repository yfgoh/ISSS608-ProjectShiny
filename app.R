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

source("data_prep.R")  # Load your reactive function
source("Question2_Server.R") # Load Q2 server
source("Question3_Server.R") # Load Q2 server

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
                                      choices = c("Song", "Album", "MusicalGroup", "Person"),
                                      selected = c("Song", "Album"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By", "Member Of"),
                                      selected = c("Creator Of", "Influenced By"), multiple = TRUE)
                        ),
                        mainPanel(
                          withSpinner(girafeOutput("sailorWorkPlot", width = "100%", height = "600px")),
                          tags$hr(),
                          htmlOutput("sailorBio")
                        )
                      )
             ),
             ######################## Question 1a ##############################
             tabPanel("Primary Influences",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("node_type_filter", "Filter Node Types:",
                                      choices = c("Song", "Album", "MusicalGroup", "Person"),
                                      selected = c("Song", "Album"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By", "Member Of"),
                                      selected = c("Creator Of", "Influenced By"), multiple = TRUE)
                        ),
                        mainPanel(
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
                                      selected = c("Song", "Album"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By", "Member Of"),
                                      selected = c("Creator Of", "Influenced By"), multiple = TRUE)
                        ),
                        mainPanel(
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
                                      value = 13, step = 1, sep = "", animate = TRUE),
                          checkboxInput(
                            inputId = "include_infinite",
                            label = "Include Infinite (Unreachable) Nodes",
                            value = TRUE
                          ),
                        ),
                        mainPanel(
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
  tabPanel("Influence of Oceanus Folk",
           tabsetPanel(
             
             ################## Tab 1 ##################
             tabPanel("Trajectory over Time",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          sliderInput("year_range_2a", "Year:",
                                      min = 1990, max = 2040,
                                      value = c(1990, 2040),
                                      step = 1,
                                      round = TRUE,
                                      sep = "",
                                      width = "100%",
                                      animate = FALSE)
                        ),
                        mainPanel(
                          fluidRow(
                            column(12, withSpinner(plotlyOutput("plot_facet_counts", height = "300px")))
                          ),
                          fluidRow(
                            column(12, withSpinner(plotlyOutput("plot_cumulative", height = "500px")))
                          ),
                          fluidRow(
                            column(12, withSpinner(plotlyOutput("plot_surprise", height = "500px")))
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
                   width = 2,
                   selectInput("selected_genre",
                               "Select Genre:",
                               choices = c("All", sort(unique(genre_influence_stats$song_genre))),
                               selected = "All",
                               width = "100%")
                 ),
                 
                 mainPanel(
                   # Row 1: Interpretation text (on top)
                   fluidRow(
                     column(
                       width = 12,
                       h4("Interpretation of Sankey Diagram"),
                       helpText("To determine which genres have been most influenced by Oceanus Folk, all songs and albums were identified. Then, the music (Songs/Albums) that influenced them were obtained to calculate the frequency and percentage of Oceanus Folk's influence across different musical genre. This analysis reveals the genres that show the strongest impact from Oceanus Folk's musical style.")
                     )
                   ),
                   
                   # Row 2: Sankey diagram (below text)
                   fluidRow(
                     column(
                       width = 12,
                       h4("Sankey Diagram"),
                       sankeyNetworkOutput("genreSankey", height = "400px")
                     )
                   ),
                   
                   br(),
                   
                   # Row 3: Data Table
                   fluidRow(
                     column(
                       width = 12,
                       h4("Data Table: Oceanus Folk Influence by Genre"),
                       uiOutput("genreTable")
                     )
                   )
                 )  
               )   
             ),     
             
             ############ Tab 3
             
             tabPanel("Outward Influence on other Artists",
                      fluidPage(
                        
                        # Row 1: Interpretation text
                        fluidRow(
                          column(
                            width = 12,
                            h4("Interpretation of Sankey Diagram"),
                            helpText("To identify the top artists most influenced by Oceanus Folk, all songs or albums by each artist were identified, along with the songs that influenced them. Those who created Oceanus Folk music or were influenced by the genre were ranked based on total exposure.")
                          )
                        ),
                        
                        # Row 2: Sankey diagram
                        fluidRow(
                          column(
                            width = 12,
                            h4("Sankey Diagram: Top Influenced Artists"),
                            sankeyNetworkOutput("artistSankey", height = "600px")
                          )
                        ),
                        
                        br(),
                        
                        # Row 3: Data Table
                        fluidRow(
                          column(
                            width = 12,
                            h4("Table: Top Artists Influenced by Oceanus Folk"),
                            uiOutput("artistInfluenceTable")
                          )
                        )
                        
                      ) # end fluidPage
             ),
             ############ Tab 4
             
             tabPanel(
               title = "Inward Influence from other Genres",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput("selected_genre",
                               "Select Genre:",
                               choices = c("All", sort(unique(genre_influence_stats$song_genre))),
                               selected = "All",
                               width = "100%")
                 ),
                 
                 mainPanel(
                   # Row 1: Interpretation text (on top)
                   fluidRow(
                     column(
                       width = 12,
                       h4("Interpretation of Sankey Diagram"),
                       helpText("To determine which genres have most influenced Oceanus Folk, all Oceanus Folk songs/albums were analyzed along with the genres of songs that influenced them. This reveals the external genres that shaped Oceanus Folk's evolution.")
                     )
                   ),
                   
                   # Row 2: Sankey diagram
                   fluidRow(
                     column(
                       width = 12,
                       h4("Sankey Diagram"),
                       sankeyNetworkOutput("influencerSankey", height = "400px")
                     )
                   ),
                   
                   br(),
                   
                   # Row 3: Data Table
                   fluidRow(
                     column(
                       width = 12,
                       h4("Data Table: Genres that Influenced Oceanus Folk"),
                       uiOutput("influencerGenreTable")
                     )
                   )
                 )
               ) 
             ),
             
             ############ Tab 5
             
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
  
  ############################### Question 3 #######################################
  tabPanel("Oceanus Folk's Rising Star",
           tabsetPanel(
             ######################## Question 3 Table ############################
             tabPanel("Artist's Star Factor",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("filter_genres_3_t", "Filter by Genre:",
                                      choices = all_genre,
                                      selected = all_genre, multiple = TRUE),
                          selectizeInput("artist_3_t_1", "Select Artist 1 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          selectizeInput("artist_3_t_2", "Select Artist 2 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE),
                          selectizeInput("artist_3_t_3", "Select Artist 3 to Compare:",
                                         choices = NULL, selected = NULL, multiple = FALSE)
                        ),
                        mainPanel(
                          withSpinner(DT::dataTableOutput("predictedStars_3_table")),
                          tags$hr(),
                          htmlOutput("insight_1c")
                        )
                      )
             ),
             ######################## Question 3a ##############################
             tabPanel("Career Trajectories & Influence Comparison",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("filter_genres_3_a", "Filter by Genre:",
                                      choices = all_genre,
                                      selected = all_genre, multiple = TRUE),
                          sliderInput("year_range_3a", "Filter by Year:", min = 2000, max = 2040,
                                      value = c(2020, 2040), step = 1, sep = "", animate = TRUE),
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
                                     h4("Music Releases"),
                                     withSpinner(plotlyOutput("predictedStars_3a_1", height = "350px"))
                              ),
                              column(width = 6,
                                     h4("Notable Music Releases"),
                                     withSpinner(plotlyOutput("predictedStars_3a_2", height = "350px"))
                              )
                            ),
                            fluidRow(
                              column(width = 6,
                                     h4("Artist Influences & Collaborations"),
                                     withSpinner(plotlyOutput("predictedStars_3a_3", height = "350px"))
                              ),
                              column(width = 6,
                                     h4("Influenced Music"),
                                     withSpinner(plotlyOutput("predictedStars_3a_4", height = "350px"))
                              )
                            ),
                            tags$hr(),
                            htmlOutput("insight_3b")
                        )
                      )
             ),
             ######################## Question 3b ##############################
             tabPanel("Emerging Stars of Oceanus Folk",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("filter_genres_3_a", "Filter by Genre:", 
                                             choices = c("Oceanus Folk", "Indie Pop", "Indie Folk")),
                          sliderInput("year_range_3a", "Filter by Year:", min = 2000, max = 2040,
                                      value = c(2020, 2040), step = 1, sep = "", animate = TRUE),
                          selectInput("selected_artists_3a", "Select Artists to Compare:",
                                      choices = c("Sailor Shift", "Maya Blue", "Juno Rivers"),
                                      selected = c("Sailor Shift", "Maya Blue", "Juno Rivers"),
                                      multiple = TRUE)
                        ),
                        mainPanel(
                          tableOutput("predictedStars_3c"),
                          htmlOutput("insight_3c")
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
  
  # Server-side reactive output for sailorWorkPlot
  output$sailorWorkPlot <- renderGirafe({
    
    # Step 0: Get name of 'Sailor Shift'
    sailor_vertex_name <- mc1_nodes_clean %>%
      filter(is_sailor == TRUE) %>%
      pull(name)
    
    # Step 1: Find outgoing edges from Sailor Shift
    sailor_out_edges <- mc1_edges_clean %>%
      filter(from == sailor_vertex_name)
    
    # Step 2: Identify neighbour node names
    sailor_out_node_names <- sailor_out_edges$to
    
    # Step 4: Identify songs/albums (filtered by user input)
    sailor_music_all <- mc1_nodes_clean %>%
      filter(name %in% sailor_out_node_names, 
             `Node Type` %in% input$node_type_filter) %>%
      pull(name)
    
    # Step 5: Build subgraph using names
    sub_nodes <- unique(c(sailor_vertex_name, sailor_music_all))
    sub_graph <- graph %>%
      activate(nodes) %>%
      filter(name %in% sub_nodes)
    
    # Filter edges based on user input
    filtered_edges <- sub_graph %>%
      activate(edges) %>%
      filter(`Edge Colour` %in% input$edge_type_filter)
    
    # Visualization
    g <- filtered_edges %>%
      ggraph(layout = "fr") +
      geom_edge_fan(
        aes(
          edge_colour = `Edge Colour`,
          start_cap = circle(1, 'mm'),
          end_cap = circle(1, 'mm')
        ),
        arrow = arrow(length = unit(1, 'mm')),
        alpha = 0.3
      ) +
      geom_point_interactive(
        aes(
          x = x,
          y = y,
          data_id = name,
          colour = `Node Colour`,
          shape = `Node Type`,
          size = ifelse(node_name %in% c("Sailor Shift", "Ivy Echos", "Wei Zhao"), 3, 1),
          tooltip = case_when(
            `Node Type` == "Album" ~ sprintf(
              "%s %s Notable: %s (%s)", node_name, genre, notable, release_date
            ),
            `Node Type` == "Song" ~ sprintf(
              "%s %s Notable: %s (%s) Single: %s", node_name, genre, notable, release_date, single
            ),
            TRUE ~ sprintf("%s", node_name)
          )
        ),
        show.legend = c(size = FALSE)
      ) +
      geom_node_text(
        aes(
          label = ifelse(node_name == "Sailor Shift", "Sailor Shift",
                         ifelse(node_name == "Ivy Echos", "Ivy Echos",
                                ifelse(node_name == "Wei Zhao", "Wei Zhao", NA)))
        ),
        fontface = "bold",
        size = 2.5,
        colour = 'red',
        show.legend = FALSE
      ) +
      scale_shape_manual(
        name = "Node Type",
        values = c(
          "Album" = 16,
          "MusicalGroup" = 15,
          "Person" = 17,
          "Song" = 10
        )
      ) +
      scale_edge_colour_manual(
        name = "Edge Colour",
        values = c(
          `Creator Of` = "#47D45A",
          `Influenced By` = "#FF5757",
          `Member Of` = "#CF57FF"
        )
      ) +
      scale_colour_manual(
        name = "Node Colour",
        values = c(
          "Musician" = "grey50",
          "Oceanus Folk" = "#0027EA",
          "Other Genre" = "#A45200"
        )
      ) +
      theme_graph(base_family = "sans") +
      theme(
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 9)
      ) +
      scale_size_identity()
    
    # Return girafe object
    girafe(ggobj = g, width_svg = 7, height_svg = 6)
  })
  
  ##############################################################################
  
  ############################### Question 1a ##################################

  # Server-side reactive output for sailorWorkPlot
  output$influencedByPlot <- renderGirafe({
      # Data Preparation
    
    # Step 1: Get the node of the sailor
    sailor_node <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_name == "Sailor Shift") %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the creator produced
    sailor_songs <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_from == sailor_node,
             creator_from != influenced_by) %>%
      pull(song_to)
    
    # Step 3: Get the songs they have influenced by
    sailor_songs_influenced <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_from == sailor_node,
             creator_from != influenced_by) %>%
      pull(influenced_by)
    
    # Step 5: Get the creators of the influenced by songs
    sailor_songs_influenced_creators <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_from == sailor_node,
             creator_from != influenced_by) %>%
      pull(influenced_by_creator)
    
    # Step 5: Combine all relevant node names
    all_node_names <- unique(c(
      sailor_node,
      sailor_songs,
      sailor_songs_influenced,
      sailor_songs_influenced_creators
    ))
    
    # Step 6: Filter graph to relevant nodes only
    sub_graph <- graph %>%
      filter(name %in% all_node_names)
    
    
    
    # Visualisation
    
    g <- sub_graph %>%
      ggraph(layout = "fr") + 
      geom_edge_fan(
        aes(
          edge_colour = `Edge Colour`,
          start_cap = circle(1, 'mm'),
          end_cap = circle(1, 'mm')
        ),
        arrow = arrow(length = unit(1, 'mm')),
        alpha = 0.3
      ) +
      geom_point_interactive(
        aes(
          x = x,
          y = y,
          data_id = name,
          colour = `Node Colour`,
          shape = `Node Type`,
          size = ifelse(node_name %in% c("Sailor Shift", "Wei Zhao"), 3, 1),
          tooltip = case_when(
            `Node Type` == "Album" ~ sprintf(
              "%s<br/>%s<br/>Notable: %s<br/>(%s)", node_name, genre, notable, release_date
            ),
            `Node Type` == "Song" ~ sprintf(
              "%s<br/>%s<br/>Notable: %s<br/>(%s)<br/>Single: %s", node_name, genre, notable, release_date, single
            ),
            TRUE ~ sprintf("%s", node_name)
          )
        ),
        show.legend = c(size = FALSE)
      )+ 
      geom_node_text(
        aes(
          label = ifelse(node_name == "Sailor Shift", "Sailor Shift",
                         ifelse(node_name == "Wei Zhao", "Wei Zhao", NA))
        ),
        fontface = "bold",
        size = 2.5,
        colour = 'red',
        show.legend = FALSE
      ) +
      scale_shape_manual(
        name = "Node Type",
        values = c(
          "Album" = 16,
          "MusicalGroup" = 15,
          "Person" = 17,
          "Song" = 10
        )
      ) +
      scale_edge_colour_manual(
        name = "Edge Colour",
        values = c(
          `Creator Of` = "#47D45A",
          `Influenced By` = "#FF5757",
          `Member Of` = "#CF57FF"
        )
      ) +
      scale_colour_manual(
        name = "Node Colour",
        values = c(
          "Musician" = "grey50",
          "Oceanus Folk" = "#0027EA",
          "Other Genre" = "#A45200"
        )
      ) +
      theme_graph() +
      theme(legend.text = element_text(size = 6),
            legend.title = element_text(size = 9)) +
      scale_size_identity()
    
    girafe(ggobj = g, width_svg = 7, height_svg = 6)
    })
  
  
  ##############################################################################
  
  ############################### Question 1b ##################################
  
  output$collabInfluenceNetwork <- renderGirafe({
    # Data Preparation
    
    # Step 1: Get the node of the sailor
    sailor_node <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_name == "Sailor Shift") %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the sailor produced
    sailor_songs <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_from == sailor_node,
             infuence_music_collaborate != sailor_node) %>%
      pull(song_to)
    
    # Step 3: Get the songs they have influenced / artists they collaborate with
    sailor_songs_collaborate_influence <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_from == sailor_node,
             infuence_music_collaborate != sailor_node) %>%
      pull(infuence_music_collaborate)
    
    # Step 4: Get the songs they have influenced
    sailor_songs_influence <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_from == sailor_node,
             infuence_music_collaborate != sailor_node,
             `Edge Colour` == "Influenced By") %>%
      pull(infuence_music_collaborate)
    
    # Step 5: Get the influenced creators of the influenced songs
    sailor_songs_influence_creators <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_from == sailor_node,
             influence_creator != sailor_node,
             !is.na(influence_creator),
             infuence_music_collaborate %in% sailor_songs_influence) %>%
      pull(influence_creator)
    
    all_nodes <- unique(c(sailor_node, 
                          sailor_songs, 
                          sailor_songs_collaborate_influence,
                          sailor_songs_influence_creators))
    
    # Create subgraph
    sub_graph <- graph %>%
      filter(name %in% all_nodes)
    
    
    
    # Visualisation
    
    g <- sub_graph %>%
      ggraph(layout = "fr") + 
      geom_edge_fan(
        aes(
          edge_colour = `Edge Colour`,
          start_cap = circle(1, 'mm'),
          end_cap = circle(1, 'mm')
        ),
        arrow = arrow(length = unit(1, 'mm')),
        alpha = 0.3
      ) +
      geom_point_interactive(
        aes(
          x = x,
          y = y,
          data_id = name,
          colour = `Node Colour`,
          shape = `Node Type`,
          size = ifelse(node_name == "Sailor Shift", 3, 1),
          tooltip = case_when(
            `Node Type` == "Album" ~ sprintf(
              "%s<br/>%s<br/>Notable: %s<br/>(%s)", node_name, genre, notable, release_date
            ),
            `Node Type` == "Song" ~ sprintf(
              "%s<br/>%s<br/>Notable: %s<br/>(%s)<br/>Single: %s", node_name, genre, notable, release_date, single
            ),
            TRUE ~ sprintf("%s", node_name)
          )
        ),
        show.legend = c(size = FALSE)
      )+ 
      geom_node_text(
        aes(
          label = ifelse(node_name == "Sailor Shift", "Sailor Shift", NA)
        ),
        fontface = "bold",
        size = 2.5,
        colour = 'red',
        show.legend = FALSE
      ) +
      scale_shape_manual(
        name = "Node Type",
        values = c(
          "Album" = 16,
          "MusicalGroup" = 15,
          "Person" = 17,
          "Song" = 10
        )
      ) +
      scale_edge_colour_manual(
        name = "Edge Colour",
        values = c(
          `Creator Of` = "#47D45A",
          `Influenced By` = "#FF5757",
          `Member Of` = "#CF57FF"
        )
      ) +
      scale_colour_manual(
        name = "Node Colour",
        values = c(
          "Musician" = "grey50",
          "Oceanus Folk" = "#0027EA",
          "Other Genre" = "#A45200"
        )
      ) +
      theme_graph() +
      theme(legend.text = element_text(size = 6),
            legend.title = element_text(size = 9)) +
      scale_size_identity()
    
    girafe(ggobj = g, width_svg = 7, height_svg = 6)
  })
    
  ##############################################################################
  
  ############################### Question 1c ##################################
  
  # Server-side reactive output for broadInfluencePlot
  
  output$broadInfluencePlot <- renderGirafe({
    # Data Preparation
    
    # Step 1: Get the node of the sailor
    sailor_node <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_name == "Sailor Shift") %>%
      pull(creator_from) %>%
      unique()
    
    genre_creators = creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(song_genre == "Oceanus Folk") %>%
      pull(creator_from)
    
    genre_music = creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(song_genre == "Oceanus Folk") %>%
      pull(song_to)
    
    genre_all_nodes <- unique(c(genre_creators,
                                genre_music))
    
    # Create subgraph
    sub_graph <- graph %>%
      filter(name %in% genre_all_nodes)
    
    # 4. Convert to igraph
    sub_igraph <- as.igraph(sub_graph)
    
    # 5. Compute distances
    artist_id <- which(V(sub_igraph)$name == sailor_node)
    distances <- distances(sub_igraph, v = artist_id, mode = "all")
    
    # 6. Create distance dataframe
    distance_df <- tibble(
      name = V(sub_igraph)$name,
      degree = as.numeric(distances[1, ])
    ) %>%
      mutate(degree = ifelse(is.infinite(degree), NA, degree))
    
    # 7. Filter based on slider and checkbox
    selected_degree <- input$degree_sep
    include_inf <- input$include_infinite
    
    # Override checkbox if selected degree is below 13
    if (selected_degree < 13) {
      include_inf <- FALSE
    }
    
    if (include_inf) {
      filtered_nodes <- distance_df %>%
        filter(is.na(degree) | degree <= selected_degree) %>%
        pull(name)
    } else {
      filtered_nodes <- distance_df %>%
        filter(!is.na(degree) & degree <= selected_degree) %>%
        pull(name)
    }
    
    filtered_nodes <- unique(c(filtered_nodes, sailor_node))
    
    # Build filtered graph
    filtered_graph <- sub_graph %>%
      filter(name %in% filtered_nodes) %>%
      activate(nodes) %>%
      left_join(distance_df, by = "name")
    
    # Visualisation
    
    g <- filtered_graph %>%
      ggraph(layout = "kk") + 
      geom_edge_fan(
        aes(
          edge_colour = `Edge Colour`,
          start_cap = circle(1, 'mm'),
          end_cap = circle(1, 'mm')
        ),
        arrow = arrow(length = unit(1, 'mm')),
        alpha = 0.3
      ) +
      geom_point_interactive(
        aes(
          x = x,
          y = y,
          data_id = name,
          colour = degree,
          shape = `Node Type`,
          size = ifelse(node_name == "Sailor Shift", 3, 1),
          tooltip = case_when(
            `Node Type` == "Album" ~ sprintf(
              "%s<br/>%s<br/>Notable: %s<br/>(%s)<br/>Degree: %s", node_name, genre, notable, release_date, degree
            ),
            `Node Type` == "Song" ~ sprintf(
              "%s<br/>%s<br/>Notable: %s<br/>(%s)<br/>Single: %s<br/>Degree: %s", node_name, genre, notable, release_date, single, degree
            ),
            TRUE ~ sprintf("%s<br/>Degree: %s", node_name, degree)
          )
        ),
        show.legend = c(size = FALSE)
      )+ 
      geom_node_text(
        aes(
          label = ifelse(node_name == "Sailor Shift", "Sailor Shift", NA)
        ),
        fontface = "bold",
        size = 3,
        colour = 'red',
        show.legend = FALSE
      ) +
      scale_shape_manual(
        name = "Node Type",
        values = c(
          "Album" = 16,
          "MusicalGroup" = 15,
          "Person" = 17,
          "Song" = 10
        )
      ) +
      scale_edge_colour_manual(
        name = "Edge Colour",
        values = c(
          `Creator Of` = "#47D45A",
          `Influenced By` = "#FF5757",
          `Member Of` = "#CF57FF"
        )
      ) +
      theme_graph() +
      theme(legend.text = element_text(size = 6),
            legend.title = element_text(size = 9)) +
      scale_size_identity() + 
      scale_color_gradientn(
        name = "Degree",
        colours = c("#2E3192", "#FFA757"),
        values = scales::rescale(c(0, 13)),
        na.value = "grey50",
        limits = c(0, 13),
        breaks = 0:13
      )
    
    girafe(ggobj = g, width_svg = 9, height_svg = 8)
  })
  
  observe({
    if (input$degree_sep < 13) {
      updateCheckboxInput(
        session,
        inputId = "include_infinite",
        value = FALSE
      )
      shinyjs::disable("include_infinite")
    } else {
      shinyjs::enable("include_infinite")
    }
  })
  
#########################################Question 2 ###################################
  
  Question2_Server(input, output, session)
  
  ############################### Question 3 Table ##################################
  
  Question3_Server(input, output, session)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  output$insight_1a <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
  })
  
  output$insight_1b <- renderUI({
    HTML("<p><strong>Insight:</strong> <To be Added>.</p>")
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