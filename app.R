#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


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


# Data loading and pre-processing

# Load raw data
mc1_data <- fromJSON("data/MC1_graph.json")

# Split into nodes and edges
mc1_nodes_raw <- as_tibble(mc1_data$nodes)
mc1_edges_raw <- as_tibble(mc1_data$links)

# Add identifying columns
mc1_nodes_raw <- mc1_nodes_raw %>%
  mutate(
    is_sailor = (
      str_detect(name, regex("sailor shift", ignore_case = TRUE))
    ) %>% replace_na(FALSE),
    is_ivy = (
      str_detect(name, regex("ivy echos", ignore_case = TRUE))
    ) %>% replace_na(FALSE),
    is_oceanus_folk = str_detect(genre, regex("oceanus folk", ignore_case = TRUE)) %>%
      replace_na(FALSE)
  )

# Convert date fields
mc1_nodes_raw <- mc1_nodes_raw %>%
  mutate(across(c(release_date, notoriety_date, written_date),
                ~as.integer(if_else(`Node Type` %in% c("Song", "Album"), ., NA_character_))))

# Handle duplicates in nodes
mc1_nodes_tagged <- mc1_nodes_raw %>%
  mutate(group_key = paste(`Node Type`, name, single, release_date, genre,
                           notable, written_date, notoriety_date, is_sailor,
                           is_oceanus_folk, sep = "|"))

# De-duplicate nodes
mc1_nodes_dedup <- mc1_nodes_tagged %>%
  group_by(group_key) %>%
  arrange(desc(!is.na(stage_name))) %>%
  slice(1) %>%
  ungroup()

# Handle duplicates in edges
mc1_edges_raw <- mc1_edges_raw %>%
  distinct(source, target, `Edge Type`, .keep_all = TRUE) %>%
  select(!key)

# Restructure nodes for tidygraph compatibility
mc1_nodes_clean <- mc1_nodes_dedup %>%
  rename(node_name = name, name = id) %>%
  mutate(name = as.character(name)) %>%
  select(`Node Type`, node_name, release_date, genre, notable, name, single, written_date, stage_name, notoriety_date, is_sailor, is_ivy, is_oceanus_folk)

# Create edge mapping
key_to_id_map <- mc1_nodes_dedup %>%
  select(group_key, kept_id = id)

id_remap <- mc1_nodes_tagged %>%
  left_join(key_to_id_map, by = "group_key") %>%
  select(original_id = id, kept_id)

# Map edges to cleaned nodes
mc1_edges_mapped <- mc1_edges_raw %>%
  left_join(id_remap, by = c("source" = "original_id")) %>%
  mutate(source = kept_id) %>%
  select(-kept_id) %>%
  left_join(id_remap, by = c("target" = "original_id")) %>%
  mutate(target = kept_id) %>%
  select(-kept_id) %>%
  rename(from = source, to = target) %>%
  mutate(from = as.character(from), to = as.character(to))

# Remove unmatched edges
mc1_edges_clean <- mc1_edges_mapped %>%
  filter(!is.na(from), !is.na(to))

# Define edge validation rules
edge_rules <- list(
  PerformerOf = list(source = c("Person", "MusicalGroup"), target = c("Song", "Album")),
  ComposerOf = list(source = c("Person"), target = c("Song", "Album")),
  ProducerOf = list(source = c("Person", "RecordLabel"), target = c("Song", "Album", "Person", "MusicalGroup")),
  LyricistOf = list(source = c("Person"), target = c("Song", "Album")),
  RecordedBy = list(source = c("Song", "Album"), target = c("RecordLabel")),
  DistributedBy = list(source = c("Song", "Album"), target = c("RecordLabel")),
  InStyleOf = list(source = c("Song", "Album"), target = c("Song", "Album", "Person", "MusicalGroup")),
  InterpolatesFrom = list(source = c("Song", "Album"), target = c("Song", "Album")),
  CoverOf = list(source = c("Song", "Album"), target = c("Song", "Album")),
  LyricalReferenceTo = list(source = c("Song", "Album"), target = c("Song", "Album")),
  DirectlySamples = list(source = c("Song", "Album"), target = c("Song", "Album")),
  MemberOf = list(source = c("Person"), target = c("MusicalGroup"))
)

# Validate edges against schema
node_type_lookup <- mc1_nodes_clean %>%
  select(name, `Node Type`) %>%
  deframe()

mc1_edges_checked <- mc1_edges_clean %>%
  mutate(
    source_type = node_type_lookup[from],
    target_type = node_type_lookup[to]
  )

mc1_edges_tagged <- mc1_edges_checked %>%
  rowwise() %>%
  mutate(
    valid = {
      rule <- edge_rules[[`Edge Type`]]
      if (is.null(rule)) TRUE
      else {
        source_type %in% rule$source && target_type %in% rule$target
      }
    }
  ) %>%
  ungroup()

# Keep only valid edges
mc1_edges_clean <- mc1_edges_tagged %>%
  filter(valid) %>%
  select(from, to, `Edge Type`)

# Temporal validation for influence relationships
influence_edge_types <- c("InStyleOf", "InterpolatesFrom", "CoverOf", "LyricalReferenceTo", "DirectlySamples")

temporal_check <- mc1_edges_clean %>%
  filter(`Edge Type` %in% influence_edge_types) %>%
  left_join(mc1_nodes_clean %>% 
              select(name, source_type = `Node Type`, source_release = release_date), 
            by = c("from" = "name")) %>%
  left_join(mc1_nodes_clean %>% 
              select(name, target_type = `Node Type`, target_release = release_date), 
            by = c("to" = "name")) %>%
  filter(source_type %in% c("Song", "Album"),
         target_type %in% c("Song", "Album")) %>%
  mutate(
    temporal_violation = case_when(
      is.na(source_release) | is.na(target_release) ~ FALSE,
      source_release < target_release ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Remove temporally inconsistent edges
non_influence_edges <- mc1_edges_clean %>%
  filter(!(`Edge Type` %in% influence_edge_types))

consistent_influence_edges <- temporal_check %>%
  filter(!temporal_violation) %>%
  select(from, to, `Edge Type`)

mc1_edges_clean <- bind_rows(non_influence_edges, consistent_influence_edges)

# Add color groupings for visualization
mc1_edges_clean <- mc1_edges_clean %>%
  mutate(`Edge Colour` = case_when(
    `Edge Type` %in% c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf", "RecordedBy", "DistributedBy") ~ "Creator Of",
    `Edge Type` %in% c("InStyleOf", "InterpolatesFrom", "CoverOf", "LyricalReferenceTo", "DirectlySamples") ~ "Influenced By",
    `Edge Type` == "MemberOf" ~ "Member Of",
    TRUE ~ "Other"
  ))

mc1_nodes_clean <- mc1_nodes_clean %>%
  mutate(
    `Node Colour` = case_when(
      `Node Type` %in% c("Person", "MusicalGroup", "RecordLabel") ~ "Musician",
      genre == "Oceanus Folk" ~ "Oceanus Folk",
      TRUE ~ "Other Genre"
    )
  )

# Create the knowledge graph
set.seed(1234)
graph <- tbl_graph(edges = mc1_edges_clean,
                   nodes = mc1_nodes_clean,
                   directed = TRUE)



# Data Preparation for influenced table

# Step 1: Get all creator names
global_creators <- mc1_nodes_clean %>%
  filter(`Node Type` %in% c("Person", "MusicalGroup"))

# Step 2: Get all outgoing edges from these creators
creator_out_edges <- mc1_edges_clean %>%
  filter(from %in% global_creators$name, `Edge Colour` == "Creator Of")

# Step 3: Get all songs made by creators
creator_music <- mc1_nodes_clean %>%
  filter(name %in% creator_out_edges$to)

# Step 4: Get all outgoing edges from songs
creator_songs_out_edges <- mc1_edges_clean %>%
  filter(from %in% creator_music$name, `Edge Colour` == "Influenced By")

# Step 5: First join to get creator names (from)
creators <- creator_out_edges %>%
  left_join(mc1_nodes_clean, by = c("from" = "name")) %>%
  select(from, to, node_name, `Node Type`) %>%
  rename(creator_from = from, creator_name = node_name, creator_node_type = `Node Type`)

# Step 6: Second join to get song names (to)
creator_and_songs <- creators %>%
  left_join(mc1_nodes_clean, by = c("to" = "name")) %>%
  select(creator_from, creator_name, creator_node_type, to, node_name, release_date, genre, notable) %>%
  rename(song_name = node_name, song_genre = genre, song_to = to) %>%
  distinct()

# Step 7: Third join to get song's influenced genre (to)
creator_and_songs_and_influenced_by <- creator_and_songs %>%
  left_join(creator_songs_out_edges %>% select(from, to), by = c("song_to" = "from"), relationship = "many-to-many") %>%
  left_join(mc1_nodes_clean %>% select(name, genre), by = c("to" = "name")) %>%
  rename(influenced_by_genre = genre, influenced_by = to) %>%
  distinct()

# Step 8: Fourth join to get influenced song's creator
creator_and_songs_and_influenced_by_creator <- creator_and_songs_and_influenced_by %>%
  left_join(creator_out_edges %>% select(from, to), by = c("influenced_by" = "to"), relationship = "many-to-many") %>%
  rename(influenced_by_creator = from)




# Data Preparation influence table

# Step 4: Get all incoming edges from songs (music and collaborators)
creator_songs_in_edges <- mc1_edges_clean %>%
  filter(to %in% creator_music$name)

# Step 5: First join to get creator names (from)
creators <- creator_out_edges %>%
  left_join(mc1_nodes_clean, by = c("from" = "name")) %>%
  select(from, to, node_name, `Node Type`) %>%
  rename(creator_name = node_name, creator_node_type = `Node Type`)

# Step 6: Second join to get song names (to)
creator_and_songs <- creators %>%
  left_join(mc1_nodes_clean, by = c("to" = "name")) %>%
  select(from, creator_name, creator_node_type, to, node_name, release_date, genre, notable) %>%
  rename(creator_from = from, song_name = node_name, creator_release_date = release_date, song_genre = genre, song_to = to) %>%
  distinct()

# Step 7: Third join to get influenced songs / collaborators
creator_and_songs_and_influences <- creator_and_songs %>%
  left_join(creator_songs_in_edges %>% select(from, to, `Edge Colour`), by = c("song_to" = "to"), relationship = "many-to-many") %>%
  left_join(mc1_nodes_clean %>% select(name, genre, node_name, release_date), by = c("from" = "name")) %>%
  rename(influence_genre = genre, infuence_music_collaborate = from, infuence_music_collaborate_name = node_name, influence_release_date = release_date) %>%
  distinct()

# Step 8: Fourth join to get influenced song's creator
creator_and_songs_and_influences_and_creators <- creator_and_songs_and_influences %>%
  left_join(creator_out_edges %>% select(from, to), by = c("infuence_music_collaborate" = "to"), relationship = "many-to-many") %>%
  rename(influence_creator = from)

# Step 9: Fifth join to get influenced song's creator name
creator_and_songs_and_influences_and_creators <- creator_and_songs_and_influences_and_creators %>%
  left_join(mc1_nodes_clean %>% select(name, node_name), by = c("influence_creator" = "name")) %>%
  rename(influence_creator_name = node_name)

# Step 10: Add release date for collaborators
creator_and_songs_and_influences_and_creators_collaborate <- creator_and_songs_and_influences_and_creators %>%
  mutate(
    influence_release_date = case_when(
      `Edge Colour` == "Creator Of" ~ creator_release_date,
      TRUE ~ influence_release_date  # Keeps original value if not "Creator Of"
    ),
    influence_creator = case_when(   # A collaborator can also be considered influenced
      `Edge Colour` == "Creator Of" ~ infuence_music_collaborate,
      TRUE ~ influence_creator
    )
  )

all_genre <- creator_music$genre %>%
  unique()

all_artists <- global_creators$node_name %>%
  unique()

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
  tabPanel("Influence of Oceanus Folk",
           tabsetPanel(
             tabPanel("Trajectory over Time",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year_range_2a", "Filter by Year:", min = 1980, max = 2040,
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
                          sliderInput("year_range_2a", "Filter by Year:", min = 1980, max = 2040,
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
                          sliderInput("year_range_2a", "Filter by Year:", min = 1980, max = 2040,
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
                          sliderInput("year_range_2a", "Filter by Year:", min = 1980, max = 2040,
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
                          sliderInput("year_range_2a", "Filter by Year:", min = 1980, max = 2040,
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
             tabPanel("Artist's Star Factor",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("filter_genres_3a", "Filter by Genre:",
                                      choices = c(all_genre),
                                      selected = c(all_genre), multiple = TRUE),
                          sliderInput("year_range_3a", "Filter by Year:", min = 2000, max = 2040,
                                      value = c(2020, 2040), step = 1, sep = "", animate = TRUE),
                        ),
                        mainPanel(
                          withSpinner(uiOutput("predictedStars_3a")),
                          tags$hr(),
                          htmlOutput("insight_1c")
                        )
                      )
             ),
             ######################## Question 3a ##############################
             tabPanel("Career Trajectories & Influence Comparison",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("filter_genres_3a", "Filter by Genre:", 
                                             choices = c("Oceanus Folk", "Indie Pop", "Indie Folk")),
                          sliderInput("year_range_3a", "Filter by Year:", min = 2000, max = 2040,
                                      value = c(2020, 2040), step = 1, sep = "", animate = TRUE),
                          selectInput("selected_artists_3b_1", "Select Artist 1 to Compare:",
                                      choices = c(all_artists),
                                      selected = c("Sailor Shift"),
                                      multiple = FALSE),
                          selectInput("selected_artists_3b_2", "Select Artist 2 to Compare:",
                                      choices = c(all_artists),
                                      selected = c("Jay Walters"),
                                      multiple = FALSE),
                          selectInput("selected_artists_3b_3", "Select Artist 3 to Compare:",
                                      choices = c(all_artists),
                                      selected = c("Min Fu"),
                                      multiple = FALSE)
                        ),
                        mainPanel(
                          plotOutput("predictedStars_3b", height = "400px"),
                          htmlOutput("insight_3b")
                        )
                      )
             ),
             ######################## Question 3b ##############################
             tabPanel("Emerging Stars of Oceanus Folk",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("filter_genres_3a", "Filter by Genre:", 
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############################################################################
  
  ############################### Question 3 Table ##################################
  
  output$predictedStars_3a <- renderUI({
    
    # Data Preparation
    
    # Step 1: To highlight songs that the creator influence that is not produced by same creator
    creator_influence_lists <- creator_and_songs_and_influences_and_creators_collaborate %>%
      group_by(creator_name, creator_node_type, song_to, song_name, creator_release_date, song_genre, notable) %>%
      distinct () %>%
      summarize(
        unique_collaborate = list(unique(na.omit(infuence_music_collaborate[creator_from != influence_creator & `Edge Colour` == "Creator Of"]))),
        unique_influence_creators = list(unique(na.omit(influence_creator[creator_from != influence_creator & `Edge Colour` == "Influenced By"]))),
        unique_influence_music = list(unique(na.omit(infuence_music_collaborate[creator_from != influence_creator & !is.na(influence_genre)])))
      )
    
    # Step 2: Aggregate unique influences per creator
    creator_stats <- creator_influence_lists %>%
      group_by(creator_name) %>%
      summarize(
        total_songs = n_distinct(song_to),
        notable_hits = sum(notable == TRUE, na.rm = TRUE),
        collaboration = length(unique(unlist(unique_collaborate))),
        influence_creators = length(unique(unlist(unique_influence_creators))),
        collaboration_influence_creator = length(unique(c(unlist(unique_influence_creators),unlist( unique_collaborate)))),
        influence_music = length(unique(unlist(unique_influence_music)))
      )
    
    # Step 3: Create Scoring Rubric
    creator_rankings <- creator_stats %>%
      mutate(
        # Normalize each metric (0-1 scale)
        songs_score = (total_songs - min(total_songs)) / (max(total_songs) - min(total_songs)),
        notable_score = (notable_hits - min(notable_hits)) / (max(notable_hits) - min(notable_hits)),
        artists_score = (collaboration_influence_creator - min(collaboration_influence_creator)) / (max(collaboration_influence_creator) - min(collaboration_influence_creator)),
        music_score = (influence_music - min(influence_music)) / (max(influence_music) - min(influence_music)),
        
        # Calculate composite score (equal weighting)
        composite_score = songs_score + notable_score + artists_score + music_score
      ) %>%
      arrange(desc(composite_score)) %>%
      select(creator_name, total_songs, notable_hits, collaboration_influence_creator, influence_music, composite_score)
    
    # Step 4: Final Ranked Table
    final_table <- creator_rankings %>%
      mutate(
        Rank = row_number(),
        `Star Factor` = round(composite_score, 2)
      ) %>%
      select(Rank, Artist = creator_name, `Total Music` = total_songs, 
             `Notable Hits` = notable_hits, `Artist Influ & Colab` = collaboration_influence_creator,
             `Music Influenced` = influence_music, `Star Factor`)
    
    HTML(
      knitr::kable(final_table, caption = "Artists Ranked by Star Factor") %>%
        kableExtra::kable_styling("striped", full_width = FALSE) %>%
        kableExtra::scroll_box(height = "300px")
    )
  })
  
  
  
  
  
  
  
  
  
  ##############################################################################
  
  ############################### Question 3a ##################################
  
  
  
  
  


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