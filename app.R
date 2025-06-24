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
             tabPanel("Primary Influences",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("degree_sep", "Degree of Separation:", min = 1, max = 20,
                                      value = c(1, 20), step = 1, sep = "", animate = TRUE),
                          selectInput("node_type_filter", "Filter Node Types:",
                                      choices = c("Song", "Album", "MusicalGroup", "Person"),
                                      selected = c("Song", "Album"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By", "Member Of"),
                                      selected = c("Creator Of", "Influenced By"), multiple = TRUE)
                        ),
                        mainPanel(
                          plotOutput("influencedByPlot", height = "400px"),
                          htmlOutput("insight_1a")
                        )
                      )
             ),
             tabPanel("Collaborations & her Influences",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("degree_sep", "Degree of Separation:", min = 1, max = 20,
                                      value = c(1, 20), step = 1, sep = "", animate = TRUE),
                          selectInput("node_type_filter", "Filter Node Types:",
                                      choices = c("Song", "Album", "MusicalGroup", "Person"),
                                      selected = c("Song", "Album"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By", "Member Of"),
                                      selected = c("Creator Of", "Influenced By"), multiple = TRUE)
                        ),
                        mainPanel(
                          visNetworkOutput("collabInfluenceNetwork", height = "500px"),
                          htmlOutput("insight_1b")
                        )
                      )
             ),
             tabPanel("Impact on Oceanus Folk Collaborators",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("degree_sep", "Degree of Separation:", min = 1, max = 20,
                                      value = c(1, 20), step = 1, sep = "", animate = TRUE),
                          selectInput("node_type_filter", "Filter Node Types:",
                                      choices = c("Song", "Album", "MusicalGroup", "Person"),
                                      selected = c("Song", "Album"), multiple = TRUE),
                          selectInput("edge_type_filter", "Filter Edge Types:",
                                      choices = c("Creator Of", "Influenced By", "Member Of"),
                                      selected = c("Creator Of", "Influenced By"), multiple = TRUE)
                        ),
                        mainPanel(
                          plotOutput("broadInfluencePlot", height = "400px"),
                          htmlOutput("insight_1c")
                        )
                      )
             )
           )
  ),
  
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
  
  tabPanel("Oceanus Folk's Rising Star",
           tabsetPanel(
             tabPanel("Artist's Star Factor",
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
                          plotOutput("careerComparePlot", height = "400px"),
                          htmlOutput("insight_3a")
                        )
                      )
             ),
             tabPanel("Career Trajectories & Influence Comparison",
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
                          tableOutput("predictedStars_3b"),
                          htmlOutput("insight_3b")
                        )
                      )
             ),
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
  
  output$collabInfluenceNetwork <- renderVisNetwork({
    nodes <- data.frame(id = 1:4, label = c("Sailor", "Ivy Echos", "Node1", "Node2"))
    edges <- data.frame(from = c(1, 1, 2), to = c(2, 3, 4))
    visNetwork(nodes, edges)
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