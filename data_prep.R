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
library(plotly)
library(networkD3)
library(htmlwidgets)


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

# Export to global environment
assign("mc1_nodes_clean", mc1_nodes_clean, envir = .GlobalEnv)
assign("mc1_edges_clean", mc1_edges_clean, envir = .GlobalEnv)
assign("graph", graph, envir = .GlobalEnv)

#################################################################################

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

# Step 1: To highlight songs that the creator influence that is not produced by same creator
creator_influence_lists <- creator_and_songs_and_influences_and_creators_collaborate %>%
    group_by(creator_name, creator_node_type, song_to, song_name, creator_release_date, song_genre, notable) %>%
    distinct() %>%
    summarize(
      unique_collaborate = list(unique(na.omit(infuence_music_collaborate[creator_from != influence_creator & `Edge Colour` == "Creator Of"]))),
      unique_influence_creators = list(unique(na.omit(influence_creator[creator_from != influence_creator & `Edge Colour` == "Influenced By"]))),
      unique_influence_music = list(unique(na.omit(infuence_music_collaborate[creator_from != influence_creator & !is.na(influence_genre)])))
    )

# Supporting lists for UI
all_genre <- creator_music$genre %>%
  unique()

all_artists <- global_creators$node_name %>%
  unique()





















###################################Question 2##############################

# Step 0: Get name of 'Sailor Shift'
sailor_vertex_name <- mc1_nodes_clean %>%
  filter(is_sailor == TRUE) %>%
  pull(name)

# Step 1: Find outgoing edges from Sailor Shift
sailor_out_edges <- mc1_edges_clean %>%
  filter(from == sailor_vertex_name)

# Step 2: Identify neighbour node names
sailor_out_node_names <- sailor_out_edges$to

# Step 4: Identify songs/albums
sailor_music_all <- mc1_nodes_clean %>%
  filter(name %in% sailor_out_node_names, `Node Type` %in% c("Song", "Album")) %>%
  pull(name)

mc1_nodes_clean %>%
  filter(name %in% sailor_music_all) %>%
  arrange(release_date) %>%
  select(`Node Type`, node_name, release_date, genre, notable, single, notoriety_date) %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>% 
  scroll_box(height = "300px")



# Prepare data
sailor_release_data <- mc1_nodes_clean %>%
  filter(name %in% sailor_music_all) %>%
  count(release_date, genre)

# Plot
ggplot(sailor_release_data, aes(x = release_date, y = n, fill = genre)) +
  geom_col() +
  geom_vline(xintercept = 2028, linetype = "dashed", color = "red") +
  annotate("text", 
           x = 2028, 
           y = max(sailor_release_data$n) + 0.5,
           label = "Viral Breakthrough", 
           color = "red", 
           size = 3.5,
           angle = 0,        # <- horizontal
           vjust = -0.5,     # <- moves it slightly above
           hjust = 0.5       # <- centers it horizontally
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Sailor Shift's Music Releases (2024–2040)",
    x = "Year",
    y = "Number of Songs/Albums",
    fill = "Genre"
  ) +
  theme_minimal()





# Data Preparation for Plot 1

# Step 1: Get all node names in Oceanus Folk genre
oceanus_nodes <- mc1_nodes_clean %>%
  filter(genre == "Oceanus Folk") %>%
  pull(name)

# Step 2: Count number of oceanus folk nodes by release date
oceanus_nodes_by_date <- mc1_nodes_clean %>%
  filter(name %in% unique(oceanus_nodes)) %>%
  count(release_date, name = "oceanus_nodes_count") %>%
  arrange(release_date) %>%  # Ensure dates are in chronological order
  mutate(cumulative_count = cumsum(oceanus_nodes_count))



# Visualisation

plot_ly(
  data = oceanus_nodes_by_date,
  x = ~release_date,
  y = ~oceanus_nodes_count,
  type = "bar",
  name = "Number of Music Releases",
  marker = list(color = "#2E3192"),
  hoverinfo = "text",
  hovertext = ~paste0(
    "Released Date: ", release_date,
    "<br>Count: ", oceanus_nodes_count
  )
) %>%
  add_trace(
    y = ~cumulative_count,
    type = "scatter",
    mode = "lines+markers",
    name = "Cumulative Count",
    line = list(color = "black", width = 2),
    marker = list(color = "red", size = 6),
    hoverinfo = "text",
    hovertext = ~paste0(
      "Released Date: ", release_date,
      "<br>Cumulative Count: ", cumulative_count
    )
  ) %>%
  layout(
    title = "Yearly Oceanus Folk Music (Song/Album) Releases",
    margin = list(b = 80, t = 80),      
    xaxis = list(
      title = NA,  
      dtick = 5,
      automargin = TRUE
    ),
    yaxis = list(
      title = "Count",
      automargin = TRUE
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1
    ),
    annotations = list(
      list(
        x = 2024, 
        y = 280,
        text = "<b>2024: Sailor Shift's Debut</b>",
        xref = "x", yref = "y",
        xanchor = "right",
        showarrow = TRUE, arrowhead = 2,
        ax = -30, ay = -40,
        font = list(color="#2E3192", size=12)
      ),
      list(
        x = 2028, 
        y = 320,
        text = "<b>2028: Sailor Shift's Breakthrough</b>",
        xref = "x", yref = "y",
        xanchor = "right",
        showarrow = TRUE, arrowhead = 2,
        ax = -30, ay = -40,
        font = list(color="#2E3192", size=12)
      )
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 2024, x1 = 2024,
        y0 = 0, y1 = 280,
        line = list(dash="dash", color="grey")
      ),
      list(
        type = "line",
        x0 = 2028, x1 = 2028,
        y0 = 0, y1 = 320,
        line = list(dash="dash", color="grey")
      )
    )
  )


# Data Preparation for Plot 2

# Step 1: Count number of influenced music by release date
influence_yearly <- creator_and_songs_and_influences_and_creators_collaborate %>%
  filter(song_to %in% unique(oceanus_nodes),
         `Edge Colour` == "Influenced By",
         infuence_music_collaborate != song_to) %>%
  distinct(infuence_music_collaborate, influence_release_date) %>%
  count(influence_release_date, name = "num_influenced_nodes") %>%
  complete(influence_release_date = 1975:2040, fill = list(num_influenced_nodes = 0)) %>%
  arrange(influence_release_date) %>%  # Ensure dates are in chronological order
  filter(cumsum(num_influenced_nodes) > 0) %>%
  mutate(cumulative_influenced = cumsum(num_influenced_nodes))



# Visualisation

plot_ly(
  data = influence_yearly,
  x = ~influence_release_date,
  y = ~num_influenced_nodes,
  type = "bar",
  name = "Number of Influenced Songs/Albums",
  marker = list(color = "#2E3192"),
  hoverinfo = "text",
  hovertext = ~paste0("Year: ", influence_release_date, "<br>Influenced: ", num_influenced_nodes)
) %>%
  add_trace(
    y = ~cumulative_influenced,
    type = "scatter",
    mode = "lines+markers",
    name = "Cumulative Influenced",
    line = list(color = "black", width = 2),
    marker = list(color = "red", size = 6),
    hoverinfo = "text",
    hovertext = ~paste0("Year: ", influence_release_date, "<br>Cumulative: ", cumulative_influenced)
  ) %>%
  layout(
    title = "Yearly Oceanus Folk Music Influence on Songs/Albums",
    margin = list(b = 80, t = 80),
    xaxis = list(
      title = NA,
      dtick = 5,
      automargin = TRUE,
      range = c(1990, 2040)
    ),
    yaxis = list(
      title = "Count",
      automargin = TRUE,
      range = c(0, 300)
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1
    ),
    annotations = list(
      list(
        x = 2024,
        y = 250,
        text = "<b>2024: Sailor Shift's Debut</b>",
        xref = "x", yref = "y",
        xanchor = "right",
        showarrow = TRUE, arrowhead = 2,
        ax = -30, ay = -40,
        font = list(color = "#2E3192", size = 12)
      ),
      list(
        x = 2028,
        y = 280,
        text = "<b>2028: Sailor Shift's Breakthrough</b>",
        xref = "x", yref = "y",
        xanchor = "right",
        showarrow = TRUE, arrowhead = 2,
        ax = -30, ay = -40,
        font = list(color = "#2E3192", size = 12)
      )
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 2024, x1 = 2024,
        y0 = 0, y1 = 280,
        line = list(dash = "dash", color = "grey")
      ),
      list(
        type = "line",
        x0 = 2028, x1 = 2028,
        y0 = 0, y1 = 320,
        line = list(dash = "dash", color = "grey")
      )
    )
  )



#### Plot 3

# Step 1: Count number of influenced artists by release date
creators_by_date <- creator_and_songs_and_influences_and_creators_collaborate %>%
  filter(song_to %in% unique(oceanus_nodes),
         influence_creator != creator_from) %>%
  # Get unique artist-date pairs first
  distinct(influence_creator, influence_release_date) %>%
  # Find first influence date for each artist
  group_by(influence_creator) %>%
  summarize(
    first_influence_date = if(n() > 0) min(influence_release_date) else NA_real_,
    .groups = "drop"
  ) %>%
  # Count new artists by first influence date
  count(first_influence_date, name = "people_count") %>%
  arrange(first_influence_date) %>%
  rename(influence_release_date = first_influence_date) %>%
  # Calculate cumulative unique artists
  mutate(cumulative_count = cumsum(people_count))



# Visualisation

plot_ly(
  data = creators_by_date,
  x = ~influence_release_date,
  y = ~people_count,
  type = "bar",
  name = "Number of Artists",
  marker = list(color = "#2E3192"),
  hoverinfo = "text",
  hovertext = ~paste0(
    "Released Date: ", influence_release_date,
    "<br>Count: ", people_count
  )
) %>%
  add_trace(
    y = ~cumulative_count,
    type = "scatter",
    mode = "lines+markers",
    name = "Cumulative Count",
    line = list(color = "black", width = 2),
    marker = list(color = "red", size = 6),
    hoverinfo = "text",
    hovertext = ~paste0(
      "Released Date: ", influence_release_date,
      "<br>Cumulative Count: ", cumulative_count
    )
  ) %>%
  layout(
    title = "Yearly Number of New Oceanus Folk Artist Influences & Collaborations",
    margin = list(b = 80, t = 80),      
    xaxis = list(
      title = NA,  
      dtick = 5,
      automargin = TRUE
    ),
    yaxis = list(
      title = "Count",
      automargin = TRUE
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1
    ),
    annotations = list(
      list(
        x = 2024, 
        y = 800,
        text = "<b>2024: Sailor Shift's Debut</b>",
        xref = "x", yref = "y",
        xanchor = "right",
        showarrow = TRUE, arrowhead = 2,
        ax = -30, ay = -40,
        font = list(color="#2E3192", size=12)
      ),
      list(
        x = 2028, 
        y = 1120,
        text = "<b>2028: Sailor Shift's Breakthrough</b>",
        xref = "x", yref = "y",
        xanchor = "right",
        showarrow = TRUE, arrowhead = 2,
        ax = -30, ay = -40,
        font = list(color="#2E3192", size=12)
      )
    ),
    shapes = list(
      list(
        type = "line",
        x0 = 2024, x1 = 2024,
        y0 = 0, y1 = 800,
        line = list(dash="dash", color="grey")
      ),
      list(
        type = "line",
        x0 = 2028, x1 = 2028,
        y0 = 0, y1 = 1120,
        line = list(dash="dash", color="grey")
      )
    )
  )


##Combined plot for cumulative numbers 

# Step 2: Select and rename columns
df1 <- oceanus_nodes_by_date %>%
  select(year = release_date, value = cumulative_count) %>%
  mutate(series = "Music Releases")

df2 <- influence_yearly %>%
  select(year = influence_release_date, value = cumulative_influenced) %>%
  mutate(series = "Influenced Songs/Albums")

df3 <- creators_by_date %>%
  select(year = influence_release_date, value = cumulative_count) %>%
  mutate(series = "New Influenced Artists")

# Step 3: Combine into one tidy data frame
combined_df <- bind_rows(df1, df2, df3)

# Step 4: Plot
plot_ly(combined_df,
        x = ~year,
        y = ~value,
        color = ~series,
        colors = c("Music Releases" = "#2E3192", 
                   "Influenced Songs/Albums" = "#EF5B5B", 
                   "New Influenced Artists" = "#FBAF00"),
        type = 'scatter',
        mode = 'lines+markers',
        hoverinfo = "text",
        hovertext = ~paste0("Year: ", year, "<br>", series, ": ", value)
) %>%
  layout(
    title = "Cumulative Oceanus Folk Impact Over Time",
    xaxis = list(title = "Year", dtick = 5),
    yaxis = list(title = "Cumulative Count"),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
    margin = list(b = 100, t = 80)
  )

#Bayesian surprise

library(zoo)

# Define general function to compute Bayesian Surprise
compute_bayesian_surprise <- function(data, year_col, count_col, window = 5) {
  data <- data %>%
    rename(year = {{year_col}}, count = {{count_col}}) %>%
    complete(year = min(year):max(year), fill = list(count = 0)) %>%
    arrange(year)
  
  # Calculate moving average (prior belief)
  data <- data %>%
    mutate(prior_mean = rollmean(count, k = window, align = "right", fill = NA))
  
  # Compute KL Divergence for Poisson distributions
  data <- data %>%
    mutate(
      surprise = ifelse(
        !is.na(prior_mean) & prior_mean > 0 & count > 0,
        count * log(count / prior_mean) - (count - prior_mean),
        NA
      )
    )
  
  return(data)
}


# 1. Surprise for Oceanus Folk releases
surprise_releases <- compute_bayesian_surprise(
  oceanus_nodes_by_date,
  release_date,
  oceanus_nodes_count,
  window = 5
)

# 2. Surprise for influenced works
surprise_influence <- compute_bayesian_surprise(
  influence_yearly,
  influence_release_date,
  num_influenced_nodes,
  window = 5
)

# 3. Surprise for artists and contributors
surprise_contributors <- compute_bayesian_surprise(
  creators_by_date,
  influence_release_date,
  people_count,
  window = 5
)


plot_data <- bind_rows(
  surprise_releases %>% select(year, surprise) %>% mutate(type = "Music Releases"),
  surprise_influence %>% select(year, surprise) %>% mutate(type = "Influenced Works"),
  surprise_contributors %>% select(year, surprise) %>% mutate(type = "Artists")
) %>%
  filter(!is.na(surprise), year >= 1990)

# Plot using Plotly
plot_ly(data = plot_data, x = ~year, y = ~surprise, color = ~type, colors = "Set1",
        type = 'scatter', mode = 'lines+markers',
        hoverinfo = "text",
        hovertext = ~paste0("Year: ", year,
                            "<br>Category: ", type,
                            "<br>Surprise: ", round(surprise, 2))) %>%
  layout(
    title = "Bayesian Surprise Across Oceanus Folk Music Releases, Influenced Works, and Artists",
    xaxis = list(title = "Year", dtick = 5, range = c(1990, max(plot_data$year))),
    yaxis = list(title = "Surprise Score (KL Divergence)"),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
    margin = list(t = 60, b = 80)
  )


## Cumulative plot 2a
# Step 2: Select and rename columns
df1 <- oceanus_nodes_by_date %>%
  select(year = release_date, value = cumulative_count) %>%
  mutate(series = "Music Releases")

df2 <- influence_yearly %>%
  select(year = influence_release_date, value = cumulative_influenced) %>%
  mutate(series = "Influenced Songs/Albums")

df3 <- creators_by_date %>%
  select(year = influence_release_date, value = cumulative_count) %>%
  mutate(series = "New Influenced Artists")

# Step 3: Combine into one tidy data frame
combined_df <- bind_rows(df1, df2, df3)

# Step 4: Plot
plot_ly(combined_df,
        x = ~year,
        y = ~value,
        color = ~series,
        colors = c("Music Releases" = "#ADD8E6", 
                   "Influenced Songs/Albums" = "#F08080", 
                   "New Influenced Artists" = "#C2E0C6"),
        type = 'scatter',
        mode = 'lines+markers',
        hoverinfo = "text",
        hovertext = ~paste0("Year: ", year, "<br>", series, ": ", value)
) %>%
  layout(
    title = "Oceanus Folk Influence Over Time (Cumulative)",
    xaxis = list(title = "Year", dtick = 5),
    yaxis = list(title = "Cumulative Count"),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
    margin = list(b = 100, t = 80)
  )






##############################Question2b#########################################################

genre_influence_stats <- creator_and_songs_and_influences_and_creators_collaborate %>%
  filter(infuence_music_collaborate != song_to) %>%
  distinct(song_to, song_genre, infuence_music_collaborate, influence_genre, `Edge Colour`) %>%
  group_by(song_genre) %>%
  summarize(
    total_music = n_distinct(song_to),
    total_influences = n_distinct(na.omit(infuence_music_collaborate[!is.na(influence_genre)])),
    oceanus_influences = n_distinct(na.omit(infuence_music_collaborate[influence_genre == "Oceanus Folk"])),
    other_influences = total_influences - oceanus_influences,
    perc_oceanus = round(oceanus_influences / total_influences * 100, 1),
    no_influences = sum(is.na(influence_genre)),
  ) %>%
  arrange(desc(perc_oceanus))

genre_influence_stats %>%
  kable(caption = "Genre Ranked by Oceanus Influence") %>%
  kable_styling("striped", full_width = F) %>%
  scroll_box(height = "300px")



###################2c#########################################################

creator_influenced_by_stats <- creator_and_songs_and_influenced_by_creator %>%
  distinct(creator_name, creator_node_type, song_to, song_genre, influenced_by, influenced_by_genre, influenced_by_creator, notable) %>%
  group_by(creator_name, creator_node_type) %>%
  summarize(
    total_music = n_distinct(song_to),
    notable_hits = n_distinct(song_to[notable == TRUE]),
    oceanus_music = n_distinct(song_to[song_genre == "Oceanus Folk"]),
    oceanus_influenced_by = n_distinct(na.omit(influenced_by[influenced_by_genre == "Oceanus Folk" & creator_name != influenced_by_creator])),
    total_oceanus_influence = oceanus_music + oceanus_influenced_by
  ) %>%
  arrange(desc(total_oceanus_influence)) %>%
  filter(creator_node_type == "Person", notable_hits > 10) %>%
  select(-creator_node_type)

creator_influenced_by_stats %>%
  head(10) %>%
  rename(
    `Artist` = creator_name,
    `Total Music` = total_music,
    `Notable Hits` = notable_hits,
    `No. of Oceanus Folk Music` = oceanus_music,
    `Oceanus Folk Influence` = oceanus_influenced_by,
    `Oceanus Folk Music & Influence` = total_oceanus_influence
  ) %>%
  kable(caption = "Ranking of Oceanus Folk Influence on Artists") %>%
  kable_styling("striped", full_width = F) %>%
  scroll_box(height = "200px")


###########Question2tab4#########################################################


genre_influenced_by_stats <- creator_and_songs_and_influenced_by_creator %>%
  filter(song_genre == "Oceanus Folk") %>%
  distinct(song_to, influenced_by, influenced_by_genre) %>%
  group_by(influenced_by_genre) %>%
  summarize(
    total_music = n_distinct(song_to),
    influenced_by = n_distinct(na.omit(influenced_by))
  ) %>%
  arrange(desc(influenced_by))

genre_influenced_by_stats %>%
  head(10) %>%
  rename(
    `Genre` = influenced_by_genre,
    `Total Influenced Music` = total_music,
    `Influenced By Oceanus Folk` = influenced_by
  ) %>%
  kable(caption = "Ranking of Oceanus Folk Influence on Music Genres") %>%
  kable_styling("striped", full_width = F) %>%
  scroll_box(height = "200px")

# Step 1: Prepare data — reverse flow direction: Genre → Oceanus Folk
sankey_df <- genre_influenced_by_stats %>%
  mutate(
    raw_source = influenced_by_genre,
    target = "Oceanus Folk",
    value = influenced_by,
    source = paste0(raw_source, " (", value, ")")
  ) %>%
  select(source, target, value) %>%
  arrange(desc(value)) %>%
  head(22)  # Top 22 genres influencing Oceanus Folk

# Step 2: Create node list
nodes <- data.frame(name = unique(c(sankey_df$source, sankey_df$target)))

# Step 3: Create link list with indices
links <- sankey_df %>%
  mutate(
    source = match(source, nodes$name) - 1,
    target = match(target, nodes$name) - 1
  )

# Step 4: Add tooltip group (Genre → Oceanus Folk)
links$group <- paste0(sankey_df$source, " → ", sankey_df$target, ": ", sankey_df$value)

# Step 5: Render Sankey
p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 13,
  nodeWidth = 30,
  sinksRight = FALSE  # ← Flip layout: sources on right if needed
)

# Step 6: Add tooltips
onRender(p, '
  function(el, x) {
    d3.select(el)
      .selectAll(".link")
      .append("title")
      .text(function(d) { return d.group; });
  }
')
#####################################################Tab 5#########################################################

# Step 1: Filter influence edges
influence_edges <- mc1_edges_clean %>%
  filter(`Edge Type` %in% c("InStyleOf", "CoverOf", "InterpolatesFrom", 
                            "LyricalReferenceTo", "DirectlySamples"))

# Step 2: Join genre and release date info
influence_genres <- influence_edges %>%
  left_join(mc1_nodes_clean %>% select(name, genre), by = c("from" = "name")) %>%
  rename(source_genre = genre) %>%
  left_join(mc1_nodes_clean %>% select(name, genre, release_date), by = c("to" = "name")) %>%
  rename(target_genre = genre, to_release = release_date) %>%
  left_join(mc1_nodes_clean %>% select(name, release_date), by = c("from" = "name")) %>%
  rename(from_release = release_date)

# Step 3a: Incoming entropy (genres influencing Oceanus Folk)
incoming_entropy_yearly <- influence_genres %>%
  filter(target_genre == "Oceanus Folk", !is.na(source_genre), !is.na(to_release)) %>%
  mutate(year = as.integer(to_release)) %>%
  group_by(year, source_genre) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(p = count / sum(count)) %>%
  summarise(entropy = entropy::entropy(p, unit = "log2")) %>%
  mutate(direction = "Incoming")

# Step 3b: Outgoing entropy (genres that Oceanus Folk influenced)
outgoing_entropy_yearly <- influence_genres %>%
  filter(source_genre == "Oceanus Folk", !is.na(target_genre), !is.na(from_release)) %>%
  mutate(year = as.integer(from_release)) %>%
  group_by(year, target_genre) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(p = count / sum(count)) %>%
  summarise(entropy = entropy::entropy(p, unit = "log2")) %>%
  mutate(direction = "Outgoing")

# Step 4: Combine and flip outgoing for mirrored effect
entropy_yearly <- bind_rows(
  incoming_entropy_yearly,
  outgoing_entropy_yearly %>% mutate(entropy = -entropy)
)

# Step 5: Plot mirrored entropy over time with hover text
entropy_plot <- ggplot(entropy_yearly, aes(
  x = year, y = entropy, fill = direction,
  text = paste0(
    "Year: ", year,
    "\nDirection: ", direction,
    "\nEntropy: ", round(abs(entropy), 3), " bits"
  )
)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(
    name = "Entropy Direction",
    values = c("Incoming" = "lightblue", "Outgoing" = "darkblue")
  ) +
  scale_y_continuous(
    breaks = seq(-4, 4, by = 1),
    labels = abs(seq(-4, 4, by = 1)),
    limits = c(-4, 4)
  ) +
  labs(
    title = "Genre Entropy of Oceanus Folk Over Time",
    x = "Year",
    y = "Genre Entropy (bits)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Add annotations after setting `max_entropy_val`
max_entropy_val <- max(abs(entropy_yearly$entropy), na.rm = TRUE)

entropy_plot <- entropy_plot +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_vline(xintercept = 2028, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  annotate("text", x = 2024.5, y = max_entropy_val + 0.4,
           label = "2024: Sailor Shift's Debut",
           color = "#2E3192", fontface = "bold", size = 3.5, hjust = 1) +
  annotate("text", x = 2028.5, y = max_entropy_val + 0.8,
           label = "2028: Sailor Shift's Breakthrough",
           color = "#2E3192", fontface = "bold", size = 3.5, hjust = 1) +
  annotate("segment", x = 2024.5, y = max_entropy_val + 0.4,
           xend = 2024, yend = max_entropy_val - 0.2,
           arrow = arrow(length = unit(0.2, "cm")), color = "grey50") +
  annotate("segment", x = 2028.5, y = max_entropy_val + 0.8,
           xend = 2028, yend = max_entropy_val - 0.2,
           arrow = arrow(length = unit(0.2, "cm")), color = "grey50")

# Use plotly for interactivity
ggplotly(entropy_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))

