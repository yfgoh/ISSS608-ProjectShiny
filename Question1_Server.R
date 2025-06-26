Question1_Server <- function(input, output, session) {
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
    
    # Visualisation
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
              "%s %s \nNotable: %s (%s)", node_name, genre, notable, release_date
            ),
            `Node Type` == "Song" ~ sprintf(
              "%s %s \nNotable: %s (%s) \n Single: %s", node_name, genre, notable, release_date, single
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
  
  ## Data Table
  output$filteredNodeTable <- DT::renderDataTable({
    sailor_node <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_name == "Sailor Shift") %>%
      pull(creator_from) %>%
      unique()
    
    sailor_songs <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_from == sailor_node,
             creator_from != influenced_by) %>%
      pull(song_to)
    
    sailor_songs_influenced <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_from == sailor_node,
             creator_from != influenced_by) %>%
      pull(influenced_by)
    
    sailor_songs_influenced_creators <- creator_and_songs_and_influenced_by_creator %>%
      filter(creator_from == sailor_node,
             creator_from != influenced_by) %>%
      pull(influenced_by_creator)
    
    all_node_names <- unique(c(
      sailor_node,
      sailor_songs,
      sailor_songs_influenced,
      sailor_songs_influenced_creators
    ))
    
    # Filter nodes only
    filtered_nodes <- graph %>%
      activate(nodes) %>%
      filter(name %in% all_node_names) %>%
      filter(`Node Type` %in% input$node_type_filter) %>%
      as_tibble() %>%
      select(name, node_name, `Node Type`, genre, notable, release_date, single)
    
    # Display the filtered node data as a table
    DT::datatable(filtered_nodes, options = list(pageLength = 5), rownames = FALSE)
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
    
    # Step 4: Get the creators of the influenced by songs
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
      activate(nodes) %>%
      filter(name %in% all_node_names)
    
    
    # 1. Filter nodes by selected Node Types
    filtered_graph <- sub_graph %>%
      activate(nodes) %>%
      filter(`Node Type` %in% input$node_type_filter)
    
    # 2. Filter edges by selected Edge Types
    filtered_graph <- filtered_graph %>%
      activate(edges) %>%
      filter(`Edge Colour` %in% input$edge_type_filter)
    
    # Visualisation
    
    g <- filtered_graph %>%
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
      theme_graph(base_family = "sans") +
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
      activate(nodes) %>%
      filter(name %in% all_nodes)
    
    # Filter nodes by selected types
    filtered_graph <- sub_graph %>%
      activate(nodes) %>%
      filter(`Node Type` %in% input$node_type_filter)
    
    # Filter edges by selected types
    filtered_graph <- filtered_graph %>%
      activate(edges) %>%
      filter(`Edge Colour` %in% input$edge_type_filter)
    
    # Visualisation
    g <- filtered_graph %>%
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
      theme_graph(base_family = "sans") +
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
      theme_graph(base_family = "sans") +
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
}