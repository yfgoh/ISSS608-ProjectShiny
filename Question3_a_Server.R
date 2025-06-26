Question3_a_Server <- function(input, output, session) {
  
  ############################### Question 3a ##################################
  filtered_artist_3_a <- reactive({
    creator_and_songs %>%
      filter(song_genre %in% input$filter_genres_3_a,
             creator_node_type %in% c("Person", "MusicalGroup")) %>%
      pull(creator_name) %>%
      unique()
  })
  
  observe({
    updateSelectizeInput(session, "artist_3_a_1", choices = filtered_artist_3_a(), selected = "Sailor Shift", server = TRUE)
    updateSelectizeInput(session, "artist_3_a_2", choices = filtered_artist_3_a(), selected = "Jay Walters", server = TRUE)
    updateSelectizeInput(session, "artist_3_a_3", choices = filtered_artist_3_a(), selected = "Min Fu", server = TRUE)
  })
  
  output$predictedStars_3a_1 <- renderPlotly({
    # Data Preparation
    
    chosen_creator_1 <- input$artist_3_a_1
    
    # Step 1: Get the node of the chosen creator
    chosen_node_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(song_to)
    
    # Step 1: Count number of music by release date
    music_by_date_1 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_1)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    chosen_creator_2 <- input$artist_3_a_2
    
    # Step 1: Get the node of the chosen creator
    chosen_node_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(song_to)
    
    # Step 1: Count number of music by release date
    music_by_date_2 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_2)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    chosen_creator_3 <- input$artist_3_a_3
    
    # Step 1: Get the node of the chosen creator
    chosen_node_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(song_to)
    
    # Step 1: Count number of music by release date
    music_by_date_3 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_3)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    # Visualisation
    
    plot_ly(
      data = music_by_date_1,
      x = ~release_date,
      y = ~cumulative_count,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "red", size = 6),
      hoverinfo = "text",
      hovertext = ~paste0(
        "Artist: ", chosen_creator_1,
        "<br>Influence Date: ", release_date,
        "<br>Cumulative Count: ", cumulative_count
      )
    ) %>%
      add_trace(
        data = music_by_date_2,
        x = ~release_date,
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = chosen_creator_2,
        line = list(color = "green", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_2,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      add_trace(
        data = music_by_date_3,
        x = ~release_date,
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = chosen_creator_3,
        line = list(color = "purple", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      layout(
        title = "Yearly Music (Song/Album) Releases",
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
        )
      )
  })
  
  output$predictedStars_3a_2 <- renderPlotly({
    # Data Preparation
    
    chosen_creator_1 <- input$artist_3_a_1
    
    # Step 1: Get the node of the chosen creator
    chosen_node_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(song_to)
    
    # Step 1: Count number of notable music by release date
    notable_music_by_date_1 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_1), notable == TRUE) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    chosen_creator_2 <- input$artist_3_a_2
    
    # Step 1: Get the node of the chosen creator
    chosen_node_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(song_to)
    
    # Step 1: Count number of notable music by release date
    notable_music_by_date_2 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_2), notable == TRUE) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    chosen_creator_3 <- input$artist_3_a_3
    
    # Step 1: Get the node of the chosen creator
    chosen_node_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(song_to)
    
    # Step 1: Count number of notable music by release date
    notable_music_by_date_3 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_3), notable == TRUE) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    # Visualisation
    
    plot_ly(
      data = notable_music_by_date_1,
      x = ~release_date,
      y = ~cumulative_count,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "red", size = 6),
      hoverinfo = "text",
      hovertext = ~paste0(
        "Artist: ", chosen_creator_1,
        "<br>Influence Date: ", release_date,
        "<br>Cumulative Count: ", cumulative_count
      )
    ) %>%
      add_trace(
        data = notable_music_by_date_2,
        x = ~release_date,
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = chosen_creator_2,
        line = list(color = "green", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_2,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      add_trace(
        data = notable_music_by_date_3,
        x = ~release_date,
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = chosen_creator_3,
        line = list(color = "purple", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      layout(
        title = "Yearly Notable Music (Song/Album) Releases",
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
        )
      )
  })
  
  output$predictedStars_3a_3 <- renderPlotly({
    # Data Preparation
    
    chosen_creator_1 <- input$artist_3_a_1
    
    # Step 1: Get the node of the chosen creator
    chosen_node_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 1: Count number of influenced artists by release date
    influence_artists_by_date_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_from %in% unique(chosen_node_1),
             influence_creator != unique(chosen_node_1)) %>%
      # Get unique artist-date pairs first
      distinct(influence_creator, influence_release_date) %>%
      # Find first influence date for each artist
      group_by(influence_creator) %>%
      summarize(
        first_influence_date = if(n() > 0) min(influence_release_date) else NA_real_,
        .groups = "drop"
      ) %>%
      # Count new artists by first influence date
      count(first_influence_date, name = "music_count") %>%
      arrange(first_influence_date) %>%
      rename(creator_release_date = first_influence_date) %>%
      # Calculate cumulative unique artists
      mutate(cumulative_count = cumsum(music_count))
    
    chosen_creator_2 <- input$artist_3_a_2
    
    # Step 1: Get the node of the chosen creator
    chosen_node_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 1: Count number of influenced artists by release date
    influence_artists_by_date_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_from %in% unique(chosen_node_2),
             influence_creator != unique(chosen_node_2)) %>%
      # Get unique artist-date pairs first
      distinct(influence_creator, influence_release_date) %>%
      # Find first influence date for each artist
      group_by(influence_creator) %>%
      summarize(
        first_influence_date = if(n() > 0) min(influence_release_date) else NA_real_,
        .groups = "drop"
      ) %>%
      # Count new artists by first influence date
      count(first_influence_date, name = "music_count") %>%
      arrange(first_influence_date) %>%
      rename(creator_release_date = first_influence_date) %>%
      # Calculate cumulative unique artists
      mutate(cumulative_count = cumsum(music_count))
    
    chosen_creator_3 <- input$artist_3_a_3
    
    # Step 1: Get the node of the chosen creator
    chosen_node_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 1: Count number of influenced artists by release date
    influence_artists_by_date_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_from %in% unique(chosen_node_3),
             influence_creator != unique(chosen_node_3)) %>%
      # Get unique artist-date pairs first
      distinct(influence_creator, influence_release_date) %>%
      # Find first influence date for each artist
      group_by(influence_creator) %>%
      summarize(
        first_influence_date = if(n() > 0) min(influence_release_date) else NA_real_,
        .groups = "drop"
      ) %>%
      # Count new artists by first influence date
      count(first_influence_date, name = "music_count") %>%
      arrange(first_influence_date) %>%
      rename(creator_release_date = first_influence_date) %>%
      # Calculate cumulative unique artists
      mutate(cumulative_count = cumsum(music_count))
    
    # Visualisation
    
    plot_ly(
      data = influence_artists_by_date_1,
      x = ~creator_release_date,
      y = ~cumulative_count,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "red", size = 6),
      hoverinfo = "text",
      hovertext = ~paste0(
        "Artist: ", chosen_creator_1,
        "<br>Influence Date: ", creator_release_date,
        "<br>Cumulative Count: ", cumulative_count
      )
    ) %>%
      add_trace(
        data = influence_artists_by_date_2,
        x = ~creator_release_date,
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = chosen_creator_2,
        line = list(color = "green", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_2,
          "<br>Influence Date: ", creator_release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      add_trace(
        data = influence_artists_by_date_3,
        x = ~creator_release_date,
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = chosen_creator_3,
        line = list(color = "purple", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Influence Date: ", creator_release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      layout(
        title = "Annual Count of New Artist Influences & Collaborations",
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
        )
      )
  })
  
  output$predictedStars_3a_4 <- renderPlotly({
    # Data Preparation
    chosen_creator_1 <- input$artist_3_a_1
    
    # Step 1: Get the node of the chosen creator
    chosen_node_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(song_to)
    
    # Step 4: Get the songs they have influenced
    creators_songs_influence_1 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_1,
             infuence_music_collaborate != chosen_node_1,
             `Edge Colour` == "Influenced By") %>%
      pull(infuence_music_collaborate)
    
    # Step 1: Count number of influenced music by release date
    influence_song_by_date_1 <- mc1_nodes_clean %>%
      filter(name %in% unique(creators_songs_influence_1)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    chosen_creator_2 <- input$artist_3_a_2
    
    # Step 1: Get the node of the chosen creator
    chosen_node_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(song_to)
    
    # Step 4: Get the songs they have influenced
    creators_songs_influence_2 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_2,
             `Edge Colour` == "Influenced By") %>%
      pull(infuence_music_collaborate)
    
    # Step 1: Count number of influenced music by release date
    influence_song_by_date_2 <- mc1_nodes_clean %>%
      filter(name %in% unique(creators_songs_influence_2)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    chosen_creator_3 <- input$artist_3_a_3
    
    # Step 1: Get the node of the chosen creator
    chosen_node_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(song_to)
    
    # Step 4: Get the songs they have influenced
    creators_songs_influence_3 <- creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(creator_name == chosen_creator_3,
             infuence_music_collaborate != chosen_node_3,
             `Edge Colour` == "Influenced By") %>%
      pull(infuence_music_collaborate)
    
    # Step 1: Count number of influenced music by release date
    influence_song_by_date_3 <- mc1_nodes_clean %>%
      filter(name %in% unique(creators_songs_influence_3)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count))
    
    # Visualisation
    
    plot_ly(
      data = influence_song_by_date_1,
      x = ~release_date,
      y = ~cumulative_count,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "red", size = 6),
      hoverinfo = "text",
      hovertext = ~paste0(
        "Artist: ", chosen_creator_1,
        "<br>Influence Date: ", release_date,
        "<br>Cumulative Count: ", cumulative_count
      )
    ) %>%
      add_trace(
        data = influence_song_by_date_2,
        x = ~release_date,
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = chosen_creator_2,
        line = list(color = "green", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_2,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      add_trace(
        data = influence_song_by_date_3,
        x = ~release_date,
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = chosen_creator_3,
        line = list(color = "purple", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      layout(
        title = "Yearly Music (Song/Album) Influence",
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
        )
      )
  })
}