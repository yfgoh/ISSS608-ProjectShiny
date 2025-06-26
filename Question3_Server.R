Question3_Server <- function(input, output, session) {

  ############################### Question 3 table ##################################
  filtered_artist <- reactive({
    creator_and_songs %>%
      filter(song_genre %in% c(input$filter_genres_3_t),
             creator_node_type %in% c("Person", "MusicalGroup")) %>%
      pull(creator_name) %>%
      unique()
  })
  
  observe({
    updateSelectizeInput(session, "artist_t_1", choices = filtered_artist(), selected = "Sailor Shift", server = TRUE)
    updateSelectizeInput(session, "artist_t_2", choices = filtered_artist(), selected = "Jay Walters", server = TRUE)
    updateSelectizeInput(session, "artist_t_3", choices = filtered_artist(), selected = "Min Fu", server = TRUE)
    updateSelectizeInput(session, "artist_a_1", choices = all_artists, selected = "Sailor Shift", server = TRUE)
    updateSelectizeInput(session, "artist_a_2", choices = all_artists, selected = "Jay Walters", server = TRUE)
    updateSelectizeInput(session, "artist_a_3", choices = all_artists, selected = "Min Fu", server = TRUE)
  })

  
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
  creator_rankings <- reactive({
    creator_stats %>%
      mutate(
        songs_score = (total_songs - min(total_songs)) / (max(total_songs) - min(total_songs)),
        notable_score = (notable_hits - min(notable_hits)) / (max(notable_hits) - min(notable_hits)),
        artists_score = (collaboration_influence_creator - min(collaboration_influence_creator)) / (max(collaboration_influence_creator) - min(collaboration_influence_creator)),
        music_score = (influence_music - min(influence_music)) / (max(influence_music) - min(influence_music)),
        composite_score = round(songs_score + notable_score + artists_score + music_score, 2)
      ) %>%
      arrange(desc(composite_score)) %>%
      select(creator_name, total_songs, notable_hits, collaboration_influence_creator, influence_music, composite_score)
  })
  
  output$predictedStars_3_table <- DT::renderDataTable({
    # Step 4: Final Ranked Table
    DT::datatable(
      creator_rankings(),
      caption = "Artists Ranked by Star Factor",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 30, 50, 100),
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "stripe hover",
      escape = FALSE,
      extensions = 'Scroller'
    )
  })
  
  ##############################################################################
  
  ############################### Question 3a ##################################
  
  output$predictedStars_3a_1 <- renderPlotly({
    # Data Preparation
    
    chosen_creator_1 <- input$artist_a_1
    
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
    
    chosen_creator_2 <- input$artist_a_2
    
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
    
    chosen_creator_3 <- input$artist_a_3
    
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
    
    chosen_creator_1 <- input$artist_a_1
    
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
    
    chosen_creator_2 <- input$artist_a_2
    
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
    
    chosen_creator_3 <- input$artist_a_3
    
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
    
    chosen_creator_1 <- input$artist_a_1
    
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
    
    chosen_creator_2 <- input$artist_a_2
    
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
    
    chosen_creator_3 <- input$artist_a_3
    
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
    chosen_creator_1 <- input$artist_a_1
    
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
    
    chosen_creator_2 <- input$artist_a_2
    
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
    
    chosen_creator_3 <- input$artist_a_3
    
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