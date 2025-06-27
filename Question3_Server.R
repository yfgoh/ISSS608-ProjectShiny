Question3_Server <- function(input, output, session) {
  ############################### Question 3 table ##################################
  filtered_artist_3_t <- reactive({
    creator_and_songs %>%
      filter(song_genre %in% c(input$filter_genres_3_t),
             creator_node_type %in% c("Person", "MusicalGroup")) %>%
      pull(creator_name) %>%
      unique()
  })
  

  observe({
    updateSelectizeInput(session, "artist_3_t_1", choices = filtered_artist_3_t(), selected = "Sailor Shift", server = TRUE)
    updateSelectizeInput(session, "artist_3_t_2", choices = filtered_artist_3_t(), selected = "Jay Walters", server = TRUE)
    updateSelectizeInput(session, "artist_3_t_3", choices = filtered_artist_3_t(), selected = "Min Fu", server = TRUE)
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
  
  filtered_creator_influence_lists <- reactive({
    creator_influence_lists %>%
    filter(creator_name %in% filtered_artist_3_t(),
           creator_release_date >= input$year_range_3_t[1],
           creator_release_date <= input$year_range_3_t[2])
  })

  # Step 2: Aggregate unique influences per creator
  creator_stats <- reactive({
    filtered_creator_influence_lists() %>%
    group_by(creator_name) %>%
    summarize(
      total_songs = n_distinct(song_to),
      notable_hits = sum(notable == TRUE, na.rm = TRUE),
      collaboration = length(unique(unlist(unique_collaborate))),
      influence_creators = length(unique(unlist(unique_influence_creators))),
      collaboration_influence_creator = length(unique(c(unlist(unique_influence_creators),unlist( unique_collaborate)))),
      influence_music = length(unique(unlist(unique_influence_music)))
      )
  })
  
  # Step 3: Create Scoring Rubric
  creator_rankings <- reactive({
    creator_stats() %>%
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
      class = "compact stripe hover",
      escape = FALSE,
      extensions = 'Scroller'
    )
  })
  
  ############################### Question 3 Radar ##################################
  
  # Extract all creators' relevant data for scaling
  all_scaled <- reactive({
    creator_rankings() %>%
    select(creator_name, total_songs, notable_hits, collaboration_influence_creator, influence_music) %>%
    mutate(across(-creator_name, ~ scales::rescale(., to = c(0, 100)))) %>%
    column_to_rownames("creator_name") 
  })
  
  ############################### Question 3 Radar 1 #################################
  
  output$predictedStars_3_radar_1 <- renderPlot({
    chosen_creator_1 = input$artist_3_t_1
    
    # Filter the data for Sailor Shift
    creator_1_data <- creator_rankings() %>%
      filter(creator_name == chosen_creator_1) %>%
      select(total_songs, notable_hits, collaboration_influence_creator, influence_music)
    
    # Extract scaled values for Sailor Shift
    creator_1_scaled <- all_scaled()[chosen_creator_1, , drop = FALSE]
    
    # Construct radar input with min and max for the chart frame
    radar_matrix_1 <- rbind(
      rep(100, ncol(creator_1_scaled)),  # Max values
      rep(0, ncol(creator_1_scaled)),    # Min values
      creator_1_scaled                   # Actual values
    )
    
    # Plot Radar Chart
    par(mar = c(2, 2, 4, 2), oma = c(1, 1, 3, 1))
    
    radarchart(radar_matrix_1,
               axistype = 1,
               pcol = "#0027EA",
               pfcol = adjustcolor("#0027EA", alpha.f = 0.3),
               plwd = 2,
               cglcol = "grey",
               cglty = 1,
               cglwd = 0.8,
               axislabcol = "black",
               caxislabels = paste0(seq(0, 100, 25), "%"),
               vlcex = 0.85,
               title = paste("Star Profile: ", chosen_creator_1),
               calcex = 0.8,
               cex.main = 1.3,
               vlabels = c("Total Music", "Notable\nHits", "Artist Influ & Colab", "Music\nInfluenced"),
               centerzero = TRUE)
  })
  
  ############################### Question 3 Radar 2 #################################
  
  output$predictedStars_3_radar_2 <- renderPlot({
    chosen_creator_2 = input$artist_3_t_2
    
    # Filter the data for Sailor Shift
    creator_2_data <- creator_rankings() %>%
      filter(creator_name == chosen_creator_2) %>%
      select(total_songs, notable_hits, collaboration_influence_creator, influence_music)
    
    # Extract scaled values for Sailor Shift
    creator_2_scaled <- all_scaled()[chosen_creator_2, , drop = FALSE]
    
    # Construct radar input with min and max for the chart frame
    radar_matrix_2 <- rbind(
      rep(100, ncol(creator_2_scaled)),  # Max values
      rep(0, ncol(creator_2_scaled)),    # Min values
      creator_2_scaled                   # Actual values
    )
    
    # Plot Radar Chart
    par(mar = c(2, 2, 4, 2), oma = c(1, 1, 3, 1))
    
    radarchart(radar_matrix_2,
               axistype = 1,
               pcol = "#FF5757",
               pfcol = adjustcolor("#FF5757", alpha.f = 0.3),
               plwd = 2,
               cglcol = "grey",
               cglty = 1,
               cglwd = 0.8,
               axislabcol = "black",
               caxislabels = paste0(seq(0, 100, 25), "%"),
               vlcex = 0.85,
               title = paste("Star Profile: ", chosen_creator_2),
               calcex = 0.8,
               cex.main = 1.3,
               vlabels = c("Total Music", "Notable\nHits", "Artist Influ & Colab", "Music\nInfluenced"),
               centerzero = TRUE)
  })
  
  ############################### Question 3 Radar 3 #################################
  
  output$predictedStars_3_radar_3 <- renderPlot({
    chosen_creator_3 = input$artist_3_t_3
    
    # Filter the data for Sailor Shift
    creator_3_data <- creator_rankings() %>%
      filter(creator_name == chosen_creator_3) %>%
      select(total_songs, notable_hits, collaboration_influence_creator, influence_music)
    
    # Extract scaled values for Sailor Shift
    creator_3_scaled <- all_scaled()[chosen_creator_3, , drop = FALSE]
    
    # Construct radar input with min and max for the chart frame
    radar_matrix_3 <- rbind(
      rep(100, ncol(creator_3_scaled)),  # Max values
      rep(0, ncol(creator_3_scaled)),    # Min values
      creator_3_scaled                   # Actual values
    )
    
    # Plot Radar Chart
    par(mar = c(2, 2, 4, 2), oma = c(1, 1, 3, 1))
    
    radarchart(radar_matrix_3,
               axistype = 1,
               pcol = "#A45200",
               pfcol = adjustcolor("#A45200", alpha.f = 0.3),
               plwd = 2,
               cglcol = "grey",
               cglty = 1,
               cglwd = 0.8,
               axislabcol = "black",
               caxislabels = paste0(seq(0, 100, 25), "%"),
               vlcex = 0.85,
               title = paste("Star Profile: ", chosen_creator_3),
               calcex = 0.8,
               cex.main = 1.3,
               vlabels = c("Total Music", "Notable\nHits", "Artist Influ & Colab", "Music\nInfluenced"),
               centerzero = TRUE)
  })
}