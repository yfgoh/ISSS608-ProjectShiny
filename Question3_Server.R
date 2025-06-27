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
      class = "stripe hover",
      escape = FALSE,
      extensions = 'Scroller'
    )
  })
}