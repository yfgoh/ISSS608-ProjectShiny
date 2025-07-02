Question3_b_Server <- function(input, output, session) {
  ############################### Question 3 b ##################################
  debounced_genres_3_b <- debounce(reactive(input$filter_genres_3_b), millis = 500)
  debounced_years_3_b <- debounce(reactive(input$year_range_3_b), millis = 500)
  
  output$dynamic_title_3b <- renderUI({
    req(input$filter_genres_3_b)  # Ensure a genre is selected
    h5(paste0(input$filter_genres_3_b, " Artists Ranked by Predicted Star Factor in 5 Years"))
  })
  
  filtered_artist_3_b <- reactive({
    creator_and_songs %>%
      filter(song_genre %in% debounced_genres_3_b(),
             creator_node_type %in% c("Person", "MusicalGroup")) %>%
      pull(creator_name) %>%
      unique() %>%
      sort()
  })
  
  observe({
    updateSelectizeInput(session, "artist_3_b_1", choices = filtered_artist_3_b(), selected = "Sailor Shift", server = TRUE)
    updateSelectizeInput(session, "artist_3_b_2", choices = filtered_artist_3_b(), selected = "Chao Wu", server = TRUE)
    updateSelectizeInput(session, "artist_3_b_3", choices = filtered_artist_3_b(), selected = "Xia Jia", server = TRUE)
  })
  
  # First, modify the initial summarization to keep the raw lists
  yearly_stats_with_lists <- reactive({
    creator_influence_lists %>%
      filter(song_genre == debounced_genres_3_b()) %>%
      group_by(creator_name, creator_release_date) %>%
      summarise(
        total_songs = n_distinct(song_to),
        notable_hits = sum(notable == TRUE, na.rm = TRUE),
        collaboration = length(unique(unlist(unique_collaborate))),
        influence_creators = length(unique(unlist(unique_influence_creators))),
        collaboration_influence_creator = length(unique(c(unlist(unique_influence_creators), unlist(unique_collaborate)))),
        influence_music = length(unique(unlist(unique_influence_music))),
        # Keep the raw lists for cumulative calculations
        unique_collaborate_list = list(unique(unlist(unique_collaborate))),
        unique_influence_creators_list = list(unique(unlist(unique_influence_creators))),
        unique_influence_music_list = list(unique(unlist(unique_influence_music))),
        .groups = "drop"
      ) %>%
      rename(year = creator_release_date)
  })
  
  # Create complete grid
  all_years <- seq(1992, 2040)
  
  artist_year_grid <- reactive({
    expand.grid(creator_name = filtered_artist_3_b(), year = all_years)
  })
  
  # Join and calculate cumulative unique values
  yearly_complete <- reactive({
    # Get the actual data frames from the reactives
    grid_data <- artist_year_grid()
    yearly_data <- yearly_stats_with_lists()
    
    grid_data %>%
      left_join(yearly_data,
                by = c("creator_name", "year")) %>%
      arrange(creator_name, year) %>%
      mutate(across(c(total_songs, notable_hits, collaboration, influence_creators,
                      collaboration_influence_creator, influence_music), ~replace_na(., 0))) %>%
      # Initialize list columns if NA
      mutate(
        unique_collaborate_list = map_if(unique_collaborate_list, is.null, ~ character(0)),
        unique_influence_creators_list = map_if(unique_influence_creators_list, is.null, ~ character(0)),
        unique_influence_music_list = map_if(unique_influence_music_list, is.null, ~ character(0))
      ) %>%
      group_by(creator_name) %>%
      mutate(
        cum_total_songs = cumsum(total_songs),
        cum_notable_hits = cumsum(notable_hits),
        # Calculate cumulative unique collaborators + influencers
        running_collab_influ = accumulate(
          map2(unique_collaborate_list, unique_influence_creators_list, ~ unique(c(.x, .y))),
          ~ unique(c(.x, .y)),
          .init = character(0)
        )[-1],
        cum_collab_influ_creator = map_int(running_collab_influ, length),
        # Calculate cumulative unique music influences
        running_influence_music = accumulate(
          unique_influence_music_list,
          ~ unique(c(.x, .y)),
          .init = character(0)
        )[-1],
        cum_influence_music = map_int(running_influence_music, length)
      ) %>%
      select(-running_collab_influ, -running_influence_music,
             -unique_collaborate_list, -unique_influence_creators_list, -unique_influence_music_list) %>%
      ungroup()
  })
  
  # Compute per-year min, max, and range for all cumulative metrics
  yearly_ranges <- reactive({
    yearly_complete() %>%
      group_by(year) %>%
      summarise(
        min_songs = min(cum_total_songs, na.rm = TRUE),
        max_songs = max(cum_total_songs, na.rm = TRUE),
        range_songs = max_songs - min_songs,
        
        min_notable = min(cum_notable_hits, na.rm = TRUE),
        max_notable = max(cum_notable_hits, na.rm = TRUE),
        range_notable = max_notable - min_notable,
        
        min_artists = min(cum_collab_influ_creator, na.rm = TRUE),
        max_artists = max(cum_collab_influ_creator, na.rm = TRUE),
        range_artists = max_artists - min_artists,
        
        min_music = min(cum_influence_music, na.rm = TRUE),
        max_music = max(cum_influence_music, na.rm = TRUE),
        range_music = max_music - min_music,
        
        .groups = "drop"
      )
  })
  
  # Join ranges back and compute normalised scores
  scored_yearly <- reactive({
    yearly_complete() %>%
      left_join(yearly_ranges(), by = "year") %>%
      mutate(
        songs_score = ifelse(range_songs > 0,
                             (cum_total_songs - min_songs) / range_songs, 0),
        notable_score = ifelse(range_notable > 0,
                               (cum_notable_hits - min_notable) / range_notable, 0),
        artists_score = ifelse(range_artists > 0,
                               (cum_collab_influ_creator - min_artists) / range_artists, 0),
        music_score = ifelse(range_music > 0,
                             (cum_influence_music - min_music) / range_music, 0),
        
        composite_score = songs_score + notable_score + artists_score + music_score
      ) %>%
      select(-starts_with("min_"), -starts_with("max_"), -starts_with("range_"))
  })
  
  trend_slopes <- reactive({
    scored_yearly() %>%
      filter(year >= debounced_years_3_b()[1], year <= debounced_years_3_b()[2]) %>%
      group_by(creator_name) %>%
      filter(!is.na(composite_score)) %>%
      nest() %>%
      mutate(
        model = map(data, ~lm(composite_score ~ year, data = .x)),
        tidied = map(model, tidy),
        rsq = map_dbl(model, ~summary(.x)$r.squared),
        slope = map_dbl(tidied, ~.x$estimate[.x$term == "year"])
      ) %>%
      select(creator_name, slope) %>%
      arrange(desc(slope)) %>%
      ungroup()
  })
  
  latest_scores <- reactive({
    scored_yearly() %>%
      filter(year == debounced_years_3_b()[2]) %>%
      select(creator_name, composite_score) %>%
      rename(current_score = composite_score)
  })
  
  score_trends <- reactive({
    trend_slopes() %>%
      left_join(latest_scores(), by = "creator_name") %>%
      mutate(projected_in_5yrs = current_score + 5 * slope) %>%
      arrange(desc(projected_in_5yrs))
  })
  
  output$predictedStars_3_b <- DT::renderDataTable({
    DT::datatable(
      score_trends() %>%
        mutate(
          `Yearly Growth` = round(slope, 3),
          `Current Star Factor` = round(current_score, 3),
          `Star Factor in 5 Years` = round(projected_in_5yrs, 3)
        ) %>%
        select(creator_name, `Current Star Factor`, `Yearly Growth`, `Star Factor in 5 Years`) %>%
        rename(
          `Artist` = creator_name
        ),
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 20),
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "compact stripe hover",
      escape = FALSE,
      extensions = 'Scroller'
    )
  })
  
  ############################## Plot ####################################
  
  output$predictedStars_3_b_plot <- renderPlotly({
    star_factor_1 <- scored_yearly() %>%
      filter(creator_name == input$artist_3_b_1,
             year >= debounced_years_3_b()[1],
             year <= debounced_years_3_b()[2]) %>%
      select(year, composite_score)
    
    star_factor_2 <- scored_yearly() %>%
      filter(creator_name == input$artist_3_b_2,
             year >= debounced_years_3_b()[1],
             year <= debounced_years_3_b()[2]) %>%
      select(year, composite_score)
    
    star_factor_3 <- scored_yearly() %>%
      filter(creator_name == input$artist_3_b_3,
             year >= debounced_years_3_b()[1],
             year <= debounced_years_3_b()[2]) %>%
      select(year, composite_score)
    
    creator_1_trend <- score_trends() %>%
      filter(creator_name == input$artist_3_b_1) %>%
      select(slope, current_score)
    
    creator_1_projection <- tibble(
      year = debounced_years_3_b()[2]:(debounced_years_3_b()[2] + 5),
      composite_score = creator_1_trend$current_score + 
        creator_1_trend$slope * (debounced_years_3_b()[2]:(debounced_years_3_b()[2] + 5) - debounced_years_3_b()[2])
    )
    
    creator_2_trend <- score_trends() %>%
      filter(creator_name == input$artist_3_b_2) %>%
      select(slope, current_score)
    
    creator_2_projection <- tibble(
      year = debounced_years_3_b()[2]:(debounced_years_3_b()[2] + 5),
      composite_score = creator_2_trend$current_score + 
        creator_2_trend$slope * (debounced_years_3_b()[2]:(debounced_years_3_b()[2] + 5) - debounced_years_3_b()[2])
    )
    
    creator_3_trend <- score_trends() %>%
      filter(creator_name == input$artist_3_b_3) %>%
      select(slope, current_score)
    
    creator_3_projection <- tibble(
      year = debounced_years_3_b()[2]:(debounced_years_3_b()[2] + 5),
      composite_score = creator_3_trend$current_score + 
        creator_3_trend$slope * (debounced_years_3_b()[2]:(debounced_years_3_b()[2] + 5) - debounced_years_3_b()[2])
    )
    
    # Visualisation
    
    plot_ly(
      data = star_factor_1,
      x = ~year,
      y = ~composite_score,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "red", size = 6),
      hoverinfo = "text",
      hovertext = ~paste0(
        "Artist: ", chosen_creator_1,
        "<br>Year: ", year,
        "<br>Star Factor: ", round(composite_score, 3)
      )
    ) %>%
      config(displayModeBar = FALSE) %>%
      add_trace(
        data = star_factor_2,
        x = ~year,
        y = ~composite_score,
        name = chosen_creator_2,
        line = list(color = "green", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_2,
          "<br>Year: ", year,
          "<br>Star Factor: ", round(composite_score, 3)
        )
      ) %>%
      add_trace(
        data = star_factor_3,
        x = ~year,
        y = ~composite_score,
        name = chosen_creator_3,
        line = list(color = "purple", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Year: ", year,
          "<br>Star Factor: ", round(composite_score, 3)
        )
      ) %>%
      add_trace(
        data = creator_1_projection,
        x = ~year,
        y = ~composite_score,
        name = chosen_creator_1,
        line = list(color = "#2E3192", width = 2),
        marker = list(opacity = 0),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_1,
          "<br>Year: ", year,
          "<br>Star Factor: ", round(composite_score, 3)
        ),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = creator_2_projection,
        x = ~year,
        y = ~composite_score,
        name = chosen_creator_2,
        line = list(color = "green", width = 2),
        marker = list(opacity = 0),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_2,
          "<br>Year: ", year,
          "<br>Star Factor: ", round(composite_score, 3)
        ),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = creator_3_projection,
        x = ~year,
        y = ~composite_score,
        name = chosen_creator_3,
        line = list(color = "purple", width = 2),
        marker = list(opacity = 0),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Year: ", year,
          "<br>Star Factor: ", round(composite_score, 3)
        ),
        showlegend = FALSE
      )%>%
      layout(
        title = "Star Factor Prediction for the Next 5 Years",
        margin = list(b = 80, t = 80),      
        xaxis = list(
          title = NA,  
          dtick = 5,
          automargin = TRUE
        ),
        yaxis = list(
          title = "Star Factor",
          automargin = TRUE
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.1
        ),
        shapes = list(
          list(
            type = "line",
            x0 = debounced_years_3_b()[2], x1 = debounced_years_3_b()[2],
            y0 = 0, y1 = max(star_factor_1$composite_score, star_factor_2$composite_score, star_factor_3$composite_score),
            line = list(dash="dash", color="grey")
          )
        ),
        annotations = list(
          list(
            x = debounced_years_3_b()[2], 
            y = max(star_factor_1$composite_score, star_factor_2$composite_score, star_factor_3$composite_score),
            text = "Projection",
            xref = "x", yref = "y",
            xanchor = "left",
            showarrow = TRUE, arrowhead = 2,
            ax = 10, ay = -10,
            font = list(color="black", size=12)
          )
        )
      )
  })
  
  ############################## Graphs ##################################
  
  genre_creator_and_songs_and_influences_and_creators_collaborate <- reactive({
    creator_and_songs_and_influences_and_creators_collaborate %>%
      filter(song_genre == debounced_genres_3_b())
  })
  
  output$predictedStars_3b_1 <- renderPlotly({
    # Data Preparation
    
    chosen_creator_1 <- input$artist_3_b_1
    
    # Step 1: Get the node of the chosen creator
    chosen_node_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(song_to)
    
    # Step 1: Count number of music by release date
    music_by_date_1 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_1)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    chosen_creator_2 <- input$artist_3_b_2
    
    # Step 1: Get the node of the chosen creator
    chosen_node_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(song_to)
    
    # Step 1: Count number of music by release date
    music_by_date_2 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_2)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    chosen_creator_3 <- input$artist_3_b_3
    
    # Step 1: Get the node of the chosen creator
    chosen_node_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(song_to)
    
    # Step 1: Count number of music by release date
    music_by_date_3 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_3)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    # Visualisation
    
    plot_ly(
      data = music_by_date_1,
      x = ~release_date,
      y = ~cumulative_count,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "#2E3192", size = 6),
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
        marker = list(color = "green", size = 6),
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
        marker = list(color = "purple", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      layout(
        title = NULL,
        margin = list(b = 80, t = 80),      
        xaxis = list(
          title = NA,  
          dtick = 5,
          automargin = TRUE,
          range = c(debounced_years_3_b()[1] - 1, debounced_years_3_b()[2] + 1)
        ),
        yaxis = list(
          title = "Yearly Count",
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
  
  output$predictedStars_3b_2 <- renderPlotly({
    # Data Preparation
    
    chosen_creator_1 <- input$artist_3_b_1
    
    # Step 1: Get the node of the chosen creator
    chosen_node_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(song_to)
    
    # Step 1: Count number of notable music by release date
    notable_music_by_date_1 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_1), notable == TRUE) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    chosen_creator_2 <- input$artist_3_b_2
    
    # Step 1: Get the node of the chosen creator
    chosen_node_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(song_to)
    
    # Step 1: Count number of notable music by release date
    notable_music_by_date_2 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_2), notable == TRUE) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    chosen_creator_3 <- input$artist_3_b_3
    
    # Step 1: Get the node of the chosen creator
    chosen_node_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(song_to)
    
    # Step 1: Count number of notable music by release date
    notable_music_by_date_3 <- mc1_nodes_clean %>%
      filter(name %in% unique(creator_songs_3), notable == TRUE) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    # Visualisation
    
    plot_ly(
      data = notable_music_by_date_1,
      x = ~release_date,
      y = ~cumulative_count,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "#2E3192", size = 6),
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
        marker = list(color = "green", size = 6),
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
        marker = list(color = "purple", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      layout(
        title = NULL,
        margin = list(b = 80, t = 80),      
        xaxis = list(
          title = NA,  
          dtick = 5,
          automargin = TRUE,
          range = c(debounced_years_3_b()[1] - 1, debounced_years_3_b()[2] + 1)
        ),
        yaxis = list(
          title = "Yearly Count",
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
  
  output$predictedStars_3b_3 <- renderPlotly({
    # Data Preparation
    
    chosen_creator_1 <- input$artist_3_b_1
    
    # Step 1: Get the node of the chosen creator
    chosen_node_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 1: Count number of influenced artists by release date
    influence_artists_by_date_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
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
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(creator_release_date >= debounced_years_3_b()[1],
             creator_release_date <= debounced_years_3_b()[2])
    
    chosen_creator_2 <- input$artist_3_b_2
    
    # Step 1: Get the node of the chosen creator
    chosen_node_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 1: Count number of influenced artists by release date
    influence_artists_by_date_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
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
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(creator_release_date >= debounced_years_3_b()[1],
             creator_release_date <= debounced_years_3_b()[2])
    
    chosen_creator_3 <- input$artist_3_b_3
    
    # Step 1: Get the node of the chosen creator
    chosen_node_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 1: Count number of influenced artists by release date
    influence_artists_by_date_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
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
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(creator_release_date >= debounced_years_3_b()[1],
             creator_release_date <= debounced_years_3_b()[2])
    
    # Visualisation
    
    plot_ly(
      data = influence_artists_by_date_1,
      x = ~creator_release_date,
      y = ~cumulative_count,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "#2E3192", size = 6),
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
        marker = list(color = "green", size = 6),
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
        marker = list(color = "purple", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Influence Date: ", creator_release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      layout(
        title = NULL,
        margin = list(b = 80, t = 80),      
        xaxis = list(
          title = NA,  
          dtick = 5,
          automargin = TRUE,
          range = c(debounced_years_3_b()[1] - 1, debounced_years_3_b()[2] + 1)
        ),
        yaxis = list(
          title = "Yearly Count",
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
  
  output$predictedStars_3b_4 <- renderPlotly({
    # Data Preparation
    chosen_creator_1 <- input$artist_3_b_1
    
    # Step 1: Get the node of the chosen creator
    chosen_node_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_1) %>%
      pull(song_to)
    
    # Step 4: Get the songs they have influenced
    creators_songs_influence_1 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_1,
             infuence_music_collaborate != chosen_node_1,
             `Edge Colour` == "Influenced By") %>%
      pull(infuence_music_collaborate)
    
    # Step 1: Count number of influenced music by release date
    influence_song_by_date_1 <- mc1_nodes_clean %>%
      filter(name %in% unique(creators_songs_influence_1)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    chosen_creator_2 <- input$artist_3_b_2
    
    # Step 1: Get the node of the chosen creator
    chosen_node_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_2) %>%
      pull(song_to)
    
    # Step 4: Get the songs they have influenced
    creators_songs_influence_2 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_2,
             `Edge Colour` == "Influenced By") %>%
      pull(infuence_music_collaborate)
    
    # Step 1: Count number of influenced music by release date
    influence_song_by_date_2 <- mc1_nodes_clean %>%
      filter(name %in% unique(creators_songs_influence_2)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    chosen_creator_3 <- input$artist_3_b_3
    
    # Step 1: Get the node of the chosen creator
    chosen_node_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(creator_from) %>%
      unique()
    
    # Step 2: Get the songs that the top creator produced
    creator_songs_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_3) %>%
      pull(song_to)
    
    # Step 4: Get the songs they have influenced
    creators_songs_influence_3 <- genre_creator_and_songs_and_influences_and_creators_collaborate() %>%
      filter(creator_name == chosen_creator_3,
             infuence_music_collaborate != chosen_node_3,
             `Edge Colour` == "Influenced By") %>%
      pull(infuence_music_collaborate)
    
    # Step 1: Count number of influenced music by release date
    influence_song_by_date_3 <- mc1_nodes_clean %>%
      filter(name %in% unique(creators_songs_influence_3)) %>%
      count(release_date, name = "music_count") %>%
      arrange(release_date) %>%  # Ensure dates are in chronological order
      mutate(cumulative_count = cumsum(music_count)) %>%
      filter(release_date >= debounced_years_3_b()[1],
             release_date <= debounced_years_3_b()[2])
    
    # Visualisation
    
    plot_ly(
      data = influence_song_by_date_1,
      x = ~release_date,
      y = ~cumulative_count,
      type = "scatter",
      mode = "lines+markers",
      name = chosen_creator_1,
      line = list(color = "#2E3192", width = 2),
      marker = list(color = "#2E3192", size = 6),
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
        marker = list(color = "green", size = 6),
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
        marker = list(color = "purple", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0(
          "Artist: ", chosen_creator_3,
          "<br>Influence Date: ", release_date,
          "<br>Cumulative Count: ", cumulative_count
        )
      ) %>%
      layout(
        title = NULL,
        margin = list(b = 80, t = 80),      
        xaxis = list(
          title = NA,  
          dtick = 5,
          automargin = TRUE,
          range = c(debounced_years_3_b()[1] - 1, debounced_years_3_b()[2] + 1)
        ),
        yaxis = list(
          title = "Yearly Count",
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