Question3_b_Server <- function(input, output, session) {
  ############################### Question 3 b ##################################
  debounced_genres <- debounce(reactive(input$filter_genres_3_b), millis = 500)
  debounced_years <- debounce(reactive(input$year_range_3_b), millis = 500)
  
  output$dynamic_title_3b <- renderUI({
    req(input$filter_genres_3_b)  # Ensure a genre is selected
    h5(paste0(input$filter_genres_3_b, " Artists Ranked by Predicted Star Factor in 5 Years"))
  })
  
  filtered_artist_3_b <- reactive({
    creator_and_songs %>%
      filter(song_genre %in% debounced_genres(),
             creator_node_type %in% c("Person", "MusicalGroup")) %>%
      pull(creator_name) %>%
      unique()
  })
  
  observe({
    updateSelectizeInput(session, "artist_3_b_1", choices = filtered_artist_3_b(), selected = "Sailor Shift", server = TRUE)
    updateSelectizeInput(session, "artist_3_b_2", choices = filtered_artist_3_b(), selected = "Lei Shen", server = TRUE)
    updateSelectizeInput(session, "artist_3_b_3", choices = filtered_artist_3_b(), selected = "Xia Cui", server = TRUE)
  })
  
  # First, modify the initial summarization to keep the raw lists
  yearly_stats_with_lists <- reactive({
    creator_influence_lists %>%
      filter(song_genre == debounced_genres()) %>%
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
      filter(year >= debounced_years()[1], year <= debounced_years()[2]) %>%
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
      arrange(desc(slope))
  })
  
  latest_scores <- reactive({
    scored_yearly() %>%
      filter(year == debounced_years()[2]) %>%
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
}