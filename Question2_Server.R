Question2_Server <- function(input, output, session) {
  
######################################### 2a ###################################
  
  # Trajectory over Time
  
  output$plot_release <- renderPlotly({
    oceanus_nodes_by_date %>%
      filter(release_date >= input$year_range_2a[1],
             release_date <= input$year_range_2a[2])%>%
      plot_ly(
        x = ~release_date,
        y = ~oceanus_nodes_count,
        type = "bar",
        name = "Number of Music Releases",
        marker = list(color = "#2E3192"),
        hoverinfo = "text",
        hovertext = ~paste0("Released Date: ", release_date,
                            "<br>Count: ", oceanus_nodes_count)
      ) %>%
      add_trace(
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = "Cumulative Count",
        line = list(color = "black", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0("Released Date: ", release_date,
                            "<br>Cumulative Count: ", cumulative_count)
      ) %>%
      layout(title = "Oceanus Folk Releases", showlegend = FALSE)
  })
  
  output$plot_influence <- renderPlotly({
    influence_yearly %>%
      filter(influence_release_date >= input$year_range_2a[1],
             influence_release_date <= input$year_range_2a[2]) %>%
      plot_ly(
        x = ~influence_release_date,
        y = ~num_influenced_nodes,
        type = "bar",
        name = "Number of Influenced Songs/Albums",
        marker = list(color = "#2E3192"),
        hoverinfo = "text",
        hovertext = ~paste0("Year: ", influence_release_date,
                            "<br>Influenced: ", num_influenced_nodes)
      ) %>%
      add_trace(
        y = ~cumulative_influenced,
        type = "scatter",
        mode = "lines+markers",
        name = "Cumulative Influenced",
        line = list(color = "black", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0("Year: ", influence_release_date,
                            "<br>Cumulative: ", cumulative_influenced)
      ) %>%
      layout(title = "Oceanus Folk Influence on Songs", showlegend = FALSE)
  })
  
  output$plot_creators <- renderPlotly({
    creators_by_date %>%
      filter(influence_release_date >= input$year_range_2a[1],
             influence_release_date <= input$year_range_2a[2]) %>%
      plot_ly(
        x = ~influence_release_date,
        y = ~people_count,
        type = "bar",
        name = "Number of Artists",
        marker = list(color = "#2E3192"),
        hoverinfo = "text",
        hovertext = ~paste0("Released Date: ", influence_release_date,
                            "<br>Count: ", people_count)
      ) %>%
      add_trace(
        y = ~cumulative_count,
        type = "scatter",
        mode = "lines+markers",
        name = "Cumulative Count",
        line = list(color = "black", width = 2),
        marker = list(color = "red", size = 6),
        hoverinfo = "text",
        hovertext = ~paste0("Released Date: ", influence_release_date,
                            "<br>Cumulative Count: ", cumulative_count)
      ) %>%
      layout(title = "Oceanus Folk Artist Influence", showlegend = FALSE)
  })

  ###########
  
  
  output$plot_surprise <- renderPlotly({
      # Standardise category labels
      plot_data <- plot_data %>%
        mutate(type = case_when(
          type == "Music Releases"    ~ "Music Releases",
          type == "Influenced Works"  ~ "Influenced Songs/Albums",
          type == "Artists"           ~ "New Influenced Artists",
          TRUE                        ~ type
        ))
      
      # Filter by year range from slider input
      filtered_data <- plot_data %>%
        filter(year >= input$year_range_2a[1],
               year <= input$year_range_2a[2])
      
      # Plot
      plot_ly(
        data = filtered_data, 
        x = ~year, 
        y = ~surprise, 
        color = ~type,
        colors = c(
          "Music Releases"          = "#ADD8E6", 
          "Influenced Songs/Albums" = "#F08080", 
          "New Influenced Artists"  = "#C2E0C6"
        ),
        type = 'scatter', 
        mode = 'lines+markers',
        hoverinfo = "text",
        hovertext = ~paste0(
          "Year: ", year,
          "<br>Category: ", type,
          "<br>Surprise: ", round(surprise, 2)
        )
      ) %>%
        layout(
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = 1.1,
            yanchor = "bottom",
            font = list(size = 12)
          ),
          title = list(
            text = "Bayesian Surprise Across Three Categories",
            x = 0.5,
            xanchor = "center"
          ),
          xaxis = list(title = "Year", dtick = 5),
          yaxis = list(title = "Surprise Score (KL Divergence)", range = c(0, 100)),
          margin = list(t = 160, b = 80),
          
          # Optional: annotations and lines can also be conditionally shown based on year range
          annotations = list(
            list(
              x = 2024, 
              y = 85,
              text = "<b>2024: Sailor Shift's Debut</b>",
              xref = "x", yref = "y",
              xanchor = "right",
              showarrow = TRUE,
              arrowhead = 2,
              ax = -30, ay = -40,
              font = list(color = "#2E3192", size = 12)
            ),
            list(
              x = 2028, 
              y = 90,
              text = "<b>2028: Sailor Shift's Breakthrough</b>",
              xref = "x", yref = "y",
              xanchor = "right",
              showarrow = TRUE,
              arrowhead = 2,
              ax = -30, ay = -40,
              font = list(color = "#2E3192", size = 12)
            )
          ),
          
          shapes = list(
            list(
              type = "line",
              x0 = 2024, x1 = 2024,
              y0 = 0, y1 = 100,
              line = list(dash = "dash", color = "grey")
            ),
            list(
              type = "line",
              x0 = 2028, x1 = 2028,
              y0 = 0, y1 = 120,
              line = list(dash = "dash", color = "grey")
            )
          )
        )
    })
  
  ###Cumulative plot for 2a
  output$plot_cumulative <- renderPlotly({
    
    # Prepare each data series
    df1 <- oceanus_nodes_by_date %>%
      select(year = release_date, value = cumulative_count) %>%
      mutate(series = "Music Releases")
    
    df2 <- influence_yearly %>%
      select(year = influence_release_date, value = cumulative_influenced) %>%
      mutate(series = "Influenced Songs/Albums")
    
    df3 <- creators_by_date %>%
      select(year = influence_release_date, value = cumulative_count) %>%
      mutate(series = "New Influenced Artists")
    
    # Combine all into one tidy frame
    combined_df <- bind_rows(df1, df2, df3)
    
    # Filter by year range from slider
    filtered_df <- combined_df %>%
      filter(year >= input$year_range_2a[1],
             year <= input$year_range_2a[2])
    
    # Build cumulative Plotly chart
    plot_ly(filtered_df,
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
        title = list(text = "Oceanus Folk Influence Over Time (Cumulative)", x = 0.5),
        xaxis = list(title = list(text = ""), dtick = 5),
        yaxis = list(title = "Cumulative Count"),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.15,
          yanchor = "bottom"
        ),
        margin = list(t = 160, b = 80),
        
        # Add annotations (you can also conditionally show these based on year range if needed)
        annotations = list(
          list(
            x = 2024, 
            y = 700,
            text = "<b>2024: Sailor Shift's Debut</b>",
            xref = "x", yref = "y",
            xanchor = "right",
            showarrow = TRUE,
            arrowhead = 2,
            ax = -30, ay = -40,
            font = list(color = "#2E3192", size = 12)
          ),
          list(
            x = 2028, 
            y = 900,
            text = "<b>2028: Sailor Shift's Breakthrough</b>",
            xref = "x", yref = "y",
            xanchor = "right",
            showarrow = TRUE,
            arrowhead = 2,
            ax = -30, ay = -40,
            font = list(color = "#2E3192", size = 12)
          )
        ),
        
        shapes = list(
          list(
            type = "line",
            x0 = 2024, x1 = 2024,
            y0 = 0, y1 = 1000,
            line = list(dash = "dash", color = "grey")
          ),
          list(
            type = "line",
            x0 = 2028, x1 = 2028,
            y0 = 0, y1 = 1000,
            line = list(dash = "dash", color = "grey")
          )
        )
      )
  })
  
  
  
  ######## Faceted Bar Graph 2a
  output$plot_facet_counts <- renderPlotly({
    
    # Prepare tidy data
    df1_counts <- oceanus_nodes_by_date %>%
      select(year = release_date, value = oceanus_nodes_count) %>%
      mutate(series = "Music Releases")
    
    df2_counts <- influence_yearly %>%
      select(year = influence_release_date, value = num_influenced_nodes) %>%
      mutate(series = "Influenced Songs/Albums")
    
    df3_counts <- creators_by_date %>%
      select(year = influence_release_date, value = people_count) %>%
      mutate(series = "New Influenced Artists")
    
    combined_counts_df <- bind_rows(df1_counts, df2_counts, df3_counts)
    
    # Filter based on year range slider
    filtered_counts_df <- combined_counts_df %>%
      filter(year >= input$year_range_2a[1],
             year <= input$year_range_2a[2])
    
    # Create the faceted plot
    p <- ggplot(filtered_counts_df, aes(x = year, y = value, fill = series)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~series, nrow = 1, scales = "fixed") +
      scale_fill_manual(values = c("Music Releases" = "#ADD8E6", 
                                   "Influenced Songs/Albums" = "#F08080", 
                                   "New Influenced Artists" = "#C2E0C6")) +
      labs(title = "Yearly Numbers by Category",
           x = NULL, y = "Yearly Count") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.spacing = unit(0.5, "lines"), 
        plot.title = element_text(hjust = 0.5)
      )
    
    # Return interactive plot
    ggplotly(p)
  })
  
  
  
  
  output$insight_2afinal <- renderUI({
    HTML(paste0(
          "<h4><b>Insights: Bayesian Surprise and Influence Trends</b></h4>",
          "<p><b>Bayesian Surprise</b> is a concept in information theory, which is used to identify moments of unexpected change in sequential data. ",
          "Based on our <a href='https://arxiv.org/html/2410.15996v1' target='_blank'>research</a>, it is suitable for analysing trends like the spread of musical influence and temporal pattern detection.</p>",
          
          "<p>The Bayesian Surprise analysis showed that Oceanus Folk’s influence was <b>intermittent rather than gradual</b>. ",
          "While cumulative trends in music releases, influenced works, and artist numbers may suggest steady growth, ",
          "Bayesian Surprise uncovers a different insight – there were intermittent surges of activity.</p>",
          
          "<p>For example, the sharp peaks in surprise scores for <b>new influenced artists (green line)</b> in years like 2004, 2010, 2013, 2017, and especially 2023 then 2030 ",
          "suggest that Oceanus Folk influence had waves of breakthroughs, not slow accumulation.</p>",
          
          "<p><b>Influenced Songs/Albums (red line)</b> increased in 2017, spiked in 2023, and some sustained growth from 2030–2031, ",
          "highlighting how Sailor’s debut and breakthrough catalysed the genre’s influence.</p>",
          
          "<p><b>Music releases (blue line)</b> generally showed lower surprise scores, confirming that the number of releases was not the main driver of influence.</p>",
          
          "<p><b>Conclusion:</b> While the overall trend shows long-term growth, Bayesian Surprise confirms that Oceanus Folk’s rise was intermittent. These bursts sustained influence momentum over time.</p>"
        ))
      })

  ######################################### 2b ###################################
  

  output$genreSankey <- renderSankeyNetwork({
    # Step 1: Start from full data
    filtered_stats <- genre_influence_stats
    
    # Step 2: Filter by selected genre (unless "All")
    if (!is.null(input$selected_genre) && input$selected_genre != "All") {
      filtered_stats <- filtered_stats %>%
        filter(song_genre == input$selected_genre)
    }
    
    # Step 3: Summarize and structure links
    filtered_stats <- filtered_stats %>%
      group_by(song_genre) %>%
      summarize(oceanus_influences = sum(oceanus_influences, na.rm = TRUE)) %>%
      mutate(
        source = "Oceanus Folk",
        raw_target = ifelse(song_genre == "Oceanus Folk", "Oceanus Folk (in-genre influence)", song_genre),
        target = paste0(raw_target, " (", oceanus_influences, ")"),
        value = oceanus_influences
      ) %>%
      select(source, target, value) %>%
      arrange(desc(value)) %>%
      head(22)
    
    # Step 4: Create nodes and links
    nodes <- data.frame(name = unique(c(filtered_stats$source, filtered_stats$target)))
    
    links <- filtered_stats %>%
      mutate(
        source = match(source, nodes$name) - 1,
        target = match(target, nodes$name) - 1
      )
    
    # Step 5: Create Sankey
    sankey <- sankeyNetwork(
      Links = as.data.frame(links),
      Nodes = as.data.frame(nodes),
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 13,
      nodeWidth = 30,
      sinksRight = TRUE
    )
    
    sankey
  })
  
  output$genreTable <- renderUI({
    selected <- input$selected_genre
    
    # Show all or filter
    table_data <- genre_influence_stats
    if (!is.null(selected) && selected != "All") {
      table_data <- table_data %>% filter(song_genre == selected)
    }
    
    table_data %>%
      select(song_genre, total_music, oceanus_influences, total_influences, perc_oceanus) %>%
      arrange(desc(oceanus_influences)) %>%
      kable(caption = ifelse(
        is.null(selected) || selected == "All",
        "All Genres Ranked by Oceanus Influence",
        paste("Details for Genre:", selected)
      )) %>%
      kable_styling("striped", full_width = FALSE) %>%
      scroll_box(height = "300px") %>%
      HTML()
  })
  
  ###############2c##############################
  
  # Outward Influence on other Artists
  
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
  
  output$artistSankey <- renderSankeyNetwork({
    # Step 1: Start from full data
    filtered_stats <- creator_influenced_by_stats
    
    # Step 2: Filter by selected artist (unless "All")
    if (!is.null(input$selected_artist) && input$selected_artist != "All") {
      filtered_stats <- filtered_stats %>%
        filter(creator_name == input$selected_artist)
    }
    
    # Step 3: Structure links
    filtered_stats <- filtered_stats %>%
      mutate(
        source = "Oceanus Folk",
        raw_target = creator_name,
        target = paste0(raw_target, " (", total_oceanus_influence, ")"),
        value = total_oceanus_influence
      ) %>%
      select(source, target, value) %>%
      arrange(desc(value)) %>%
      head(22)
    
    # Step 4: Create nodes and links
    nodes <- data.frame(name = unique(c(filtered_stats$source, filtered_stats$target)))
    
    links <- filtered_stats %>%
      mutate(
        source = match(source, nodes$name) - 1,
        target = match(target, nodes$name) - 1
      )
    
    # Step 5: Create Sankey
    sankey <- sankeyNetwork(
      Links = as.data.frame(links),
      Nodes = as.data.frame(nodes),
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 13,
      nodeWidth = 30,
      sinksRight = TRUE
    )
    
    sankey
  })
  
  output$artistInfluenceTable <- renderUI({
    selected <- input$selected_artist
    
    # Filter table data
    table_data <- creator_influenced_by_stats
    if (!is.null(selected) && selected != "All") {
      table_data <- table_data %>% filter(creator_name == selected)
    }
    
    table_data %>%
      rename(
        `Artist` = creator_name,
        `Total Music` = total_music,
        `Notable Hits` = notable_hits,
        `No. of Oceanus Folk Music` = oceanus_music,
        `Oceanus Folk Influence` = oceanus_influenced_by,
        `Oceanus Folk Music & Influence` = total_oceanus_influence
      ) %>%
      kable(caption = ifelse(
        is.null(selected) || selected == "All",
        "Top Artists Influenced by Oceanus Folk",
        paste("Details for Artist:", selected)
      )) %>%
      kable_styling("striped", full_width = FALSE) %>%
      scroll_box(height = "300px") %>%
      HTML()
  })
  
  
  
  
  
  
  
###################### 2d 
  
  output$influencerSankey <- renderSankeyNetwork({
    
    # Step 1: Filter data if specific genre is selected
    filtered_stats <- genre_influenced_by_stats
    if (!is.null(input$selected_inward_influence_genre) && input$selected_inward_influence_genre != "All") {
      filtered_stats <- filtered_stats %>%
        filter(influenced_by_genre == input$selected_inward_influence_genre)
    }
    
    # Step 2: Prepare Sankey structure
    sankey_df <- filtered_stats %>%
      mutate(
        raw_source = influenced_by_genre,
        target = "Oceanus Folk",
        value = influenced_by,
        source = paste0(raw_source, " (", value, ")")
      ) %>%
      select(source, target, value) %>%
      arrange(desc(value)) %>%
      head(22)  # Optional: top 22 influencers
    
    # Step 3: Define node list
    nodes <- data.frame(name = unique(c(sankey_df$source, sankey_df$target)))
    
    # Step 4: Create links with index mapping
    links <- sankey_df %>%
      mutate(
        source = match(source, nodes$name) - 1,
        target = match(target, nodes$name) - 1
      )
    
    # Step 5: Tooltip group
    links$group <- paste0(sankey_df$source, " → ", sankey_df$target, ": ", sankey_df$value)
    
    # Step 6: Render Sankey object
    sankey_plot <- sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 13,
      nodeWidth = 30,
      sinksRight = FALSE  # Flip if you want left-to-right instead
    )
    
    # Step 7: Attach tooltip behavior
    onRender(sankey_plot, '
    function(el, x) {
      d3.select(el)
        .selectAll(".link")
        .append("title")
        .text(function(d) { return d.group; });
    }
  ')
  })
  
  output$influencerGenreTable <- renderUI({
    selected <- input$selected_inward_influence_genre
    
    # Filter table if a specific genre is selected
    table_data <- genre_influenced_by_stats
    if (!is.null(selected) && selected != "All") {
      table_data <- table_data %>% filter(influenced_by_genre == selected)
    }
    
    # Rename and render
    table_data %>%
      rename(
        `Genre` = influenced_by_genre,
        `Total Influenced Music` = total_music,
        `Influenced By Oceanus Folk` = influenced_by
      ) %>%
      kable(caption = ifelse(
        is.null(selected) || selected == "All",
        "All Genres That Influenced Oceanus Folk",
        paste("Details for Genre:", selected)
      )) %>%
      kable_styling("striped", full_width = FALSE) %>%
      scroll_box(height = "200px") %>%
      HTML()
  })
  
##################################Tab 5##################3  
  
  output$entropyPlot <- renderPlotly({
    req(input$entropy_max_year)
    
    # Filter data cumulatively from 1990 up to selected max year
    filtered_entropy <- entropy_yearly %>%
      filter(year >= 1990, year <= input$entropy_max_year)
    
    # Step 2: Find max for annotation placement
    max_entropy_val <- max(abs(filtered_entropy$entropy), na.rm = TRUE)
    
    # Step 3: Build the mirrored bar plot
    entropy_plot <- ggplot(filtered_entropy, aes(
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
      theme(legend.position = "bottom") +
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
    
    # Step 4: Return interactive plot
    ggplotly(entropy_plot, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
  })

  
  output$entropy_description <- renderUI({
    HTML(
      "<h4>Insights</h4>
    <p><b>Before 2013: Limited Influence during earlier years</b><br>
    During the early years of Oceanus Folk, both incoming and outgoing entropy remained sparse, indicating limited cross-genre interaction. 
    However, there are more bars for incoming entropy, showing that Oceanus Folk was more often influenced by other genres than influencing others. 
    This suggests that during the earlier years, Oceanus Folk was still developing its identity by absorbing influence from a diverse set of genres.</p>
    
    <p><b>2013 to 2024: Influence Growth</b><br>
    Between 2013 and 2024, we observe a gradual and sustained rise in both incoming and outgoing entropy. 
    This period can be seen as the maturation phase of Oceanus Folk, where it begins to both learn from and contribute to the wider music landscape.</p>
    
    <p><b>2024 onwards: Sailor Shift's Rise</b><br>
    Incoming entropy began to decline, suggesting that Oceanus Folk was no longer absorbing influence from other genres as it solidified its musical identity.<br>
    In contrast, outgoing entropy peaked and remained elevated, reflecting that other genres increasingly drew inspiration from Oceanus Folk. 
    This sustained high outgoing influence signals that Oceanus Folk had become a genre of reference across many genres.</p>"
    )
  })
  
  
} 