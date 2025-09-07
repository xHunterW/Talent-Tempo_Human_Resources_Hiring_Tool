library(shiny) 

function(input, output, session) {
  
  observeEvent(input$score_btn, {
    
    req(input$job_desc)
    
    if (nchar(input$job_desc) < 30) {
      output$score_output <- renderPrint({
        "Please provide a longer job description (at least ~30 characters) for meaningful scoring."
      })
      output$missing_keywords <- renderDataTable(NULL)
      output$external_gap_table <- renderDataTable(NULL)
      return(NULL)
    }
    
    req(input$job_filter)
    
    withProgress(message = "Scoring your job description...", value = 0, {
      
      incProgress(0.2, detail = "Filtering matching job titles...")
      
      filtered_df <- filter_jobs_by_regex(offline_df, input$job_filter)
      
      if (nrow(filtered_df) < 50) {
        output$score_output <- renderPrint({"Not enough postings matched your filter to train a model."})
        return(NULL)
      }
      
      filtered_df <- filtered_df %>%
        mutate(created_month = floor_date(created, unit = "month"))
      
      monthly_averages <- filtered_df %>%
        group_by(source, created_month) %>%
        summarize(avg_time_fill = mean(time_to_fill_days, na.rm = TRUE), .groups = "drop") %>%
        filter(created_month >= as.Date("2023-01-01") & created_month <= as.Date("2024-08-01"))
      
      incProgress(0.4, detail = "Training Ridge model...")
      
      tfidf_results <- generate_tfidf_matrix(filtered_df$description_clean)
      model <- train_ridge_model(tfidf_results$matrix, filtered_df$time_to_fill_days)
      
      incProgress(0.6, detail = "Predicting Time-to-Fill...")
      
      predicted_time_fill <- max(predict_user_time_fill(
        model = model,
        vocab_terms = tfidf_results$vocab,
        user_text = clean_text(input$job_desc)
      ), 5)
      
      important_terms <- find_predictive_buzzwords(
        tfidf_vocab = tfidf_results$vocab,
        ridge_model = model,
        ud_model = ud_model,
        top_n = 50  # Grab extra first, then filter
      )
      
      important_terms <- important_terms %>%
        final_buzzword_filter() %>%
        apply_custom_boost(boost_buckets) %>%
        head(5)
      
      external_jobs <- filtered_df %>% filter(source == "external")
      
      external_vocab <- generate_tfidf_matrix(external_jobs$description_clean)$vocab
      user_tokens <- str_split(tolower(input$job_desc), "\\s+")[[1]]
      
      external_terms <- external_vocab$term
      missing_from_user <- setdiff(external_terms, user_tokens)
      
      external_gap_terms <- external_vocab %>%
        filter(term %in% missing_from_user) %>%
        arrange(desc(term_count)) %>%
        head(5)
      
      incProgress(1, detail = "Done!")
      
      output$predicted_score_box <- renderUI({
        tags$div(
          style = "background-color: #e6f2ff; padding: 20px; border-radius: 10px; text-align: center;",
          tags$h4("Predicted Time-to-Fill (Days)"),
          tags$h2(round(predicted_time_fill, 0))
        )
      })
      
      output$internal_avg_box <- renderUI({
        internal_avg <- filtered_df %>%
          filter(source == "internal") %>%
          summarize(avg = mean(time_to_fill_days, na.rm = TRUE)) %>%
          pull(avg)
        
        tags$div(
          style = "background-color: #e6ffe6; padding: 20px; border-radius: 10px; text-align: center;",
          tags$h4("Internal Avg Time-to-Fill (Days)"),
          tags$h2(round(internal_avg, 0))
        )
      })
      
      output$external_avg_box <- renderUI({
        external_avg <- filtered_df %>%
          filter(source == "external") %>%
          summarize(avg = mean(time_to_fill_days, na.rm = TRUE)) %>%
          pull(avg)
        
        tags$div(
          style = "background-color: #fff0e6; padding: 20px; border-radius: 10px; text-align: center;",
          tags$h4("External Avg Time-to-Fill (Days)"),
          tags$h2(round(external_avg, 0))
        )
      })
      
      output$missing_keywords <- renderDataTable({
        datatable(
          important_terms %>% select(term),
          options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
          rownames = FALSE,
          caption = 'Top Internal Buzzwords'
        )
      })
      
      output$external_gap_table <- renderDataTable({
        datatable(
          external_gap_terms %>% select(term),
          options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
          rownames = FALSE,
          caption = 'Top Missing External Buzzwords'
        )
      })
      
      output$job_posting_trend <- renderPlot({
        job_counts <- filtered_df %>%
          mutate(created_month = floor_date(created, unit = "month")) %>%
          group_by(created_month) %>%
          summarize(job_count = n(), .groups = "drop") %>%
          arrange(created_month) %>%
          mutate(sma_job_count = zoo::rollmean(job_count, k = 3, fill = NA, align = "right"))
        
        ggplot(job_counts, aes(x = created_month, y = sma_job_count)) +
          geom_line(color = "#4CAF50", size = 1.5) +
          geom_point(color = "#4CAF50", size = 2) +
          labs(title = "Total Job Postings Featuring Keyword (3-Month Moving Average)", x = "Created Month", y = "Smoothed Job Postings") +
          theme_minimal() +
          scale_x_date(limits = c(as.Date("2023-01-01"), as.Date("2024-08-01")), date_labels = "%b %Y", date_breaks = "3 months") +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
      })
      
      output$time_fill_trend <- renderPlot({
        monthly_averages %>%
          group_by(source) %>%
          arrange(created_month) %>%
          mutate(sma_avg_time_fill = zoo::rollmean(avg_time_fill, k = 3, fill = NA, align = "right")) %>%
          ggplot(aes(x = created_month, y = sma_avg_time_fill, color = source)) +
          geom_line(size = 1.5) +
          geom_point(size = 2) +
          labs(title = "Average Time-to-Fill (3-Month Moving Average)", x = "Created Month", y = "Smoothed Days", color = "Job Source") +
          theme_minimal() +
          scale_color_manual(values = c("internal" = "blue", "external" = "red")) +
          scale_x_date(limits = c(as.Date("2023-01-01"), as.Date("2024-08-01")), date_labels = "%b %Y", date_breaks = "3 months") +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
      })
      
    })
  })
}

    
    
    
