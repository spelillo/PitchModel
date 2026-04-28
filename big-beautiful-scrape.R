# --- THE NEW RESILIENT LOOP ---
for (i in 1:(length(date_chunks) - 1)) {
  
  s_date <- date_chunks[i]
  e_date <- date_chunks[i+1] - 1
  
  timestamp <- format(Sys.time(), "%H:%M:%S")
  message(sprintf("[%s] Processing: %s to %s", timestamp, s_date, e_date))
  
  # Construct the URL manually to bypass baseballr's internal timeout
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher&game_date_gt=", 
                s_date, "&game_date_lt=", e_date)
  
  # Step A: Download & Read directly
  daily_data <- tryCatch({
    read_csv(url, show_col_types = FALSE)
  }, error = function(e) {
    message("   !!! Download failed. Savant might be busy. Retrying..."); return(NULL)
  })
  
  if (!is.null(daily_data) && nrow(daily_data) > 0) {
    
    message(sprintf("   -> Successfully read %s rows.", nrow(daily_data)))
    
    # Step B: Clean for BigQuery
    clean_data <- daily_data %>%
      rename_with(~str_replace_all(., "\\.", "_")) %>%
      mutate(across(everything(), as.character))
    
    # Step C: BigQuery Upload
    tryCatch({
      bq_table_upload(
        x = bq_table(project_id, dataset_id, table_id),
        values = clean_data,
        write_disposition = "WRITE_APPEND",
        create_disposition = "CREATE_IF_NEEDED"
      )
      message("   -> SUCCESS: Uploaded to BigQuery.")
    }, error = function(e) {
      message("   !!! UPLOAD ERROR: ", e$message)
    })
    
  } else {
    message("   -> No data found for these dates.")
  }
  
  # Increase sleep to 5 seconds to stay under the radar
  Sys.sleep(5)
}
