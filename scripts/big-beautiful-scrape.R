library(tidyverse)
library(bigrquery)
library(lubridate)

# --- 1. CONFIGURATION ---
project_id <- "pitchmodel-494200"
dataset_id <- "pitch_model_analytics"
table_id <- "all_pitches_2023_2026"

# --- 2. AUTHENTICATION ---
if (Sys.getenv("GCP_AUTH_JSON") != "") {
  tmp_auth <- tempfile(fileext = ".json")
  writeLines(Sys.getenv("GCP_AUTH_JSON"), tmp_auth)
  bq_auth(path = tmp_auth)
  message("SUCCESS: Service Account Authenticated.")
}

# --- 3. DEFINE DATE CHUNKS ---
# Sanitized definition
date_chunks <- seq(as.Date("2025-04-01"), as.Date("2026-04-28"), by = "4 days")

message(sprintf("--- SCRAPE START: %s CHUNKS ---", length(date_chunks)-1))

# --- 4. THE LOOP ---
for (i in 1:(length(date_chunks) - 1)) {
  
  s_date <- date_chunks[i]
  e_date <- date_chunks[i+1] - 1
  
  # Offseason Filter
  if (!(month(s_date) %in% 4:10)) {
    next
  }
  
  timestamp <- format(Sys.time(), "%H:%M:%S")
  message(sprintf("[%s] Processing: %s to %s", timestamp, s_date, e_date))
  
  # Manual URL Construction
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher&game_date_gt=", 
                as.character(s_date), "&game_date_lt=", as.character(e_date))
  
  # Step A: Download
  daily_data <- tryCatch({
    read_csv(url, show_col_types = FALSE)
  }, error = function(e) {
    message("   !!! Savant Timeout. Skipping chunk."); return(NULL)
  })
  
  if (!is.null(daily_data) && nrow(daily_data) > 0) {
    
    # Step B: Clean Headers
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
      message(sprintf("   -> SUCCESS: %s rows uploaded.", nrow(clean_data)))
    }, error = function(e) {
      message("   !!! BQ UPLOAD ERROR: ", e$message)
    })
    
  } else {
    message("   -> No data found.")
  }
  
  Sys.sleep(5)
}

message("--- SCRAPE FINISHED ---")
