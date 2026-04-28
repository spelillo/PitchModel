library(tidyverse)
library(bigrquery)
library(lubridate)
library(httr)

# --- 1. CONFIGURATION ---
# These must match your GCP and GitHub Secrets exactly
project_id <- "pitchmodel-494200"
dataset_id <- "pitch_model_analytics"
table_id   <- "all_pitches_2023_2026"

# --- 2. AUTHENTICATION ---
# GitHub Actions provides the JSON via an environment variable
if (Sys.getenv("GCP_AUTH_JSON") != "") {
  # Write the secret to a temporary file for bigrquery to read
  tmp_auth <- tempfile(fileext = ".json")
  writeLines(Sys.getenv("GCP_AUTH_JSON"), tmp_auth)
  bq_auth(path = tmp_auth)
  message("SUCCESS: Authenticated with BigQuery using Service Account.")
} else {
  message("WARNING: GCP_AUTH_JSON not found. Script may fail if running in GitHub Actions.")
}

# --- 3. DEFINE DATE CHUNKS ---
# DEFINING THIS OBJECT HERE PREVENTS THE "object 'date_chunks' not found" ERROR
# We are focusing on the 2025-2026 MLB Regular Seasons
date_chunks <- seq(as.Date("2025-04-01"), as.Date("2026-04-28"), by = "4 days")

message(sprintf("--- STARTING MLB SCRAPE: %s CHUNKS PLANNED ---", length(date_chunks)-1))

# --- 4. THE RESILIENT LOOP ---
for (i in 1:(length(date_chunks) - 1)) {
  
  s_date <- date_chunks[i]
  e_date <- date_chunks[i+1] - 1
  
  # Skip winter months to save resources (Only April-October)
  if (!(month(s_date) %in% 4:10)) {
    message(sprintf("[%s] Skipping offseason: %s", format(Sys.time(), "%H:%M:%S"), s_date))
    next
  }
  
  timestamp <- format(Sys.time(), "%H:%M:%S")
  message(sprintf("[%s] Processing: %s to %s", timestamp, s_date, e_date))
  
  # Construct the URL manually 
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher&game_date_gt=", 
                s_date, "&game_date_lt=", e_date)
  
  # Step A: Download & Read with timeout protection
  daily_data <- tryCatch({
    # Using read_csv directly on the URL
    read_csv(url, show_col_types = FALSE)
  }, error = function(e) {
    message("   !!! Download failed. Savant might be busy. Retrying in next cycle..."); return(NULL)
  })
  
  if (!is.null(daily_data) && nrow(daily_data) > 0) {
    
    message(sprintf("   -> Successfully read %s rows.", nrow(daily_data)))
    
    # Step B: Clean for BigQuery
    # Replace dots in column names (e.g., release.speed -> release_speed)
    # Convert all to character for maximum ingest compatibility
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
  
  # Stay under the radar
  Sys.sleep(5)
}

message("--- SCRAPE COMPLETE ---")
