library(tidyverse)
library(bigrquery)
library(lubridate)

# 1. CONFIG
project_id <- "pitchmodel-494200"
dataset_id <- "pitch_model_analytics"
table_id   <- "all_pitches_2023_2026"

# 2. AUTH
if (Sys.getenv("GCP_AUTH_JSON") != "") {
  tmp_auth <- tempfile(fileext = ".json")
  writeLines(Sys.getenv("GCP_AUTH_JSON"), tmp_auth)
  bq_auth(path = tmp_auth)
  message("Auth Success.")
}

# 3. DEFINE DATES
# We use a standard sequence. This MUST be defined here.
date_chunks <- seq(as.Date("2025-04-01"), as.Date("2026-04-28"), by = "4 days")

# Explicit check to stop the 'object not found' error
if (!exists("date_chunks")) {
  stop("Critical Failure: date_chunks was not created.")
}

message(sprintf("Starting scrape for %s chunks...", length(date_chunks)-1))

# 4. LOOP
for (i in 1:(length(date_chunks) - 1)) {
  s_date <- date_chunks[i]
  e_date <- date_chunks[i+1] - 1
  
  # Only run during the season
  if (!(month(s_date) %in% 4:10)) next
  
  message(sprintf("[%s] Fetching: %s to %s", Sys.time(), s_date, e_date))
  
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher&game_date_gt=", 
                s_date, "&game_date_lt=", e_date)
  
  daily_data <- tryCatch({
    read_csv(url, show_col_types = FALSE)
  }, error = function(e) return(NULL))
  
  if (!is.null(daily_data) && nrow(daily_data) > 0) {
    clean_data <- daily_data %>%
      rename_with(~str_replace_all(., "\\.", "_")) %>%
      mutate(across(everything(), as.character))
    
    bq_table_upload(
      x = bq_table(project_id, dataset_id, table_id),
      values = clean_data,
      write_disposition = "WRITE_APPEND",
      create_disposition = "CREATE_IF_NEEDED"
    )
    message("Uploaded successfully.")
  }
  
  Sys.sleep(5)
}
