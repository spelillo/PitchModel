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
  message("Service Account authenticated.")
}

# 3. DEFINE INTERVALS (Renamed to force a fresh read)
pitch_date_intervals <- seq(as.Date("2023-03-30"), as.Date("2026-10-01"), by = "4 days")

message(sprintf("Scrape start: %s intervals found.", length(pitch_date_intervals)))

# 4. LOOP
for (i in 1:(length(pitch_date_intervals) - 1)) {
  s_date <- pitch_date_intervals[i]
  e_date <- pitch_date_intervals[i+1] - 1
  
  if (!(month(s_date) %in% 4:10)) next
  
  timestamp <- format(Sys.time(), "%H:%M:%S")
  message(sprintf("[%s] MLB Scrape: %s to %s", timestamp, s_date, e_date))
  
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher&game_date_gt=", 
                as.character(s_date), "&game_date_lt=", as.character(e_date))
  
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
    message("Upload successful.")
  }
  Sys.sleep(5)
}
