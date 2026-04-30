library(tidyverse)
library(bigrquery)
library(lubridate)

# --- 1. CONFIG ---
project_id <- "pitchmodel-494200"
dataset_id <- "pitch_model_analytics"
table_id   <- "all_pitches_2023_2026"

# --- 2. AUTH ---
if (Sys.getenv("GCP_AUTH_JSON") != "") {
  tmp_auth <- tempfile(fileext = ".json")
  writeLines(Sys.getenv("GCP_AUTH_JSON"), tmp_auth)
  bq_auth(path = tmp_auth)
}

# --- 3. DATES (Dynamic 7-Day Window) ---
# This ensures we always look at the last week relative to "today"
start_date <- Sys.Date() - 7
end_date   <- Sys.Date()

# We only need two dates for a single 7-day chunk
full_scrape_dates <- c(start_date, end_date)
date_chunks <- full_scrape_dates

message(sprintf("Scraping window: %s to %s", start_date, end_date))

# --- 4. THE LOOP ---
for (i in 1:(length(full_scrape_dates) - 1)) {
  s_date <- full_scrape_dates[i]
  e_date <- full_scrape_dates[i+1] - 1
  
  if (!(month(s_date) %in% 4:10)) next
  
  # Standard Savant URL
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
    message(sprintf("Uploaded: %s to %s", s_date, e_date))
  }
  Sys.sleep(5)
}
