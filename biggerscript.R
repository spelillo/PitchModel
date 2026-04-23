library(tidyverse)
library(googledrive)
library(vroom)

# --- 1. GOOGLE AUTH ---
# This looks for the secret you added to GitHub
setup_google_auth <- function() {
  target_path <- "google_key.json"
  writeLines(Sys.getenv("GOOGLE_AUTH"), target_path)
  drive_auth(path = target_path)
}
setup_google_auth()

# --- 2. FETCH LATEST DATA ---
# We grab the last 7 days to keep the sync fast
end_date <- Sys.Date()
start_date <- end_date - 7
savant_url <- paste0(
  "https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details",
  "&game_date_gt=", start_date,
  "&game_date_lt=", end_date,
  "&player_type=pitcher"
)

message(paste("Syncing Statcast:", start_date, "to", end_date))

new_pitches <- read_csv(savant_url, show_col_types = FALSE) %>%
  select(
    player_name, 
    events, 
    description, 
    b_hand = stand, # Renaming to match your app.R
    p_throws, 
    balls, 
    strikes, 
    on_3b, 
    on_2b, 
    on_1b, 
    outs_when_up, 
    inning, 
    inning_topbot, 
    pitch_name, 
    home_score, 
    away_score
  ) %>%
  # Standardize runners to 0/1 binary logic
  mutate(
    on_1b = ifelse(is.na(on_1b) | on_1b == 0, 0, 1),
    on_2b = ifelse(is.na(on_2b) | on_2b == 0, 0, 1),
    on_3b = ifelse(is.na(on_3b) | on_3b == 0, 0, 1)
  )

# --- 3. MERGE WITH MASTER ---
# Download master from Drive, append new data, remove duplicates, and re-upload
drive_download("hugemegadata.csv", overwrite = TRUE)
master_data <- vroom("hugemegadata.csv")

final_data <- bind_rows(master_data, new_pitches) %>% 
  distinct() # This is the "Magic" button that prevents double-counting

vroom_write(final_data, "hugemegadata.csv", delim = ",")
drive_put("hugemegadata.csv", name = "hugemegadata.csv")
message("Weekly Sync Complete. Master CSV updated on Google Drive.")