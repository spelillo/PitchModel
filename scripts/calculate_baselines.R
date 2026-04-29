library(bigrquery)
library(tidyverse)
library(lubridate)

# --- AUTHENTICATION ---
if (Sys.getenv("GCP_AUTH_JSON") != "") {
  tmp_auth <- tempfile(fileext = ".json")
  writeLines(Sys.getenv("GCP_AUTH_JSON"), tmp_auth)
  bigrquery::bq_auth(path = tmp_auth)
  message("SUCCESS: Baseline script authenticated.")
}

# --- CONFIG ---
project_id <- "pitchmodel-494200"
dataset_id <- "pitch_model_analytics"
table_id   <- "pitcher_performance_baselines"

# 1. Pull 2024-Present Data
sql <- "SELECT player_name, game_date, balls, strikes, stand, on_1b, on_2b, on_3b, outs_when_up, inning, pitch_name 
        FROM `pitchmodel-494200.pitch_model_analytics.view_pitch_analytics_clean` 
        WHERE game_date >= '2024-01-01'"

raw_data <- bq_table_download(bq_project_query(project_id, sql))

# 2. Simulation Function
calculate_threshold <- function(p_name, df) {
  p_data <- df %>% filter(player_name == p_name) %>% arrange(game_date)
  if(nrow(p_data) < 150) return(NULL) 
  
  if(nrow(p_data) > 500) p_data <- tail(p_data, 500)
  
  # Time-Machine Loop
  backlog <- map_df(50:nrow(p_data), function(i) {
    history <- p_data[1:(i-1), ]
    current <- p_data[i, ]
    
    # --- RECURSIVE HUNT FOR SAMPLE SIZE >= 10 ---
    match_found <- FALSE
    attempt_level <- 1
    pred_data <- data.frame()
    
    while(match_found == FALSE & attempt_level <= 4) {
      if(attempt_level == 1) {
        # Level 1: Full Context
        pred_data <- history %>% filter(
          balls == current$balls, strikes == current$strikes,
          stand == current$stand, on_1b %in% current$on_1b,
          on_2b %in% current$on_2b, on_3b %in% current$on_3b,
          outs_when_up == current$outs_when_up
        )
      } else if(attempt_level == 2) {
        # Level 2: Drop Outs
        pred_data <- history %>% filter(
          balls == current$balls, strikes == current$strikes,
          stand == current$stand, on_1b %in% current$on_1b,
          on_2b %in% current$on_2b, on_3b %in% current$on_3b
        )
      } else if(attempt_level == 3) {
        # Level 3: Drop Runners
        pred_data <- history %>% filter(
          balls == current$balls, strikes == current$strikes,
          stand == current$stand
        )
      } else {
        # Level 4: Baseline (Count Only)
        pred_data <- history %>% filter(
          balls == current$balls, strikes == current$strikes
        )
      }
      
      if(nrow(pred_data) >= 10) {
        match_found <- TRUE
      } else {
        attempt_level <- attempt_level + 1
      }
    }
    
    if(!match_found) return(NULL) 

    pred_summary <- pred_data %>%
      count(pitch_name) %>%
      mutate(prob = n / sum(n)) %>%
      slice_max(prob, n = 1, with_ties = FALSE)
    
    # NEW: The Sanity Cap (No one is 100% predictable)
    if(pred_summary$prob > 0.95) pred_summary$prob <- 0.95
    
    data.frame(
      conf = pred_summary$prob, 
      match = ifelse(pred_summary$pitch_name == current$pitch_name, 1, 0)
    )
  })
  
  if(nrow(backlog) == 0) return(NULL)

  # Find point where rolling accuracy hits 80%
  threshold <- backlog %>%
    arrange(desc(conf)) %>%
    mutate(cum_acc = cummean(match)) %>%
    filter(cum_acc >= 0.80) %>%
    slice_tail(n = 1) %>%
    pull(conf)
  
  if(length(threshold) == 0) threshold <- 0.75
  
  return(data.frame(
    player_name = p_name, 
    baseline_80_threshold = threshold, 
    last_updated = Sys.time()
  ))
}

# 3. Process & Upload
all_pitchers <- unique(raw_data$player_name)
baselines <- map_df(all_pitchers, ~calculate_threshold(.x, raw_data))

bq_table_upload(
  x = bq_table(project_id, dataset_id, table_id),
  values = baselines,
  write_disposition = "WRITE_TRUNCATE"
)

message("SUCCESS: Contextual baselines updated and uploaded.")
