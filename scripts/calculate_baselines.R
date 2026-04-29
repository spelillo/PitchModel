library(bigrquery)
library(tidyverse)

# --- AUTHENTICATION (COPY-PASTE THIS) ---
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

# 1. Pull Historical Data (2024 to Present)
sql <- "SELECT player_name, game_date, balls, strikes, stand, on_1b, on_2b, on_3b, outs_when_up, inning, pitch_name 
        FROM `pitchmodel-494200.pitch_model_analytics.view_pitch_analytics_clean` 
        WHERE game_date >= '2024-01-01'"

raw_data <- bq_table_download(bq_project_query(project_id, sql))

# 2. Threshold Calculation Engine
get_baseline <- function(p_name, df) {
  p_data <- df %>% filter(player_name == p_name) %>% arrange(game_date)
  if(nrow(p_data) < 100) return(NULL) # Skip low sample pitchers
  
  # Run backlog simulation
  backlog <- map_df(101:nrow(p_data), function(i) {
    history <- p_data[1:(i-1), ]
    current <- p_data[i, ]
    
    # Simulate Model Prediction
    pred <- history %>%
      filter(balls == current$balls, strikes == current$strikes) %>%
      count(pitch_name) %>%
      mutate(prob = n / sum(n)) %>%
      slice_max(prob, n = 1, with_ties = FALSE)
    
    if(nrow(pred) == 0) return(NULL)library(bigrquery)
library(tidyverse)

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
  if(nrow(p_data) < 150) return(NULL) # Need a decent sample size
  
  # Only backtest the most recent 500 pitches to keep it relevant
  if(nrow(p_data) > 500) p_data <- tail(p_data, 500)
  
  # Time-Machine Loop: Predict each pitch using previous data
  backlog <- map_df(50:nrow(p_data), function(i) {
    history <- p_data[1:(i-1), ]
    current <- p_data[i, ]
    
    pred <- history %>%
      filter(balls == current$balls, strikes == current$strikes) %>%
      count(pitch_name) %>%
      mutate(prob = n / sum(n)) %>%
      slice_max(prob, n = 1, with_ties = FALSE)
    
    if(nrow(pred) == 0) return(NULL)
    data.frame(conf = pred$prob, match = ifelse(pred$pitch_name == current$pitch_name, 1, 0))
  })
  
  # Find the 80% accuracy floor
  threshold <- backlog %>%
    arrange(desc(conf)) %>%
    mutate(cum_acc = cummean(match)) %>%
    filter(cum_acc >= 0.80) %>%
    slice_tail(n = 1) %>%
    pull(conf)
  
  # Default if 80% is impossible for that pitcher
  if(length(threshold) == 0) threshold <- 0.75
  
  return(data.frame(player_name = p_name, baseline_80_threshold = threshold, last_updated = Sys.time()))
}

# 3. Process & Upload
all_pitchers <- unique(raw_data$player_name)
baselines <- map_df(all_pitchers, ~calculate_threshold(.x, raw_data))

bq_table_upload(
  x = bq_table(project_id, dataset_id, table_id),
  values = baselines,
  write_disposition = "WRITE_TRUNCATE"
)
    data.frame(conf = pred$prob, match = ifelse(pred$pitch_name == current$pitch_name, 1, 0))
  })
  
  # Find point where rolling accuracy hits 80%
  threshold <- backlog %>%
    arrange(desc(conf)) %>%
    mutate(cum_acc = cummean(match)) %>%
    filter(cum_acc >= 0.80) %>%
    slice_tail(n = 1) %>%
    pull(conf)
  
  # Default to 0.70 if the pitcher is too unpredictable to hit 80%
  if(length(threshold) == 0) threshold <- 0.70
  
  return(data.frame(player_name = p_name, baseline_80_threshold = threshold))
}

# 3. Process & Upload
all_pitchers <- unique(raw_data$player_name)
baselines <- map_df(all_pitchers, ~get_baseline(.x, raw_data))

bq_table_upload(
  x = bq_table(project_id, dataset_id, table_id),
  values = baselines,
  write_disposition = "WRITE_TRUNCATE"
)
