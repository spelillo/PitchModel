library(shiny)
library(tidyverse)
library(bigrquery)
library(bslib)
library(ggplot2)
library(shinyWidgets)
library(shinyjs)

# --- 1. CONFIGURATION & AUTH ---
options(gargle_oauth_cache = FALSE, gargle_oauth_email = TRUE)
addResourcePath(prefix = "root", directoryPath = ".")

# Root path for deployment bundle - ensure this file is checked when publishing!
key_path <- "google_key_2.json"

if (!file.exists(key_path)) {
  stop("FATAL: google_key_2.json not found in the deployment bundle.")
}

bq_auth(path = key_path)

project_id <- "pitchmodel-494200"
dataset_id <- "pitch_model_analytics"
view_id    <- "view_pitch_analytics_clean"
full_path  <- paste0(project_id, ".", dataset_id, ".", view_id)

# --- 2. LISTS, METADATA & HELPERS ---
pitch_list <- c(
  "4-Seam Fastball", "Sinker", "Cutter", "Slider", "Sweeper", "Slurve",
  "Curveball", "Knuckle Curve", "Slow Curve", "Changeup", "Split-Finger", 
  "Forkball", "Screwball", "Knuckleball", "Eephus", "Pitch Out", "Other", "Unknown"
)

pa_outcomes <- list(
  "Single" = "single", "Double" = "double", "Triple" = "triple", "Home Run" = "home_run",
  "Strikeout" = "strikeout", "Walk" = "walk", "Intent Walk" = "intent_walk", "Hit By Pitch" = "hit_by_pitch",
  "Field Out" = "field_out", "Force Out" = "force_out", "Grounded Into DP" = "grounded_into_double_play",
  "Double Play" = "double_play", "Triple Play" = "triple_play", "Strikeout DP" = "strikeout_double_play",
  "Fielders Choice" = "fielders_choice", "Fielders Choice Out" = "fielders_choice_out", "Field Error" = "field_error",
  "Sac Fly" = "sac_fly", "Sac Bunt" = "sac_bunt", "Sac Fly DP" = "sac_fly_double_play",
  "Catcher Interf" = "catcher_interf", "Truncated PA" = "truncated_pa"
)

pitch_results <- c(
  "Ball" = "ball", "Blocked Ball" = "blocked_ball", "Automatic Ball" = "automatic_ball",
  "Called Strike" = "called_strike", "Swinging Strike" = "swinging_strike", 
  "Swinging Strike Blocked" = "swinging_strike_blocked", "Automatic Strike" = "automatic_strike",
  "Foul" = "foul", "Foul Tip" = "foul_tip", "Foul Bunt" = "foul_bunt", "Bunt Foul Tip" = "bunt_foul_tip",
  "Missed Bunt" = "missed_bunt", "Hit Into Play" = "hit_into_play", "Hit By Pitch" = "hit_by_pitch", 
  "Pitchout" = "pitchout"
)

get_pitch_category <- function(pitch) {
  case_when(
    pitch %in% c("4-Seam Fastball", "Sinker", "Cutter") ~ "Fastball",
    pitch %in% c("Slider", "Sweeper", "Slurve", "Curveball", "Knuckle Curve", "Slow Curve") ~ "Breaking",
    pitch %in% c("Changeup", "Split-Finger", "Forkball", "Screwball", "Knuckleball", "Eephus") ~ "Off-speed",
    TRUE ~ "Specialty"
  )
}

# --- 3. UI ---
ui <- page_navbar(
  useShinyjs(),
  theme = bs_theme(preset = "darkly", primary = "#00bc8c"),
  
  # BRANDING
  title = tags$span(
    tags$head(
      # THIS LINE SETS THE TAB TEXT
      tags$title("PitchModel"), 
      
      # This sets the tab icon (favicon)
      tags$link(rel = "icon", type = "image/png", href = "root/white-favicon.png")
    ),
    img(src = "root/pitchmodelcopy.png", height = "40px", style = "margin-right: 10px;"),
    "" 
  ),
  
  sidebar = sidebar(
    width = 375, 
    title = "Live Game State",
    accordion(
      open = c("Session Control", "Personnel", "At-Bat State"),
      accordion_panel(
        "Session Control",
        uiOutput("session_status_ui"),
        layout_columns(
          actionButton("start_session", "Start", class = "btn-success w-100"),
          actionButton("end_session", "End", class = "btn-danger w-100")
        )
      ),
      accordion_panel(
        "Personnel",
        selectInput("active_pitcher", "Select Pitcher:", choices = NULL, selectize = TRUE, width = "100%"),
        layout_columns(
          radioGroupButtons("p_hand", "P-Hand", choices = c("RHP", "LHP"), status = "secondary"),
          radioGroupButtons("b_hand", "B-Hand", choices = c("RHB", "LHB"))
        ),
        actionButton("add_rookie", "Add New Pitcher", icon = icon("user-plus"), class = "btn-sm btn-outline-info w-100")
      ),
      accordion_panel(
        "At-Bat State",
        layout_columns(
          numericInput("balls", "Balls", value = 0, min = 0, max = 3),
          numericInput("strikes", "Strikes", value = 0, min = 0, max = 2)
        ),
        radioGroupButtons("outs", "Outs", choices = c("0", "1", "2"), justified = TRUE, status = "warning"),
        selectInput("runners", "Runners On", choices = c("None", "1B", "2B", "3B", "1B-2B", "1B-3B", "2B-3B", "Loaded")),
        hr(),
        layout_columns(
          actionButton("new_batter", "New Batter", class = "btn-sm btn-outline-warning w-100"),
          actionButton("new_inning", "New Inning", class = "btn-sm btn-outline-danger w-100")
        )
      ),
      accordion_panel(
        "Game Context",
        layout_columns(
          numericInput("away_score", "Away Score", value = 0, min = 0),
          numericInput("home_score", "Home Score", value = 0, min = 0)
        ),
        uiOutput("scoreboard_ui")
      )
    )
  ),
  
  nav_panel("Live Tracking",
            div( 
              layout_columns(
                col_widths = c(8, 4),
                uiOutput("dynamic_prediction_box"),
                value_box(title = "Cumulative Session Accuracy", value = textOutput("session_acc"), theme = "success")
              ),
              layout_columns(
                col_widths = c(7, 5), 
                card(
                  card_header("Log Pitch"),
                  layout_columns(
                    col_widths = c(4, 8),
                    div(
                      selectInput("actual_pitch", "Pitch Thrown:", choices = pitch_list, selectize = FALSE, size = 12),
                      actionButton("submit", "SUBMIT & LOG", class = "btn-lg btn-success w-100")
                    ),
                    div(layout_columns(
                      div(tags$b("Result"), checkboxGroupInput("pitch_res", NULL, choices = pitch_results)),
                      div(tags$b("Outcome"), checkboxGroupInput("pa_res", NULL, choices = pa_outcomes))
                    ))
                  )
                ),
                card(card_header("Model Confidence"), uiOutput("situation_text"), plotOutput("pie_chart", height = "300px"))
              )
            )
  ),
  
  nav_panel("Model Performance",
            layout_columns(
              card(card_header("Accuracy Trend (Cumulative)"), plotOutput("accuracy_trend")),
              value_box(title = "Relay Confidence Threshold", value = textOutput("current_threshold"), 
                        p("Confidence needed for 80% 1:1 match probability."), theme = "info")
            ),
            card(card_header("Session Pitch Log"), DT::dataTableOutput("session_table"))
  ),
  
  nav_panel("About",
            card(
              card_header("Project Methodology & Documentation"),
              tagList(
                h5("1. The Prediction Engine"),
                p("PitchModel utilizes a hierarchical search algorithm to identify pitch patterns. It queries a proprietary BigQuery dataset sourced from ", 
                  tags$a(href="https://baseballsavant.mlb.com/", "Baseball Savant", target="_blank"), 
                  " via automated scraping and cloud-data warehousing. The model prioritizes local game state (count, runners, inning) and weights current-session 3x as heavily as historical data to account for a pitcher's immediate 'feel' and sequence tendencies."),
                
                h5("2. Weighted Skill Scoring"),
                p("To accurately assess model performance beyond simple 1:1 matches, we utilize a tiered scoring system:"),
                tags$ul(
                  tags$li(tags$b("1.00 Pts (Exact Match):"), " The model correctly identifies the specific pitch thrown."),
                  tags$li(tags$b("0.75 Pts (Category Match):"), " The pitch thrown matches the velocity/movement profile of the primary prediction (e.g., predicted Sinker, threw 4-Seam). This rewards the model for identifying the correct velocity band."),
                  tags$li(tags$b("0.50 Pts (Secondary Match):"), " The pitch thrown was the model's second-most-likely prediction. This rewards the model for having the 'right answer' in its top-two considerations.")
                ),
                
                h5("3. In-Game Recommendation Threshold"),
                p("The app employs a dynamic Bayesian calibration. It begins with a 70% confidence floor. After 15 pitches are logged, the system identifies the lowest confidence level where the model has achieved an 80% accuracy for 1:1 matches. This becomes the new 'High Confidence' threshold, represented by the green checkmark on the tracking screen."),
                
                hr(),
                p(style="text-align: center; font-style: italic;", 
                  "A product of ", 
                  tags$a(href="https://spelillo.github.io/seanpelilloenterprises/", "Sean Pelillo Enterprises", target="_blank"),
                  tags$br(),
                  "Last updated April 28, 2026 | © 2026 PitchModel, by Sean Pelillo Enterprises"
                )
              )
            )
  )
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  
  session_active <- reactiveVal(TRUE)
  current_session_scores <- reactiveVal(numeric(0))
  pitcher_hands <- reactiveVal(list())
  exact_match_count <- reactiveVal(0) 
  game_state_idx <- reactiveVal(1) 
  dynamic_threshold <- reactiveVal(0.70)
  
  inning_sequence <- expand.grid(side = c("Top", "Bot"), inn = c(as.character(1:9), "10+")) %>%
    arrange(match(inn, c(as.character(1:9), "10+")), desc(side))
  
  session_pitches <- reactiveVal(data.frame(
    pitch_name = character(), 
    top_pred = character(), second_pred = character(), model_conf = numeric(), actual_pitch = character(),
    b_hand_clean = character(), inning = character(), orientation = character(),
    outs_when_up = numeric(), balls = numeric(), strikes = numeric(),
    on_1b = numeric(), on_2b = numeric(), on_3b = numeric(),
    stringsAsFactors = FALSE
  ))
  
  output$scoreboard_ui <- renderUI({
    idx <- game_state_idx()
    header <- tags$tr(
      tags$th(""), 
      lapply(c(1:9, "10+"), function(i) tags$th(i, style="text-align:center; padding: 2px; color: #888; font-size: 0.8em;"))
    )
    make_row <- function(type) {
      tags$tr(
        tags$td(tags$b(substr(type, 1, 1)), style="color: #888; padding-right: 5px;"),
        lapply(1:10, function(i) {
          this_idx <- if(type == "Top") (i*2 - 1) else (i*2)
          is_active <- (idx == this_idx)
          style <- paste0(
            "width: 28px; height: 28px; border-radius: 4px; text-align: center; line-height: 28px; font-size: 0.75em; cursor: pointer; ",
            if(is_active) "background-color: #00bc8c; color: white; font-weight: bold; border: 2px solid #fff;" 
            else "background-color: #2c3e50; color: #aaa;"
          )
          tags$td(
            actionLink(
              inputId = paste0("nav_", this_idx),
              label = substr(type, 1, 1),
              style = style,
              onclick = sprintf("Shiny.setInputValue('jump_to_state', %d, {priority: 'event'})", this_idx)
            )
          )
        })
      )
    }
    tags$table(style="width:100%; border-spacing: 3px; border-collapse: separate; margin-top: 10px;",
               header, make_row("Top"), make_row("Bot")
    )
  })
  
  observeEvent(input$jump_to_state, {
    game_state_idx(input$jump_to_state)
    updateNumericInput(session, "balls", value = 0)
    updateNumericInput(session, "strikes", value = 0)
    updateRadioGroupButtons(session, "outs", selected = "0")
    updateSelectInput(session, "runners", selected = "None")
  })
  
  output$session_status_ui <- renderUI({
    if(session_active()) span("● RECORDING ACTIVE", style="color: #00bc8c; font-weight: bold; font-size: 0.9em;")
    else span("○ SESSION INACTIVE", style="color: #e74c3c; font-weight: bold; font-size: 0.9em;")
  })
  
  observeEvent(input$start_session, { session_active(TRUE) })
  observeEvent(input$end_session, {
    session_active(FALSE)
    session_pitches(session_pitches()[0,])
    current_session_scores(numeric(0))
    game_state_idx(1)
  })
  
  observeEvent(input$new_batter, { 
    updateNumericInput(session, "balls", value = 0)
    updateNumericInput(session, "strikes", value = 0) 
  })
  
  observeEvent(input$new_inning, { 
    updateNumericInput(session, "balls", value = 0)
    updateNumericInput(session, "strikes", value = 0)
    updateRadioGroupButtons(session, "outs", selected = "0")
    updateSelectInput(session, "runners", selected = "None")
    game_state_idx(min(game_state_idx() + 1, 20))
  })
  
  output$session_acc <- renderText({
    scores <- current_session_scores()
    if(is.null(scores) || length(scores) == 0) return("0.0%")
    scales::percent(mean(scores, na.rm = TRUE), accuracy = 0.1)
  })
  
  observe({
    p_names <- bq_table_download(bq_project_query(project_id, paste0("SELECT DISTINCT player_name FROM `", full_path, "` ORDER BY player_name")))
    updateSelectizeInput(session, "active_pitcher", choices = c("", p_names$player_name), server = TRUE) 
  })
  
  active_pitcher_data <- reactive({
    req(input$active_pitcher != "")
    query <- sprintf("SELECT * FROM `%s` WHERE player_name = '%s'", full_path, input$active_pitcher)
    df <- bq_table_download(bq_project_query(project_id, query))
    
    # --- NEW BASELINE LOOKUP LOGIC ---
    baseline_query <- sprintf("SELECT baseline_80_threshold FROM `%s.%s.pitcher_performance_baselines` WHERE player_name = '%s'", 
                              project_id, dataset_id, input$active_pitcher)
    
    # Use tryCatch to prevent the app from crashing if the table doesn't exist yet
    baseline_df <- tryCatch({
      bq_table_download(bq_project_query(project_id, baseline_query))
    }, error = function(e) return(NULL))
    
    if(!is.null(baseline_df) && nrow(baseline_df) > 0) {
      dynamic_threshold(baseline_df$baseline_80_threshold[1])
    } else {
      dynamic_threshold(0.75) # Global fallback if no baseline found
    }
    # --------------------------------
    
    if(nrow(df) == 0) return(NULL)
    df %>% mutate(
      b_hand_clean = case_when(as.character(stand) == "R" ~ "RHB", as.character(stand) == "L" ~ "LHB", TRUE ~ as.character(stand)),
      on_1b = ifelse(is.na(on_1b) | on_1b == "" | on_1b == "0", 0, 1),
      on_2b = ifelse(is.na(on_2b) | on_2b == "" | on_2b == "0", 0, 1),
      on_3b = ifelse(is.na(on_3b) | on_3b == "" | on_3b == "0", 0, 1),
      balls = as.numeric(balls), strikes = as.numeric(strikes),
      outs_when_up = as.numeric(outs_when_up), 
      inning = as.character(inning), 
      pitch_name = ifelse(is.na(pitch_name) | pitch_name == "", "Unknown", as.character(pitch_name))
    )
  })
  
  observe({
    df <- session_pitches()
    req(nrow(df) >= 15, "top_pred" %in% names(df), "model_conf" %in% names(df))
    perf_check <- df %>%
      arrange(desc(model_conf)) %>%
      mutate(is_correct = ifelse(actual_pitch == top_pred, 1, 0),
             running_acc = cummean(is_correct)) %>%
      filter(running_acc >= 0.80) %>% slice_tail(n = 1)
    if(nrow(perf_check) > 0) dynamic_threshold(perf_check$model_conf)
  })
  
  search_results <- reactive({
    req(input$active_pitcher != "", input$balls, input$strikes, input$outs)
    state <- inning_sequence[game_state_idx(), ]
    cur_inn_label <- as.character(state$inn)
    
    base_hist <- active_pitcher_data()
    live_data <- session_pitches()
    
    # --- 3x WEIGHTING LOGIC ---
    # We repeat the live rows 3 times to make them 3x more influential than historical rows
    if(!is.null(live_data) && nrow(live_data) > 0) {
      pool <- bind_rows(base_hist, live_data[rep(seq_len(nrow(live_data)), each = 3), ])
    } else { 
      pool <- base_hist 
    }
    
    if(is.null(pool) || nrow(pool) == 0) return(NULL)
    
    # Hierarchical filter logic
    l1 <- pool %>% filter(balls == as.numeric(input$balls), 
                          strikes == as.numeric(input$strikes), 
                          b_hand_clean == input$b_hand,
                          on_1b == ifelse(str_detect(input$runners, "1B"), 1, 0), 
                          on_2b == ifelse(str_detect(input$runners, "2B"), 1, 0), 
                          on_3b == ifelse(str_detect(input$runners, "3B"), 1, 0),
                          outs_when_up == as.numeric(input$outs), 
                          inning == cur_inn_label) 
    
    exact_match_count(nrow(l1))
    final_df <- l1
    
    # Fallback if too few samples
    if(nrow(final_df) < 50) {
      final_df <- bind_rows(final_df, pool %>% 
                              filter(balls == as.numeric(input$balls), 
                                     strikes == as.numeric(input$strikes), 
                                     b_hand_clean == input$b_hand) %>% 
                              slice_head(n = 50 - nrow(final_df)))
    }
    return(final_df)
  })
  
  all_pitches_freq <- reactive({
    res <- search_results()
    if(is.null(res)) return(data.frame(pitch_name=c("Unknown"), Prob=1, Count=0))
    res %>% group_by(pitch_name) %>% summarise(Count = n()) %>% mutate(Prob = Count / sum(Count)) %>% arrange(desc(Prob))
  })
  
  output$dynamic_prediction_box <- renderUI({
    req(all_pitches_freq())
    p <- all_pitches_freq()
    conf <- p$Prob[1]
    thresh <- dynamic_threshold()
    yellow_floor <- thresh * 0.5
    
    status_info <- if (conf >= thresh) {
      list(icon = "check-circle", label = "High Confidence") 
    } else if (conf >= yellow_floor) {
      list(icon = "exclamation-triangle", label = "Medium Confidence") 
    } else {
      list(icon = "exclamation-triangle", label = "Low Confidence")
    }
    
    value_box(
      title = status_info$label, 
      value = p$pitch_name[1], 
      p(paste0("Model: ", scales::percent(conf, 1), 
               " | Target: ", scales::percent(thresh, 1), 
               " (Pitcher Baseline)")), # Fixed parenthesis here
      theme = "success", 
      showcase = bsicons::bs_icon(status_info$icon)
    ) # Fixed parenthesis here
  })
    
    output$situation_text <- renderUI({
      req(all_pitches_freq())
      p <- all_pitches_freq()
      HTML(paste0("<b>Sample: </b>", sum(p$Count), " (", exact_match_count(), " exact)<hr>", 
                  paste(sapply(1:nrow(p), function(i) paste0("<span style='color: #00bc8c;'><b>", p$pitch_name[i], "</b></span>: ", scales::percent(p$Prob[i], 1))), collapse="<br>")))
    })
    
    output$pie_chart <- renderPlot({
      req(all_pitches_freq())
      ggplot(all_pitches_freq(), aes(x = "", y = Prob, fill = pitch_name)) +
        geom_bar(stat = "identity", width = 1, color = "#222222") + coord_polar("y") +
        scale_fill_brewer(palette = "Set3") + theme_void() + theme(legend.text = element_text(color="white", size = 12))
    }, bg="transparent")
    
    output$current_threshold <- renderText({ scales::percent(dynamic_threshold(), 1) })
    
    observeEvent(input$submit, {
      if(!session_active()) { showNotification("Session inactive.", type = "error"); return() }
      p_data <- all_pitches_freq()
      top_pred <- p_data$pitch_name[1]
      second_pred <- if(nrow(p_data) > 1) p_data$pitch_name[2] else "None"
      
      score <- case_when(
        input$actual_pitch == top_pred ~ 1.0,
        get_pitch_category(input$actual_pitch) == get_pitch_category(top_pred) ~ 0.75,
        input$actual_pitch == second_pred ~ 0.50,
        TRUE ~ 0.0
      )
      current_session_scores(c(current_session_scores(), score))
      
      state <- inning_sequence[game_state_idx(), ]
      new_memory_row <- data.frame(
        pitch_name = input$actual_pitch, top_pred = top_pred, second_pred = second_pred,
        model_conf = p_data$Prob[1], actual_pitch = input$actual_pitch,
        b_hand_clean = input$b_hand, inning = state$inn, orientation = state$side,
        outs_when_up = as.numeric(input$outs), balls = as.numeric(input$balls), strikes = as.numeric(input$strikes),
        on_1b = ifelse(str_detect(input$runners, "1B"), 1, 0), on_2b = ifelse(str_detect(input$runners, "2B"), 1, 0), on_3b = ifelse(str_detect(input$runners, "3B"), 1, 0),
        stringsAsFactors = FALSE
      )
      session_pitches(bind_rows(session_pitches(), new_memory_row))
      
      if (length(input$pa_res) > 0) {
        updateNumericInput(session, "balls", value = 0); updateNumericInput(session, "strikes", value = 0)
      } else if (length(input$pitch_res) > 0) {
        if (any(input$pitch_res %in% c("ball", "blocked_ball"))) updateNumericInput(session, "balls", value = min(input$balls + 1, 3)) 
        else updateNumericInput(session, "strikes", value = min(input$strikes + 1, 2))
      }
      updateCheckboxGroupInput(session, "pitch_res", selected = character(0)); updateCheckboxGroupInput(session, "pa_res", selected = character(0))
    })
    
    output$accuracy_trend <- renderPlot({
      df <- session_pitches(); req(nrow(df) > 0)
      df %>% mutate(pitch_num = row_number(), is_correct = ifelse(actual_pitch == top_pred, 1, 0), cum_acc = cummean(is_correct)) %>%
        ggplot(aes(x = pitch_num, y = cum_acc)) + geom_line(color = "#00bc8c", linewidth = 1.2) + geom_point(size = 3, color = "#00bc8c") + ylim(0, 1) + labs(x = "Pitches Logged", y = "Cumulative 1:1 Accuracy") + theme_minimal() + theme(text = element_text(color = "white"), panel.grid.major = element_line(color = "#444444"))
    }, bg = "transparent")
    
    output$session_table <- DT::renderDataTable({
      session_pitches() %>% select(pitch_name, top_pred, second_pred, model_conf, actual_pitch) %>%
        mutate(model_conf = scales::percent(model_conf, 1))
    })
}

shinyApp(ui, server)
