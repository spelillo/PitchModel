library(shiny)
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(bslib)
library(ggplot2)
library(shinyWidgets)
library(vroom)
library(shinyjs)

# --- 1. CONFIGURATION & AUTH ---
# Ensure .secrets/google_key.json is in your project folder
drive_auth(path = ".secrets/google_key.json")
gs4_auth(path = ".secrets/google_key.json")

# Download the master historical file from Google Drive
# This ensures the app always has the data from your weekly automated scraper
tryCatch({
  drive_download("hugemegadata.csv", overwrite = TRUE)
  HISTORICAL_DATA <- "hugemegadata.csv"
}, error = function(e) {
  showNotification("Warning: Master CSV not found on Drive. Using local backup.", type = "warning")
  HISTORICAL_DATA <- "hugemegadata.csv" 
})

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1ge65vfqUEQeT4l76OatlCANpEk8EfAVdYOgwTiRkQ2s/"

# 2. Lists & Metadata
pitch_list <- c(
  "4-Seam Fastball", "Sinker", "Cutter", "Slider", "Sweeper", "Slurve",
  "Curveball", "Knuckle Curve", "Slow Curve", "Changeup", "Split-Finger", 
  "Forkball", "Screwball", "Knuckleball", "Eephus", "Pitch Out", "Other", "Unknown"
)

pa_outcomes <- c(
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

# --- 3. GLOBAL PRE-LOAD ---
if (file.exists(HISTORICAL_DATA)) {
  global_names <- vroom::vroom(HISTORICAL_DATA, col_select = player_name, show_col_types = FALSE) %>%
    distinct(player_name) %>%
    pull(player_name) %>%
    sort()
} else {
  global_names <- c("Error: Data file missing")
}

# --- 4. UI ---
ui <- page_sidebar(
  useShinyjs(),
  theme = bs_theme(preset = "darkly", primary = "#00bc8c"),
  title = "Scout Pro: Situation-First Engine",
  fillable = FALSE,
  
  sidebar = sidebar(
    width = 375, 
    title = "Live Game State",
    accordion(
      open = c("Personnel", "At-Bat State", "Game Context"), 
      accordion_panel(
        "Personnel",
        actionButton("show_help", "How this works", icon = icon("circle-info"), class = "btn-info btn-sm w-100 mb-3"),
        selectInput("active_pitcher", "Select Pitcher:", choices = c("", global_names), selectize = TRUE, width = "100%"),
        actionButton("add_rookie", "Add New Pitcher", icon = icon("user-plus"), class = "btn-sm btn-outline-info w-100"),
        hr(),
        layout_columns(
          radioGroupButtons("p_hand", "P-Hand", choices = c("RHP", "LHP"), status = "secondary"),
          radioGroupButtons("b_hand", "B-Hand", choices = c("RHB", "LHB"), status = "secondary")
        )
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
          actionButton("new_batter", "New Batter", class = "btn-sm btn-outline-warning w-100", icon = icon("user")),
          actionButton("new_inning", "New Inning", class = "btn-sm btn-outline-danger w-100", icon = icon("rotate-right"))
        )
      ),
      accordion_panel(
        "Game Context",
        layout_columns(
          numericInput("away_score", "Away Score", value = 0, min = 0),
          numericInput("home_score", "Home Score", value = 0, min = 0)
        ),
        sliderInput("inning_num", "Inning:", min = 1, max = 9, value = 1, step = 1),
        radioGroupButtons("orientation", "Half Inning:", choices = c("Top", "Bot"), justified = TRUE, status = "primary")
      )
    )
  ),
  
  div( 
    layout_columns(
      col_widths = c(5, 3, 4),
      uiOutput("primary_prediction_box"),
      value_box(
        title = "Session Accuracy",
        value = textOutput("session_acc"),
        showcase = bsicons::bs_icon("graph-up"),
        theme = "success"
      ),
      card(
        card_header("Session Management"),
        card_body(
          uiOutput("session_status_ui"),
          layout_columns(
            actionButton("start_session", "Start Session", class = "btn-success w-100"),
            actionButton("end_session", "End Session", class = "btn-danger w-100")
          )
        )
      )
    ),
    
    layout_columns(
      col_widths = c(7, 5), 
      card(
        card_header("Log Current Pitch", class = "bg-primary text-white"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),
            div(
              selectInput("actual_pitch", "Pitch Thrown:", choices = pitch_list, selectize = FALSE, size = 15, width = "100%"),
              actionButton("submit", "SUBMIT & LOG", class = "btn-lg btn-success w-100", style = "margin-top: 10px;")
            ),
            div(
              layout_columns(
                col_widths = c(6, 6),
                div(
                  tags$b("Pitch Result"),
                  checkboxGroupInput("pitch_res", NULL, choices = pitch_results)
                ),
                div(
                  tags$b("PA Outcome"),
                  checkboxGroupInput("pa_res", NULL, choices = pa_outcomes)
                )
              )
            )
          )
        )
      ),
      
      card(
        card_header("Situational Analysis Filters"), 
        card_body(
          checkboxGroupInput("active_filters", "Active Filters:",
                             choices = c("Batter Hand" = "b_hand", "Inning" = "inning", "Outs" = "outs", "Count" = "count", "Runners" = "runners"),
                             selected = c("b_hand", "inning", "outs", "count", "runners"),
                             inline = TRUE),
          hr(),
          uiOutput("situation_text"),
          div(style = "height: 320px; display: flex; justify-content: center;", 
              plotOutput("pie_chart", height = "100%", width = "100%"))
        )
      )
    )
  )
)

# --- 5. SERVER ---
server <- function(input, output, session) {
  
  session_active <- reactiveVal(FALSE)
  current_session_scores <- reactiveVal(numeric(0))
  pitcher_hands <- reactiveVal(list())
  
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Scout Pro Documentation",
      easyClose = TRUE, size = "l",
      footer = modalButton("Close"),
      tagList(
        p("Created by ", tags$b("Sean Pelillo"), "."),
        p("This app uses a Master Data Lake on Google Drive for high-speed situational predictions while logging your live scouting data to Google Sheets.")
      )
    ))
  })
  
  output$session_status_ui <- renderUI({
    if(session_active()) span("● RECORDING ACTIVE", style="color: #00bc8c; font-weight: bold;") else span("○ INACTIVE", style="color: #e74c3c;")
  })
  
  observeEvent(input$start_session, { session_active(TRUE); current_session_scores(numeric(0)); showNotification("Session Started") })
  observeEvent(input$end_session, { session_active(FALSE); showNotification("Session Ended") })
  
  output$session_acc <- renderText({
    scores <- current_session_scores()
    if(length(scores) == 0) return("0.0%")
    scales::percent(mean(scores), accuracy = 0.1)
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
  })
  
  # Historical Loader
  hist_data <- reactive({
    req(file.exists(HISTORICAL_DATA))
    df <- vroom::vroom(HISTORICAL_DATA, show_col_types = FALSE, delim = ",") %>%
      mutate(
        b_hand_clean = case_when(b_hand == "R" ~ "RHB", b_hand == "L" ~ "LHB", TRUE ~ b_hand),
        pitch_name = ifelse(is.na(pitch_name) | pitch_name == "", "Unknown", pitch_name)
      )
    hl <- setNames(as.character(df$p_throws), df$player_name)
    pitcher_hands(hl)
    return(df)
  })
  
  # Pitcher Hand Lock
  observeEvent(input$active_pitcher, {
    if(input$active_pitcher != "") {
      hl <- pitcher_hands()
      if(input$active_pitcher %in% names(hl)) {
        hand <- ifelse(hl[[input$active_pitcher]] %in% c("R", "RHP"), "RHP", "LHP")
        updateRadioGroupButtons(session, "p_hand", selected = hand)
        shinyjs::disable("p_hand")
      }
    } else { shinyjs::enable("p_hand") }
  })
  
  # CORE SEARCH ENGINE
  search_results <- reactive({
    req(input$active_pitcher != "")
    pool <- hist_data() %>% filter(player_name == input$active_pitcher)
    
    if ("b_hand" %in% input$active_filters) pool <- pool %>% filter(b_hand_clean == input$b_hand)
    if ("inning" %in% input$active_filters) pool <- pool %>% filter(inning == as.numeric(input$inning_num))
    if ("outs" %in% input$active_filters) pool <- pool %>% filter(outs_when_up == as.numeric(input$outs))
    if ("count" %in% input$active_filters) pool <- pool %>% filter(balls == as.numeric(input$balls), strikes == as.numeric(input$strikes))
    if ("runners" %in% input$active_filters) {
      on1 <- ifelse(str_detect(input$runners, "1B"), 1, 0)
      on2 <- ifelse(str_detect(input$runners, "2B"), 1, 0)
      on3 <- ifelse(str_detect(input$runners, "3B"), 1, 0)
      pool <- pool %>% filter(on_1b == on1, on_2b == on2, on_3b == on3)
    }
    return(pool)
  })
  
  all_pitches_freq <- reactive({
    req(search_results())
    search_results() %>% group_by(pitch_name) %>% summarise(Count = n()) %>%
      mutate(Prob = Count / sum(Count)) %>% arrange(desc(Prob))
  })
  
  output$situation_text <- renderUI({
    req(all_pitches_freq(), input$active_pitcher)
    p <- all_pitches_freq()
    if(nrow(p) == 0) return(HTML("<div style='color: #e74c3c;'><b>No situational matches.</b></div>"))
    sit_str <- paste0("<div style='font-size: 1.05em; line-height: 1.4;'><b>Sample: ", sum(p$Count), " total pitches.</b></div>")
    pitch_lines <- sapply(1:nrow(p), function(i) {
      paste0("<span style='color: #00bc8c;'><b>", p$pitch_name[i], "</b></span>: ", scales::percent(p$Prob[i], 1), " (", p$Count[i], ")")
    })
    HTML(paste0(sit_str, paste(pitch_lines, collapse="<br>")))
  })
  
  output$pie_chart <- renderPlot({
    req(all_pitches_freq())
    p <- all_pitches_freq()
    if(nrow(p) == 0) return(NULL)
    ggplot(p, aes(x = "", y = Prob, fill = pitch_name)) +
      geom_bar(stat = "identity", width = 1, color = "#222222") + coord_polar("y", start = 0) +
      geom_text(aes(label = scales::percent(Prob, 1)), position = position_stack(vjust = 0.5), color = "white", fontface="bold", size=4) +
      scale_fill_brewer(palette = "Set3") + theme_void() + theme(legend.text = element_text(color="white", size=10))
  }, bg="transparent")
  
  output$primary_prediction_box <- renderUI({
    req(all_pitches_freq())
    p <- all_pitches_freq()
    if(nrow(p) == 0) return(value_box(title = "Pitch Prediction", value = "No Data", theme = "primary"))
    top_3_summary <- paste0("1: ", p$pitch_name[1], " (", scales::percent(p$Prob[1], 1), ")",
                            if(nrow(p) >= 2) paste0(" | 2: ", p$pitch_name[2], " (", scales::percent(p$Prob[2], 1), ")") else "",
                            if(nrow(p) >= 3) paste0(" | 3: ", p$pitch_name[3], " (", scales::percent(p$Prob[3], 1), ")") else "")
    value_box(title = "Pitch Prediction", value = p$pitch_name[1], p(top_3_summary), theme = "primary")
  })
  
  # SUBMIT EVENT
  observeEvent(input$submit, {
    if(!session_active()) { showNotification("Start Session first!", type = "error"); return() }
    
    # 1. Prediction Accuracy
    p_names <- all_pitches_freq()$pitch_name
    score <- case_when(input$actual_pitch == p_names[1] ~ 1.0, input$actual_pitch == p_names[2] ~ 0.67, input$actual_pitch == p_names[3] ~ 0.33, TRUE ~ 0)
    current_session_scores(c(current_session_scores(), score))
    
    # 2. LOG TO GOOGLE SHEET (Journaling)
    new_entry <- data.frame(
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
      Pitcher_Name = input$active_pitcher,
      P_Hand = input$p_hand, 
      B_Hand = input$b_hand, 
      Inning = input$inning_num,
      Balls = input$balls, 
      Strikes = input$strikes, 
      Outs = as.numeric(input$outs),
      Away_Score = input$away_score,
      Home_Score = input$home_score,
      Actual_Pitch = input$actual_pitch,
      Pitch_Result = paste(input$pitch_res, collapse = ", "),
      PA_Outcome = paste(input$pa_res, collapse = ", "),
      Accuracy_Score = score
    )
    sheet_append(SHEET_URL, new_entry)
    
    # 3. AUTO-UPDATE COUNT
    ball_types <- c("ball", "blocked_ball", "automatic_ball", "pitchout")
    strike_types <- c("called_strike", "swinging_strike", "swinging_strike_blocked", "automatic_strike", "foul", "foul_tip", "foul_bunt", "bunt_foul_tip", "missed_bunt")
    
    if (length(input$pa_res) > 0) {
      updateNumericInput(session, "balls", value = 0)
      updateNumericInput(session, "strikes", value = 0)
    } else if (length(input$pitch_res) > 0) {
      if (any(input$pitch_res %in% ball_types)) {
        updateNumericInput(session, "balls", value = min(input$balls + 1, 3))
      } else if (any(input$pitch_res %in% strike_types)) {
        updateNumericInput(session, "strikes", value = min(input$strikes + 1, 2))
      }
    }
    
    updateCheckboxGroupInput(session, "pitch_res", selected = character(0))
    updateCheckboxGroupInput(session, "pa_res", selected = character(0))
    showNotification("Logged & Count Updated.")
  })
}

shinyApp(ui, server)