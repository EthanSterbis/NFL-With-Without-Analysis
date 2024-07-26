# libraries
library(shiny)
library(tidyverse)
library(nflfastR)
library(gt)
library(gtExtras)
library(scales)
library(ggimage)
library(stringr)
library(dplyr)
library(nflreadr)

# UI
ui <- fluidPage(
  titlePanel("Sterb's NFL With / Without Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Select Season(s)", choices = 2006:2023, selected = 2023, multiple = TRUE),
      selectInput("team", "Select Team", choices = teams_colors_logos$team_abbr),
      uiOutput("player_ui"),
      uiOutput("xplayers_ui"),
      uiOutput("wplayers_ui"),
      actionButton("submit", "SUBMIT")
    ),
    mainPanel(
      gt_output("outputTable")
    )
  ),
  tags$div(
    tags$p("*Point Per Reception scoring"),
    tags$p("*With / Without is determined by having above / below 1.0 PPR fantasy point by a player in that week")
  )
)

# server
server <- function(input, output, session) {
  
  # list of players for user to select based on selected season(s), team
  focusPlayersList <- reactive({
    req(input$season, input$team)
    nflreadr::load_ff_opportunity(seasons = as.numeric(input$season)) %>%
      mutate(season = as.numeric(season)) %>% 
      filter(posteam %in% input$team) %>%
      ungroup() %>%
      summarize(full_name) %>% 
      filter(!is.na(full_name)) %>% 
      unique() %>% 
      arrange(full_name) %>% 
      pull(full_name)
  })
  
  # update player selection options
  output$player_ui <- renderUI({
    selectInput("player", "Select Player", choices = focusPlayersList(), multiple = FALSE)
  })
  
  # fetch possible players to be excluded
  xPlayersList <- reactive({
    req(input$season, input$team, input$player)
    nflreadr::load_ff_opportunity(seasons = as.numeric(input$season)) %>%
      mutate(season = as.numeric(season)) %>% 
      filter(posteam %in% input$team) %>%
      ungroup() %>%
      summarize(full_name) %>% 
      filter(!is.na(full_name)) %>% 
      filter(full_name != input$player) %>% 
      unique() %>% 
      arrange(full_name) %>% 
      pull(full_name)
  })
  
  # update player to be excluded based on user input
  output$xplayers_ui <- renderUI({
    selectInput("xplayers", "Select Players to Exclude", choices = xPlayersList(), multiple = TRUE)
  })
  
  # fetch possible players to be included
  wPlayersList <- reactive({
    req(input$season, input$team, input$player)
    nflreadr::load_ff_opportunity(seasons = as.numeric(input$season)) %>%
      mutate(season = as.numeric(season)) %>% 
      filter(posteam %in% input$team) %>%
      ungroup() %>%
      summarize(full_name) %>% 
      filter(!is.na(full_name)) %>% 
      filter(full_name != input$player) %>% 
      unique() %>% 
      arrange(full_name) %>% 
      pull(full_name)
  })
  
  # update player to be excluded based on user input
  output$wplayers_ui <- renderUI({
    selectInput("wplayers", "Select Players to Include", choices = wPlayersList(), multiple = TRUE)
  })
  
  # reactive expression to generate output table
  outputTable <- eventReactive(input$submit, {
    req(input$season, input$team, input$player)
    focusSeasons <- as.numeric(input$season)
    focusTeams <- input$team
    focusPlayer <- input$player
    focusXPlayers <- input$xplayers
    focusWPlayers <- input$wplayers
    
    # loading data
    sched <- nflreadr::load_schedules(seasons = focusSeasons) %>% 
      filter(home_team == focusTeams | away_team == focusTeams) %>% 
      mutate(opp = ifelse(home_team == focusTeams, away_team, home_team)) %>% 
      select(season, game_id, week, opp, home_team, away_team)
    
    ffOppDepth <- nflreadr::load_ff_opportunity(seasons = focusSeasons) %>%
      mutate(season = as.numeric(season)) %>% 
      filter(posteam %in% focusTeams) %>%
      ungroup() %>%
      summarize(season, posteam, week, game_id, player_id, full_name, position,
                pass_attempt, rec_attempt, rush_attempt, pass_completions, receptions,
                pass_yards_gained, rec_yards_gained, rush_yards_gained,
                pass_touchdown, rec_touchdown, rush_touchdown,
                pass_two_point_conv, rec_two_point_conv, rush_two_point_conv,
                pass_first_down, rec_first_down, rush_first_down,
                pass_interception,
                fumbles_lost = rec_fumble_lost + rush_fumble_lost,
                pass_fantasy_points, rec_fantasy_points, rush_fantasy_points,
                total_yards_gained, total_touchdown, total_first_down,
                total_fantasy_points, total_fantasy_points_exp,
                pass_attempt_team, rec_attempt_team, rush_attempt_team, receptions_team,
                pass_yards_gained_team, rec_yards_gained_team, rush_yards_gained_team,
                pass_touchdown_team, rush_touchdown_team, total_yards_gained_team,
                total_touchdown_team, total_first_down_team)
    
    total_fantasy_points_by_week <- ffOppDepth %>%
      group_by(full_name, season, week) %>%
      summarize(total_fantasy_points_weekly = sum(total_fantasy_points, na.rm = TRUE)) %>%
      ungroup()
    
    ffOppDepth_wide <- total_fantasy_points_by_week %>%
      pivot_wider(names_from = full_name, values_from = total_fantasy_points_weekly, values_fill = list(total_fantasy_points_weekly = 0))
    
    ffOppDepthPart <- ffOppDepth %>% 
      left_join(ffOppDepth_wide, by = c("season", "week")) %>% 
      filter(!is.na(full_name), !is.na(position))
    
    ffOppDepthPartPlayer <- ffOppDepthPart %>% 
      filter(full_name == focusPlayer) %>% 
      filter(across(all_of(focusXPlayers), ~ . < 1.0)) %>%
      filter(across(all_of(focusWPlayers), ~ . > 1.0)) %>%
      summarise(season, week, posteam, game_id, player_id, full_name, position,
                pass_attempt, pass_completions, pass_yards_gained, pass_touchdown, pass_two_point_conv, pass_first_down, pass_interception, pass_fantasy_points,
                
                rush_attempt, rush_yards_gained, rush_touchdown, rush_two_point_conv, rush_first_down, rush_fantasy_points,
                
                rec_attempt, receptions,
                tgt_share = rec_attempt / rec_attempt_team,
                rec_yards_gained, rec_touchdown, rec_two_point_conv, rec_first_down, rec_fantasy_points,
                
                fumbles_lost, total_yards_gained, total_touchdown, total_first_down, total_fantasy_points, total_fantasy_points_exp,
                pass_attempt_team, rush_attempt_team, rec_attempt_team, receptions_team, pass_yards_gained_team, rush_yards_gained_team, pass_touchdown_team, rush_touchdown_team, total_yards_gained_team, total_touchdown_team, total_first_down_team)
    
    ffOppDepthPartPlayerOVRAVG <- ffOppDepthPart %>% 
      filter(full_name == focusPlayer) %>% 
      summarise(season, week, posteam, game_id, player_id, full_name, position,
                pass_attempt, pass_completions, pass_yards_gained, pass_touchdown, pass_two_point_conv, pass_first_down, pass_interception, pass_fantasy_points,
                
                rush_attempt, rush_yards_gained, rush_touchdown, rush_two_point_conv, rush_first_down, rush_fantasy_points,
                
                rec_attempt, receptions,
                tgt_share = rec_attempt / rec_attempt_team,
                rec_yards_gained, rec_touchdown, rec_two_point_conv, rec_first_down, rec_fantasy_points,
                
                fumbles_lost, total_yards_gained, total_touchdown, total_first_down, total_fantasy_points, total_fantasy_points_exp,
                pass_attempt_team, rush_attempt_team, rec_attempt_team, receptions_team, pass_yards_gained_team, rush_yards_gained_team, pass_touchdown_team, rush_touchdown_team, total_yards_gained_team, total_touchdown_team, total_first_down_team)
    
    avg_row <- ffOppDepthPartPlayer %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      mutate(season = first(ffOppDepthPartPlayer$season),
             posteam = first(ffOppDepthPartPlayer$posteam),
             player_id = first(ffOppDepthPartPlayer$player_id),
             full_name = first(ffOppDepthPartPlayer$full_name),
             position = first(ffOppDepthPartPlayer$position),
             game_id = "SAMPLE AVG")
    
    ovr_avg_row <- ffOppDepthPartPlayerOVRAVG %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      mutate(season = first(ffOppDepthPartPlayerOVRAVG$season),
             posteam = first(ffOppDepthPartPlayerOVRAVG$posteam),
             player_id = first(ffOppDepthPartPlayerOVRAVG$player_id),
             full_name = first(ffOppDepthPartPlayerOVRAVG$full_name),
             position = first(ffOppDepthPartPlayerOVRAVG$position),
             game_id = "OVERALL AVG")
    
    homeLogos <- teams_colors_logos %>% 
      select(team_abbr, team_logo_wikipedia, team_logo_espn) %>% 
      rename(home_team_logo_wikipedia = team_logo_wikipedia,
             home_team_logo_espn = team_logo_espn)
    
    awayLogos <- teams_colors_logos %>% 
      select(team_abbr, team_logo_wikipedia, team_logo_espn) %>% 
      rename(away_team_logo_wikipedia = team_logo_wikipedia,
             away_team_logo_espn = team_logo_espn)
    
    ffOppDepthPartPlayerwAvg <- bind_rows(ffOppDepthPartPlayer, avg_row, ovr_avg_row) %>%
      left_join(sched, by = c("season", "game_id", "week")) %>% 
      mutate(opp = ifelse(game_id == "SAMPLE AVG", "SAMPLE AVG", opp),
             opp = ifelse(game_id == "OVERALL AVG", "OVERALL AVG", opp),
             week = ifelse(opp == "SAMPLE AVG", "SAMPLE AVG", week),
             week = ifelse(opp == "OVERALL AVG", "OVERALL AVG", week)) %>% 
      select(-game_id) %>% 
      select(season, week, home_team, away_team, opp, everything()) %>%
      left_join(homeLogos, by = c("home_team" = "team_abbr")) %>%
      left_join(awayLogos, by = c("away_team" = "team_abbr"))
    
    # reassurance of changing week and opp
    ffOppDepthPartPlayerwAvg <- ffOppDepthPartPlayerwAvg %>%
      mutate(week = ifelse(week %in% c("SAMPLE AVG", "OVERALL AVG"), week, week),
             opp = ifelse(week %in% c("SAMPLE AVG", "OVERALL AVG"), week, opp))
    
    # get sameple row and average row for only numeric columns
    sample_avg_row <- ffOppDepthPartPlayerwAvg %>%
      filter(week == "SAMPLE AVG") %>%
      select(where(is.numeric))
    
    overall_avg_row <- ffOppDepthPartPlayerwAvg %>%
      filter(week == "OVERALL AVG") %>%
      select(where(is.numeric))
    
    # calculate percentage difference
    diff_values <- (sample_avg_row - overall_avg_row) / overall_avg_row * 100
    
    # create the DIFF row
    diff_row <- diff_values %>%
      mutate(week = "PERCENT OVER/UNDER", opp = "PERCENT OVER/UNDER", full_name = first(ffOppDepthPartPlayerwAvg$full_name),
             position = first(ffOppDepthPartPlayerwAvg$position))
    
    # append DIFF row to the original data frame
    ffOppDepthPartPlayerwAvgFIN <- bind_rows(ffOppDepthPartPlayerwAvg, diff_row) %>% 
      mutate(season = ifelse(season < 1000, NA_real_, season),
             season = ifelse(week %in% c("SAMPLE AVG", "OVERALL AVG", "PERCENT OVER/UNDER"), NA_real_, season),
             posteam = ifelse(week %in% c("SAMPLE AVG", "OVERALL AVG", "PERCENT OVER/UNDER"), NA_real_, posteam),
             player_id = ifelse(is.na(player_id), lag(player_id, 1), player_id)) %>% 
      mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))
    
    # get player name for table output
    player_name <- unique(ffOppDepthPartPlayerwAvgFIN$full_name)
    
    # get season(s) text for table output
    seasons_text <- paste(focusSeasons, collapse = ", ")
    
    # title text string
    title_text <- paste0(
      "**",
      player_name,
      " Statistics | ",
      seasons_text,
      " NFL Season(s)**"
    )
    
    # get xPlayers text for table
    xplyrs_text <- paste(focusXPlayers, collapse = ", ")
    
    # get wPlayers text for table
    wplyrs_text <- paste(focusWPlayers, collapse = ", ")
    
    # subtitle text string
    if (length(focusXPlayers) == 0 & length(focusWPlayers) == 0) {
      subtitle_textA <- ""
    } else if (length(focusXPlayers) == 0) {
      subtitle_textA <- paste0("*Games with ", wplyrs_text, "*")
    } else if (length(focusWPlayers) == 0) {
      subtitle_textA <- paste0("*Games without ", xplyrs_text, "*")
    } else {
      subtitle_textA <- paste0("*Games with ", wplyrs_text, " | Without ", xplyrs_text, "*")
    }
    
    caption_text <- "@EthanSterbis on X, data via nflreadR"
    
    subtitle_text <- paste0(subtitle_textA, "\n\n", caption_text)
    
    # round values to 2 decimals
    round_to_two_decimals <- function(df) {
      df %>% mutate(across(where(is.numeric), round, 2))
    }
    
    # apply function
    ffOppDepthPartPlayerwAvgFIN <- ffOppDepthPartPlayerwAvgFIN %>%
      round_to_two_decimals()
    
    # custom color scale function
    custom_color_scale <- function(values) {
      colors <- scales::col_numeric(
        palette = c("red", "white", "#3BCC00"),
        domain = c(-100, 100)
      )(values)
      # handle values outside domain (|percent| > 100)
      colors[values > 100] <- scales::col_numeric(
        palette = c("red", "white", "#3BCC00"),
        domain = c(-100, 100)
      )(100)
      colors[values < -100] <- scales::col_numeric(
        palette = c("red", "white", "#3BCC00"),
        domain = c(-100, 100)
      )(-100)
      colors
    }
    
    # output table
    outputTable <- ffOppDepthPartPlayerwAvgFIN %>% 
      gt(rowname_col = "season") %>% 
      cols_hide(player_id) %>% 
      cols_hide(posteam) %>%
      cols_hide(home_team_logo_espn) %>%
      cols_hide(away_team_logo_espn) %>% 
      tab_header(title = md(title_text),
                 subtitle = md(subtitle_text)) %>%
      tab_stubhead(label = "SEASON") %>% 
      gt_img_rows(home_team_logo_wikipedia, height = 32) %>%
      gt_img_rows(away_team_logo_wikipedia, height = 32) %>%
      cols_label(opp = "OPP",
                 full_name = "Player",
                 position = "POS",
                 pass_attempt = "ATT",
                 pass_completions = "CMP",
                 pass_yards_gained = "YDS",
                 pass_touchdown = "TD",
                 pass_two_point_conv = "2PC",
                 pass_first_down = "1D",
                 pass_interception = "INT",
                 pass_fantasy_points = "FP",
                 rush_attempt = "ATT",
                 rush_yards_gained = "YDS",
                 rush_touchdown = "TD",
                 rush_two_point_conv = "2PC",
                 rush_first_down = "1D",
                 rush_fantasy_points = "FP",
                 rec_attempt = "TGT",
                 receptions = "REC",
                 tgt_share = "TGT SHARE",
                 rec_yards_gained = "YDS",
                 rec_touchdown = "TD",
                 rec_two_point_conv = "2PC",
                 rec_first_down = "1D",
                 rec_fantasy_points = "FP",
                 fumbles_lost = "FUM",
                 total_yards_gained = "TOT YDS",
                 total_touchdown = "TOT TD",
                 total_first_down = "TOT 1D",
                 total_fantasy_points = "TOT FP",
                 total_fantasy_points_exp = "EXP FP",
                 pass_attempt_team = "PASS ATT",
                 rush_attempt_team = "RUSH ATT",
                 rec_attempt_team = "TGT",
                 receptions_team = "REC",
                 pass_yards_gained_team = "PASS YDS",
                 rush_yards_gained_team = "RUSH YDS",
                 pass_touchdown_team = "PASS TD",
                 rush_touchdown_team = "RUSH TD",
                 total_yards_gained_team = "TOT YDS",
                 total_touchdown_team = "TOT TD",
                 total_first_down_team = "TOT 1D",
                 home_team_logo_wikipedia = "HOME",
                 away_team_logo_wikipedia = "AWAY") %>%
      cols_align(align = "center", columns  = everything()) %>% 
      tab_spanner(
        label = "PLAYER",
        columns = c("fumbles_lost",
                    "total_yards_gained",
                    "total_touchdown",
                    "total_first_down",
                    "total_fantasy_points",
                    "total_fantasy_points_exp")
      ) %>%
      tab_style(
        style = list(
          cell_text(v_align = "middle")
        ),
        locations = cells_body(columns = vars(home_team_logo_wikipedia))
      ) %>%
      cols_move_to_start(columns = c("season", "week", "away_team_logo_wikipedia", "home_team_logo_wikipedia", "full_name", "position",
                                     "total_fantasy_points", "total_fantasy_points_exp", "total_yards_gained", "total_touchdown", "total_first_down", "fumbles_lost",
                                     "pass_attempt", "pass_completions", "pass_yards_gained", "pass_touchdown", "pass_first_down", "pass_interception", "pass_fantasy_points",
                                     "rush_attempt", "rush_yards_gained", "rush_touchdown", "rush_first_down", "rush_fantasy_points",
                                     "rec_attempt", "receptions", "tgt_share", "rec_yards_gained", "rec_touchdown", "rec_first_down", "rec_fantasy_points",
                                     "pass_attempt_team", "rush_attempt_team", "rec_attempt_team", "receptions_team", "pass_yards_gained_team", "rush_yards_gained_team", "pass_touchdown_team", "rush_touchdown_team", "total_first_down_team", "total_touchdown_team")) %>%
      tab_spanner(
        label = "PASSING",
        columns = c(
          "pass_attempt",
          "pass_completions",
          "pass_yards_gained",
          "pass_touchdown",
          "pass_first_down",
          "pass_interception",
          "pass_fantasy_points"
        )
      ) %>%
      tab_spanner(
        label = "RUSHING",
        columns = c(
          "rush_attempt",
          "rush_yards_gained",
          "rush_touchdown",
          "rush_first_down",
          "rush_fantasy_points"
        )
      ) %>%
      tab_spanner(
        label = "RECEIVING",
        columns = c(
          "rec_attempt",
          "receptions",
          "tgt_share",
          "rec_yards_gained",
          "rec_touchdown",
          "rec_first_down",
          "rec_fantasy_points"
        )
      ) %>%
      tab_spanner(
        label = "TEAM",
        columns = c(
          "pass_attempt_team",
          "rush_attempt_team",
          "rec_attempt_team",
          "receptions_team",
          "pass_yards_gained_team",
          "rush_yards_gained_team",
          "pass_touchdown_team",
          "rush_touchdown_team",
          "total_yards_gained_team",
          "total_touchdown_team",
          "total_first_down_team"
        )
      ) %>%
      cols_hide(opp) %>%
      cols_hide(pass_two_point_conv) %>%
      cols_hide(rush_two_point_conv) %>% 
      cols_hide(rec_two_point_conv) %>%
      cols_hide(home_team) %>% 
      cols_hide(away_team) %>% 
      cols_hide(full_name) %>% 
      cols_hide(position) %>% 
      fmt_number(
        columns = "total_fantasy_points_exp",
        rows = week != "DIFF",
        decimals = 2
      ) %>%
      data_color(
        columns = where(is.numeric),
        rows = week == "PERCENT OVER/UNDER",
        colors = custom_color_scale
      ) %>%
      fmt_missing(
        columns = "season",
        missing_text = ""
      ) %>% 
      gt_theme_538()
    
    outputTable 
  })
  
  output$outputTable <- render_gt({
    outputTable()
  })
}

# run the application 
shinyApp(ui = ui, server = server)
