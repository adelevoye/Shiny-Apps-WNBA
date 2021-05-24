rm(list = ls())

library(tidyverse) 
library(readr)
library(shiny)
library(rvest)
library(janitor)
library(shinyWidgets)

# Load function

espn_wnba_player_box <- function(game_id){
  options(stringsAsFactors = FALSE)
  options(scipen = 999)
  play_base_url <- "http://cdn.espn.com/wnba/playbyplay?render=false&userab=1&xhr=1&"
  
  ## Inputs
  ## game_id
  full_url <- paste0(play_base_url,
                     "gameId=", game_id)
  raw_play_df <- jsonlite::fromJSON(full_url)[["gamepackageJSON"]]
  raw_play_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df),flatten=TRUE)
  #---- Player Box ------
  players_df <- jsonlite::fromJSON(jsonlite::toJSON(raw_play_df[["boxscore"]][["players"]]), flatten=TRUE) %>%
    tidyr::unnest(.data$statistics) %>%
    tidyr::unnest(.data$athletes)
  stat_cols <- players_df$names[[1]]
  stats <- players_df$stats
  
  stats_df <- as.data.frame(do.call(rbind,stats))
  colnames(stats_df) <- stat_cols
  
  players_df <- players_df %>%
    dplyr::filter(!.data$didNotPlay) %>%
    dplyr::select(.data$starter,.data$ejected, .data$didNotPlay,.data$active,
                  .data$athlete.displayName,.data$athlete.jersey,
                  .data$athlete.id,.data$athlete.shortName,.data$athlete.position.name,
                  .data$athlete.position.abbreviation,.data$team.shortDisplayName,
                  .data$team.name, .data$team.logo,.data$team.id,.data$team.abbreviation,
                  .data$team.color,.data$team.alternateColor
    )
  
  player_box <- dplyr::bind_cols(stats_df,players_df) %>%
    dplyr::select(.data$athlete.displayName,.data$team.shortDisplayName, tidyr::everything())
  player_box <- player_box %>% 
    janitor::clean_names() %>% 
    dplyr::rename(
      '+/-'=.data$x,
      fg3 = .data$x3pt
    )
  
  return(player_box)
}

# Function to build df

buildf <- function(gameid_vec) {
  player_stats <- list()
  for (i in gameid_vec) {
    players <- espn_wnba_player_box(game_id = i)
    players <- players %>%
      select(1:20, 22:26, 28:29) %>%
      mutate(game_id = i)
    player_stats <- rbind(player_stats,players) 
    Sys.sleep(1)}
  
  
  player_stats <- player_stats %>%
    separate(fg, c('fgm', 'fga')) %>%
    separate(fg3, c('fg3m', 'fg3a')) %>%
    separate(ft, c('ftm', 'fta')) 
  
  player_stats$fgm <- as.numeric(player_stats$fgm)
  player_stats$fga <- as.numeric(player_stats$fga)
  player_stats$fg3m <- as.numeric(player_stats$fg3a)
  player_stats$ftm <- as.numeric(player_stats$ftm)
  player_stats$fta <- as.numeric(player_stats$fta)
  player_stats$fg3a <- as.numeric(player_stats$fg3a)
  player_stats$pts <- as.numeric(player_stats$pts)
  player_stats$reb <- as.numeric(player_stats$reb)
  player_stats$ast <- as.numeric(player_stats$ast)
  player_stats$stl <- as.numeric(player_stats$stl)
  player_stats$blk <- as.numeric(player_stats$blk)
  player_stats$to <- as.numeric(player_stats$to)
  player_stats$`+/-` <- as.numeric(player_stats$`+/-`)
  
  player_stats <- player_stats %>%
    mutate(bonus = pts + reb + ast + stl + blk + fgm + fg3m + ftm,
           malus = (fga - fgm) + (fg3a - fg3m) + (fta - ftm) + to)
  
  player_stats <- player_stats %>%
    mutate(score_fl = bonus - malus)
  
  player_stats <- player_stats %>% 
    group_by(athlete_display_name, team_short_display_name) %>%
    summarize(score_all = sum(score_fl),
              plusminus = sum(`+/-`),
              n_games = n())
  
  player_stats <- player_stats %>% 
    mutate(score_pg = score_all/n_games, 
           plusminus_pg = plusminus/n_games)
  
  player_stats <- player_stats %>% 
    arrange(desc(score_pg))
  
  player_stats <- player_stats %>% 
    select(athlete_display_name, team_short_display_name, 
           score_pg, score_all, plusminus_pg, plusminus, n_games)
  
  player_stats <- player_stats %>% 
    rename(Player = athlete_display_name, 
           Team = team_short_display_name, 
           Fantasy_Score_PG = score_pg, 
           Fantasy_Score_all = score_all, 
           Plus_Minus_PG = plusminus_pg, 
           Plus_Minus_all = plusminus, 
           Games_played = n_games)
  
  player_stats$Fantasy_Score_PG <- round(player_stats$Fantasy_Score_PG,1)
  player_stats$Plus_Minus_PG <- round(player_stats$Plus_Minus_PG,1)
  
  player_stats}

# Season started at 401320565, two last numbers growing
test2 <- c(401320585, 401320584)
buildf(gameids_picked)



#builddffunct(2019)

# UI config
#      Data scraped from basketball reference.
#Formula used for bonus is = PTS + REB + AST + STL + BLK + FGM + 3PM + FTM
#Formula used for malus is = (FGA-FGM) + (3PA-3PM) + (FTA-FTM) + TOV
#Results shown for overall season scores as well as per game average"

ui <- fluidPage(
  # App title
  titlePanel("Fantasy points for specific games, WNBA, 2021 season"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Get game-specific fantasy points for WNBA players"),
      p("This app is based on ESPN's game IDs and will only work for the 2021 season"),
      p("Enter the range of game IDs you are interested in below. You will get fantasy scores for all games played in the range.
        The first game of the 2021 was coded by ESPN as number 401320565. 
        Games are coded in chronological order, so the second game in the season was 401320566, etc. 
        Just find the first and last game in the time period you are interested in!
        Or if you want the x first games of the season, select 401320565 to 401320565 + x. 
        The app will return an error if you select a game ID for a game that has not been played yet."),
      p("Games ID can easily be found in the URLs of ESPN's boxscores"),
      p("You might have to be patient, the table can take a couple of minutes to be generated"),
      
    #Input  
    numericRangeInput(
      inputId = "gamesids", label = "Numeric Range Input:",
      value = c(401320565, 401320567)
    ),
    br(),
    br(),
    br(),
    p("Data scraped from ESPN"),
    p("Formula used for bonus is = PTS + REB + AST + STL + BLK + FGM + 3PM + FTM"),
    p("Formula used for malus is = (FGA-FGM) + (3PA-3PM) + (FTA-FTM) + TOV"),
    p("Results shown for overall scores in the range as well as per game average"),
    p("A huge thanks to Saiem Gilani and the WeHoop package for their 
      help accessing ESPN data"),
    ),
    # Output
    mainPanel(dataTableOutput("player_stats"))
  ))


# Define server logic ----


server <- function(input, output) {
  # Inputs
  gameids_picked <- reactive({
    seq(input$gamesids[1], input$gamesids[2], by = 1)
  })
  # Table
  output$player_stats = renderDataTable({
    buildf(gameids_picked())})
}

# Run the app ----
shinyApp(ui = ui, server = server)
