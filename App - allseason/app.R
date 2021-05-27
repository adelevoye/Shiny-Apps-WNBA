rm(list = ls())

library(tidyverse) 
library(readr)
library(shiny)
library(rvest)
library(janitor)

# Load function

scrape_stats_wnba <- function(season){
  #total stats
  #scrape
  
  url <- paste0("https://www.basketball-reference.com/wnba/years/",season,"_totals.html")
  stats_tot <- url %>% 
    read_html() %>% 
    rvest::html_table() %>% 
    .[[1]]
  
  #clean
  player_stats_tot <- stats_tot %>% 
    remove_empty("cols") %>%
    janitor::clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() 
  
  
  #advanced
  url <- paste0("https://www.basketball-reference.com/wnba/years/",season,"_advanced.html")
  stats_adv <- url %>% 
    read_html() %>% 
    rvest::html_table() %>% 
    .[[1]]
  
  player_stats_adv <- stats_adv %>% 
    remove_empty("cols") %>%
    janitor::clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() 
  
  player_stats <- full_join(player_stats_tot, player_stats_adv,
                            by = c("player", "pos", "g", "mp"))
  return(player_stats)
}


# Test --------------------------------------------------------------------


# Test
#test <- scrape_stats_wnba(as.numeric("2020"))
# 
# test_player_stats <- test %>%
#   group_by(player) %>%
#   summarize(bonus = pts + trb + ast + stl + blk + fg + x3p + ft,
#             malus = (fga - fg) + (x3pa - x3p) + (fta - ft) + tov,
#             gp = g)
# 
# test_player_stats <- test_player_stats %>%
#   mutate(score_fl = bonus - malus, 
#          score_pg = (bonus - malus)/gp)
# 
# test_player_stats$score_pg <- round(test_player_stats$score_pg,1)
# test_player_stats <- test_player_stats %>% 
#   arrange(desc(score_fl))

# Function to build df if fantasy league selected

fct_ssfl_season = function(season) {
  # Get season stats
  test <- scrape_stats_wnba(season)
  # Build fantasy score from raw stats
  test_player_stats <- test %>%
    group_by(player) %>%
    summarize(bonus = pts + trb + ast + stl + blk + fg + x3p + ft,
              malus = (fga - fg) + (x3pa - x3p) + (fta - ft) + tov,
              gp = g)
  test_player_stats <- test_player_stats %>%
    mutate(score_fl = bonus - malus, 
           score_pg = (bonus - malus)/gp)
  # Round result
  test_player_stats$score_pg <- round(test_player_stats$score_pg,1)
  # Arrange by top scores
  test_player_stats <- test_player_stats %>% 
    arrange(desc(score_fl))
  test_player_stats
}


#builddffunct(2019)

# Function to build df if fantasy league selected

fct_season_stat = function(season, stat) {
  # Get stats for season
  test <- scrape_stats_wnba(season)
  # Select stat of interest
  test_player_stats <- test %>%
    select(player, stat)
  # Get number of games played for each player
  game_played <- test %>%
    group_by(player) %>%
    summarize(gp = g)
  # Merge stat and nbr of games
  test_player_stats <- left_join(test_player_stats, game_played, by = 'player')
  test_player_stats
}


#test_our <- builddffunct2(2019, 'per')


# Function to build df for all time, all stats

fct_allt_stats = function(stat) {
  # Get data for all seasons
  test_alltime <- list()
  for (i in 1997:2021) {
    test_loop <- scrape_stats_wnba(i)
    test_alltime <- rbind(test_alltime, test_loop)}
  # Extract stat of interest
  test_player_stats <- test_alltime %>%
    select(player, stat)
  # Find nbr of games played for each athlete
  game_played <- test_alltime %>%
    group_by(player) %>%
    summarize(gp = sum(g))
  # Merge
  test_player_stats <- left_join(test_player_stats, game_played, by = 'player')
  # Some players appear multiple time, need to group by player
  # Aggregate instead of group me to be able to index by col number
  test_player_stats <- aggregate(test_player_stats[,2:3], list(test_player_stats$player), mean)
  # Only keep players who've played more than 50 games
  test_player_stats <- test_player_stats %>%
    filter(gp >= 50)
  test_player_stats <- test_player_stats %>%
    rename(Player = Group.1)
}


#test4 <- fct_allt_stats('per')



# Function to build df for all time, fantasy score


# test_alltime <- list()
# for (i in 1997:2021) {
#   test_loop <- scrape_stats_wnba(i)
#   test_alltime <- rbind(test_alltime, test_loop)}
# test_player_stats <- test_alltime %>%
#   group_by(player) %>%
#   summarize(bonus = pts + trb + ast + stl + blk + fg + x3p + ft,
#             malus = (fga - fg) + (x3pa - x3p) + (fta - ft) + tov,
#             gp = g)
# test_player_stats <- test_player_stats %>%
#   filter(gp >= 50)
# test_player_stats <- test_player_stats %>%
#   mutate(score_fl = bonus - malus, 
#          score_pg = (bonus - malus)/gp)
# test_player_stats$score_pg <- round(test_player_stats$score_pg,1)
# test_player_stats <- test_player_stats %>% 
#   arrange(desc(score_fl))
# test_player_stats



# UI config
#      Data scraped from basketball reference.
#Formula used for bonus is = PTS + REB + AST + STL + BLK + FGM + 3PM + FTM
#Formula used for malus is = (FGA-FGM) + (3PA-3PM) + (FTA-FTM) + TOV
#Results shown for overall season scores as well as per game average"

ui <- fluidPage(
  # App title
  titlePanel("Season-long individual statistics, WNBA"),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      helpText("Get season-long individual or all time statistics for WNBA players. Note that all-time numbers (limited to players having played 50 games or more) might take some time to be generated"),
    #Input - season  
      selectInput("season", 
                  label = "Choose a season",
                  choices = list("All time",
                                 "1997", 
                                 "1998",
                                 "1999", 
                                 "2000",
                                 "2001", 
                                 "2002",
                                 "2003",
                                 "2004", 
                                 "2005",
                                 "2006",
                                 "2007", 
                                 "2008",
                                 "2009",
                                 "2010", 
                                 "2011",
                                 "2012",
                                 "2013", 
                                 "2014",
                                 "2015",
                                 "2016", 
                                 "2017",
                                 "2018",
                                 "2019", 
                                 "2020",
                                 "2021"),
                  selected = '2021'),
    #Input - stat
    selectInput("stat", 
                label = "Choose a statistic",
                choices = list("Fantasy Score (Swish Swish FL)", 
                               "e_fg_percent",
                               "per", 
                               "ts_percent",
                               "x3p_ar", 
                               "f_tr",
                               "orb_percent",
                               "trb_percent", 
                               "ast_percent",
                               "stl_percent",
                               "blk_percent", 
                               "tov_percent",
                               "usg_percent",
                               "o_rtg", 
                               "d_rtg",
                               "ows",
                               "dws", 
                               "ws",
                               "ws_40"),
                selected = "Fantasy Score (Swish Swish FL)"),
    br(),
    br(),
    br(),
    p("Data scraped from the Basketball Reference website."),
    p("All-time fantasy scores not available."),
    p("See https://www.basketball-reference.com/about/glossary.html for definitions of advanced statistics"),
    p("See https://fantasy.swish-swish.net/user-manual/ for fantasy scores definitions"),
    ),
    # Output
    mainPanel(dataTableOutput("player_stats"))
  ))



# Define server logic ----


server <- function(input, output) {
  # Inputs season
  season_picked <- reactive({
    input$season
  })
  
  # Inputs stat
  stat_picked <- reactive({
    input$stat
  })
  
  # Generate Table
  output$player_stats = renderDataTable({
    if(season_picked() == 'All time') {
      fct_allt_stats(stat_picked())}
    else {
      if(stat_picked() == 'Fantasy Score (Swish Swish FL)') {
        fct_ssfl_season(season_picked())}
      else {fct_season_stat(season_picked(), stat_picked())}
    }
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)


# Missing: all time SSFL
