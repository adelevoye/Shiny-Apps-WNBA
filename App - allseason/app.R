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


# Test
# test <- scrape_stats_wnba(as.numeric("2020"))
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

# Function to build df

builddffunct = function(season) {
  test <- scrape_stats_wnba(season)
  test_player_stats <- test %>%
    group_by(player) %>%
    summarize(bonus = pts + trb + ast + stl + blk + fg + x3p + ft,
              malus = (fga - fg) + (x3pa - x3p) + (fta - ft) + tov,
              gp = g)
  test_player_stats <- test_player_stats %>%
    mutate(score_fl = bonus - malus, 
           score_pg = (bonus - malus)/gp)
  test_player_stats$score_pg <- round(test_player_stats$score_pg,1)
  test_player_stats <- test_player_stats %>% 
    arrange(desc(score_fl))
  test_player_stats
}


#builddffunct(2019)

# UI config
#      Data scraped from basketball reference.
#Formula used for bonus is = PTS + REB + AST + STL + BLK + FGM + 3PM + FTM
#Formula used for malus is = (FGA-FGM) + (3PA-3PM) + (FTA-FTM) + TOV
#Results shown for overall season scores as well as per game average"

ui <- fluidPage(
  # App title
  titlePanel("Season-long fantasy points, WNBA"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Get season-long fantasy points for WNBA players."),
    #Input  
      selectInput("season", 
                  label = "Choose a season",
                  choices = list("1997", 
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
                                 "2021")),
    br(),
    br(),
    br(),
    p("Data scraped from the Basketball Reference website"),
    p("Formula used for bonus is = PTS + REB + AST + STL + BLK + FGM + 3PM + FTM"),
    p("Formula used for malus is = (FGA-FGM) + (3PA-3PM) + (FTA-FTM) + TOV"),
    p("Results shown for overall season scores as well as per game average"),
    ),
    # Output
    mainPanel(dataTableOutput("player_stats"))
  ))



# Define server logic ----


server <- function(input, output) {
  # Inputs
  season_picked <- reactive({
    input$season
  })
  # Table
  output$player_stats = renderDataTable({
    builddffunct(season_picked())})
}

# Run the app ----
shinyApp(ui = ui, server = server)
