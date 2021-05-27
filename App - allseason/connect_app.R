rm(list = ls())
library(rsconnect)
library(tidyverse) 
library(readr)
library(shiny)
library(rvest)
library(janitor)
library(xml2)

setwd('/Users/angeledelevoye/Documents/GitHub/Shiny-Apps-WNBA/App - allseason')

getwd()

rsconnect::setAccountInfo(name='angeledelevoye', 
                          token='A1193B4BCEB3977E967F9F7962D749B6', 
                          secret='pEYgUWeufwTvuoSsT83V8TIUZohXvY0ZzaebSVm/')

rsconnect::deployApp(appName = 'app_-_allseason')

rsconnect::showLogs(appName="app_-_allseason",streaming=TRUE)
