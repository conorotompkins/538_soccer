library(robotstxt)
library(rvest)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)


paths_allowed(
  paths = c("https://projects.fivethirtyeight.com/soccer-predictions/champions-league/")
)


fivethirtyeight <- read_html("https://projects.fivethirtyeight.com/soccer-predictions/champions-league/")

tables <- fivethirtyeight %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  .[[250]]


fivethirtyeight %>% 
  html_nodes("forecast-selector")
