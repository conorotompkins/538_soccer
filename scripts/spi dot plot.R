library(tidyverse)
library(purrr)
library(gganimate)
library(ggrepel)
library(broom)
library(lubridate)

theme_set(theme_bw())

data <- read_csv("data/spi_matches.csv")

df_home <- data %>% 
  select(team1, date, league, spi1) %>% 
  rename(team = team1,
         spi = spi1) %>% 
  mutate(venue = "home")

df_away <- data %>% 
  select(team2, date, league, spi2) %>% 
  rename(team = team2,
         spi = spi2) %>% 
  mutate(venue = "away")

df_all <- bind_rows(df_home, df_away) %>% 
  arrange(league, team, date) %>% 
  group_by(league) %>% 
  mutate(league_game_number = dense_rank(date)) %>% 
  filter(date < Sys.Date())

df_test <- df_all %>% 
  filter(!(str_detect(league, "UEFA"))) %>% 
  arrange(team, date) %>% 
  #summarize(spi = last(spi)) %>% 
  group_by(league) %>% 
  mutate(league_median = median(spi)) %>% 
  ungroup() %>% 
  mutate(league = fct_reorder(league, league_median))

df_test %>% 
  ggplot(aes(league, spi, color = league)) +
  geom_jitter(width = .3) +
  guides(color = FALSE) +
  coord_flip() +
  transition_time(league_game_number) +
  labs(subtitle = "Game number: {frame_time}")
