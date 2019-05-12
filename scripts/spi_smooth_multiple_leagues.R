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
  mutate(number = dense_rank(date))

top_leagues <- df_all %>% 
  group_by(league) %>% 
  summarize(spi_median = median(spi)) %>% 
  arrange(desc(spi_median)) %>% 
  filter(!str_detect(league, "UEFA")) %>% 
  top_n(5) %>% 
  select(-spi_median)

df_top_leagues <- df_all %>% 
  semi_join(top_leagues)
  filter(date < Sys.Date(),
         league == "Barclays Premier League")

df_smooth <- df_top_leagues %>%
  nest(-team) %>% 
  mutate(m = map(data, loess,
                 formula = spi ~ number, span = .5),
         spi_smooth = purrr::map(m, `[[`, "fitted"))

df_smooth <- df_smooth %>% 
  select(-m) %>% 
  unnest()

spi_smooth_gif <- df_smooth %>% 
  ggplot(aes(number, spi_smooth, color = team, group = team)) +
  geom_line() +
  geom_point() +
  #geom_segment(aes(xend = 885, yend = spi_smooth), linetype = 2, colour = 'grey') +
  #geom_label(aes(885, label = team),
  #           hjust = 1,
  #           vjust = 0) +
  facet_wrap(~league, ncol = 1) +
  #geom_label_repel(aes(date + 5, label = team),
  #                 direction = "x",
  #                 hjust = 1,
  #                 vjust = 0,
  #                 max.iter = 500) +
  guides(color = FALSE) +
  transition_reveal(number) +
  coord_cartesian(clip = 'off')

animate(spi_smooth_gif, height = 900, width = 900, duration = 10, nframes = 10)
#anim_save("output/spi_smooth.gif", height = 9, width = 100)

