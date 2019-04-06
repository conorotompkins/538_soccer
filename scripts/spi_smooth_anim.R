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
  group_by(league, team) %>% 
  mutate(team_game_number = dense_rank(date))

df_epl <- df_all %>% 
  filter(date < Sys.Date(),
         league == "Barclays Premier League")

df_epl_smooth <- df_epl %>%
  nest(-team) %>% 
  mutate(m = map(data, loess,
                          formula = spi ~ team_game_number, span = .5),
         spi_smooth = purrr::map(m, `[[`, "fitted"))

df_epl_smooth <- df_epl_smooth %>% 
  select(-m) %>% 
  unnest()

df_epl_last <- df_epl %>% 
  group_by(team) %>% 
  summarize(date = last(date),
            spi = last(spi))

spi_smooth_gif <- df_epl_smooth %>% 
  ggplot(aes(date, spi_smooth, color = team, group = team)) +
  geom_line() +
  geom_point() +
  geom_segment(aes(xend = ymd("2019-04-05"), yend = spi_smooth), linetype = 2, colour = 'grey') +
  geom_label(aes(ymd("2019-04-05"), label = team),
             hjust = 1,
             vjust = 0) +
  #geom_label_repel(aes(date + 5, label = team),
  #                 direction = "x",
  #                 hjust = 1,
  #                 vjust = 0,
  #                 max.iter = 500) +
  guides(color = FALSE) +
  transition_reveal(date) +
  coord_cartesian(clip = 'off')

animate(spi_smooth_gif, height = 900, width = 900, duration = 30)
anim_save("output/spi_smooth.gif", height = 9, width = 100)

