library(tidyverse)
library(googledrive)
library(googlesheets4)
library(readxl)
library(janitor)
library(lubridate)
library(ggrepel)
library(gganimate)
library(scales)
library(ggimage)

theme_set(theme_minimal())

path <- drive_download("538 Champions League Probabilities", overwrite = TRUE) %>% 
  pull(local_path)

data <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(read_excel, skip = 2, col_types = "text", path = path, .id = "path")

data

df <- data %>% 
  clean_names() %>%
  select(date = path, team, spi, win_final) %>% 
  mutate(date = ymd(date),
         win_final = str_replace_all(win_final, "<1%", "0.001"),
         win_final = str_replace_all(win_final, "—", "NA"),
         win_final = as.numeric(win_final),
         spi = as.numeric(spi))

df <- df %>%
  mutate(team = str_remove_all(team, "[[:digit:]]"),
         team = str_remove(team, " pts"),
         team = str_remove(team, " pt"),
         team = case_when(str_detect(team, "Schalke") ~ "Schalke 04",
                          !str_detect(team, "Schalke") ~ team)) %>% 
  arrange(team, date) %>% 
  mutate(id = str_c(team, date)) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup()

df %>% 
  count(team, sort = TRUE) %>%
  summarize(check = mean(n == max(n)))

#df %>% 
#  count(team, sort = TRUE) %>%
#  View()

df %>% 
  select(id, date, team, win_final) %>% 
  group_by(date) %>% 
  top_n(10, win_final) %>% 
  arrange(date, desc(win_final))


club_logos <- tibble(team = c("Barcelona",
                              "Man. City",
                              "Real Madrid",
                              "Bayern Munich",
                              "Liverpool",
                              "Juventus",
                              "Atlético Madrid",
                              "PSG",
                              "Tottenham",
                              "Napoli",
                              "Roma",
                              "Man. United",
                              "Porto",
                              "Dortmund",
                              "Galatasary",
                              "Valencia",
                              "Inter Milan",
                              "Benfica",
                              "Lyon",
                              "Ajax",
                              "Shakhtar",
                              "Young Boys",
                              "Schalke 04",
                              "Hoffenheim",
                              "Club Brugge",
                              "CSKA Moscow",
                              "Monaco",
                              "AEK Athens",
                              "PSV",
                              "Red Star",
                              "Lokomotiv",
                              "Viktoria Plzen"),
                     url = c("https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/83.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/382.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/86.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/132.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/364.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/111.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/1068.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/160.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/367.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/114.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/104.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/360.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/437.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/124.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/432.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/94.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/110.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/1929.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/167.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/139.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/493.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/2722.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/133.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/7911.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/570.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/1963.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/174.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/887.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/148.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/2290.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/442.png&w=56",
                             "https://secure.espn.com/combiner/i?img=/i/teamlogos/soccer/500/11706.png&w=56"))



df_graph <- df %>% 
  semi_join(club_logos) %>% 
  left_join(club_logos) #%>% 
  #filter(date <= "2019-04-01")

df_graph %>% 
  count(team, url, sort = TRUE)

df_graph %>% 
  ggplot(aes(team, spi)) +
  geom_image(aes(image = url)) +
  coord_flip()

df_graph %>% 
  ggplot(aes(date, win_final, group = team)) +
  geom_line(aes(color = team, size = spi)) +
  geom_point(aes(color = team)) +
  #geom_image(aes(x = ymd("2019-07-01"), image = url), size = .05) +
  geom_image(data = df_graph %>% filter(date == last(date)),
             aes(image = url)) +
  geom_label(data = df_graph %>% filter(date == last(date)),
             aes(x = last(df_graph$date) + 40, label = team),
             hjust = -.1,
             vjust = 0) +
  scale_size_continuous(range = c(.1, 3)) +
  guides(color = FALSE) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(5.5, 110, 5.5, 5.5))


timeline <- df_graph %>% 
  ggplot(aes(date, win_final, group = team)) +
  geom_line(aes(color = team)) +
  geom_point(aes(color = team)) +
  #geom_image(aes(x = ymd("2019-07-01"), image = url), size = .05) +
  geom_image(aes(image = url), size = .05) +
  geom_label(aes(x = last(df_graph$date) + 40, label = team),
             hjust = -.1,
             vjust = 0) +
  scale_size_continuous(range = c(.1, 3)) +
  guides(color = FALSE) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(5.5, 110, 5.5, 5.5))
timeline

timeline_gif <- timeline +
  transition_reveal(date)

gif_duration <- 10

animate(timeline_gif, height = 450, width = 1200, duration = gif_duration, nframes = gif_duration * 20, end_pause = 40)
anim_save("output/champions_league_win_prob.gif")


df_graph %>% 
  ggplot(aes(spi, win_final)) +
  geom_point() +
  geom_image(aes(image = url), size = .05) +
  #geom_smooth() +
  transition_reveal(date)

