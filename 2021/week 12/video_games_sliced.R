library(tidyverse)
library(extrafont)
library(zoo)
library(scales)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')


#what games are the most played?
highest <- 
  games %>%
  mutate(time = zoo::as.yearmon(paste(year, month), "%Y %B")) %>%
  group_by(gamename) %>%
  summarise(peak_players = max(peak), time = time[which.max(peak)]) %>%
  top_n(10, peak_players) %>%
  arrange(desc(peak_players))

  
#what games are the most played LATELY?
highest_currently <-
  games %>%
  mutate(time = zoo::as.yearmon(paste(year, month), "%Y %B")) %>%
  filter(time == 'Feb 2021') %>%
  group_by(gamename) %>%
  summarise(current = max(avg)) %>%
  top_n(10, current) %>%
  arrange(desc(current))


#highest number of players in the first month
highest_firstmonth <- 
  games %>%
  mutate(time = zoo::as.yearmon(paste(year, month), "%Y %B")) %>%
  group_by(gamename) %>%
  summarise(first_month = min(time), 
            peak = peak[which.min(time)]) %>%
  top_n(10, peak) %>%
  arrange(desc(peak))


#Games at steam after COVID-19
highest_covid <-
  games %>%
  mutate(time = zoo::as.yearmon(paste(year, month), "%Y %B")) %>%
  filter(time >= 'Jan 2020') %>%
  group_by(gamename) %>%
  summarise(mean_avg = max(mean(avg))) %>%
  top_n(10, mean_avg) %>%
  arrange(desc(mean_avg))


###### plot ######

theme_set(theme_dark(base_family = "MV Boli"))

theme_update(plot.title = element_text(color = "white",
                                       size = 20, hjust = 0,
                                       margin = margin(0,0,0,0)),
             plot.title.position = "plot",
             plot.subtitle = element_text(color = "white", 
                                         size = 10,
                                         margin = margin(0,0,10,0)),
             plot.caption = element_text(color = "white", 
                                         size = 9,
                                         margin = margin(20, 0, -10, 0)),
             plot.caption.position = "plot",
             plot.background = element_rect(fill = "#252525"),
             panel.background = element_rect(fill = "transparent"),
             panel.grid.minor = element_line(color = "#252525"),
             panel.grid.major.y = element_line(color = "grey20"),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             axis.text = element_text(size = 13),
             strip.background = element_blank(),
             strip.text = element_text(colour = "white"),
             legend.position = "bottom",
             plot.margin = margin(30,30,30,30))

#higest peak
highest %>%
  ggplot(aes(y = peak_players, x = reorder(gamename, peak_players))) +
  geom_col(fill = "#e7298a",
           width = 0.6) +
  geom_text(aes(label = time),
            color = "#df65b0", size = 3, hjust = -0.15, vjust = 0.5,
            family = "MV Boli") +
  coord_flip() +
  scale_y_continuous(label = comma,
                     limits = c(0, 4.5e6),
                     expand = c(0.05, 0.05)) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  labs(title = "Top 10 Games at Steam",
       subtitle = "This graphic shows the highest peak number of games at Steam from the first month until February 2021 and its first month played",
       caption = "SteamChart | TidyTuesday - 2021 - Week 12 | Tiara Esy Pramukti") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey20"),
        axis.text.y = element_text(color = "#df65b0"))

ggsave("top_10_games_at_steam.png", width = 10, height = 6.5)


#highest first month
highest_firstmonth %>%
  ggplot(aes(y = peak, x = reorder(gamename, peak))) +
  geom_col(fill = "#99d8c9",
           width = 0.6) +
  geom_text(aes(label = first_month),
            color = "#ccece6", size = 3, hjust = -0.15, vjust = 0.5,
            family = "MV Boli") +
  coord_flip() +
  scale_y_continuous(label = comma,
                     limits = c(0, 1.5e6),
                     expand = c(0.05, 0.05)) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  labs(title = "Highest in the first month",
       subtitle = "This graphic shows the highest peak number of games at Steam in its first month played",
       caption = "SteamChart | TidyTuesday - 2021 - Week 12 | Tiara Esy Pramukti") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey20"),
        axis.text.y = element_text(color = "#ccece6"))

ggsave("highest_first_month.png", width = 10, height = 6.5)


#games at steam during covid
# games %>%
#   mutate(time = zoo::as.yearmon(paste(year, month), "%Y %B")) %>%
#   filter(gamename %in% highest_covid$gamename &
#          time >= "Jan 2020") %>%
#   mutate(game = ifelse(gamename == gamename[which.max(avg)], gamename, "others")) %>%
#   ggplot() +
#   geom_line(aes(x = time, y = avg, group = gamename, color = game, size = game)) +
#   #geom_point() +
#   scale_colour_manual(values = c('#9e9ac8', '#d9d9d9')) +
#   scale_size_manual(values=c(1.5,0.2))
