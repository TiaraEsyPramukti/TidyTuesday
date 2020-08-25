##### --------------------------------------------------------------------------
## Tidytuesday - Week 35 - Chopped
## Tiara Esy Pramukti
##### --------------------------------------------------------------------------


library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(extrafont)

theme_update(
  text = element_text(family = "Consolas"),
  axis.line = element_line(size = 1),
  axis.title.x = element_text(family = "Consolas", size = 16,
                              margin = margin(15,0,0,0)),
  axis.title.y = element_text(family = "Consolas", size = 16,
                              margin = margin(0,15,0,0)),
  axis.text = element_text(size = 13),
  panel.background = element_rect(fill = "beige"),
  #panel.grid = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(color = "grey90"),
  panel.grid.minor.y = element_blank(),
  plot.background = element_rect(fill = "beige"),
  plot.title = element_text(size = 20, hjust = .5,
                            margin = margin(0,0,10,0)),
  plot.title.position = "plot",
  plot.caption = element_text(color = "grey40", 
                              size = 9,
                              margin = margin(20, 0, -20, 0)),
  plot.caption.position = "plot",
  plot.margin = margin(30,30,25,30),
  strip.background = element_blank(),
  strip.text = element_text(colour = "black")
)


chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

chopped_average <-
  chopped %>%
  group_by(season) %>%
  mutate(avg_rating = mean(episode_rating, na.rm = T)) %>%
  unique()


## Episode rating --------------------------------------------------------------

p <- chopped_average %>%
  mutate(season = as.factor(season)) %>%
  ggplot(aes(x = series_episode, y = episode_rating)) 

for (i in 1:45) {
  p <- p + geom_segment(
    data = chopped_average %>% filter(season == i),
    aes(x = min(series_episode)-0.5, xend = max(series_episode)+0.5,
        y = avg_rating, yend = avg_rating),
    color = "#dfc27d",
    size = 1.5) 
  }

p <- p +
  geom_segment(aes(yend = avg_rating, xend = series_episode), 
               color = "#dfc27d") +
  geom_point(aes(col = season), show.legend = F) +
  ylim(5,10.5) +
  labs(title = "Chopped TV Game Show Ratings",
       x = "Series Episode",
       y = "Rating",
       caption = "Tidytuesday - Chopped\nby Tiara Esy Pramukti")
p

ggsave("episode_rating.png", width = 11, height = 6)


## Appetizer Ingredients -------------------------------------------------------

chopped %>%
  unnest_tokens(appetizer, appetizer, token = 'regex', pattern=", ") %>%
  select(appetizer) %>%
  transmute(
    appetizer = str_to_title(appetizer),
    appetizer = str_remove_all(appetizer, "\\s")
  ) %>%
  count(appetizer, sort = TRUE) %>%
  filter(n >= 2) %>%
  ggplot() +
  geom_text_wordcloud(aes(label = appetizer, size = n,
                          color = n, grid_margin = 3)) +
  scale_color_gradient2(low = "#d0d1e6",
                        mid = "#3690c0",
                        high = "#023858",
                        midpoint = 4,
                        guide = F) +
  labs(title = "Most Used Ingredients in Appetizer",
       caption = "Tidytuesday - Chopped\nby Tiara Esy Pramukti") +
  theme(axis.line = element_blank())

ggsave("appetizer_ingredients.png", width = 11, height = 7)


## Entree Ingredients ----------------------------------------------------------

chopped %>%
  unnest_tokens(entree, entree, token = 'regex', pattern=", ") %>%
  select(entree) %>%
  transmute(
    entree = str_to_title(entree),
    entree = str_remove_all(entree, "\\s")
  ) %>%
  count(entree, sort = TRUE) %>%
  filter(n >= 2) %>%
  ggplot() +
  geom_text_wordcloud(aes(label = entree, size = n,
                          color = n, grid_margin = 3)) +
  scale_color_gradient2(low = "#bfd3e6",
                        mid = "#8c6bb1",
                        high = "#4d004b",
                        midpoint = 4,
                        guide = F) +
  labs(title = "Most Used Ingredients in Entree",
       caption = "Tidytuesday - Chopped\nby Tiara Esy Pramukti") +
  theme(axis.line = element_blank())

ggsave("entree_ingredients.png", width = 11, height = 7)


## Dessert Ingredients ---------------------------------------------------------

chopped %>%
  unnest_tokens(dessert, dessert, token = 'regex', pattern=", ") %>%
  select(dessert) %>%
  transmute(
    dessert = str_to_title(dessert),
    dessert = str_remove_all(dessert, "\\s")
  ) %>%
  count(dessert, sort = TRUE) %>%
  filter(n >= 2) %>%
  ggplot() +
  geom_text_wordcloud(aes(label = dessert, size = n,
                          color = n, grid_margin = 3)) +
  scale_color_gradient2(low = "#fcc5c0",
                        mid = "#dd3497",
                        high = "#49006a",
                        midpoint = 4,
                        guide = F) +
  labs(title = "Most Used Ingredients in Dessert",
       caption = "Tidytuesday - Chopped\nby Tiara Esy Pramukti") +
  theme(axis.line = element_blank())

ggsave("dessert_ingredients.png", width = 11, height = 7)




##### --------------------------------------------------------------------------
## Inspired by Cédric Scherer
##### --------------------------------------------------------------------------

  