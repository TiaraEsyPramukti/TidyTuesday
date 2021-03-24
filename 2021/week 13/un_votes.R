library(tidyverse)
library(extrafont)
library(scales)


unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')


rollcalls_issues <-
  roll_calls %>%
  select(rcid, date) %>%
  left_join(issues %>% select(-short_name),
            by = "rcid")


theme_set(theme_minimal(base_family = "DejaVu Sans"))
theme_update(text = element_text(color = "#1a1a1a"),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "bottom",
             legend.title = element_blank(),
             panel.background = element_rect(fill = "transparent", 
                                             color = "transparent"),
             panel.border = element_blank(),
             panel.grid = element_blank(),
             plot.title = element_text(size = 20, hjust = 0,
                                       margin = margin(0,0,0,0)),
             plot.title.position = "plot",
             plot.subtitle = element_text(size = 10,
                                          margin = margin(0,0,10,0)),
             plot.caption = element_text(size = 6,
                                         margin = margin(20, 0, -10, 0)),
             plot.caption.position = "plot",
             plot.background = element_rect(fill = "#c6dbef"),
             plot.margin = margin(30,30,30,30))

        
# Issues over time

## 1st version
issue_percent <- 
  rollcalls_issues %>%
  filter(!is.na(issue)) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year, issue) %>%
  summarise(n = n()) %>%
  mutate(
    n = as.numeric(n),
    percent_issue = n / sum(n))


issue_percent %>%
  ggplot(aes(x = year, y = percent_issue, fill = issue)) +
  geom_col(width = 1) +
  scale_fill_manual(values = c('#40004b','#762a83','#8e0152','#c51b7d','#7fbc41','#276419')) +
  scale_x_continuous(name = "", breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), 
                     expand = c(0.02,0.05)) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "Issues over Time",
       subtitle = "This graphic shows percentage of vote issues started in 1946",
       caption = "Citizen Statistician | TidyTuesday - 2021 - Week 13 | Tiara Esy Pramukti")

ggsave("issues_over_time_1.png", width = 10, height = 5)



## 2nd version
issue_percent_group <- 
  rollcalls_issues %>%
  filter(!is.na(issue)) %>%
  mutate(year = case_when(
    format(date, "%Y") <= 1950 ~ "1950",
    format(date, "%Y") <= 1960 ~ "1960",
    format(date, "%Y") <= 1970 ~ "1970",
    format(date, "%Y") <= 1980 ~ "1980",
    format(date, "%Y") <= 1990 ~ "1990",
    format(date, "%Y") <= 2000 ~ "2000",
    format(date, "%Y") <= 2010 ~ "2010",
    format(date, "%Y") <= 2020 ~ "2020")) %>%
  group_by(year, issue) %>%
  summarise(n = n()) %>%
  mutate(
    n = as.numeric(n),
    percent_issue = n / sum(n))


issue_percent_group %>%
  ggplot(aes(x = as.numeric(year), y = percent_issue, fill = issue)) +
  geom_area() +
  scale_fill_manual(values = c('#40004b','#762a83','#8e0152','#c51b7d','#7fbc41','#276419')) +
  scale_x_continuous(name = "", breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), 
                     expand = c(0.02,0.05)) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "Issues over Time",
       subtitle = "This graphic shows percentage of vote issues with year grouping",
       caption = "Citizen Statistician | TidyTuesday - 2021 - Week 13 | Tiara Esy Pramukti")

ggsave("issues_over_time_2.png", width = 10, height = 5)


# Tiara Esy Pramukti