# week 1 2019 - TidyTuesday

# load needed libraries
library(tidyverse) # wrangle and visualize the data
library(lubridate) # deal with dates
library(viridis) # color palette
library(extrafont) # add fonts to R

# open file
tweets <- readRDS("rstats_tweets.rds")


# tidy the dataframe
tweets_tidy <- tweets %>%
  mutate(year = year(created_at), # create variable year 
         weekday = wday(created_at, label = TRUE),
         weekday = fct_relevel(weekday, "Mon",
                               "Tue",
                               "Wed",
                               "Thu",
                               "Fri",
                               "Sat",
                               "Sun"), # create variable day of the week and relevel it
         month = month(created_at, label = TRUE), # create variable month
         week = week(created_at)) %>% # create variable week of the year
  filter(year > 2013) %>% # only include last 5 years
  count(year, weekday, week, month)

## plot ##
p <- tweets_tidy %>%
  ggplot(aes(week, weekday, fill = n)) +
  geom_tile(colour = "grey30") +
  facet_grid(year ~ month, scales = "free") + # divide by month and year
  scale_fill_viridis(name = "Number of Tweets",
                     option = "plasma", guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(3.5, units = "mm"),
    barwidth = unit(50, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = 0.5)) + # manipulate dimensions of the legend's scale 
  labs(title = "#rstats Tweets in the last 5 years",
       x = "", y = "") +
  theme(plot.title = element_text(family = "Cooper Black",
                                  size = 30, hjust = 0.5),
        plot.background = element_rect(fill = "#d0d3d4"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour = "black",
                                   family = "Cooper Black", size = 12),
        axis.text.x = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(family = "Cooper Black", size = 18, face = "bold"),
        legend.text = element_text(family = "Cooper Black", size = 15),
        legend.title = element_text(family = "Cooper Black", size = 15),
        legend.position = "bottom") 

ggsave("rstats.jpg", p, units = "cm", height = 20, width = 40, dpi = "retina")

