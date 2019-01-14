# week 2 2019 - TidyTuesday

# libraries needed
library(tidyverse)
library(data.table)
library(ggrepel)
library(lubridate)
library(ggdark)
library(ggrepel)
library(extrafont)

# import and load fonts
font_import()
loadfonts(device = "win")

# open the file
imdb_ratings <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

# glimpse the dataframe
glimpse(imdb_ratings)

# tidy the file
imdb_ratings_tidy <- imdb_ratings %>%
  mutate(year = year(date), # create variable year
         decade = year - year %% 10) # create variable decade


# create variable above_below_median grouped by year
imdb_ratings_tidy <- imdb_ratings_tidy %>%
  group_by(year) %>%
  mutate(above_below_median = ifelse(av_rating > median(av_rating), 
                               1, 0))

# create a dataframe with highest average tv_ratings per decade
imdb_top_perdecade <- imdb_ratings_tidy %>%
  group_by(decade) %>%
  top_n(1, wt = av_rating)

#----plot----#
plot <- imdb_ratings_tidy %>%
  group_by(year) %>%
  ggplot(aes(x = factor(year),
             y= av_rating, label = title)) +
  geom_jitter(aes(color = factor(above_below_median)), size = 3, alpha = 0.3) +
  geom_boxplot(alpha = 0, color = "grey50", outlier.colour = "red") +
  geom_label_repel(data = imdb_top_perdecade, # label with the top show per decade
                   fontface = "bold",
                   color = "grey50",
                   size = 6,
                   family = "Agency FB",
                   direction = "x") +
  stat_summary(fun.y = median, geom = "line", aes(group = 1), 
               linetype = 1, colour = "grey50", size = 1) # create a line based on the median +
  dark_theme_gray() +
  theme(plot.title = element_text(family = "Agency FB", face = "bold",
                                  size = 40, hjust = 0.5),
        plot.subtitle = element_text(family = "Agency FB", face = "bold",
                                     size = 20, hjust = 0.5),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(family = "Agency FB", size = 15),
        axis.text = element_text(family = "Agency FB", size = 15),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(family = "Agency FB", size = 15),
        legend.text = element_text(family = "Agency FB", size = 15),
        legend.title = element_text(family = "Agency FB", size = 15)) +
  scale_color_manual(values = c("#d35400", "#1abc9c"), labels = c("Below the Median", 
                                                                  "Above the Median")) + 
  labs(title = "Average Rating of Tv Shows",
       subtitle = "Parenthood, Third Watch and Breaking Bad had\nthe highest average rating for each decade", 
       x = "Year", 
       y = "Average Rating", 
       color = "Shows") +
  facet_wrap(vars(decade), ncol = 3, scales = "free_x")

plot

ggsave("av_ratings_imdb.jpg", plot, units = "cm", 
       height = 25, width = 40, dpi = "retina")
