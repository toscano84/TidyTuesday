# Tidy Tuesday Week 33
library(tidyverse)
library(data.table)
library(viridis)
library(extrafont)
library(extrafont)
library(gganimate)

options(scipen = 999)

# import and load fonts
font_import()
loadfonts(device = "win")

# load data frame
malaria <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-13/malaria_deaths_age.csv")

# five countries with more malaria mortality
top_5 <- malaria %>%
  rename(country = entity) %>%
  filter(!str_detect(country, pattern = c("Sub-Saharan|Middle East|World|Low SDI
                                          |SDI|Asia"))) %>%
  group_by(country) %>%
  summarize(deaths = sum(deaths)) %>%
  top_n(5, deaths)


#------plot-----#
plot_ani <- malaria %>%
  group_by(year, entity) %>%
  summarise(sum_deaths = sum(deaths)) %>%
  filter(entity %in% top_5$country) %>%
  mutate(entity = case_when(entity == 
                              "Democratic Republic of Congo" ~ "DR Congo",
                            TRUE ~ entity)) %>%
  ggplot(aes(year, sum_deaths, group = entity)) + 
  geom_line(aes(colour = entity), size = 3) + 
  geom_segment(aes(xend = 2016, yend = sum_deaths, colour = entity), linetype = 2) + 
  geom_point(size = 2) +
  scale_color_viridis_d() +
  geom_text(aes(x = 2016.5, label = entity,
                colour = entity),
            nudge_x = 0.5,
            family = "Agency FB",
            size = 8, hjust = 0) + 
  scale_y_continuous(breaks = seq(0, 350000, by = 50000),
                     labels = c("0", "50", "100", "150",
                                "200", "250", "300", "350")) +
  scale_x_continuous(limits = c(1990, 2025),
                     breaks = seq(1990, 2015, by = 5),
                     labels = c("'90", "'95", "'00", "'05",
                                "'10", "'15")) +
  labs(title = 'The Five Countries with more Malaria Mortality', 
       subtitle = "Between 1990 and 2016", y = 'Deaths (thousands)',
       X = "Year") + 
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(family = "Agency FB", 
                                  size = 24),
        plot.subtitle = element_text(family = "Agency FB", 
                                     size = 18),
        axis.title = element_text(family = "Agency FB", 
                                  size = 20, hjust = 0.5),
        axis.text = element_text(family = "Agency FB",
                                 size = 16)) +
  transition_reveal(entity, year)  


plot_ani
