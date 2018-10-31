# Tidy Tuesday Week 31
library(tidyverse)
library(data.table)
library(lubridate)
library(viridis)
library(extrafont)
library(ggridges)
library(hrbrthemes)

options(scipen = 999)

# import and load fonts
font_import()
loadfonts(device = "win")

#open data frame
rdownloads <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-30/r_downloads_year.csv")


#--------plot--------#
# downloads per month
r_months <- rdownloads %>%
  filter(os %in% c("win", "osx")) %>% # only include windows and mac OS
  count(date = as.Date(date), os) %>%
  mutate(month = lubridate::month(date), # convert date to month
         os = case_when(os == "win" ~ "Windows",
                        os == "osx" ~ "Mac OS")) %>% # change the names of cases within the os variable
  ggplot(aes(x = n, y = factor(month), fill = month)) +
  geom_density_ridges_gradient(alpha = 0.3, color = "white",
                               scale = 3, size = 0.4, rel_min_height = 0.02) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0), labels = c("Jan", 
                                                   "Feb", 
                                                   "Mar", 
                                                   "Apr", 
                                                   "May", 
                                                   "Jun", 
                                                   "Jul", 
                                                   "Aug", 
                                                   "Sep", 
                                                   "Oct", 
                                                   "Nov", 
                                                   "Dec")) +
  scale_fill_viridis(option = "inferno") +
  labs(title = 'R Downloads',
       subtitle = 'Distribution of R downloads per Month\nand operating System',
       x = NULL, y = NULL) +
  hrbrthemes::theme_ipsum_rc() +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        axis.text = element_text(family = "Bauhaus 93", size = 12),
        strip.text = element_text(family = "Bauhaus 93", size = 16),
        plot.title = element_text(family = "Bauhaus 93", size = 24),
        plot.subtitle = element_text(family = "Bauhaus 93", size = 18)) +
  facet_wrap(~ os)

r_months

# downloads per day
r_day <- rdownloads %>%
  filter(os %in% c("win", "osx")) %>% # only include windows and mac OS
  count(date = as.Date(date), os) %>%
  mutate(day = lubridate::wday(date), # convert date to day of the week
         os = case_when(os == "win" ~ "Windows",
                        os == "osx" ~ "Mac OS")) %>% # change the names of cases within the os variable
  ggplot(aes(x = n, y = factor(day), fill = day)) +
  geom_density_ridges_gradient(alpha = 0.3, color = "white",
                               scale = 3, size = 0.4, rel_min_height = 0.02) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0), labels = c("Sun", 
                                                   "Mon", 
                                                   "Tue", 
                                                   "Wed", 
                                                   "Thu", 
                                                   "Fri", 
                                                   "Sat")) +
  scale_fill_viridis(option = "inferno") +
  labs(title = 'R Downloads',
       subtitle = 'Distribution of R downloads per day\nand operating System',
       x = NULL, y = NULL) +
  hrbrthemes::theme_ipsum_rc() +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        axis.text = element_text(family = "Bauhaus 93", size = 12),
        strip.text = element_text(family = "Bauhaus 93", size = 16),
        plot.title = element_text(family = "Bauhaus 93", size = 24),
        plot.subtitle = element_text(family = "Bauhaus 93", size = 18)) +
  facet_wrap(~ os)

r_day


