# week 3 tidytuesday 2019

library(tidyverse) # wrangle and visualize data
library(data.table) # in this case to open the file with the function fread
library(lubridate) # manipulate dates and times
library(ggdark) # theme for plots
library(extrafont) # add new fonts to base R

# open file
space_launches <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv")


# create new dataframe with new variables
space_launches_us <- space_launches %>%
  filter(state_code == "US") %>% #only include usa launches
  mutate(administration = case_when(between(launch_year, 1957, 1960) ~ "Eisenhower",
                                    between(launch_date, "1961-01-20", "1963-11-22") ~ "Kennedy",
                                    between(launch_date, "1963-11-23", "1969-01-19") ~ "Johnson",
                                    between(launch_date,"1969-01-20", "1974-08-09") ~ "Nixon",
                                    between(launch_date,"1974-08-10", "1977-01-19") ~ "Ford",
                                    between(launch_date,"1977-01-20", "1981-01-19") ~ "Carter",
                                    between(launch_date,"1981-01-20", "1989-01-19") ~ "Reagan",
                                    between(launch_date,"1989-01-20", "1993-01-19") ~ "Bush I",
                                    between(launch_date,"1993-01-20", "2001-01-19") ~ "Clinton",
                                    between(launch_date,"2001-01-20", "2009-01-19") ~ "Bush II",
                                    between(launch_date,"2009-01-20", "2017-01-19") ~ "Obama",
                                    between(launch_date,"2017-01-20", "2019-01-17") ~ "Trump"
                                    ), # create variable based on presidential administrations
         party = case_when(administration %in% c("Kennedy", "Johnson", "Carter", "Clinton", "Obama") ~ "Democratic",
                           administration %in% c("Eisenhower", "Nixon", "Ford", "Reagan", "Bush I", "Bush II",
                                                 "Trump") ~ "Republican")) %>% # party variable
  drop_na(administration) # delete missing values in the variable administration



##----plot------##
# function created to have separate breaks due to the use of facets in the plot
breaks_created <- function(x) { 
  if (max(x) < 1961) seq(1957, 1960, 1) 
  else if (max(x) < 1964) seq(1960, 1964, 1)
  else if (max(x) < 1969) seq(1963, 1969, 1)
  else if (max(x) < 1975) seq(1969, 1974, 1)
  else if (max(x) < 1977) seq(1974, 1977, 1)
  else if (max(x) < 1981) seq(1977, 1981, 1)
  else if (max(x) < 1989) seq(1981, 1989, 1)
  else if (max(x) < 1994) seq(1989, 1994, 1)
  else if (max(x) < 2001) seq(1993, 2001, 1)
  else if (max(x) < 2010) seq(2001, 2009, 1)
  else if (max(x) < 2018) seq(2009, 2017, 1)
  else (seq(2017, 2018, 1))}

# plot creation
p <- space_launches_us %>%
  group_by(launch_year, party, administration) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = launch_year, y = n, fill = party)) +
  geom_area() +
  dark_theme_grey() +
  labs(title = "Space Launches during each Presidential Administration", 
       subtitle =, x = "", y = "") +
  scale_fill_manual(name = "Party", values = c("navyblue", "red"), 
                    labels = c("Democratic", "Republican")) +
  scale_x_continuous(breaks = breaks_created) +
  theme(plot.title = element_text(family = "Agency FB", face = "bold",
                                  size = 30, hjust = 0.5),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", size = 0.2),
        panel.grid.minor = element_line(color = "grey20", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Agency FB", size = 15),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(family = "Agency FB", size = 20, face = "bold"),
        legend.text = element_text(family = "Agency FB", size = 15),
        legend.title = element_text(family = "Agency FB", size = 15)) +
  facet_wrap(vars(fct_reorder(administration, launch_year)), ncol = 3, scales = "free_x")

p

ggsave("plot_us_space.jpg", p, units = "cm", 
       height = 25, width = 40, dpi = "retina")




  
 
