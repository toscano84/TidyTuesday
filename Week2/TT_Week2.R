# tidytuesday - week 2

# load needed libraries
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggalt)
library(grid)
library(png)
library(scales)
library(magick)
library(extrafont)

# import and load fonts
font_import()
loadfonts(device = "win")

# set theme
theme_set(theme_bw())

# load data
nfl_salaries <- read_excel("tidy_tuesday_week2.xlsx")

# tidy the dataframe
nfl_salaries2018 <- nfl_salaries %>%
  select_all(str_to_lower) %>% # lower the case of all columns
  filter(year == 2018) %>% # include only the year 2018
  rename_at(vars(-year, -cornerback, -quarterback, -linebacker, -safety),
            funs(sub(" ", "_", .))) %>% # rename columns with space between them with an underscore(_)
  gather(position, salaries, -year) # reshape data from wide to long
  

# dataframe with the top salary by position  
top_1 <- nfl_salaries2018 %>% 
  group_by(position) %>% 
  top_n(1) %>%
  unique()
View(top_1)

# dataframe with the bottom salary by position 
bottom_1 <- nfl_salaries2018 %>% 
  group_by(position) %>% 
  top_n(-1) %>%
  unique()
View(bottom_1)

# bind both datasets
tb_salaries2018 <- left_join(top_1, bottom_1, by = c("position", "year"), 
                         suffix = c("_top", "_bottom"))
# Create diff variable. Difference between top and bottom salaries
tb_salaries2018 <- tb_salaries2018 %>%
  mutate(diff = salaries_top - salaries_bottom)

#----plot the graph----#
# load image
nfl <- image_read("nfl_logo.png")

# geom_dumbbell
nfl_salary_diff <- ggplot() + 
  annotation_custom(rasterGrob(nfl), 
                    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_segment(data = tb_salaries2018 , aes(y = position, yend = position , 
                                         x = 0, xend = 1), 
               color = "#b2b2b2", size=0.75) +
  geom_dumbbell(data = tb_salaries2018, aes(y = position, x = salaries_bottom,
                                         xend = salaries_top),
                size = 2, 
                colour = "grey",
                size_x = 4, size_xend = 4,
                colour_x = "red", colour_xend = "#0000FF") +
  # text written above points
  geom_text(data = filter(tb_salaries2018, position == "wide_receiver"),
            aes(x = salaries_bottom, y = position, label = "Lowest Salary"),
            color = "red", size = 4.5, vjust = -2, 
            fontface= "bold", family = "Georgia") +
  geom_text(data = filter(tb_salaries2018, position == "wide_receiver"),
            aes(x = salaries_top, y = position, label = "Highest Salary"),
            color = "#0000FF", size = 4.5, vjust = -2, fontface = "bold", family = "Georgia") +
  # text written below points
  geom_text(data = tb_salaries2018, aes(x = salaries_bottom, y = position, 
                                        label = dollar(round(salaries_bottom))),
            color = "red", size = 4, vjust = 2.5, family = "Georgia", 
            fontface = "bold") +
  geom_text(data = tb_salaries2018, aes(x = salaries_top, y = position, label = dollar(round(salaries_top))),
            color = "#0000FF", size = 4, vjust = 2.5, family = "Georgia", fontface = "bold") +
  # column representing the difference between top and bottom salaries
  geom_rect(data = tb_salaries2018, aes(xmin = max(salaries_top * 1.07), 
                                        xmax = max(salaries_top * 1.2), 
                                   ymin = -Inf, ymax = Inf), fill = "grey") +
  geom_text(data = tb_salaries2018, aes(label = dollar(round(diff)), y = position,
                                        x = max(salaries_top * 1.13)), 
            fontface = "bold", size = 3.5, family = "Georgia") +
  geom_text(data = filter(tb_salaries2018, position == "wide_receiver"), 
            aes(x = max(salaries_top * 2.3), y = position, label = "DIFFERENCE"),
            vjust = -2.5, size = 4, fontface = "bold", family = "Cooper Black") +
  scale_y_discrete(labels = c("Cornerback","Defensive Lineman","Linebacker",
                              "Offensive Lineman","Quarterback","Running Back",
                              "Safety","Special Teamer","Tight End",
                              "Wide Receiver")) +
  labs(x = NULL, 
       y = NULL, 
       title = "NFL Salary Differences by Position", 
       subtitle = "Year 2018") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Georgia"),
        plot.subtitle = element_text(hjust=0.5, face = "italic", family = "Georgia"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(family = "Cooper Black", face = 
                                     "bold", size = 13, color = "grey"))

nfl_salary_diff
