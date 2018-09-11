# Tidy Tuesday Week 24

# needed libraries
library(tidyverse)
library(viridis)
library(extrafont)
library(statebins)
library(hrbrthemes)

# import and load fonts
font_import()
loadfonts(device = "win")

# load file
cats_vs_dogs <- read_delim("cats_vs_dogs.txt", delim = ",") 

#---plot statebin map---#
bin_map_dogs <- cats_vs_dogs %>%
  ggplot(aes(state = state, fill = percent_dog_owners)) +
  geom_statebins() +
  coord_equal() +
  scale_fill_viridis(
    name = "Percentage of Dog Owners",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5)) + 
  labs(title="Percentage of Dog Owners in the USA", 
       caption = "\nDataSource: https://data.world/datanerd/cat-vs-dog-popularity-in-u-s") +
  theme_ft_rc(grid="") +
  theme(axis.text=element_blank(), 
        plot.title = element_text(family = "mono",
                                  hjust = 0.5)) +
  theme(legend.position = "bottom")

bin_map_dogs

