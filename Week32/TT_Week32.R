# Tidy Tuesday week 32
library(tidyverse)
library(ggmap)
library(data.table)
library(extrafont)
library(hrbrthemes)


# import and load fonts
font_import()
loadfonts(device = "win")

# load file

wind_us <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-06/us_wind.csv")


# explore data - find top 5 manufacturers

wind_us %>%
  count(t_manu, sort = TRUE) %>%
  filter(t_manu != "missing") %>%
  slice(1:5)



#---plot ggmap---#
# create us map
us <- c(left = -125, bottom = 24, right = -66, top = 50)
map <- get_stamenmap(us, zoom = 5, maptype = "watercolor")
map <- ggmap(map)


#-final plot-#
map + geom_point(aes(xlong, ylat, color = t_manu),
                 data = filter(wind_us, t_manu %in% c("GE Wind", 
                                                       "Vestas",
                                                       "Siemens",
                                                       "Mitsubishi",
                                                       "Gamesa")), # top 5 maufacturers
                 alpha = 0.6, 
                 size = 5) +
  labs(title = "US Wind Turbines", subtitle = "Top 5 Manufacturers",
       color = NULL) +
  scale_color_manual(values = c("#E47917",
                                "#3874BA",
                                "#EA0033",
                                "#1FA0A0",
                                "#104277")) +
  hrbrthemes::theme_modern_rc() +
  theme(plot.title = element_text(family = "Agency FB", size = 30),
        plot.subtitle = element_text(family = "Agency FB", size = 24),
        legend.title = element_text(family = "Agency FB", size = 20),
        legend.text = element_text(family = "Agency FB", size = 18),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "grey50"))
