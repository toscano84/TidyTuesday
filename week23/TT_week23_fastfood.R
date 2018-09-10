# Tidy Tuesday Week 23

# needed libraries
library(tidyverse)
library(ggthemes)
library(readr)

# load file
fast_food <- read_delim("fastfood_calories.txt", delim = ",")


# function needed to use geom_flat_violin - see code here https://github.com/tidyverse/ggplot2/issues/2459
"%||%" <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }

GeomFlatViolin <-
  ggproto(
    "GeomFlatViolin",
    Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )


#-----------plot-----------#

plot_tt23 <- fast_food %>%
  #only include Chicken in the plot
  filter(str_detect(item, "Chicken|chicken")) %>%
  # to upper case the cases in the variable restaurant
  mutate_at(vars(restaurant), funs(toupper)) %>%
  ggplot(aes(x = factor(restaurant), y = calories, 
             fill = factor(restaurant))) +
  # use of geom_flat_violin
  geom_flat_violin(position = position_nudge(x = .2, y = 0),
                   alpha = .6) +
  geom_point(aes(y = calories, color = factor(restaurant)), 
             size = 2, alpha = 0.8) +
  # add mean
  stat_summary(fun.y = "mean", geom = "point", 
               color = "black", shape = 23,
               size = 3) +
  geom_boxplot(width = .2, guides = FALSE, 
               outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE) +
  scale_color_manual(values = c("#DA1620", "#0066b3", "#FFAA00", "#F7AD56", 
                                "#FF0017", "#FCDD2A","#038D43", "#682A8A")) +
  scale_fill_manual(values = c("#DA1620", "#0066b3", "#FFAA00", "#F7AD56", 
                               "#FF0017", "#FCDD2A","#038D43", "#682A8A")) +
  labs(title = "Fast Food Restaurants", 
       subtitle = "Chicken Calories", color = "Restaurant") +
  coord_flip() +
  theme_fivethirtyeight() 

plot_tt23
ggsave("Fast_Food.jpg", plot_tt23, width = 30, height = 20, unit = "cm")
