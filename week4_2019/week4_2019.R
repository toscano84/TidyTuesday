# week 4 2019 - TidyTuesday

library(tidyverse) # wrangle, visualization of data
library(data.table) # load file
library(albersusa) # map of all 50 states plus DC
library(broom) # in this case to tidy a shape file
library(ggalt) # create coordinate system in maps
library(viridis) # color palette
library(extrafont) # add fonts to R


options(scipen = 999)

# load file
incarceration <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-22/incarceration_trends.csv")

# tidy the dataframe
murder_crime_tbl <- incarceration %>% 
  mutate(decade = year -year %% 10) %>% # create variable decade)
  group_by(decade, state) %>% # group by decade and state
  mutate(crime = rowSums(cbind(violent_crime, murder_crime),na.rm=TRUE) 
         / total_pop * 100) %>% # create variable crime that corresponds to the sum of violent crime and murder crime
  summarize(crime_rate = mean(crime, na.rm = TRUE)) %>%
  filter(!state %in% c("AK", "AL") | decade != 1970)

#--- create map of the us states

us <- usa_composite() %>%
  tidy(., region = "iso_3166_2") # use tidy function from the broom package to create a dataframe of the us map

#---- create quantiles
number_quantiles <- 5
labels <- c()

quantiles <- quantile(murder_crime_tbl$crime_rate, 
                      probs = seq(0, 1, length.out = no_classes + 1))

# create custom labels for the quantiles
for(i in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[i], 2), 
                             " - ", 
                             round(quantiles[i + 1], 2)))
}
# Remove last label
labels <- labels[1:length(labels)-1]

# create new variable based on quantiles
murder_crime_tbl$crime_rate_quantiles <- cut(murder_crime_tbl$crime_rate, 
                                          breaks = quantiles, 
                                          labels = labels, 
                                          include.lowest = T)


#----------build plot: fill varriable correspond to the quantiles-----#
p <-  ggplot() +
  geom_map(data = us, map = us,
           aes(x = long, y = lat,
               map_id = id),
           color = "grey30",
           fill = NA) +
  geom_map(data = murder_crime_tbl, map = us,
           aes(fill = crime_rate_quantiles, map_id = state),
           color = "grey30") +
  scale_fill_viridis_d(option = "plasma",
                     name = "Violent\nand Murder Crime Rate",
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    )) +
  facet_wrap(vars(decade)) +
  coord_proj(us_laea_proj) +
  labs(title = "Violent and Murder Crime in the USA",
       subtitle = "Mean Rate (%) by decade ",
       x = "",
       y ="") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Cooper Black",
                                  size = 30, hjust = 0.5),
        plot.subtitle = element_text(family = "Cooper Black",
                                  size = 20, hjust = 0.5),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(family = "Cooper Black", size = 18, 
                                  face = "bold"),
        legend.text = element_text(family = "Cooper Black", size = 14),
        legend.title = element_text(family = "Cooper Black", size = 18),
        plot.background = element_rect(fill = "#e8e8ea"),
        panel.grid = element_line(color = "grey70"),
        legend.direction = "vertical",
        legend.position = c(0.85, 0.25))

ggsave("crime_usa.jpg", p, units = "cm", 
       height = 25, width = 40, dpi = "retina")

