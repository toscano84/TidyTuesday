#tidytuesday - week 1

# load needed libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(fiftystater)
library(stringr)
library(jpeg)
library(grid)
library(magick)
library(scales)

# load data
avg_tuition <- read_excel("us_avg_tuition.xlsx")

#==data preparation for graph 1==#
# create new dataframe
avg_tuition_tidy <- avg_tuition %>% 
  gather("Year", "Tuition", -State) # reshape data from wide to long

# filter data
avg_tuition_tidy_filter <- avg_tuition_tidy %>%
  filter(Year == "2015-16")# include only values of the year 2015-16

# create new variable - ranking
avg_tuition_tidy_filter$ranking <- rank(-avg_tuition_tidy_filter$Tuition) # create the variable ranking in relation to the value of tuition

#lower the case of State variable
avg_tuition_tidy_filter$State <- str_to_lower(avg_tuition$State)#to put the states with lower case as the map database

# lower the cases of all column names
avg_tuition_tidy_filter <- avg_tuition_tidy_filter %>%
  select_all(str_to_lower)

# load dataframe for the map of the USA
usa_map <- fifty_states # assign fifty_states to a new dataframe called usa_map

#join the dataframes
usa_map_new <- usa_map %>% 
  left_join(avg_tuition_tidy_filter, by = c("id" = "state"))

#==Create plot 1==#
# first step - create a variable to center the values of the longitude and latitude so that
# the rank is mapped at the center of each state
center_rank_values <- data.frame(region=tolower(state.name), 
                                 long=state.center$x, lat=state.center$y)

# second step - join the dataframes
rank_text <-  usa_map_new %>%
  select(id, ranking) %>%
  left_join(center_rank_values, by = c("id" = "region"))

# set theme for the graph
theme_set(theme_bw())

#given that the centering did not work for alaska and hawaii, I will change these values "manually"
# compute the means of both states for long and lat
with(usa_map_new, mean(long[id == "alaska"], na.rm = TRUE))
with(usa_map_new, mean(lat[id == "alaska"], na.rm = TRUE))
with(usa_map_new, mean(long[id == "hawaii"], na.rm = TRUE))
with(usa_map_new, mean(lat[id == "hawaii"], na.rm = TRUE))

# assing the computed mean values to both states.NOTE: The lat value is not exactly correct. 
# With the computed mean (26.30) the rank appeared outside of the state. Therefore, I changed it a bit to fit as I intend it to.
rank_text <- rank_text %>%
  mutate(long = ifelse(id == "alaska", -117.12, long),
         long = ifelse(id == "hawaii", -107.21, long),
         lat = ifelse(id == "alaska", 28.30, lat),
         lat = ifelse(id == "hawaii", 26.21, lat))

# Map Tuition graph 1
map_tuition_graph1 <- usa_map_new %>% 
  ggplot() +
  geom_polygon(col = "white", aes(long, lat, group = group, fill = tuition)) +
  geom_text(data = rank_text, aes(long, lat, label = ranking), family = "mono", size = 4) + coord_map() +
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(family = "mono"),
        legend.text = element_text(family = "mono"),
        title = element_text(face = "bold", family = "mono")) + 
  labs(title = "Ranking of States by Tuition", subtitle = "Average Tuition in 2015-16", fill = "Tuition") +
  scale_fill_gradient2(low = "red", mid = "pink", high = "blue", 
                       midpoint = 10000)
map_tuition_graph1


#==data preparation for graph 2==#
# use the avg_tuition_tidy_filter dataframe computed above
avg_tuition_tidy_graph2 <- avg_tuition_tidy_filter %>%
  mutate(tuition_levels = case_when(tuition > mean(tuition, na.rm = TRUE) ~ "Above Average",
                          tuition < mean(tuition, na.rm = TRUE) ~ "Below Average",
                          TRUE ~ "USA Average")) 

# add a new row with the average of the USA       
avg_tuition_tidy_graph2 <- add_row(avg_tuition_tidy_graph2, state = "USA Average", 
                                   year = "2015-16", tuition = mean(avg_tuition_tidy_graph2$tuition, na.rm = TRUE),
                                   tuition_levels = "USA Average")
#variable tuition_levels to factor 
avg_tuition_tidy_graph2$tuition_levels <- factor(avg_tuition_tidy_graph2$tuition_levels, 
                                       levels = c("Below Average","USA Average",
                                                  "Above Average"))

#load image from the net
uni <- image_read("http://moneyuniversitythebook.com/wp-content/uploads/2016/01/shutterstock_589740642.jpg")
#resize proportionally to width: 600px
uni <- image_scale(uni, "600")


# Map Tuition graph 2
map_tuition_graph2 <- avg_tuition_tidy_graph2 %>%
  ggplot(aes(x = reorder(state, tuition), y = tuition, fill = factor(tuition_levels))) +
  annotation_custom(rasterGrob(uni), 
                    xmin = 0, xmax = 50, ymin = 12050, ymax = 18000) + 
  geom_bar(stat = "identity", width = 0.8) + 
  scale_fill_manual(values = c("Below Average" = "#BCF5A9",
                                "USA Average" = "#40FF00",
                                "Above Average" = "#173B0B")) +
  geom_text(aes(label = dollar(round(tuition)),
                angle = 90, family = "mono"),
            hjust = -0.1, 
            color = "black",
            size = 4) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0, face = "bold", family = "mono"),
        axis.text.y = element_text(face = "bold", family = "mono"),
        title = element_text(face = "bold", family = "mono"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = 10, family = "mono")) +
  coord_cartesian(ylim = c(0, max(avg_tuition_tidy_graph2$tuition) * 1.2)) +
  scale_y_continuous(breaks = c(0, 4000, 8000, 12000), 
                     labels = c("$0k", "$4k", "$8K", "$12K")) +
  labs(title = "Average Tuition by State", 
       subtitle = "Year 2015-16", 
       caption = "\nDataSource: https://trends.collegeboard.org/", x = "State", 
       y = "Average Tuition",
       fill = "Tuition Comparison")

map_tuition_graph2




