# Tidy Tuesday Week 25

# libraries needed
library(tidyverse)
library(data.table)
library(fiftystater)
library(viridis)
library(gridExtra)
library(extrafont)


# import and load fonts
font_import()
loadfonts(device = "win")

# load database
us_airports <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-18/us-airports.csv")


# tidy the dataframe for plot 1
us_tidy <- us_airports %>%
  group_by(state, year) %>%
  summarize(total_passengers = sum(passengers)) %>%
  #from long to wide format
  spread(year, total_passengers) %>%
  na.omit(.) %>%
  # create new variable - change in percentage of total passengers
  mutate(perct_change = (`2017`- `2012`) / `2012` * 100)

#-----plots-----#
# create theme
theme_personal <- theme(title = element_text(family = "Cambria", size = 12, hjust = 0.5),
                      plot.title = element_text(family = "Cambria", 
                                                color = "white", 
                                                size = 20, 
                                                hjust = 0),
                      plot.subtitle = element_text(family = "Cambria", 
                                                   face = "italic", 
                                                   color = "white", 
                                                   size = 14, 
                                                   hjust = 0), 
                      plot.caption = element_text(family = "Cambria", 
                                                  color = "white", 
                                                  size = 10, 
                                                  hjust = 0),
                      plot.background = element_rect(fill = "#0B0B3B", 
                                                     color = NA), 
                      panel.background = element_rect(fill = "#0B0B3B", 
                                                      color = NA),
                      legend.background = element_rect(fill = "#0B0B3B", 
                                                       color = NA),
                      legend.text = element_text(family = "Cambria", 
                                                 size = 10, color = "white"),
                      legend.title = element_text(family = "Cambria", 
                                                  size = 12, color = "white"),
                      axis.line = element_blank(), 
                      axis.ticks = element_blank(),
                      panel.grid = element_blank(), 
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      legend.position = "right")

#-----plot 1-----#
# bar plot
plot_1 <- us_tidy %>%
  filter(!state %in% c("GU", "AS", "WK", "PR", "MP", "VI")) %>%
           ggplot(aes(y = perct_change, fill = perct_change,
                    label = state, hjust = ifelse(perct_change > 0, -0.2, 1.2) , 
                    x = reorder(state, perct_change))) + 
  geom_col() +
  geom_text(size = 2.2) +
  coord_flip() +
  scale_fill_viridis(option = "magma") +
  guides(fill = FALSE) +
  theme_personal
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text= element_blank())

plot_1

#-----plot 2-----#
# create new data frame
us_states_complete <- us_tidy %>%
  mutate(state_new = case_when(state == "AK" ~ "Alaska",
                                state == "AL" ~ "Alabama",
                                state == "AR" ~ "Arkansas",
                                state == "AZ" ~ "Arizona",
                                state == "CA" ~ "California",
                                state == "CO" ~ "Colorado",
                                state == "CT"~ "Connecticut",
                                state == "DE" ~ "Delaware",
                                state == "FL" ~ "Florida",
                                state == "GA" ~ "Georgia",
                                state == "HI" ~ "Hawaii",
                                state == "IA" ~ "Iowa",
                                state == "ID" ~ "Idaho",
                                state == "IL" ~ "Illinois",
                                state == "IN" ~ "Indiana",
                                state == "KS" ~ "Kansas",
                                state == "KY" ~ "Kentucky",
                                state == "LA" ~ "Louisiana",
                                state == "MA" ~ "Massachusetts",
                                state == "MD" ~ "Maryland",
                                state == "ME" ~ "Maine",
                                state == "MI" ~ "Michigan",
                                state == "MN" ~ "Minnesota",
                                state == "MO" ~ "Missouri",
                                state == "MS" ~ "Mississippi",
                                state == "MT" ~ "Montana",
                                state == "NC" ~ "North Carolina",
                                state == "ND" ~ "North Dakota",
                                state == "NE" ~ "Nebraska", 
                                state == "NH" ~ "New Hampshire",
                                state == "NJ" ~ "New Jersey",
                                state == "NM" ~ "New Mexico",
                                state == "NV" ~ "Nevada",
                                state == "NY" ~ "New York",
                                state == "OH" ~ "Ohio",
                                state == "OK" ~ "Oklahoma",
                                state == "OR" ~ "Oregon",
                                state == "PA" ~ "Pennsylvania",
                                state == "RI" ~ "Rhode Island",
                                state == "SC" ~ "South Carolina",
                                state == "SD" ~ "South Dakota",
                                state == "TN" ~ "Tennessee",
                                state == "TX" ~ "Texas",
                                state == "UT" ~ "Utah",
                                state == "VA" ~ "Virginia",
                                state == "VT" ~ "Vermont",
                                state == "WA" ~ "Washington",
                                state == "WI" ~ "Wisconsin",
                                state == "WV" ~ "West Virginia",
                                state == "WY" ~ "Wyoming")) %>%
  mutate_at(vars(state_new), tolower)

# join data frames
us_airports_map <- us_states_complete %>%
  left_join(fifty_states, by = c("state_new" = "id"))


# map plot
plot_2 <- us_airports_map %>%
  na.omit(.) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, fill = perct_change, group = group), color = "grey") +
  scale_fill_viridis(option = "magma", 
                     breaks = c(-20, 0, 20), labels = c("-20%", "0%", "20%")) +
  labs(title = "Change (%) in Total Passengers in Airports per State",
       subtitle = "Between 2012 to 2017",
       fill = "Percentage\nChange",
       caption = "Source: faa.gov") +
  theme_personal

plot_2

# Put together the 2 plots
plot <- grid.arrange(plot_1, plot_2, 
                     widths = c(1.2, 3),
                     clip = FALSE)

ggsave("plot_airports.jpg", plot, width = 40, height = 30, units = "cm")

