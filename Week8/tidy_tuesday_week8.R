# tidy tuesday week 8

#libraries needed
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(extrafont)
library(rgeos)
library(geojsonio)
library(broom)

# import and load fonts
font_import()
loadfonts(device = "win")

# read file
honey <- read_csv("honeyproduction.csv")
View(honey)

# tidy the dataset and create variable that corresponds to percentage change in honez production between 1998 and 2012
honey_totalprod <- honey %>%
  group_by(state) %>%
  select(state, totalprod, year) %>%
  spread(year, totalprod) %>%
  mutate(perc_change_prod = round((`2012` - `1998` ) / `1998` * 100, digits = 2)) %>%
  na.omit

# create new variable - state names to enable the join with the json file
honey_totalprod <- honey_totalprod %>%
  mutate(state_long = case_when(state == "AL" ~ "Alabama",
                                state == "AR" ~ "Arkansas",
                                state == "AZ" ~ "Arizona",
                                state == "CA" ~ "California",
                                state == "CO" ~ "Colorado",
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
                                state == "NJ" ~ "New Jersey",
                                state == "NM" ~ "New Mexico",
                                state == "NV" ~ "Nevada",
                                state == "NY" ~ "New York",
                                state == "OH" ~ "Ohio",
                                state == "OK" ~ "Oklahoma",
                                state == "OR" ~ "Oregon",
                                state == "PA" ~ "Pennsylvania",
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
                                state == "WY" ~ "Wyoming"))

# Hexbin available in https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map. Download it and then:
us_hex <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# fortify the data 
us_hex@data <- us_hex@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
us_fortified <- tidy(us_hex, region = "google_name")

# join both datasets
us_fortified <- us_fortified %>% 
  left_join(honey_totalprod, by=c("id" = "state_long")) 
 

# center the name of the states
centered_states <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=us_hex@data$iso3166_2))


# plot map
p1 <- us_fortified %>%
  na.omit() %>%
  ggplot(aes()) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = perc_change_prod), colour = "white", alpha=0.9) +
  geom_text(data = centered_states, aes(x = x, y = y, label=id), fontface = "bold", color="white", size = 4, alpha = 0.9) +
  scale_fill_gradient(name = "Change", low = "black", high = "#FFD700", breaks = c(-100, -50, 0, 50, 100)) +
  labs(title = "Change (%) in Honey Production per State",
       subtitle = "1998-2012") +
  theme(text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        plot.title = element_text(family = "Georgia", size= 22, hjust=0.5, color = "#4e4d47"),
        plot.subtitle = element_text(family = "Georgia", size= 16, hjust=0.5, color = "#4e4d47"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(family = "Georgia"),
        legend.position = "right") +
  coord_map() 


