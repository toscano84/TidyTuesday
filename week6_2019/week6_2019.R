# week 6 Tidy Tuesday

library(tidyverse) # wrangle, visualization data
library(data.table) # load file
library(geojsonio) # open json files
library(cartogram) # create cartograms
library(broom) # tidy data frames
library(rgeos) # manipulation of spatial objects
library(viridis) # palette
library(extrafont) # add new fonts to R

# open data frame
hpi_usa <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")


# tidy the data frame
hpi_usa_tbl <- hpi_usa %>% 
  mutate(state_full_name = as.factor(case_when(state == "AL" ~ "Alabama",
                                               state == "AK" ~ "Alaska",
                                state == "AR" ~ "Arkansas",
                                state == "AZ" ~ "Arizona",
                                state == "CA" ~ "California",
                                state == "CO" ~ "Colorado",
                                state == "CT" ~ "Connecticut",
                                state == "DC" ~ "District of Columbia",
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
                                state == "MD" ~ "Maryland",
                                state == "ME" ~ "Maine",
                                state == "MA" ~ "Massachusetts",
                                state == "MI" ~ "Michigan",
                                state == "MN" ~ "Minnesota",
                                state == "MO" ~ "Missouri",
                                state == "MS" ~ "Mississippi",
                                state == "MT" ~ "Montana",
                                state == "NC" ~ "North Carolina",
                                state == "NH" ~ "New Hampshire",
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
                                state == "WY" ~ "Wyoming"))) %>% # create new variable as factor with the state name in a long format
  group_by(year, state_full_name) %>%
  summarize(price_index_avg = mean(price_index, na.rm = TRUE)) %>% # create variable with mean price index per year and state
  mutate(id_row = 1:n()) %>% # this step is important for the spread function to work
  spread(year, price_index_avg) %>% # from long to wide format
  mutate(change_price_index = (`2018` - `2000`) / `2000` * 100) %>% # create variable based on the changes in the house price index from 2000 to 2018
  select(-id_row)
  
  
#-----manipulate hexbin file of the USA------

# open hexbin file
# Hexbin available in https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
us_hexagonal <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# create variable region
us_hexagonal@data <- us_hexagonal@data %>% 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# merge both data frames
us_hexagonal@data <- us_hexagonal@data %>% 
  left_join(., hpi_usa_tbl, by=c("google_name"="state_full_name"))


# create the cartogram using the change_price_index variable
cartogram <- cartogram(us_hexagonal, 'change_price_index')


# use the broom package to make the spatial object a data frame and then merge it with the cartogram
us_fortified <- tidy(cartogram, region = "google_name")
us_fortified <-  us_fortified %>% 
  left_join(. , cartogram@data, by=c("id"="google_name")) 

# Important step to center the state labels
centers <- cbind.data.frame(data.frame(gCentroid(cartogram, byid=TRUE), 
                                       id=cartogram@data$iso3166_2))

#----plot-----#
#---alter the key legend with specific breaks---
# create breaks
new_breaks <- c(50,100,150,200,250,300)
# find the min for the labels
minvalue <- min(us_fortified$change_price_index, na.rm = T)

# create labels
labels <- c()
breaks <- c(minvalue, new_breaks)
# round the labels 
for(i in 1:length(breaks)){
  labels <- c(labels,round(breaks[i + 1], 2))
}

labels <- labels[1:length(labels)-1]
# create a new variable based on breaks
us_fortified$breaks <- cut(us_fortified$change_price_index, 
                     breaks = breaks, 
                     include.lowest = TRUE, 
                     labels = labels)

breaks_scale <- levels(us_fortified$breaks)
labels_scale <- rev(breaks_scale)

p <- ggplot() +
  geom_polygon(data = us_fortified, 
               aes(fill = breaks, 
                   x = long, y = lat, group = group) , 
               size=0.05, color="grey40") +
      # create manual scale based on the breaks 
  scale_fill_manual(values = rev(cividis(8)), # reverse cividis scale from the viridis palette
    breaks = rev(breaks_scale),
    name = "Change of Price Index (%)",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(3.4, units = "mm"),
      keywidth = unit(18, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = TRUE,
      reverse = TRUE,
      label.position = "bottom"
    )
  ) +
  geom_text(data=centers, aes(x=x, y=y, label=id), 
            color="grey25", size=5, alpha=0.6, family = "Cooper Black") +
  labs(title =  "House Price Index",
       subtitle = "From 2000 to 2018") +
  theme_void() +
  theme(panel.grid = element_blank(),
    legend.position = c(0.5, 0.87),
    axis.line = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "#333333", color = NA), 
    panel.background = element_rect(fill = "#333333", color = NA), 
    legend.background = element_rect(fill = "#333333", color = NA),
    legend.text = element_text(size= 12, color = "grey50", family = "Cooper Black"),
    plot.title = element_text(size= 28, hjust=0.5, color = "grey50", family = "Cooper Black"),
    plot.subtitle = element_text(size= 14, hjust=0.5, vjust = -5, color = "grey50", family = "Cooper Black"),
    legend.title = element_text(size= 16, hjust=0.5, vjust = -5, color = "grey50", family = "Cooper Black")) +
  coord_map()

ggsave("hpi_index_changes.jpg", p, units = "cm", 
       height = 25, width = 40, dpi = "retina")

