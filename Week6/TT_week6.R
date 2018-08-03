# tidy Tuesday week 6

# libraries needed
library(here) # to create a path to the current directory
library(tidyverse) # to load packages related to data cleaning (e.g. dplyr) and data visualization(ggplot2)
library(readxl) # to load excel files
library(leaflet) # to create interactive maps
library(htmlwidgets) # to save leaflet as an html file
library(htmltools) # labels more compatible with html files
library(leaflet.extras) # to add more tile options
library(skimr) # summary statistics
library(mapview) # to save interactive map as an image


# base tile
leaflet() %>% 
  addProviderTiles("CartoDB.DarkMatter") %>%
  setView(lng = -100, lat = 35, zoom = 4)

# load files
# starbucks
starbucks <- read_excel(here("coffee_chains.xlsx"), sheet = 1)
glimpse(starbucks)

# dunkin' donuts
dunkin_donuts <- read_excel(here("coffee_chains.xlsx"), 
                            sheet = 3)
glimpse(dunkin_donuts)

# wrangling data
# starbucks
starbucks_tidy <- starbucks %>%
  # select only coffee locations in the USA
  filter(Country == "US", 
         Brand == "Starbucks") %>%
  # select only columns of interest. Without longitude and latitude we cannot map the locations 
  select(Brand, 
         lng = Longitude, 
         lat = Latitude, 
         Country) %>%
  # tidy the columns' names
  select_all(tolower)
glimpse(starbucks_tidy)

# dunkin' donuts
dunkin_donuts_tidy <- dunkin_donuts %>%
  # various cases where the name is not well spelled. we need to recode them
  mutate(biz_name = recode(biz_name,
                           "Donuts Dunkin" = "Dunkin' Donuts",
                           "Dunkin' Donuts-baskln Robbins" = "Dunkin' Donuts",
                           "Dunkin' Donuts Center" = "Dunkin' Donuts",
                           "Dunkin' Donuts/Baskin Robbins" = "Dunkin' Donuts")) %>%
  # select only cases where the coffee chain is Dunkin' Donuts
  filter(biz_name == "Dunkin' Donuts") %>%
  # select columns of interest and change its names 
  select(brand = biz_name, 
         lng = loc_LONG_poly, 
         lat = loc_LAT_poly, 
         country = e_country) %>%
  mutate(country = 
           case_when(country == "USA" ~ "US"))
glimpse(dunkin_donuts_tidy)


# combine cases
coffee_chains <- bind_rows(starbucks_tidy, dunkin_donuts_tidy)
skim(coffee_chains)

# create colors
pal_color <- colorFactor(palette = c("#007042", 
                                     "#ea4498"),
                         levels = c("Starbucks", 
                                    "Dunkin' Donuts"))

#-----leaflet map-----#
coffee_chains_interactive <- 
  #add coffe_chains to our map
  coffee_chains %>% 
  leaflet(width = "100%", 
        options = leafletOptions(preferCanvas = TRUE)) %>% 
  addProviderTiles("CartoDB.DarkMatter", 
                   options = providerTileOptions(
                     updateWhenZooming = FALSE,
                     updateWhenIdle = TRUE)) %>% 
  addProviderTiles("CartoDB.DarkMatter") %>% 
  addCircleMarkers(data = filter(coffee_chains, brand == "Starbucks"),# add brand filter
                   radius = 3,
                   label = ~htmlEscape(brand),
                   color = ~pal_color(brand),
                   popup = ~paste0("<b>", brand),
                   group = "Starbucks") %>%
  addCircleMarkers(data = filter(coffee_chains, brand == "Dunkin' Donuts"),# add brand filter
                   radius = 3,
                   label = ~htmlEscape(brand),
                   color = ~pal_color(brand),
                   popup = ~paste0("<b>", brand),
                   group = "Dunkin' Donuts" ) %>%
  # add legend
  addLegend(pal = pal_color, 
            values = c("Starbucks", "Dunkin' Donuts"),
            # opacity of .5, legend title called Brand and its position on the topright corner
            opacity = 0.5, title = "Brand", position = "topright") %>%
  # add layers to select the brands that are mapped
  addLayersControl(overlayGroups = c("Starbucks", "Dunkin' Donuts")) 


coffee_chains_interactive

# save as a webmap
saveWidget(coffee_chains_interactive, "coffee_usa.html")

# save as an image
mapshot(coffee_chains_interactive, file = "coffee_usa.jpg")
