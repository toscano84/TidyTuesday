# leaflet week 15
library(leaflet)
library(readxl) # to load excel file
library(tidyverse) # data cleaning, wrangling, visualization
library(htmlwidgets) # to save leaflet as an html file
library(mapview) # to save leaflet as an image
library(htmltools) # label more compatible with html files


# load files
breweries <- read_excel("week15_beers.xlsx", sheet = 2)
beers <- read_excel("week15_beers.xlsx", sheet = 1)

#join datasets
beers_breweries <- beers %>%
  left_join(breweries, by = c("brewery_id" = "id"))

# rename columns names
beers_breweries <- beers_breweries %>%
  rename(beers_name  = name.x, breweries_name = name.y)
  
# create tile - tile made in mapbox
map_tile <- "https://api.mapbox.com/styles/v1/toscano84/cjjpgsezebtb72roknl6gatmb/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoidG9zY2Fubzg0IiwiYSI6ImNqanBncTdsYzBhdXIzcG1hdGFmb3h2ZzUifQ.Jw3BTDv4M4fLluvCsRHI6A"


# check the types of ounces and create palette
beers_breweries %>%
  group_by(ounces) %>%
  count()

pal_ounces <- colorFactor(palette = c("#FAAC58", "#FE9A2E", "#FF8000", "#DF7401",
                                      "#B45F04", "#8A4B08", "#61380B"),
                   levels = c("8.4", "12", "16", "16.9", "19.2", "24", "32"))

# plot the map
map_beers <- beers_breweries %>% 
  leaflet() %>% 
  addTiles(urlTemplate = map_tile) %>% 
  addCircleMarkers(radius = 3,
                   color = ~pal_ounces(ounces),
                   label = ~htmlEscape(beers_name),
                   popup = ~paste0("<b>", beers_name,"</b>",
                                   "<br/>","(Ounces: ", ounces,")"))
map_beers

# save in html format
saveWidget(map_beers, "beers_USA.html")



