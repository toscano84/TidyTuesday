# tidytuesday - week 3

# load needed libraries
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(grid)
library(jpeg)
library(scales)
library(magick)
library(extrafont)
library(janitor)
library(maptools)
library(gganimate)

# import and load fonts
font_import()
loadfonts(device = "win")

# load data
global_mortality <- read_excel("global_mortality.xlsx")

# create EU region
global_mortality<- global_mortality %>%
  mutate(region = case_when(country == "Portugal" ~ "European Union",
                            country == "Spain" ~ "European Union",
                            country == "Greece" ~ "European Union",
                            country == "Austria" ~ "European Union",
                            country == "Italy" ~ "European Union",
                            country == "Malta" ~ "European Union",
                            country == "Cyprus" ~ "European Union",
                            country == "Bulgaria" ~ "European Union",
                            country == "Romania" ~ "European Union",
                            country == "United Kingdom" ~ "European Union",
                            country == "Ireland" ~ "European Union",
                            country == "Germany" ~ "European Union",
                            country == "Poland" ~ "European Union",
                            country == "Hungary" ~ "European Union",
                            country == "Latvia" ~ "European Union",
                            country == "Estonia" ~ "European Union",
                            country == "Lithuania" ~ "European Union",
                            country == "Sweden" ~ "European Union",
                            country == "Denmark" ~ "European Union",
                            country == "Finland" ~ "European Union",
                            country == "Netherlands" ~ "European Union",
                            country == "Slovenia" ~ "European Union",
                            country == "Croatia" ~ "European Union",
                            country == "France" ~ "European Union",
                            country == "Belgium" ~ "European Union",
                            country == "Luxembourg" ~ "European Union",
                            country == "Slovakia" ~ "European Union",
                            country == "Czech Republic" ~ "European Union"))
# tidy the dataframe
global_mortality_tidy <- global_mortality %>%
  select_all(str_to_lower) %>%
  clean_names() %>%
  rename_at(.vars = vars(ends_with("_percent")),
            .funs = funs(sub("(_)percent$", "", .))) %>%
  gather(cause_of_death, mortality_rate, 4:35) %>%
  filter(cause_of_death == "cancers", region == "European Union") %>%
  group_by(country, year)

# turn the map of Europe to a dataframe
# create a new empty object called 'temporary' in which to store a zip file
temporary <- tempfile(fileext = ".zip")
# download the zip file which is on Eurostat website and
download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/NUTS_2010_60M_SH.zip", 
              temporary)
# unzip
unzip(temporary)

# read administrative boundaries of Europe
europe <- readShapePoly(fn="NUTS_2010_60M_SH/data/NUTS_RG_60M_2010")

# convert the shape file to a dataframe
europe <- fortify(europe, region='NUTS_ID')

# prepare the data to join with the global_mortality_tidy dataframe
eu_28 <- europe %>%
  mutate(country = case_when(id == "PT" ~ "Portugal",
                             id == "ES" ~ "Spain",
                             id == "EL" ~ "Greece",
                             id == "AT" ~ "Austria",
                             id == "IT" ~ "Italy",
                             id == "MT" ~ "Malta",
                             id == "CY" ~ "Cyprus",
                             id == "BG" ~ "Bulgaria",
                             id == "RO" ~ "Romania",
                             id == "UK" ~ "United Kingdom",
                             id == "IE" ~ "Ireland",
                             id == "DE" ~ "Germany",
                             id == "PL" ~ "Poland",
                             id == "HU" ~ "Hungary",
                             id == "LV" ~ "Latvia",
                             id == "EE" ~ "Estonia",
                             id == "LT" ~ "Lithuania",
                             id == "SE" ~ "Sweden",
                             id == "DK" ~ "Denmark",
                             id == "FI" ~ "Finland",
                             id == "NL" ~ "Netherlands",
                             id == "SI" ~ "Slovenia",
                             id == "HR" ~ "Croatia",
                             id == "FR" ~ "France",
                             id == "BE" ~ "Belgium",
                             id == "LU" ~ "Luxembourg",
                             id == "SK" ~ "Slovakia",
                             id == "CZ" ~ "Czech Republic"))

# join both databases
eu_28_cancer_mortality <- global_mortality_tidy %>%
  left_join(eu_28, by = "country")

# assign path to magick to enable the creation of gifs
magickPath <- shortPathName("C:/Program Files/ImageMagick-6.9.9-Q16-HDRI/convert.exe")

# create a dataframe with the limits of Europe
long <- c(60.64878, 24.08464,-31.26192, 56.00000)
lat <- c(80.58823, 34.83469, 39.45479,74.00000)
europe.limits <- data.frame(long, lat)
eu_28_cm <- eu_28_cancer_mortality %>%
  filter(long > min(europe.limits$lon) & long < max(europe.limits$lon) & 
           lat  > min(europe.limits$lat) & lat  < max(europe.limits$lat))
 
  
# create a dataframe to center the labels in the plot
label_data<- eu_28_cm %>%
  group_by(country, year) %>%
  summarise(long = mean(long), lat = mean(lat), 
            mortality_rate = mean(mortality_rate))

# image read
eu_flag<- image_read("EU.jpg")

#--------plot the graph---------#
p <- ggplot(data = eu_28_cm, aes(frame = year)) +
  annotation_custom(rasterGrob(eu_flag), 
                    xmin = -28, xmax = -8, ymin = 74, ymax = 54) +
  geom_polygon(data = eu_28_cm, col = "white", 
               aes(long, lat, group = group, 
                   fill = mortality_rate)) +
  geom_text(data = label_data, family = "Georgia", size = 4, 
            fontface = "bold", 
            aes(x = long, y = lat, 
                label=paste(round(mortality_rate, digits=1),"%"))) +
  theme_void() +
  scale_fill_gradient2(breaks = seq(0,40, by = 5),
                       labels=seq(0,40,by=5), low = "yellow",
                       high = "blue", mid = "grey70",
                       midpoint = 25) +
  theme(legend.position = "bottom",
        legend.key.width = unit(3,"cm"),
        plot.title = element_text(family = "Georgia", face = "bold", 
                                  size = 15)) +
  labs(title = "Cancer Mortality Rate in the European Union in year: ",
       fill = NULL)
gganimate(p, interval = 1)
gganimate(p, interval = 1, "cancer_mortality_EU.gif")
