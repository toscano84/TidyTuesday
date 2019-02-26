# week 7 Tidy Tuesday

library(tidyverse) # wrangle, visualization data
library(data.table) # load file
library(viridis) # palette
library(extrafont) # add new fonts to R
library(waffle) # create waffle plot



options(scipen = 999) # remove scientific notation

# import and load fonts
font_import()
loadfonts(device = "win")

# importing fontawesome font
fa_font <- tempfile(fileext = ".ttf")
download.file("http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/fonts/fontawesome-webfont.ttf?v=4.3.0",
              destfile = fa_font, method = "curl")

font_import(paths = dirname(fa_font), prompt = FALSE)

fonts()
if (.Platform$OS.type == "windows") loadfonts("win")


# open data frame
fed_spend <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv") 


glimpse(fed_spend)


# tidy the data frame
fed_spend_tbl <- fed_spend %>%
  group_by(year) %>%
  mutate(sum_rd = sum(rd_budget),
         perc_rd = round((rd_budget / sum_rd * 100),0)) %>%
  ungroup() %>%
  filter(year > 2014) %>% # only include the last 3 years
  select(year, department, perc_rd) %>%
  group_by(department) %>% # important step to remove NAs after changing the format from long to wide
  mutate(n = row_number()) %>% # this step is important for the spread function to work
  spread(department, perc_rd) %>%
  select(-n) %>%
  select_if(~mean(.) > 4) %>%
  select(year, DOD, HHS,NIH, everything())



# turn each year data into a vector
year_2015 <- unlist(fed_spend_tbl[1, 2:6])
year_2016 <- unlist(fed_spend_tbl[2, 2:6])
year_2017 <- unlist(fed_spend_tbl[3, 2:6])


# scale color manual
cols <- c("#F8C932", "#E55B2F", "#A42C60", "#61156E", "#120A33")

# set theme
theme_new <- ggdark::dark_theme_gray() +
  theme(plot.title = element_text(family = "Cooper Black", face = "bold",
                                  size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = "Cooper Black", face = "bold",
                                     size = 14, hjust = 0.5),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(family = "Cooper Black", size = 12),
        axis.text = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Cooper Black", size = 10),
        legend.title = element_text(family = "Cooper Black", size = 10)) 

# Make waffle graph for each year
fed1 <- waffle(year_2015, rows = 5, size = 0.3, pad = 0.5, colors = cols, 
              use_glyph = "dollar", glyph_size = 8) +
  labs(title = "Top 5 Departments per R&D Budget (%)",
       subtitle = "Year 2015") +
  theme_new
  
fed2 <-  waffle(year_2016, rows = 5, size = 0.3, pad = 1, colors = cols, 
              use_glyph = "dollar", glyph_size = 8) +
  labs(subtitle = "Year 2016") +
  theme_new

fed3 <-  waffle(year_2017, rows = 5, size = 0.3, pad = 1, colors = cols, 
              use_glyph = "dollar", glyph_size = 8) +
  labs(subtitle = "Year 2017",
       x = "Each dollar sign represents ~1% of the RD budget") +
  theme_new


# iron function to arrange the three plots into one
iron(fed1, fed2, fed3)



