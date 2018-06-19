# tidy tuesday week 11 #


library(tidyverse) # to load. wrangle and visualize data
library(broom)    # to transform a shape file in a dataframe    
library(patchwork) # to put plots together
library(viridis)   # to create the scale color viridis
library(extrafont)  # to add fonts

# load and check fifa asidata
fifa <- read_delim("fifa.txt", delim = ",") %>%
  glimpse()

# create variable country_code to enable the join with the shape file of ASIA
fifa <- fifa %>%
  mutate(country_code = case_when(country == "Azerbaijan" ~ "AZE",
                                  country == "Armenia" ~ "ARM",
                                  country == "Bahrain" ~ "BHR",
                                  country == "Bangladesh" ~ "BGD",
                                  country == "Myanmar" ~ "MMR",
                                  country == "Brunei" ~ "BRN",
                                  country == "Cambodia" ~ "KHM",
                                  country == "Sri Lanka" ~ "LKA",
                                  country == "China" ~ "CHN",
                                  country == "Afghanistan" ~ "AFG",
                                  country == "Georgia" ~ "GEO",
                                  country == "Hong Kong" ~ "HKG",
                                  country == "India" ~ "IND",
                                  country == "Indonesia" ~ "IDN",
                                  country == "Iran" ~ "IRN",
                                  country == "Israel" ~ "ISR",
                                  country == "Iraq" ~ "IRQ",
                                  country == "Japan" ~ "JPN",
                                  country == "Jordan" ~ "JOR",
                                  country == "Kyrgyzstan" ~ "KGZ",
                                  country == "South Korea" ~ "KOR",
                                  country == "North Korea" ~ "PRK",
                                  country == "Kuwait" ~ "KWT", 
                                  country == "Kazakhstan" ~ "KAZ",
                                  country == "Laos" ~ "LAO",
                                  country == "Lebanon" ~ "LBN",
                                  country == "Mongolia" ~ "MNG",
                                  country == "Oman" ~ "OMN",
                                  country == "Maldives" ~ "MVN",
                                  country == "Malaysia" ~ "MYS",
                                  country == "Macau" ~ "MAC",
                                  country == "Nepal" ~ "NPL",
                                  country == "Pakistan" ~ "PAK",
                                  country == "Palestine" ~ "PSE",
                                  country == "Philippines" ~ "PHL",
                                  country == "Qatar" ~ "QAT",
                                  country == "Saudi Arabia" ~ "SAU",
                                  country == "Singapore" ~ "SGP",
                                  country == "Sri Lanka" ~ "LKA",
                                  country == "Syria" ~ "SYR",
                                  country == "Taiwan" ~ "TWN",
                                  country == "Tajikistan" ~ "TJK",
                                  country == "Thailand" ~ "THA",
                                  country == "Timor" ~ "TLS",
                                  country == "Turkey" ~ "TUR",
                                  country == "Turkmenistan" ~ "TKM",
                                  country == "Uzbekistan" ~ "UZB",
                                  country == "Vietnam" ~ "VNM",
                                  country == "Yemen" ~ "YEM")) 

# create the shape file of ASIA
asia <- wrld_simpl[wrld_simpl$REGION == 142, ] 

# list of Asian country codes
asia_codes <- asia@data %>% rownames()
asia@data$country_code <- asia_codes

# filter data for Asian countries
asia_data <- fifa %>% filter(country_code %in% asia_codes)

# join datasets
asia@data <- asia@data %>% left_join(asia_data)
rownames(asia@data) <- asia@data$country_code 

# Transform the previous object in a dataframe
asia_df <- tidy(asia) %>% left_join(. , asia@data, by=c("id"="ISO3"))

##-----make 3 plots and put them together------#
# plot 1 - gdp share #
plot_1 <- asia_df %>%
  ggplot() +
  geom_polygon(data = filter(asia_df, !is.na(gdp_weighted_share)), col = "white", 
               aes(fill = gdp_weighted_share, 
                   x = long, y = lat, group = group), alpha=0.9) + 
  scale_fill_viridis(direction = -1, 
                     name = "GDP share") + 
  labs(title = "GDP Weighted Share in Asia", 
       subtitle = "World Cup 2010") +
  theme(title = element_text(family = "Georgia", size = 14, hjust = 0.5),
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.text = element_text(family = "Georgia", size = 8),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_blank()) +
  coord_equal() 

# plot 2 - tv audience share #
plot_2 <- asia_df %>%
  ggplot() +
  geom_polygon(data = filter(asia_df, !is.na(tv_audience_share)), col = "white", 
               aes(fill = tv_audience_share, 
                   x = long, y = lat, group = group), alpha=0.9) + 
  scale_fill_viridis(direction = -1, 
                     name = "TV audience share") + 
  labs(title = "TV Audience Share in Asia", 
       subtitle = "World Cup 2010") +
  theme(title = element_text(family = "Georgia", size = 14, hjust = 0.5),
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.text = element_text(family = "Georgia", size = 8),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_blank()) +
  coord_equal() 

# plot 3 - population share #
plot_3 <- asia_df %>%
  ggplot() +
  geom_polygon(data = filter(asia_df, !is.na(population_share)), col = "white", 
               aes(fill = population_share, 
                   x = long, y = lat, group = group), alpha=0.9) + 
  scale_fill_viridis(direction = -1, 
                     name = "Population share") + 
  labs(title = "Population Share in Asia", 
       subtitle = "World Cup 2010") +
  theme(title = element_text(family = "Georgia", size = 14, hjust = 0.5),
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.text = element_text(family = "Georgia", size = 8),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_blank()) +
  coord_equal() 

# create the final plot by adding the previous 3
plot_final <- plot_1 + plot_2 + plot_3 + plot_layout(ncol = 2)
plot_final

