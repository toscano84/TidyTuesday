# Tidy Tuesday Week 26

# libraries needed
library(tidyverse)
library(data.table)
library(viridis)
library(extrafont)
library(worldtilegrid)


# import and load fonts
font_import()
loadfonts(device = "win")

# load file
invasion_threat <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-25/table_1.csv")

# create tiles file - check Bob Rudis blog https://rud.is/b/2018/08/27/simplifying-world-tile-grid-creation-with-geom_wtg/
country_tiles <- data.frame(
  ctry = worldtilegrid::wtg$alpha.3)

# create new variable country
country_tiles <- country_tiles %>%
  mutate(country = case_when(ctry == "ALB" ~ "Albania",
                             ctry == "ARG" ~ "Argentina",
                             ctry == "ARM" ~ "Armenia",
                             ctry == "AUS" ~ "Australia",
                             ctry == "AUT" ~ "Austria",
                             ctry == "AZE" ~ "Azerbaijan",
                             ctry == "BDI" ~ "Burundi",
                             ctry == "BEL" ~ "Belgium",
                             ctry == "BFA" ~ "Burkina Faso",
                             ctry == "BGD" ~ "Bangladesh",
                             ctry == "BGR" ~ "Bulgaria",
                             ctry == "BIH" ~ "Bosnia and Herzegovina",
                             ctry == "BLR" ~ "Belarus",
                             ctry == "BLZ" ~ "Belize",
                             ctry == "BRA" ~ "Brazil",
                             ctry == "BRB" ~ "Barbados",
                             ctry == "CAN" ~ "Canada",
                             ctry == "CHE" ~ "Switzerland",
                             ctry == "CHL" ~ "Chile",
                             ctry == "CHN" ~ "China",
                             ctry == "CMR" ~ "Cameroon",
                             ctry == "COD" ~ "Congo (Republic of)",
                             ctry == "COL" ~ "Colombia",
                             ctry == "CPV" ~ "Cape Verde",
                             ctry == "CRI" ~ "Costa Rica",
                             ctry == "CYP" ~ "Cyprus",
                             ctry == "CZE" ~ "Czech Republic",
                             ctry == "DEU" ~ "Germany",
                             ctry == "DNK" ~ "Denmark",
                             ctry == "DOM" ~ "Dominican Republic",
                             ctry == "DZA" ~ "Algeria",
                             ctry == "ECU" ~ "Ecuador",
                             ctry == "EGY" ~ "Egypt",
                             ctry == "ESP" ~ "Spain",
                             ctry == "EST" ~ "Estonia",
                             ctry == "ETH" ~ "Ethiopia",
                             ctry == "FIN" ~ "Finland",
                             ctry == "FJI" ~ "Fiji",
                             ctry == "FRA" ~ "France",
                             ctry == "GBR" ~ "United Kingdom",
                             ctry == "GEO" ~ "Georgia (Republic)",
                             ctry == "GHA" ~ "Ghana",
                             ctry == "GIN" ~ "Guinea",
                             ctry == "GMB" ~ "Gambia",
                             ctry == "GNB" ~ "Guinea-Bissau",
                             ctry == "GNQ" ~ "Equatorial Guinea",
                             ctry == "GRC" ~ "Greece",
                             ctry == "HND" ~ "Honduras",
                             ctry == "HRV" ~ "Croatia",
                             ctry == "HUN" ~ "Hungary",
                             ctry == "IDN" ~ "Indonesia",
                             ctry == "IND" ~ "India",
                             ctry == "IRL" ~ "Ireland",
                             ctry == "IRN" ~ "Iran",
                             ctry == "IRQ" ~ "Iraq",
                             ctry == "ISL" ~ "Iceland",
                             ctry == "ISR" ~ "Israel",
                             ctry == "ITA" ~ "Italy",
                             ctry == "JAM" ~ "Jamaica",
                             ctry == "JOR" ~ "Jordan",
                             ctry == "JPN" ~ "Japan",
                             ctry == "KAZ" ~ "Kazakhstan",
                             ctry == "KEN" ~ "Kenya",
                             ctry == "KGZ" ~ "Kyrgyzstan",
                             ctry == "KHM" ~ "Cambodia",
                             ctry == "KOR" ~ "Korea Republic of",
                             ctry == "LAO" ~ "Laos",
                             ctry == "LBN" ~ "Lebanon",
                             ctry == "LKA" ~ "Sri Lanka",
                             ctry == "LTU" ~ "Lithuania",
                             ctry == "LUX" ~ "Luxembourg",
                             ctry == "LVA" ~ "Latvia",
                             ctry == "MAR" ~ "Morocco",
                             ctry == "MDA" ~ "Moldova",
                             ctry == "MDG" ~ "Madagascar",
                             ctry == "MEX" ~ "Mexico",
                             ctry == "MKD" ~ "Macedonia",
                             ctry == "MLI" ~ "Mali",
                             ctry == "MLT" ~ "Malta",
                             ctry == "MNG" ~ "Mongolia",
                             ctry == "MOZ" ~ "Mozambique",
                             ctry == "MUS" ~ "Mauritius",
                             ctry == "MWI" ~ "Malawi",
                             ctry == "MYS" ~ "Malaysia",
                             ctry == "NER" ~ "Niger",
                             ctry == "NGA" ~ "Nigeria",
                             ctry == "NIC" ~ "Nicaragua",
                             ctry == "NLD" ~ "Netherlands",
                             ctry == "NOR" ~ "Norway",
                             ctry == "NPL" ~ "Nepal",
                             ctry == "NZL" ~ "New Zealand",
                             ctry == "PAK" ~ "Pakistan",
                             ctry == "PAN" ~ "Panama",
                             ctry == "PER" ~ "Peru",
                             ctry == "PHL" ~ "Philippines",
                             ctry == "POL" ~ "Poland",
                             ctry == "PRT" ~ "Portugal",
                             ctry == "PRY" ~ "Paraguay",
                             ctry == "QAT" ~ "Qatar",
                             ctry == "ROU" ~ "Romania",
                             ctry == "RUS" ~ "Russian Federation",
                             ctry == "RWA" ~ "Rwanda",
                             ctry == "SAU" ~ "Saudi Arabia",
                             ctry == "SDN" ~ "Sudan",
                             ctry == "SGP" ~ "Singapore",
                             ctry == "SLV" ~ "El Salvador",
                             ctry == "SUR" ~ "Suriname",
                             ctry == "SVK" ~ "Slovakia",
                             ctry == "SVN" ~ "Slovenia",
                             ctry == "SWE" ~ "Sweden",
                             ctry == "TGO" ~ "Togo",
                             ctry == "THA" ~ "Thailand",
                             ctry == "TJK" ~ "Tajikistan",
                             ctry == "TTO" ~ "Trinidad and Tobago",
                             ctry == "TUN" ~ "Tunisia",
                             ctry == "TUR" ~ "Turkey",
                             ctry == "UKR" ~ "Ukraine",
                             ctry == "URY" ~ "Uruguay",
                             ctry == "USA" ~ "USA",
                             ctry == "VEN" ~ "Venezuela",
                             ctry == "VNM" ~ "Vietnam",
                             ctry == "VUT" ~ "Vanuatu",
                             ctry == "YEM" ~ "Yemen",
                             ctry == "ZAF" ~ "South Africa"))

# join both datasets
invasion_threat_tidy <- invasion_threat %>%
  left_join(country_tiles, by = "country")

#----plot----#
invasion_threat_tidy %>%
  mutate_at(vars(ctry), as.character) %>% # I had to transform the ctry variable to character - Without it cant make the plot
  mutate(
    invasion_threat = cut(
      x = invasion_threat,
      breaks = c(1, 0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60, 0.55,
                 0.50, 0.45, 0.40, 0.35, 0.30, 0.25, 0.20,
                 0.15, 0.10, 0.05, 0)) # cut the invasion_threat variable in various intervals
  ) %>%
  ggplot(aes(country = ctry, fill = invasion_threat)) +
  geom_wtg() + 
  geom_text(aes(label = stat(alpha.2)), 
            stat="wtg", size=2) + # adding text with the country abbreviations
  coord_equal() +
  scale_fill_viridis_d(option = "inferno",
                              na.value="grey", 
                              guide = guide_legend(reverse = TRUE)) +
  labs(title = "Global Threat  to Agriculture from Invasive Species",
       fill = "Invasion Threat", caption = "Source:http://www.pnas.org/content/113/27/7575") +
  hrbrthemes::theme_ft_rc() +
  theme_enhance_wtg() +
  theme(legend.position = "right", 
        plot.title = element_text(family = "Bell MT", 
                                  hjust = 0.5, size = 18),
        legend.title = element_text(family = "Bell MT"),
        legend.text = element_text(family = "Bell MT", size = 12),
        plot.caption = element_text(family = "Bell MT"))
