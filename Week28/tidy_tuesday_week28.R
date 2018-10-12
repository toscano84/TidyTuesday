# Tidy Tuesday Week 28

# libraries needed
library(tidyverse)
library(data.table)
library(extrafont)
library(ggthemes)
library(mice)
library(extrafont)
library(randomcoloR)

# load file
voters <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-09/voter_turnout.csv")

# import and load fonts
font_import()
loadfonts(device = "win")

# create new variables -type of election and region
voters_tidy <- voters %>%
  mutate(election_type = ifelse(year %in% seq(1982, 2014, 4), "midterm", "presidential"), # create variable type of election
         voter_turnout = votes / eligible_voters * 100, # create voter turnout in %
         region = case_when(state %in% c("Connecticut", # create region variable
                                         "Maine", 
                                         "Massachusetts", 
                                         "New Hampshire", 
                                         "Rhode Island", 
                                         "Vermont", 
                                         "New Jersey", 
                                         "New York", 
                                         "Pennsylvania") ~ "Northeast",
                            state %in% c("Illinois", 
                                         "Indiana", 
                                         "Michigan", 
                                         "Ohio", 
                                         "Wisconsin", 
                                         "Iowa", 
                                         "Kansas", 
                                         "Minnesota", 
                                         "Missouri", 
                                         "Nebraska", 
                                         "North Dakota", 
                                         "South Dakota") ~ "Midwest",
                            state %in% c("Delaware", 
                                         "Florida", 
                                         "Georgia", 
                                         "Maryland", 
                                         "North Carolina", 
                                         "South Carolina", 
                                         "Virginia", 
                                         "District of Columbia", 
                                         "West Virginia",
                                         "Alabama", 
                                         "Kentucky", 
                                         "Mississippi", 
                                         "Tennessee",
                                         "Arkansas", 
                                         "Louisiana", 
                                         "Oklahoma", 
                                         "Texas") ~ "South",
                            state %in% c("Arizona", 
                                         "Colorado", 
                                         "Idaho", 
                                         "Montana", 
                                         "Nevada", 
                                         "New Mexico", 
                                         "Utah", 
                                         "Wyoming",
                                         "Alaska", 
                                         "California", 
                                         "Hawaii", 
                                         "Oregon", 
                                         "Washington") ~ "West")) %>%
  filter(alphanumeric_state_code != 0) 


# impute missing values
voters_tidy <- mice(voters_tidy, m= 5, maxit = 50, meth = "cart", seed = 100)
voters_tidy <- mice::complete(voters_tidy,1)

sum(is.na(voters_tidy)) # check if there are missing values

#----plot ----#
# create random color for the 50 states plus DC
palette <- distinctColorPalette(51)

# create theme
theme_personal <- theme(strip.text.x = element_text(family = "Cooper Black", 
                                  size = 12, hjust = 0.5, 
                                  vjust = 2, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(family = "Cooper Black", 
                                size = 14),
      plot.subtitle = element_text(family = "Cooper Black", 
                                   size = 11),
      axis.title = element_blank(),
      axis.text = element_text(family = "Cooper Black", 
                               size = 10), 
      panel.grid.major.y = element_line(colour = "#848484"),
      legend.title = element_text(family ="Cooper Black", size = 12), 
      plot.caption = element_text(family = "Cooper Black", 
                                  size = 10))

# plot
plot1 <- voters_tidy %>%
ggplot(aes(year, state, colour = state)) +
  geom_point(aes(size = voter_turnout), alpha = 0.6) +
  guides(color = FALSE) + 
  scale_x_continuous(limits = c(1979, 2015),
                     breaks = seq(1980, 2014, by = 2),
                     labels = c("'80", "'82", "'84", "'86", "'88", "'90", 
                                "'92","'94", "'96", "'98","'00","'02", "'04", "'06",
                                "'08",  "'10","'12", "'14")) +
  scale_color_manual(values = palette) +
  labs(title = "Voter Turnout in Elections", subtitle = " From 1980 to 2014",
       caption = "Source: Star Tribune", size = "Voter Turnout (%)") +
  facet_wrap( ~ region, scales = "free") +
  theme_fivethirtyeight() + 
  theme_personal
 
plot1
