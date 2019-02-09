# week 5 2019 - TidyTuesday

library(tidyverse) # wrangle, visualization of data
library(data.table) # load file
library(extrafont) # add fonts to R
library(gghighlight) # highlight values in a plot

options(scipen = 999) # remove scientific notation


# load data frame
milk_facts <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/milk_products_facts.csv")

# create manual palette
cols <-  c("#578300", "#991200", "#1280A1", "yellow", "Tomato",
           "MediumSeaGreen", "DodgerBlue", "blue", "#FA8072", 
           "#FFC300", "#FF5733", "#C70039", "#900C3E", "#571845",
           "#D35400", "#000080") 

# create plot
p <- milk_facts %>%
  gather(key = types_milk, value = avg_consumption, 2:ncol(.)) %>%
  filter(types_milk != "fluid_milk") %>% #do not include fluid_milk
  mutate(types_milk2 = recode(types_milk,
                                  "butter" = "Butter", 
          "cheese_american" = "Cheese American", 
          "cheese_cottage" = "Cheese Cottage", 
          "cheese_other" = "Cheese Other",
          "dry_buttermilk" = "Dry Buttermilk", 
          "dry_nonfat_milk" = "Dry non-fat Milk", 
          "dry_whey" = "Dry Whey",
         "dry_whole_milk" = "Dry Whole Milk",
         "evap_cnd_canned_whole_milk" = "Evap. and Canned whole Milk",
         "evap_cnd_bulk_whole_milk" = "Evap. and Canned Bulk whole Milk",
         "evap_cnd_bulk_and_can_skim_milk" = "Evap. and Canned Bulk & skim Milk",
         "fluid_yogurt" = "Fluid Yogurt",
         "frozen_ice_cream_regular" = "Frozen Ice cream Regular",
         "frozen_ice_cream_reduced_fat" = "Frozen Ice Cream reduced Fat",
         "frozen_other" = "Frozen Other",
         "frozen_sherbet" = "Frozen Sherbet",
         "fluid_milk" = "Fluid Milk")) %>%
  group_by(year, types_milk2) %>%
  summarize(avg_milk_consumed = mean(avg_consumption, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_milk_consumed)) +
  guides(color = FALSE) +
  geom_line(aes(colour = types_milk2), size = 3.5) +
  gghighlight(use_direct_label = FALSE, use_group_by = FALSE,
              unhighlighted_colour = alpha("grey20", 0.3)) +
  facet_wrap(vars(types_milk2)) + 
  labs(title = "Average Consumption of Milk Products (lbs per person)",
       subtitle = "Not including fluid Milk",
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = c(seq(1975, 2015, by = 10)),
                     limits = c(1975, 2017)) +
  ggdark::dark_theme_bw() +
  scale_color_manual(values = cols) +
  theme(plot.title = element_text(family = "Agency FB", face = "bold",
                                  size = 30, hjust = 0.5),
        plot.subtitle = element_text(family = "Agency FB", face = "bold",
                                  size = 15, hjust = 0.5),
        plot.background = element_rect(fill = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Agency FB", size = 15),
        legend.key = element_blank(),
        axis.title = element_text(family = "Agency FB", size = 20, face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(family = "Agency FB", size = 20, face = "bold"),
        legend.text = element_text(family = "Agency FB", size = 15),
        legend.title = element_text(family = "Agency FB", size = 15))


ggsave("milk_consumption.jpg", p, units = "cm", 
       height = 25, width = 40, dpi = "retina")


           


