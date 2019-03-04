# week 8 Tidy Tuesday

library(tidyverse) # wrangle, visualization data
library(data.table) # load file
library(highcharter) # interactive data visualizations

options(scipen = 999)

# open data
phd <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")




# top 20 fields
phd_per <- phd  %>%
  group_by(field) %>%
  summarize(phd_number = sum(n_phds, na.rm = TRUE)) %>%
  mutate(phd_perc = phd_number / sum(phd_number) * 100) %>%
  top_n(20, phd_perc) %>%
  arrange(desc(phd_perc))


# highcharter plot
hchart(phd_per, "bar",
       hcaes(x = fct_reorder(field, phd_perc),
             y = phd_perc)) %>%
  hc_add_theme(hc_theme_chalk()) %>%
  hc_title(text = "Percentage of PhDs: Top 20 Fields", margin = 10, 
           fontSize = "50px") %>%
  hc_xAxis(title = list(text = NULL)) %>% 
  hc_yAxis(title = list(text = "Percentage")) %>% 
  hc_subtitle(text = "From 2008 to 2017") %>%
  hc_credits(enabled = TRUE,
             text = "Tidy Tuesday Week 8",
             style = list(
               fontSize = "14px"
             )
  )
  


