# week 4 Tidy Tuesday

# libraries needed
library(tidyverse)
library(data.table)
library(hrbrthemes)
library(gridExtra)

# load database
gender_australia <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-04-23/week4_australian_salary.csv") %>%
  separate(occupation, into = "job", sep = ";", extra = "drop")

# select top 20 jobs
top20_jobs <- gender_australia %>%
  top_n(20, average_taxable_income) %>%
  select(job)

# select bottom 20 jobs
bottom20_jobs <- gender_australia %>%
  top_n(-20, average_taxable_income) %>%
  select(job)

# create data frame with the diff between men and women in average payment
diff_bottom <- gender_australia %>%
  filter(job %in% bottom20_jobs$job)

diff_top <- gender_australia %>%
  filter(job %in% top20_jobs$job)

#----------plot-----------#
# plot highest paying jobs
plot_top <- ggplot(diff_top, aes(average_taxable_income, job)) +
  geom_line(aes(group = job)) +
  geom_point(aes(color = gender), size = 3) + 
  scale_colour_manual(values = c("blue", "red", "orange"), 
                      labels = c("Women", "Men", "Median Income")) +
  scale_x_continuous(breaks = c(200000, 300000, 400000, 500000), 
                     labels = c("$200k", "$300k", "$400K", "$500K")) +
  geom_vline(aes(xintercept=median(diff_top$average_taxable_income),
                 color="median"), linetype="dashed",
             size=1) +
  labs(title = "The 20 highest paying jobs in Australia", 
       subtitle = "Differences of Income between men and women", 
       x = NULL,
       y = NULL) +
  theme_ft_rc(grid = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))

# plot lowest paying jobs
plot_bottom <- ggplot(diff_bottom, aes(average_taxable_income, job)) +
  geom_line(aes(group = job)) +
  geom_point(aes(color = gender), size = 3) +
  scale_colour_manual(values = c("blue", "red", "orange"), 
                      labels = c("Women", "Men", "Median Income")) +
  scale_x_continuous(breaks = c(20000, 30000, 40000), 
                     labels = c("$20k", "$30K", "$40K")) +
  geom_vline(aes(xintercept=median(diff_bottom$average_taxable_income),
                 color="median"), linetype="dashed",
             size=1) +
  labs(title = "The 20 lowest paying jobs in Australia", 
       subtitle = "Differences of Income between men and women",
       x = NULL,
       y = NULL) +
  theme_ft_rc(grid = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")
        , axis.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5))


bottom_top <- grid.arrange(plot_bottom, plot_top, ncol=2)

ggsave("tt_week4.jpg", bottom_top, width = 40, height = 20, units = "cm")
