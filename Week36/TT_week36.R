# Tidy Tuesday week 36

# load libraries
pacman::p_load(tidyverse,tidytext, data.table,wordcloud2, forcats, viridis, ggrepel, extrafont)


# load file
medium <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-12-04/medium_datasci.csv")


# create variable word with unnest_tokens function
medium_tokens <- medium %>% unnest_tokens(word, input = title)

# remove stopwords
medium_tokens <- medium_tokens %>%
  anti_join(stop_words)

# remove numbers
rem_numbers <- medium_tokens %>% filter(str_detect(word, "^[0-9]")) %>% 
  select(word) %>% unique()

# cleaned dataset
medium_clean <- medium_tokens %>% 
  anti_join(rem_numbers, by = "word")

# word_counts
medium_wc <- medium_clean %>%
  count(word, sort = TRUE)

#------------------------plot 1-------------------
#--------------------sentiment plot-------------
medium_join <- medium_clean %>%
  inner_join(get_sentiments("nrc"), by = c("word")) %>%
  count(sentiment) %>%
  mutate(total = sum(n),
         prop = n / total) %>%
  mutate_at(vars(sentiment), funs(toupper))

ggplot(medium_join, aes(x = fct_reorder(sentiment, prop), y = prop, fill = sentiment)) +
  geom_col() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.1),
                     labels = c("0", "10", "20", 
                                "30", "40")) +
  coord_flip() +
  labs(title = "Medium Blog:", 
       subtitle = "Sentiment Analysis of Posts about Data Science", 
       x = "Sentiment", y = "Frequency (%)") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Agency FB", size = 36),
        plot.subtitle = element_text(family = "Agency FB", size = 22),
        axis.text = element_text(family = "Agency FB", size = 18),
        axis.title= element_text(family = "Agency FB", size = 20),
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.text = element_text(family = "Agency FB", size = 20))

#-----------------------plot 2---------------------------
#---------------------chatterplot---------------------
medium_claps <- medium_clean %>%
  group_by(word) %>%
  summarize(avg_claps = mean(claps))

# join
medium_new <- medium_wc %>%
  left_join(medium_claps, by = "word")

# choose top 100 words
medium_final <- medium_new %>%
  top_n(100, n) %>%
  na.omit()

medium_final %>%
  ggplot(aes(avg_claps, n, label = word)) + 
  geom_text_repel(segment.alpha = 0, aes(colour=avg_claps,
                                         size = n)) + 
  labs(title = "Top 100 Words from Medium Blog Posts about Data Science", 
       subtitle = "Word Frequency per Average Number of Claps", 
       x = "Average Number of Claps", y = "Frequency") + 
  guides(size = FALSE) +
  scale_color_viridis(name = "Average Number of Claps",
                      direction = -1, guide = guide_colorbar(
                        direction = "horizontal",
                        barheight = unit(2, units = "mm"),
                        barwidth = unit(100, units = "mm"),
                        draw.ulim = F,
                        title.position = 'top',
                        title.hjust = 0.5,
                        label.hjust = 0.5)) +
  theme_minimal() +
  theme(plot.title = element_text(family = "Agency FB", size = 36),
        plot.subtitle = element_text(family = "Agency FB", size = 22),
        axis.text = element_text(family = "Agency FB", size = 18),
        axis.title= element_text(family = "Agency FB", size = 20),
        legend.position = "top",
        legend.title = element_text(family = "Agency FB", size = 20), 
        legend.text = element_text(family = "Agency FB", size = 20))



#---------------------plot 3-----------------------
#--------------------wordcloud-------------------
medium_final %>% wordcloud2(., size = 1.6)
