# tidy tuesday week 7

#libraries
library(readr)
library(ggplot2)
library(janitor) # to clean data
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(mice) # to deal with missing values
library(jpeg)
library(magick) # to read jpeg files
library(grid) # to add images to the plot
library(extrafont)
library(cowplot) # to add images to the axis
library(reshape2)

# import and load fonts
font_import()
loadfonts(device = "win")

# read file
star_wars_characters <- read_csv("StarWars.csv") %>%
  select(16:29) # select columns with the favorability ratings

# change columns' names  
colnames(star_wars_characters) <- unlist(star_wars_characters[1, ]) # assign the first row to columns
star_wars_characters <- star_wars_characters[-1,] %>% # delete first row
  clean_names() 


# make tidy the data
star_wars_characters_new <- star_wars_characters %>%
  rowid_to_column() %>% # without it, the spread function below will give an error
  gather(sw_character, favorability, -rowid) %>% # from wide to long format
   mutate(level_favorability = case_when(favorability == "Very favorably" ~ 2,
                                favorability == "Somewhat favorably" ~ 1,
                                favorability == "Neither favorably nor unfavorably (neutral)" ~ 0,
                                favorability == "Somewhat unfavorably" ~ -1,
                                favorability == "Very unfavorably" ~ -2)) %>%
  select(1:2, 4) %>%
  spread(sw_character, level_favorability) %>% # from long to wide format
  select(-1) %>%
  .[-which(rowMeans(is.na(.)) > 0.5), ] # remove rows with more than 50% NA

# convert the variables to integers
star_wars_characters_new [ ] <- lapply(star_wars_characters_new, function(x)
   as.integer(as.numeric(x)))

#-----dealing with the remaining missing cases-----#
# use of library mice #
# imputing the missing data
star_wars_final <- mice(star_wars_characters_new, m = 5, maxit = 50, meth = "pmm", seed = 500)

# back to dataframe
star_wars_final <- mice::complete(star_wars_final,1)
View(star_wars_final)
any(is.na(star_wars_final)) # to check if there are still missing cases

#-----plot-----#
# set theme
theme_set(theme_bw())

# image read
sw<- image_read("star_wars.jpg")


# divide the correlation graph in two
cor_div <- function(x) {
  L <- R <- cor(x)
  
  R[lower.tri(R, diag = TRUE)] <- NA
  R <- melt(R)
  names(R)[3] <- "dots"
  
  L[upper.tri(L, diag = TRUE)] <- NA
  L <- melt(L)
  names(L)[3] <- "labels"
  
  merge(R, L)
}

# Calculate df with cor_list
df <- star_wars_final %>%
  do(cor_div(.)) 

# plot - part1
pcor <- ggplot(df, aes(x = Var1, var, y = Var2)) +
  geom_abline(slope = -1, color = "black", linetype = 2, size = 0.5, intercept = nlevels(df$Var1) + 1) +
  annotation_custom(rasterGrob(sw), 
                    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(aes(col = dots, size = abs(dots)), shape = 16) +
  geom_text(aes(col = labels,  size = abs(labels), label = round(labels, 2)), family = "Georgia", fontface = "bold", size = 3) +
  scale_size(range = c(0, 6)) +
  scale_color_gradient2(name = "r", limits = c(-1, 1), low = "blue", mid = "grey", high = "red") +
  scale_y_discrete("", labels = c("Yoda", "R2-D2", "Princess Leia Organa", "Padmé Amidala","Obi-Wan Kenobi", "Luke Skywalker", 
                                  "Lando Calrissian", "Jar Jar Binks", "Han Solo", 
                                  "Emperor Palpatine", "Darth Vader", "C-3PO", 
                                  "Boba Fett", "Anakin Skywalker"), 
                   limits = rev(levels(df$Var1))) +
  scale_x_discrete("", labels = c("Anakin Skywalker", "Boba Fett", "C-3PO",
                                  "Darth Vader", "Emperor Palpatine", "Han Solo",
                                  "Jar Jar Binks", "Lando Calrissian", "Luke Skywalker",
                                  "Obi-Wan Kenobi", "Padmé Amidala", "Princess Leia Organa", 
                                  "R2-D2", "Yoda")) +
  guides(size = FALSE) +
  coord_fixed() +
  labs(title = "Correlation of favorability ratings of Star Wars characters", hjust = 1) +
  theme(plot.title = element_text(family = "Berlin Sans FB", face = "bold", 
                                  size = 15, hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text.y = element_text(family = "Calibri", size = 11, face = "bold", angle = 45, hjust = 1),
        axis.text.x = element_text(family = "Calibri", size = 11, face = "bold", angle = 45, hjust = 1), 
        panel.grid = element_blank(),
        legend.text = element_text(family = "Calibri", size = 11, hjust = 0.5),
        legend.title = element_text(family = "Calibri", size = 14),
        strip.background = element_blank())

# load images to put in the xaxis
yoda <- image_read("yoda.jpg")
r2_d2 <- image_read("r2d2.jpg")
organa <- image_read("organa.jpg")
padme <- image_read("padme.jpg")
lando <- image_read("lando.jpg")
obiwan <- image_read("obiwan.jpg")
luke <- image_read("luke.jpg")
jarjar <- image_read("jarjar.jpg")
emperor <- image_read("palpatine.jpg")
hansolo <- image_read("hansolo.jpg")
darth <- image_read("darth.jpg")
c3po <- image_read("c3po.jpg")
boba <- image_read("boba.jpg")
anakin <- image_read("anakin.jpg")

# create axis with images
pcor_image <- axis_canvas(pcor, axis = 'x') + 
  draw_image(anakin, x = 0.5, scale = 1) +
  draw_image(boba, x = 1.5, scale = 1) +
  draw_image(c3po, x = 2.5, scale = 1) +
  draw_image(darth, x = 3.5, scale = 1) +
  draw_image(emperor, x = 4.5, scale = 1) +
  draw_image(hansolo, x = 5.5, scale = 1) +
  draw_image(jarjar, x = 6.5, scale = 1) +
  draw_image(lando, x = 7.5, scale = 1) +
  draw_image(luke, x = 8.5, scale = 1) +
  draw_image(obiwan, x = 9.5, scale = 1) +
  draw_image(padme, x = 10.5, scale = 1) +
  draw_image(organa, x = 11.5, scale = 1) +
  draw_image(r2_d2, x = 12.5, scale = 1) +
  draw_image(yoda, x = 13.5, scale = 1) 

# add axis with images to the main plot
y <- ggdraw(insert_xaxis_grob(pcor, pcor_image, position = "bottom"))
y

