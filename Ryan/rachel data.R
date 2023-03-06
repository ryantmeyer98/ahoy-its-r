# OBJECTIVE ----
# does body weight depends on population density

# MODEL ----
# DV: body size = weight_animal
# Fixed: density = success
# random: island ???

# LIBRARIES ----
library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(multcompView)

# READ IN THE DATA ----
mouse.df <- read_csv("Resources DO NOT EDIT/rachel_data/mass_island_characteristics.csv")

# INITIAL PLOTTING ----
mouse.df %>%
  ggplot(aes(success, weight_animal, color = sampling_site, shape = sampling_site)) +
  geom_point() +
  theme_classic()

# CREATE OUR MODELS ----

# is there a correlation between weight and success
# no, there is no relationship between animal weight and trapping success 
cor.test( ~ weight_animal + success, 
          data = mouse.df,
          method = "pearson",
          conf.level = 0.95)

cor.test(mouse.df$weight_animal, mouse.df$success, method = "pearson")

# TEST ASSUMPTIONS ----

# RUN MODEL ----

# FINAL FIGURE ----


