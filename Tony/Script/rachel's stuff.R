#load libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(multcompView)
library(plotly)
library(vegan)

#reading in data
r.df <- read_csv("Resources DO NOT EDIT/rachel_data/mass_island_characteristics.csv")

#graphing animal weight by success
plot.plot <- r.df %>% ggplot(aes(success, weight_animal, shape = as.factor(date_year), color = sampling_site)) +
  geom_point() +
  theme_classic()
plot.plot

#looking at interactive graph
ggplotly(plot.plot)

#correlation test - no correlation
cor.test(r.df$success, r.df$weight_animal, method = "pearson")

#graphing animal weight by island distance
island.plot <- r.df %>% ggplot(aes(island_distance, weight_animal, shape = as.factor(date_year), color = sampling_site)) +
  geom_point() +
  theme_classic()

#looking at interactive graph
ggplotly(island.plot)

test.df <- r.df %>% select(success, weight_animal, island_distance) %>%
  na.omit()

compPC <- prcomp(test.df, center = TRUE, scale = TRUE)
summary(compPC)
cor(test.df)
