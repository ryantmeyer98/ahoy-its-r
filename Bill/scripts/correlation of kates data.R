library(tidyverse)
library(readxl)
library(janitor)
library(plotly)

r.df <- read_csv("Resources DO NOT EDIT/rachel_data/mass_island_characteristics.csv")

plot.plot <- r.df %>% ggplot(aes(success, weight_animal, shape=as.factor(date_year) , color=sampling_site))+
  geom_point()
ggplotly(plot.plot)


cor.test(r.df$success, r.df$weight_animal, method = c("pearson", "kendall", "spearman"))

plot.plot <- r.df %>% ggplot(aes(island_distance, weight_animal, shape=as.factor(date_year) , color=sampling_site))+
  geom_point()
ggplotly(plot.plot)

library(vegan)

test.df <- r.df %>% select(success, weight_animal, island_distance) %>% 
  na.omit()

out <- princomp(test.df )
biplot(out)
summary(out)

ev <- eigenvals(out)
ev

loadings <- scores (out, display = 'species', scaling = 0)
loadings
