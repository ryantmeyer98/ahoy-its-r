library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(janitor)
## Load Anova Specific libraries -----
library(car)
library(emmeans)
library(lme4)
library(multcomp)
library(multcompView)
library(scales)  

island.df <- read_csv("Resources DO NOT EDIT/rachel_data/mass_island_characteristics.csv")

cor.test(island.df$weight_animal, island.df$island_distance, method = "pearson")

#such a small sample size hard to describe sample to popualtion of island
#use a PCA to describe
install.packages("vegan")
library(vegan)
#factoextra

test.df <- island.df %>% 
  select(success, weight_animal, island_distance) %>% 
  na.omit() 

princomp(test.df)


