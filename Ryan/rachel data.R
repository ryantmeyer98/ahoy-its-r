# OBJECTIVE ----
# Determine if there is a relationship between mouse size and density
# DV: body size 
# Fixed: density
# random: island 

# LIBRARIES ----
library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(multcompView)

# READ IN THE DATA ----
mouse.df <- read_csv("Resources DO NOT EDIT/rachel_data/mass_island_characteristics.csv")

# DATA MANIPULATIONS ----

# to determine if there is a relationship between body size and density, we first need density

# create a data frame with a 1 for every mouse measurement
test.df <- mouse.df %>%
  mutate(mouse = 1)

# create a summary data frame to see how many mice were caught on each island total 
test.df <- test.df %>%
  group_by(sampling_site) %>%
  summarize(total_mice = sum(mouse))

# now what if we put this value back into the full data frame
mouse.df <- mouse.df %>%
  mutate(n_mouse = case_when(
    sampling_site == "D'Arcy Island" ~ 7,
    sampling_site == "Mandarte Island" ~ 127,
    sampling_site == "Portland Island" ~ 3,
    sampling_site == "Saltspring Island" ~ 4, 
    sampling_site == "Saturna Island" ~ 1,
    sampling_site == "Sidney Island" ~ 34,
    sampling_site == "Vancouver" ~ 12,
    sampling_site == "Vancouver Island" ~ 16
  ))

# so we now have a data frame with the density of mice and the weight
# lets try a model 

# ANALYSIS ----
mandarte.df <- mouse.df %>%
  filter(sampling_site == "Mandarte Island")
not_mandarte.df <- mouse.df %>%
  filter(sampling_site != "Mandarte Island")

# create model 
one.lm <- lmer(weight_animal ~ success * (1|site_section),
               data = mandarte.df,
               REML = TRUE)

one.lm <- lm(weight_animal ~ success, data = not_mandarte.df)

# run model 
Anova(one.lm, type = "3", test.statistic = "F")

# random effects
ranova(one.lm)

# test assumptions
residuals <- resid(one.lm)
plot(fitted(one.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# what do these data look like?
mouse.df %>%
  ggplot(aes(success, weight_animal, color = sampling_site)) +
  geom_point() +
  geom_smooth(method = "lm")



