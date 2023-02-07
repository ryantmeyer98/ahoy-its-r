
# LOAD LIBRARIES ----
# tidyverse for all of the tidyverse language
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(car)

# DV: n
# fixed effects: zone
# random effects: time

# read in the data 

# 1
emergence.df <- read_csv("Resources DO NOT EDIT/kate_data/adult_emergence_weekly.csv")
# 2
complete.df <- read_excel("Resources DO NOT EDIT/kate_data/kates complete_adults_KE.xlsx")
# 3
treatment.df <- read_csv("Resources DO NOT EDIT/kate_data/treatment_assignments_RM.csv")

# pseudoreplication fix
complete.df <- complete.df %>%
  group_by(week_eclosed, zone) %>%
  summarize(avg_n = mean(n))

# create model 
one.lm <- lmer(log(n+1) ~ zone * (1|week_eclosed) * (1|zone:ovicup),
               data = complete.df,
               REML = TRUE)

# glmer
one.lm <- glmer(n ~ zone + (1|week_eclosed) * (1|zone:ovicup),
                family = poisson,
                data = complete.df)

summary(one.lm)

# run model 
Anova(one.lm, type = "3")
summary(one.lm)

# random effect
ranova(one.lm)
# expected mean 
# covariance parameter estimate
# standard error
# contrasts

# check assumptions 
residuals <- resid(one.lm)
plot(fitted(one.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)






