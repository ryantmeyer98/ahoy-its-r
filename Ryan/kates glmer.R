
# LOAD LIBRARIES ----
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(car)

# DV: n
# fixed effects: zone
# random effects: time

# read in the data 

# 2
complete.df <- read_excel("Resources DO NOT EDIT/kate_data/kates complete_adults_KE.xlsx")

##############################################################################################
# So this model uses a normal distribution for the data. I did a log+1 transformation because
# of the zeroes just to see what would happen. I know that's now how you are actually supposed
# to handle the data. However, it does show the syntax for random and nested effects if that
# is useful. 
##############################################################################################

# create model 
one.lm <- lmer(log10(n+1) ~ zone * (1|week_eclosed) * (1|zone:ovicup),
               data = complete.df,
               REML = TRUE)

# run model - gives the results of the model from the car package
Anova(one.lm, type = "3", test.statistic = "F")
# ranova is the function for testing random effects from the lmerTest package
ranova(one.lm)
# summary gives a summary of the model and is a function from base R
summary(one.lm)


##############################################################################################
# This model is the one that uses the poisson distribution with the same random and nested 
# effects. glmer is a function within the lme4 package. I'm working on figuring out the details,
# but glmer seems to be a bit more versatile function compared to lmer where you can better
# specify the family. Analysis of random effects is also a little different here and I am 
# working on that as well. 
##############################################################################################

# glmer
two.lm <- glmer(n ~ zone + (1|week_eclosed) * (1|zone:ovicup),
                family = poisson,
                data = complete.df)

# run model - notice it uses a chi-square distribution, which i think is appropriate here as 
# you have count data, hence the poisson distribution
Anova(two.lm, type = "3")
# summary
summary(two.lm)

# this is how you pull out the random intercepts, still working on it
ranef(two.lm)

# checking assumptions
# check assumptions 
residuals <- resid(two.lm)
plot(fitted(two.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)




# expected mean 
# covariance parameter estimate
# standard error
# contrasts








