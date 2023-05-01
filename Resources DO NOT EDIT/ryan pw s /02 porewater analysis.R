# LIBRARIES ----
library(tidyverse)
library(multcompView)
library(emmeans)
library(lme4)
library(lmerTest)
library(car)

# READ IN THE DATA ----

# read in the data 
porewater.df <- read_csv("output/porewater/cleaned data/clean porewater.csv") %>%
  mutate(year = as.factor(year),
         treatment = as.factor(treatment),
         block_ns = as.factor(block_ns))

# data frame for each nutrient
nitrogen.df <- porewater.df %>%
  filter(nutrient == "no3_mgl")

ammonia.df <- porewater.df %>%
  filter(nutrient == "nh3_mgl")

phosphorus.df <- porewater.df %>%
  filter(nutrient == "drp_ugl")

# remove ammonia outliers
ammonia.df <- ammonia.df %>%
  filter(value < 4)

# remove phosphorus outliers
phosphorus.df <- phosphorus.df %>%
  filter(value < 5000)

# NITRATE NITROGEN ANALYSIS ----

# chi square for non-normal data?

# model ----
# nitrate nitrogen is a function of treatment with the random effects year_date and block
# sqrt transformation - helps, but does not make the data normal
# remove mean add subplot within plot
nitrogen.lm = lmer(sqrt(value) ~ treatment * (1|year_date) + (1|subplot_point/plot) + 
                     (1|block_ns),
                   data = nitrogen.df, 
                   REML = TRUE) 


# results of the anova
Anova(nitrogen.lm, type = "3", test.statistic = "F")
summary(nitrogen.lm)

# significance of the random effects 
ranova(nitrogen.lm)

# test assumptions
residuals <- resid(nitrogen.lm)
plot(fitted(nitrogen.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# mean separation ----

# create emmean model 
nitrogen.emm = emmeans(nitrogen.lm, ~ treatment, adjust = "sidak")

# plot
plot(nitrogen.emm, comparisons = TRUE)

# run model 
multcomp::cld(nitrogen.emm, Letters = letters, adjust = "sidak")

# get p-values and save as a dataframe
emminteraction = emmeans(nitrogen.emm, pairwise ~treatment,
                        adjust = "sidak", alpha = 0.5)
emminteraction

# save as data frame
nitrogen_results.df <- 
  as.data.frame(multcomp::cld(nitrogen.emm, Letters = letters, adjust = "sidak"))

# back transformation
nitrogen_results.df <- nitrogen_results.df %>%
  mutate(corr_emmean = emmean^2) %>%
  mutate(corr_se = SE^2) %>%
  mutate(corr_plus_se = corr_emmean+corr_se) %>%
  mutate(corr_minus_se = corr_emmean-corr_se) %>%co
  select(treatment, .group, corr_emmean, corr_plus_se, corr_minus_se)

# save results
write_csv(nitrogen_results.df, file = "output/porewater/results/nitrogen results.csv")

# AMMONIA ANALYSIS ----

# model ----
# ammonia is a function of treatment with the fixed factors year_jdate and block
nh3.lm = lmer(sqrt(value) ~ treatment * (1|year_date) + (1|subplot_point/plot) + (1|block_ns),
              data = ammonia.df, 
              REML = TRUE) 

# look at residuals 
residuals <- resid(nh3.lm)
plot(fitted(nh3.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model to see results
Anova(nh3.lm, type = "3", test.statistic = "F")

# test the random effects
ranova(nh3.lm)

# mean separation ----
# not necessary - ammonia not significantly different 


# save results
# create results dataframe
ammonia_results.df <- ammonia.df %>%
  group_by(treatment) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            se = sd/19) # 19 is the square root of n = 361

# save results dataframe
write_csv(ammonia_results.df, file = "output/porewater/results/ammonia results.csv")


# DRP ANALYSIS ----

# model ----
# drp is a function of treatment with the fixed factors year_jdate and block
drp.lm = lmer(log10(value+1) ~ treatment * (1|year_date) + (1|subplot_point/plot) + (1|block_ns),
              data = phosphorus.df, 
              REML = TRUE) 

# look at residuals 
residuals <- resid(drp.lm)
plot(fitted(drp.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model 
# reject the null hypothesis, at least two groups have a mean difference different from 0
Anova(drp.lm, type = "3", test.statistic = "F")

# test the random effects
# in the model
ranova(drp.lm)

# summary means
drp_results.df <- phosphorus.df %>%
  group_by(treatment) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            se = sd/19)
# save results
write_csv(drp_results.df, file = "output/porewater/results/phosphorus results.csv")

