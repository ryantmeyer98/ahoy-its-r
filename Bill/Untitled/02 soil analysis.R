# LIBRARIES ----
library(tidyverse)
library(multcompView)
library(emmeans)
library(lme4)
library(lmerTest)
library(car)

# OVERALL RESULTS ----
# Pennycress use results in statistically significant reductions in soil nitrogen,
# phosphorus, potassium, and sulfur and statistically significant increases in soil
# calcium and magnesium. 

# READ IN THE DATA ----
soil.df <- read_csv("output/soils/soils_long_raw.csv")

# we are only interested in spring right now, so lets reduce the data frame to that 
reduced.df <- soil.df %>%
  filter(month == 6)

# also we want to get rid of the 2020 data because we had no pc establishment then
reduced.df <- soil.df %>%
  filter(year != 2020)

# now, we are interested in determining differences in the dv by our iv:treatment, 
# will do a facet by variable to see what may be interesting here
reduced.df %>%
  ggplot(aes(treatment, value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")

# one of the things we are interested in is soil fertility, so lets look at the macronutrients
# that plants really like - nitrogen, potassium, phosphorus, sulfur, calcium, magnesium 
# https://www.fao.org/global-soil-partnership/areas-of-work/soil-fertility/en/ 

# to do so i will create a dataframe for each macronutrient so i dont have to pipe into a glm

n.df <- reduced.df %>%
  filter(variable == "soils_no3_n_ppm")

p.df <- reduced.df %>%
  filter(variable == "soils_p_ppm")

k.df <- reduced.df %>%
  filter(variable == "soils_k_ppm")

s.df <- reduced.df %>%
  filter(variable == "soils_s_ppm")

ca.df <- reduced.df %>%
  filter(variable == "soils_ca_ppm")

mg.df <- reduced.df %>%
  filter(variable == "soils_mg_ppm")

# now, what are our factors:
# fixed = treatment
# random = date, block_no
# will include subplot nested within plot to properly replicate the data

# model = DV ~ treatment * (1|date) + (1|block_no) 

# N ANALYSIS ----

# log 10 transformation for normality
n.lm = lmer(log10(value) ~ treatment * (1|date) + (1|block_no) + (1|subplot_point/plot),
            data = n.df, 
            REML = TRUE)

# check assumptions 
residuals <- resid(n.lm)
plot(fitted(n.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model 
Anova(n.lm, type = "3", test.statistic = "F")

# test random effects
ranova(n.lm)

# significant difference between groups, post hoc tests

# mean separation

# create emmean model 
n.emm = emmeans(n.lm, ~ treatment, adjust = "sidak")

# get p-values
emminteraction = emmeans(n.emm, pairwise ~treatment,
                         adjust = "sidak", alpha = 0.5)
emminteraction$contrasts

# save as data frame with letters
n_results.df <- 
  as.data.frame(multcomp::cld(n.emm, Letters = letters, adjust = "sidak"))

# back transform data
n_results.df <- n_results.df %>%
  mutate(corr_emmean = 10^emmean) %>%
  mutate(corr_se = 10^SE) %>%
  mutate(corr_plus_se = corr_emmean + corr_se) %>%
  mutate(corr_minus_se = corr_emmean - corr_se) %>%
  select(treatment, .group, corr_emmean, corr_plus_se, corr_minus_se, corr_se)

# P ANALYSIS ---

# create model - log10 transformation for normality 
p.lm = lmer(log10(value) ~ treatment *(1|date) + (1|block_no) + (1|subplot_point/plot),
            data = p.df, 
            REML = TRUE)

# check assumptions 
residuals <- resid(p.lm)
plot(fitted(p.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model 
Anova(p.lm, type = "3", test.statistic = "F")

# random effects
ranova(p.lm)

# fail to reject Ho, groups are the same

# results file
p_results.df <- p.df %>%
  group_by(treatment) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            se = sd/11.6)

# K ANALYSIS ----

# create model - log10 transformation for normality 
k.lm = lmer(log10(value) ~ treatment *(1|date) + (1|block_no) + (1|subplot_point/plot),
            data = k.df, 
            REML = TRUE)

# check assumptions 
residuals <- resid(k.lm)
plot(fitted(k.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model 
Anova(k.lm, type = "3", test.statistic = "F")

# random effects
ranova(k.lm)

# mean separation, glm significant 

# create emmean model 
k.emm = emmeans(k.lm, ~ treatment, adjust = "sidak")

# get p-values
emminteraction = emmeans(k.emm, pairwise ~treatment,
                         adjust = "sidak", alpha = 0.5)
emminteraction$contrasts

# save as data frame with letters
k_results.df <- 
  as.data.frame(multcomp::cld(k.emm, Letters = letters, adjust = "sidak"))

# back transform data
k_results.df <- k_results.df %>%
  mutate(corr_emmean = 10^emmean) %>%
  mutate(corr_se = 10^SE) %>%
  mutate(corr_plus_se = corr_emmean + corr_se) %>%
  mutate(corr_minus_se = corr_emmean - corr_se) %>%
  select(treatment, .group, corr_emmean, corr_plus_se, corr_minus_se, corr_se)

# S ANALYSIS ----

# create model - dont think these need a transformation
s.lm = lmer(value ~ treatment *(1|date) + (1|block_no) + (1|subplot_point/plot),
            data = s.df, 
            REML = TRUE)

# check assumptions 
residuals <- resid(s.lm)
plot(fitted(s.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model 
Anova(s.lm, type = "3", test.statistic = "F")

# random effects
ranova(s.lm)

# fail to reject null hypothesis
# save results dataframe 
s_results.df <- s.df %>%
  group_by(treatment) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            se = sd/11.6)


# CA ANALYSIS ----

# create model - log10 transformation for normality 
ca.lm = lmer(log10(value) ~ treatment *(1|date) + (1|block_no) + (1|subplot_point/plot),
            data = ca.df, 
            REML = TRUE)

# check assumptions 
residuals <- resid(ca.lm)
plot(fitted(ca.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model 
Anova(ca.lm, type = "3", test.statistic = "F")

# random effects
ranova(ca.lm)

# mean separation, glm significant

# create emmean model 
ca.emm = emmeans(ca.lm, ~ treatment, adjust = "sidak")

# get p-values
emminteraction = emmeans(ca.emm, pairwise ~treatment,
                         adjust = "sidak", alpha = 0.5)
emminteraction$contrasts

# save as data frame with letters
ca_results.df <- 
  as.data.frame(multcomp::cld(ca.emm, Letters = letters, adjust = "sidak"))

# back transform data
ca_results.df <- ca_results.df %>%
  mutate(corr_emmean = 10^emmean) %>%
  mutate(corr_se = 10^SE) %>%
  mutate(corr_plus_se = corr_emmean + corr_se) %>%
  mutate(corr_minus_se = corr_emmean - corr_se) %>%
  select(treatment, .group, corr_emmean, corr_plus_se, corr_minus_se, corr_se)

# MG ANALYSIS ----

# create model - no transformation for normality - ask bill about this normality too, its meh
mg.lm = lmer(value ~ treatment *(1|date) + (1|block_no) + (1|subplot_point/plot), 
             data = mg.df, 
             REML = TRUE)

# check assumptions 
residuals <- resid(mg.lm)
plot(fitted(mg.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model 
Anova(mg.lm, type = "3", test.statistic = "F")

# random effects
ranova(mg.lm)

# mean separation

# create emmean model 
mg.emm = emmeans(mg.lm, ~ treatment, adjust = "sidak")

# get p-values
emminteraction = emmeans(mg.emm, pairwise ~treatment,
                         adjust = "sidak", alpha = 0.5)
emminteraction

# save as data frame with letters
mg_results.df <- 
  as.data.frame(multcomp::cld(mg.emm, Letters = letters, adjust = "sidak"))


# SAVE RESULTS TO OUTPUT FOLDER

# nitrogen
write_csv(n_results.df, file = "output/soils/n results.csv")

# phosphorus
write_csv(p_results.df, file = "output/soils/p results.csv")

# potassium
write_csv(k_results.df, file = "output/soils/k results.csv")

# sulfur
write_csv(s_results.df, file = "output/soils/s results.csv")

# calcium
write_csv(ca_results.df, file = "output/soils/ca results.csv")

# magnesium 
write_csv(mg_results.df, file = "output/soils/mg results.csv")





