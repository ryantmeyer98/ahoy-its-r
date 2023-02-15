
# LOAD LIBRARIES ----
library(tidyverse)
library(janitor)
library(lme4)
library(lmerTest)
library(dgof)

# READ IN THE DATA ----

# read in the data
seeds.df <- read_csv("Bill/data/Liza one way anova seeds/Field data marvin_2022.csv") %>%
  clean_names()

# rename columns
seeds.df <- seeds.df %>%
  rename(no_seeds = main_seeds,
         area_mm = o_area,
         width_mm = o_width,
         length_mm = o_length)

# separate rep and source
seeds.df <- seeds.df %>%
  separate(line, c("source", "replicate"), "-", remove = FALSE) %>%
  select(-line)

# relevel the factors 
seeds.df <- seeds.df %>%
  mutate(source = fct_relevel(source, c("WT", "Mutant1", "Mutant2", "Mutant3", "Mutant4", 
                              "Mutant5", "Mutant6", "Mutant7", "Mutant8", "Mutant9")))

# what if we make it long
seeds_long.df <- seeds.df %>%
  pivot_longer(-c(id, source, replicate, no_seeds),
               names_to = "name",
               values_to = "value")

# quick exploratory plot
seeds_long.df %>%
  ggplot(aes(source, value)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 1) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.5) +
  coord_flip() +
  facet_wrap(~name)

# create the model
one.lm <- lmer(tgw_g ~ source * (1|replicate),
               data = seeds.df,
               REML = TRUE)

Anova(one.lm, type = "3", test.statistic = "F")
ranova(one.lm)
summary(one.lm)

# test assumptions
# visual assumptions 
residuals <- resid(one.lm)
plot(fitted(one.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# statistical tests

# normality of residuals
# Ho: residuals normally distributed about the mean, Ha: not normally distributed about mean
# p-value < 0.05, reject null hypothesis, residuals not normal
shapiro.test(residuals)

# HOV
# Ho: variance is homogenous, Ha: variance is not homogenous
# p-value > 0.05, fail to reject null hypothesis, variance is homogenous. 
leveneTest(residuals(one.lm) ~ seeds.df$source)

ks.test(residuals, "pnorm")


# trying glmer
two.lm <- glmer(sqrt(tgw_g) ~ source *(1|replicate),
                family = Gamma,
                data = seeds.df)

# test assumptions
# visual assumptions 
residuals2 <- resid(two.lm)
plot(fitted(two.lm), residuals2)
qqnorm(residuals2)
qqline(residuals2)
hist(residuals2)

# statistical tests
# normality
shapiro.test(residuals2)
# hov - brown forseith 
leveneTest(residuals(two.lm) ~ seeds.df$source)


install.packages("dgof")
ks.test(residuals, "pnorm")




install.packages("olsrr")
library(olsrr)
ols_test_normality(one.lm)


install.packages("nortest")
library(nortest)
ad.test(residuals)
shapiro.test(residuals)

