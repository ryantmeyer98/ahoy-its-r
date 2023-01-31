# # install packages
# install.packages("Hmisc")
# install.packages("multcomp")
# install.packages("multcompView")

# Load libraries -----
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

# read file -----
s.df <- read_excel("data/liza/Field data marvin_2022.xlsx") %>% 
  clean_names()

# clean up the column names -----
s.df <- s.df %>% 
  rename(
    seed_no = main_seeds,
    area_mm = o_area,
    width_mm = o_width,
    lenght_mm = o_length)

# Clean grouping factors ------
s.df <- s.df %>% 
  separate(line, c("source", "replicate"),  "-", remove=FALSE)

# Create factors ------
s.df <- s.df %>% 
  mutate(line= as.factor(line),
         source = as.factor(source),
         replicate = as.factor(replicate))

# reorder factors ----
levels(s.df$source)
s.df <- s.df %>% 
  mutate(source = fct_relevel(source, 
                              c("WT", "Mutant1", "Mutant2", "Mutant3", "Mutant4", "Mutant5",
                                "Mutant6", "Mutant7", "Mutant8", "Mutant9" )))

# Graph data ------
tgw.plot <- s.df %>% 
  ggplot(aes(x=source, y = tgw_g, 
             color=source, group = source)) +
  stat_summary(
    fun=mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.2)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.2)) +
  theme_light() 
tgw.plot

## Save Plot ----
ggsave(file="Outputs/Seed_TGW_plot.pdf", 
       tgw.plot,
       width = 5, height = 5, 
       units="in")

# Assumptions Tests ------
## Homogeneity of Variance----

leveneTest(tgw_g ~ source,
           data= s.df)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df  F value  Pr(>F)
# group  9  0.9507    0.4868
#        80    
# Note this is not signficant so assumtion met

bartlett.test(tgw_g ~ source,
              data= s.df)
# Bartlett test of homogeneity of variances
# 
# data:  tgw_g by source
# Bartlett's K-squared = 12.573, df = 9, p-value = 0.1829
# same result for Bartlet Test


# ONE-WAY ANOVA -----
## Anova using aov -----
tgw_anova.model <- aov(tgw_g  ~ source, data = s.df)
summary(tgw_anova.model)

#               Df  Sum Sq  Mean Sq   F value   Pr(>F)    
# source        9   1.268   0.14087   6.332   1e-06 ***
#   Residuals   80  1.780   0.02225                   
# yep there is a difference

## Anova using lm ----
tgw_anova_lm.model <- lm(tgw_g  ~ source, data = s.df)
summary(tgw_anova_lm.model)
Anova(tgw_anova_lm.model, type=3)

#Checking Assumptions -----
# Checking normality graphically ----
par(mfrow = c(1,2))  # This code put two plots in the same window
hist(tgw_anova_lm.model$residuals)   # Makes histogram of residuals  
plot(tgw_anova_lm.model, which = 2)   # Makes Q-Q plot

# Checking homoscedasticity (Homogeneity of variances) -----
plot(tgw_anova_lm.model, which = 1)  # Makes residuals VS fitted plot


#or this way 
qqPlot(tgw_anova_lm.model, distribution = "norm")

# Checking normality statistically ----
shapiro.test(tgw_anova_lm.model$residuals)

# Shapiro-Wilk normality test
# data:  tgw_anova_lm.model$residuals
# W = 0.97465, p-value = 0.07552
# all looks OK but its close

# Post Hoc tests ---------
tgw_model.emm <- emmeans(tgw_anova_lm.model, ~ source)
tgw_model.emm

# plot of comparisons
# blue are confidence intervals, red arrows overlap mean no significant diff
plot(tgw_model.emm , comparisons = TRUE)


## Pairwise comparisons with emmeans -----
tgw_emm_pairs = emmeans(tgw_model.emm, 
                           pairwise ~ source,
                           adjust="sidak")
summary(tgw_emm_pairs)
tgw_emm_pairs$emmeans

tgw_emm_pairs$contrasts

## Pairwise with letters -----
cld<- multcomp::cld(tgw_emm_pairs,
                    alpha=.05,
                    Letters=letters)



## Post hoc test plot of 95% CI
# https://broom.tidymodels.org/reference/tidy.emmGrid.html
# https://broom.tidymodels.org/reference/tidy.summary_emm.html
library(broom)
## 95% CI from emmean -----
tidy(tgw_model.emm)

ggplot(tidy(tgw_model.emm, conf.int = TRUE), aes(source, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))

# Emmean and SE plot ------
# marginal averages
marginal <- emmeans(tgw_model.emm, "source")
tidy(marginal)

tidy(emmeans(tgw_model.emm, "source")) %>% 
  ggplot(aes(source, estimate, color=source)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error)) +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5) 

# the olde way ------
tgw_emmeans.df <- as.data.frame(tgw_emm_pairs$emmeans)

# Note this is the same as the first plot
tgw_emmeans.plot <- tgw_emmeans.df %>% 
  ggplot(aes(x=source, color=source)) +
  geom_point(aes(y=emmean), size=3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                stat="identity", width = 0.2) +
  labs(x="", y="Weight (g)") 
tgw_emmeans.plot



# A different graph and method ----
# https://schmidtpaul.github.io/DSFAIR/compactletterdisplay.html
# add letters to each mean
model_means_cld <- cld(object = tgw_model.emm,
                       adjust = "sidak",
                       Letters = letters,
                       alpha = 0.05)

# show output
model_means_cld

## now the new plot version ------
# optional: sort factor levels of groups column according to highest mean

# ...in means table
model_means_cld <- model_means_cld %>% 
  mutate(.group = fct_reorder(.group, emmean))

# # ...in data table
# s.df <- s.df %>% 
#   mutate(.group = fct_relevel(source, levels(model_means_cld$.group)))

# base plot setup
tgw_letters.plot <-s.df %>% 

  ggplot() +
  # x-axis
  scale_x_discrete(name = "Source") +
  # black data points
  geom_point(
    data = s.df,
    aes(y = tgw_g, x = source),
    shape = 16,
    alpha = 0.5,
    position = position_nudge(x = -0.3)) +
  # black boxplot
  geom_boxplot(
    data = s.df,
    aes(y = tgw_g, x = source),
    width = 0.15,
    outlier.shape = NA,
    position = position_nudge(x = -0.1)) +
  # red mean value
  geom_point(
    data = model_means_cld,
    aes(y = emmean, x = source),
    size = 2,
    color = "red",
    position = position_nudge(x = 0.1) ) +
  # red mean errorbar
  geom_errorbar(
    data = model_means_cld,
    aes(ymin = lower.CL, ymax = upper.CL, x = source),
    width = 0.1,
    color = "red",
    position = position_nudge(x = 0.1)) +
  # red letters
  geom_text(
    data = model_means_cld,
    aes(
      y = emmean,
      x = source,
      label = str_trim(.group)
    ),
    position = position_nudge(x = 0.2),
    hjust = 0,
    color = "red") +
  labs(y="tgw (g)", x="")
# caption
labs(caption = str_wrap("Black dots represent raw data. Red dots and error bars 
                       represent (estimated marginal) means ± 95% confidence 
                       interval per group. Means not sharing any letter are 
                       significantly different by the Sidak-test at the 5% 
                       level of significance.", width = 70)) +
  theme_classic() 
tgw_letters.plot
## Save Plot ----
ggsave(file="Outputs/TGW_letters_plot.pdf", 
       tgw_letters.plot,
       width = 5, height = 5, 
       units="in")

