# # install packages
# install.packages("Hmisc")
install.packages("multcomp")
install.packages("multcompView")

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
s.df <- read_excel("Resources DO NOT EDIT/Untitled/Field data marvin_2022.xlsx") %>% 
  clean_names()

# clean up the column names -----
s.df <- s.df %>% 
  rename(
    seed_no = main_seeds,
    area_mm2 = o_area,
    width_mm = o_width,
    length_mm = o_length,
    sample_wt_g = weight_g,
    seed_wt_g = tgw_g)

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

# length plot
length.plot <- s.df %>% 
  ggplot(aes(source, length_mm, color=source))+
  stat_summary(
    fun=mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.2)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.2)) +
  labs(x = "", y = "") +
  theme_light() +
  ggtitle("Length (mm)") +
  coord_flip() 
length.plot

# width plot
width.plot <- s.df %>% 
  ggplot(aes(source, width_mm, color=source))+
  stat_summary(
    fun=mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.2)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.2)) +
  labs(x = "Mutant", y = "") +
  theme_light() +
  # theme(axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank(),
  #       axis.line.y = element_blank()) +
  ggtitle("Width (mm)") + 
  coord_flip() +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"))
width.plot

# area plot
area.plot <- s.df %>% 
  ggplot(aes(source, area_mm2, color=source))+
  stat_summary(
    fun=mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.2)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.2)) +
  theme_light() +
  # theme(axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank(),
  #       axis.line.y = element_blank()) +
  ggtitle("Area (mm^2)") +
  labs(x = "Mutant", y = "") +
  coord_flip()  
# theme(axis.text.x = element_text(vjust = 0.5, angle = 30)) +
# theme(plot.margin = margin(0, 0, 0, 0, "pt"),
#       axis.text.y = element_blank(),
#       axis.ticks.y = element_blank(),
#       axis.line.y = element_blank(),
#       axis.title.y = element_blank(),
#       axis.ticks.length.y = unit(0, "pt"))
area.plot

# patchwork 
length.plot +  theme(plot.margin = margin(0, 0, 0, 0, "pt")
)+
  plot_spacer() + 
  width.plot +  theme(plot.margin = margin(0, 0, 0, 0, "pt"),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.line.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks.length.y = unit(0, "pt"))+
  plot_spacer() +
  area.plot +  theme(plot.margin = margin(0, 0, 0, 0, "pt"),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.line.y = element_blank(),
                     axis.title.y = element_blank(),
                     axis.ticks.length.y = unit(0, "pt"))+
  plot_layout(widths = c(2, -.1, 2, -.1, 2), guides = "collect", ncol = 5)

# https://stackoverflow.com/questions/70218158/how-to-reduce-the-space-between-to-plots-when-using-patchwork
# G1 + plot_spacer() + G2 + plot_layout(widths = c(4, -1.1 ,4.5),guides = "collect")& theme(legend.position = "top")


# Graph data ------
weight.plot <- s.df %>% 
  ggplot(aes(x=source, y = seed_wt_g, 
             color=source, group = source)) +
  stat_summary(
    fun=mean, na.rm = TRUE, geom = "point", size = 3,
    position = position_dodge(width = 0.2)) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE, geom = "errorbar", width = 0.3,
    position = position_dodge(width = 0.2)) +
  theme_light() 
weight.plot

## Save Plot ----
ggsave(file="output/Weed_weight_plot.pdf", 
       weight.plot,
       width = 5, height = 5, 
       units="in")

# Assumptions Tests ------
## Homogeneity of Variance----

leveneTest(seed_wt_g ~ source,
           data= s.df)

# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  9  0.3862 0.9385
#       80   

bartlett.test(seed_wt_g ~ source,
              data= s.df)

# Bartlett test of homogeneity of variances
# 
# data:  seed_wt_g by source
# Bartlett's K-squared = 16.786, df = 9, p-value = 0.05217

# ONE-WAY ANOVA -----
## Anova using aov -----
weight_anova.model <- aov(seed_wt_g  ~ source, data = s.df)
summary(weight_anova.model)

# Df Sum Sq Mean Sq F value Pr(>F)    
# source       9  3.287  0.3652   25.02 <2e-16 ***
# Residuals   80  1.168  0.0146                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1        
# yep there is a difference

## Anova using lm ----
weight_anova_lm.model <- lm(seed_wt_g  ~ source, data = s.df)
summary(weight_anova_lm.model)
Anova(weight_anova_lm.model, type=3)

#Checking Assumptions -----
# Checking normality graphically ----
par(mfrow = c(1,2))  # This code put two plots in the same window
hist(weight_anova_lm.model$residuals)   # Makes histogram of residuals  
plot(weight_anova_lm.model, which = 2)   # Makes Q-Q plot

# Checking homoscedasticity (Homogeneity of variances) -----
plot(weight_anova_lm.model, which = 1)  # Makes residuals VS fitted plot


#or this way 
qqPlot(weight_anova_lm.model, distribution = "norm")

# Checking normality statistically ----
shapiro.test(weight_anova_lm.model$residuals)

#use residuals because accounting for variation in samples from the treatment means

# Shapiro-Wilk normality test
# data:  weight_anova_lm.model$residuals
# W = 0.91646, p-value = 2.39e-05

# SO WE SHOULD STOP HERE AS THE ASSUMPTION IS VIOLATED or not
# THE CODE BELOW WOULD SSUME THAT RESIDUALS WERE NORMALLY DISTRIBUTED
# Post Hoc tests ---------
weight_model.emm <- emmeans(weight_anova_lm.model, ~ source)
weight_model.emm
#accounts for the difference in sample number, better to preport

# plot of comparisons
# blue are confidence intervals, red arrows overlap mean no significant diff
plot(weight_model.emm , comparisons = TRUE)


## Pairwise comparisons with emmeans -----
weight_emm_pairs = emmeans(weight_model.emm, 
                           pairwise ~ source,
                           adjust="tukey")
summary(weight_emm_pairs)
weight_emm_pairs$emmeans

weight_emm_pairs$contrasts

## Pairwise with letters -----
cld <- multcomp::cld(weight_model.emm,
                     alpha=.05,
                     Letters=letters)
cld


data.df <- as.data.frame(multcomp::cld(weight_emm_pairs, Letters = letters,
                                       adjust = "tukey"))

## Post hoc test plot of 95% CI
# below re different ways
# https://broom.tidymodels.org/reference/tidy.emmGrid.html
# https://broom.tidymodels.org/reference/tidy.summary_emm.html


ggplot(data=cld, aes( x= reorder(source, emmean), y = emmean, color = source)) +
  geom_point()+
  geom_errorbar(aes(min = emmean-SE, ymax = emmean+SE), width=0.3) +
  geom_text(aes(label = .group), hjust=0,vjust = 0)+
  labs(x="", y="Weight (g)")  +
  theme_light()

# A different graph and method ----
# https://schmidtpaul.github.io/DSFAIR/compactletterdisplay.html
# add letters to each mean
model_means_cld <- cld(object = weight_model.emm,
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
ggplot() +
  # x-axis
  scale_x_discrete(name = "Source") +
  # black data points
  geom_point(
    data = s.df,
    aes(y = seed_wt_g, x = source),
    shape = 16,
    alpha = 0.5,
    # position = position_nudge(x = 0)
    position = position_dodge2(width=0.05)
  ) +
  # black boxplot
  geom_boxplot(
    data = s.df,
    aes(y = seed_wt_g, x = source),
    width = 0.15,
    outlier.shape = NA,
    position = position_nudge(x = -0.2)) +
  # red mean value
  geom_point(
    data = model_means_cld,
    aes(y = emmean, x = source),
    size = 2,
    color = "red",
    position = position_nudge(x = 0.2)
  ) +
  # red mean errorbar
  geom_errorbar(
    data = model_means_cld,
    aes(ymin = lower.CL, ymax = upper.CL, x = source),
    width = 0.1,
    color = "red",
    position = position_nudge(x = 0.2)) +
  # red letters
  geom_text(
    data = model_means_cld,
    aes(
      y = emmean,
      x = source,
      label = str_trim(.group)
    ),
    position = position_nudge(x = 0.3),
    hjust = 0,
    color = "red") +
  labs(y="weight (g)", x="") +
  # caption
  labs(caption = str_wrap("Black dots represent raw data. Red dots and error bars 
                       represent (estimated marginal) means ± 95% confidence 
                       interval per group. Means not sharing any letter are 
                       significantly different by the Sidak-test at the 5% 
                       level of significance.", width = 70)) +
  theme_classic() 

