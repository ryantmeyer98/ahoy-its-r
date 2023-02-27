# # install packages
# install.packages("Hmisc")
# install.packages("multcomp")
# install.packages("multcompView")
# install.packages("FSA")
# install.packages("rcompanion")

# Load libraries -----
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(janitor)
library(plotly)

## Load Anova Specific libraries -----
library(car)
library(emmeans)
library(FSA)
library(rcompanion)

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

# patchwork combined plot
# https://stackoverflow.com/questions/70218158/how-to-reduce-the-space-between-to-plots-when-using-patchwork
# G1 + plot_spacer() + G2 + plot_layout(widths = c(4, -1.1 ,4.5),guides = "collect")& theme(legend.position = "top")

combined.plot <- length.plot +  theme(plot.margin = margin(0, 0, 0, 0, "pt"))+
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
combined.plot

## Save Plot ----
ggsave(file="Bill/output/Weed_weight_length_width_plot.pdf", 
       combined.plot,
       width = 8, height = 6, 
       units="in")

# ONE-WAY ANOVA -----
## Anova using lm ----
weight_anova_lm.model <- lm(seed_wt_g  ~ source, data = s.df)
summary(weight_anova_lm.model)
Anova(weight_anova_lm.model, type=3)

# Checking Assumptions -----
# Assumptions Tests ------
## Homogeneity of Variance----
leveneTest(seed_wt_g ~ source, data= s.df)

# Checking normality graphically ----
par(mfrow = c(1,2))  # This code put two plots in the same window
hist(weight_anova_lm.model$residuals)   # Makes histogram of residuals  
plot(weight_anova_lm.model, which = 2)   # Makes Q-Q plot

# Checking homoscedasticity (Homogeneity of variances) -----
plot(weight_anova_lm.model, which = 1)  # Makes residuals VS fitted plot

# Checking normality statistically ----
shapiro.test(weight_anova_lm.model$residuals)

# SO WE SHOULD STOP HERE AS THE ASSUMPTION IS VIOLATED ---------
# THE CODE BELOW WOULD ASSUME THAT RESIDUALS WERE NORMALLY DISTRIBUTED
# Post Hoc tests ---------
weight_model.emm <- emmeans(weight_anova_lm.model, ~ source)
weight_model.emm

# plot of comparisons
# blue are confidence intervals, red arrows overlap mean no significant diff
plot(weight_model.emm , comparisons = TRUE)

## Pairwise comparisons with emmeans -----
weight_emm_pairs = emmeans(weight_model.emm, 
                             pairwise ~ source,
                             adjust="tukey")
summary(weight_emm_pairs)

# just the emmeans
weight_emm_pairs$emmeans

## Pairwise with letters -----
cld <- multcomp::cld(weight_model.emm,
              alpha=.05,
              Letters=letters)
cld


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
model_means_cld <- multcomp::cld(object = weight_model.emm,
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
fancy.plot <- ggplot() +
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
fancy.plot

# Nonparametric Tests ------
# If parametric test assumptions are violated and you cant transform data
# you do non parametric tests

# Kruskal Wallis test ---
kruskal.test(seed_wt_g ~ source, data = s.df)

dunn.test = dunnTest(seed_wt_g ~ source,
              data = s.df,
              method="sidak")      # Adjusts p-values for multiple comparisons;
dunn.test

# get out the resituals
PT = dunn.test$res

# get compact letter display showing groups ----
cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)
