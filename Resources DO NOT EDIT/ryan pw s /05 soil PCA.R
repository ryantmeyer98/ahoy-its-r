# LIBRARIES 
library(tidyverse)
library(devtools)
library(psych)
library(ggbiplot)
library(janitor)
library(car)
library(emmeans)
library(lme4)
library(lmerTest)
library(multcompView)

# READ IN THE DATA ----
soil.df <- read_csv("output/soils/soils_all_years_raw_wide.csv")

# remove NA values
# now this isn't ideal that we have to trim the data here, but you cannot have NAs in the PCA
# so there isn't really a choice 
soil.df <- soil.df %>%
  select(-soils_na_ppm, -soils_na_ppm_base_satpct) %>%
  na.omit()

# CORRELATION MATRIX

# first lets do a correlation matrix to see how things look
pairs.panels(soil.df[,12:31], # [,12:33] tells r to only use columns 12-33
             method = "pearson",
             stars = TRUE,
             gap = 0, 
             pch = 21)

# here's my takeaway from this, while there are some variables that are not highly correlated
# most of the variables here are correlated, which is not surprising given they are all related
# in soil fertility, there are a lot of relationships in there worth exploring, but for now I am 
# going to go ahead and do the PCA

# CREATE PCA MODEL ----
# centering and scaling the model is very important because (from Lise) these things
# make sure that everything is scaled correctly so one variable isn't forcing the model
pca.model <- prcomp(na.omit(soil.df[,12:31]),
                    center = TRUE, 
                    scale. = TRUE)

attributes(pca.model) # tells us the column names made, usefully for making sure it worked

print(pca.model) # gives the eigenvectors and stdev

summary(pca.model) # other iseful stats

# now we want to pull out the eigenvalues 

# create a dataframe from the summary # USE ROW TO COLUMNS 
ev.df <- as.data.frame(summary(pca.model)$importance) %>%
  rownames_to_column()

# pivot the dataframe long, get only stdev values, and square stdev to get eigenvalue
ev.df <- ev.df %>%
  pivot_longer(cols = starts_with("P"),
               names_to = "PC",
               values_to = "value")

# fix the names
ev.df <- ev.df %>%
  mutate(variable = case_when(
    rowname == "Standard deviation" ~ "stdev",
    rowname == "Proportion of Variance" ~ "prop_variance",
    rowname == "Cumulative Proportion" ~ "cum_prop"
  )) %>%
  select(-rowname)

# get eigenvalues
ev.df <- ev.df %>%
  filter(variable == "stdev") %>%
  mutate(eigenvalue = value^2) %>%
  select("PC", "eigenvalue")

# pull out the eigenvalues only over 1
ev_final.df <- ev.df %>%
  filter(eigenvalue > 1)

# so we will keep PC1, PC2, PC3, and PC4 with eigenvalues over 1

# now that we have the variation explained by the PCs, lets see what the loading are 

# this creates a data frame for the loadings from the pca model
loadings <- as.data.frame(pca.model$rotation[,1:4])

# now that we have the loadings we can combine the PC loadings and the soils data
pca_axes <- predict(pca.model, newdata = soil.df)

# turn into dataframe
pca_axes.df <- as.data.frame(pca_axes)

# combine with soil data
full.df <- cbind(soil.df, pca_axes.df)

# what does the biplot look like 
biplot <- ggbiplot(pca.model, obs.scale = 1, var.scale = 1, ellipse = TRUE,
                   circle = TRUE, ellipse.prob = 0.68) 
biplot <- biplot + scale_color_discrete(name = '')
biplot <- biplot + theme(legend.direction = 'horizontal', legend.position = 'top')
biplot


# PLOTTING ----
full.df %>%
  filter(season == "spring_summer") %>%
  filter(year != 2020) %>%
  ggplot(aes(PC1, PC2, color = treatment)) +
  geom_point(size = 3) +
  facet_wrap(~year) +
  theme_bw()

# it looks like PC1 has some good shit going on, what if we try running our mixed model
# on PC1
pca.lm = lmer(PC1 ~ treatment * (1|date) + (1|block_no) + (1|subplot_point/plot),
              data = full.df,
              REML = TRUE)

# check assumptions of the model - honestly this looks pretty good gonna roll with it 
residuals <- resid(pca.lm)
plot(fitted(pca.lm), residuals)
qqnorm(residuals)
qqline(residuals)
hist(residuals)

# run the model - reject null hypothesis, at least two group means differ from zero
Anova(pca.lm, type = "3")
ranova(pca.lm)

# post F tests
# https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/
emm1 = emmeans(pca.lm, specs = pairwise ~ treatment, adjust = "bonferroni")
test.df <- as.data.frame(emm1)



# save our interactions as an outputs with letter notation
pca_results.df <- as.data.frame(multcomp::cld(pca.emm, Letters = letters, adjust = "sidak"))

# now we plot the results
test.df %>%
  filter(treatment != ".") %>%
  ggplot(aes(treatment, emmean)) +
  stat_summary(fun = mean, na.rm = TRUE, geom = "point", size = 3) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width = 0.3) +
  coord_cartesian(ylim = c(-4, 4)) +
  labs(x = "Treatment", y = "Estimated Marginal Mean (PC1)") +
  geom_text(aes(x = 1, y = 2, label = "a")) +
  geom_text(aes(x = 2, y = 2.4, label = "ab")) +
  geom_text(aes(x = 3, y = 3.2, label = "b")) +
  theme_classic()

# so things are looking different here and that is good, but we need to know what is loading the 
# model, so lets look at the loadings 

loadings.df <- loadings %>%
  rownames_to_column()

loadings.df %>%
  ggplot(aes(PC1, rowname, fill = rowname)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(legend.position = "none")

# kate says do pca again and drop lowest loadings   

loading_long.df <- loadings.df %>%
  pivot_longer(cols = starts_with("P"),
               names_to = "PC",
               values_to = "value")

loading_long.df %>%
  ggplot(aes(value, rowname, fill = rowname)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~PC)

attributes(soil.df)
names(soil.df[,12:31])
