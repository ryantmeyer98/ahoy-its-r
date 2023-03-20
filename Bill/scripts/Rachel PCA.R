# https://finnstats.com/index.php/2021/05/07/pca/

install.packages("psych")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(vegan)

library(psych)
library(readxl)
library(tidyverse)
library(GGally)


i.df <- read_excel("Bill/data/rachel_pca/island_pca.xlsx", na = "NA")

i_pca.df <- i.df %>% select(success, islandsize, distance, predation)

pairs.panels(i_pca.df,
             gap = 0,
             bg = c("red", "yellow", "blue")[i.df$sampling_site],
             pch=21)

ggpairs(i.df,
        columns = 9:12)

pc.model <- prcomp(i_pca.df,
             center = TRUE,
             scale. = TRUE)
attributes(pc.model)

print(pc.model)

summary(pc.model)


# Loadings <- as.data.frame(PCA3$CA$v.eig[,1:2])
pairs.panels(pc.model$x,
             gap=0,
             bg = c("red", "yellow", "blue")[i.df$sampling_site],
             pch=21)

i_biplot <- ggbiplot(pc.model,
              obs.scale = 1,
              var.scale = 1,
              # groups = i.df$sampling_site,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
i_biplot <- i_biplot + scale_color_discrete(name = '')
i_biplot <- i_biplot + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(i_biplot)


# # extract loadings
# If you use the princomp package you can extract the loadings like this:
# PCA <- princomp(data,cor=T)
# PCA 
# PCA$loadings
# Loadings <- as.data.frame(PCA$loadings[,1:2])

# If you use prcomp you can do:
#   PCA2 <- prcomp(data)
#   Loadings <- as.data.frame(PCA2$rotation[,1:2])

# If you use vegan:
#   PCA3 <- rda(data)

# loadings
pca_loadings <- as.data.frame(pc.model$rotation[,1:2])

# PCA data
pca_axes <- predict(pc.model, newdata = i_pca.df)
pc_axes.df <- as.data.frame(pca_axes)

comb.df <- bind_cols(i.df, pc_axes.df)

comb.df %>% 
  ggplot(aes(PC1, weight_animal)) +
  geom_point() +
  geom_smooth(method="lm")

lm.model <- lm(weight_animal ~ PC1, data=comb.df)
summary(lm.model)

# With vegan
# https://ourcodingclub.github.io/tutorials/ordination/#section4
# https://www.flutterbys.com.au/stats/tut/tut14.2.html
v.df <- i.df %>% select(sampling_site, success, islandsize, distance, predation) 
PCA <- rda(v.df[-1], scale=TRUE)
PCA
biplot(PCA)

summary(PCA, scaling=2)


barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 


# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
# Also try to do it for the first three axes

# Extract scores
vegan.pca.df <- as.data.frame(PCA$CA$v[,1:2])

# extract data 
 #PC1
test.df<-  as.data.frame(cbind(PCA$CA$u[,1],PCA$CA$u[,2]))

 