# https://finnstats.com/index.php/2021/05/07/pca/

library(devtools)
library(ggbiplot)
library(vegan)
library(psych)
library(readxl)
library(tidyverse)
library(GGally)

# reading in the data
i.df <- read_excel("Bill/data/rachel_pca/island_pca.xlsx", na = "NA")

# we will be using these variables in the PCA, so we select ohese out
i_pca.df <- i.df %>% select(success, islandsize, distance, predation)

# correlation matrix

# http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
# https://cran.r-project.org/web/packages/psych/vignettes/intro.pdf 
pairs.panels(i_pca.df, # function is pairs.panels and call dataframe 
             method = "pearson", # use a pearson correlation
             stars = TRUE, # add stars for significance 
             gap = 0, # remove gaps between plots 
             pch = 21) # choose shape for points on scatter plot

# i like this correlation matrix a lot more
# https://www.r-bloggers.com/2021/06/ggpairs-in-r-a-brief-introduction-to-ggpairs/
ggpairs(i.df, # call data frame
        columns = 9:12) # tell R what columns you want to use 

# this looks like it creates the pca model 
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
pca.model <- prcomp(i_pca.df, # creates a model from prcomp using the i_pca.df data frame
                   center = TRUE, # provide means
                   scale. = TRUE) # provide standard deviation

attributes(pca.model) # this tells us the columns that our pca model will output

print(pca.model) # this gives is the output from our pca model with standard deviation 
# and pc scores 

summary(pca.model) # stdev and variance for the PCs


# Loadings <- as.data.frame(PCA3$CA$v.eig[,1:2])

# now we can run a corelation matrix on the PCs to see if multicollinearity is an issue
pairs.panels(pca.model$x,
             gap=0,
             bg = c("red", "yellow", "blue")[i.df$sampling_site],
             pch=21)

# next, we want to create a biplot
i_biplot <- ggbiplot(pca.model, # create a plot called i_biplot from the function biplot
                     # and the data pca.model
                     obs.scale = 1, # sets scale of x and y axes 
                     var.scale = 1, # sets scale of lines 
                     # groups = i.df$sampling_site,
                     ellipse = TRUE, # not sure what this does
                     circle = TRUE, # puts a neat circle around the data 
                     ellipse.prob = 0.68) # not sure what this does
i_biplot <- i_biplot + scale_color_discrete(name = '')
i_biplot <- i_biplot + theme(legend.direction = 'horizontal',
                             legend.position = 'top')
print(i_biplot)

# now that we have the variation explained by the PCs, lets see what the loading are 

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

# loadings table
#rows, columns
pca_loadings <- as.data.frame(pca.model$rotation[,1:2])

#look at PCA scores
# PCA data
pca_axes <- predict(pca.model, newdata = i_pca.df)
pca_axes.df <- as.data.frame(pca_axes)

#same as cbind
comb.df <- bind_cols(i.df, pca_axes.df)



