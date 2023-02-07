
# install.packages("glmmTMB")


#---- load libraries ----
library(tidyverse)
library(ggpubr)
library(rstatix)
library(pscl)
library(boot)
library(readxl)
library(glmmTMB)


# Examples from paper
zinb = glmmTMB(count~spp * mined + (1|site), zi=~spp * mined,
               data=Salamanders, family=nbinom2)

hnb = glmmTMB(count~spp * mined + (1|site), zi=~spp * mined,
              data=Salamanders, family=truncated_nbinom2)


adults <- read_excel("Resources DO NOT EDIT/kate_data/kates complete_adults_KE.xlsx") %>% 
  select(-1)

adults <- adults %>%
  select("ovicup", "week_eclosed", "zone", "n") %>%
  mutate(zone = as.factor(zone))


### go with quadratic if week is going to be a quantiative value ###
## see how predicted values match up for the real data ##


# poisson and negative binomial with quadratic *

# SAJ thinks negative binomial has the best results *

#### zero-inflated poisson glm ####

#https://stats.oarc.ucla.edu/r/dae/zip/#:~:text=Zero%2Dinflated%20Poisson%20regression%20is,zeros%20can%20be%20modeled%20independently.

#run the zero inflated poisson model, week_eclosed treated like a block?
# how to have zone * week_eclosed interaction?
# use week as a class variable?
summary(m1 <- zeroinfl(n ~ zone + week_eclosed, data = adults))
m1

adults$ovicup <- as.factor(adults$ovicup)

summary(m2 <- glm(n ~ ovicup:zone * week_eclosed, family="poisson", data=adults))
m2
Anova(m2, type=3)

  #calculate the null model
mnull <- update(m1, . ~ 1)

#compare null model with the poisson-zero-inflated
pchisq(2 * (logLik(m1) - logLik(mnull)), df = 2, lower.tail = FALSE)


dput(coef(m1, "count"))
#c(`(Intercept)` = 0.500731403572206, zone3 = 2.63298402093441, 
#  zone4 = 2.77073494453332, week_eclosed = -0.0620232000688045)

dput(coef(m1, "zero"))
#c(`(Intercept)` = 3.70687009321953, zone3 = 8.46476097482973, 
#zone4 = 9.14806201972012, week_eclosed = -0.444424446335665)



#We can get confidence intervals for the parameters and the 
#exponentiated parameters using bootstrapping. For the Poisson model,
#these would be incident risk ratios, for the zero inflation model, 
#odds ratios. We use the boot package. First, we get the coefficients 
#from our original model to use as start values for the model to 
#speed up the time it takes to estimate. Then we write a short 
#function that takes data and indices as input and returns the 
#parameters we are interested in. Finally, we pass that to the boot 
#function and do 1200 replicates, using snow to distribute across 
#four cores. Note that you should adjust the number of cores to 
#whatever your machine has. Also, for final results, one may wish 
#to increase the number of replications to help ensure stable results.



f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(n ~ zone + week_eclosed, data = data[i, ],
                start = list(count = c( 0.50073, 2.63298, 2.77073, -0.06202   ), 
                             zero = c(3.7069,8.4648, 9.1481, -0.4444 )))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
res <- boot(adults, f, R = 1200, parallel = "snow", ncpus = 4)

## print results
res

#The results are alternating parameter estimates and standard errors. 
#That is, the first row has the first parameter estimate from our 
#model. The second has the standard error for the first parameter. 
#The third column contains the bootstrapped standard errors, which 
# "are considerably larger than those estimated by zeroinfl." - true?



## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9, 11, 13, 15), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

# compare to the confidence interavals from the original model estimates
confint(m1)

##the bootstrapped CI's seem smaller in general   
## especially for the zero_zone3 and zero_zone4


## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9, 11, 13, 15), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaLL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(m1))
## print results
expparms

CI <- setNames(as.data.frame(t(sapply(1:nrow(df), function(row)
  boot.ci(res, conf = 0.95, type = "basic", index = row)$basic[, 4:5]))),
  c("lower", "upper"))

CI <- as.data.frame(t(sapply(c(1, 3, 5, 7, 9, 11, 13, 15), function(row) 
  boot.ci(res, conf = 0.95, type = "basic", index = row)$basic[, 4:5])),
  c("lower", "upper"))

row.names(CI) <- names(coef(m1))

#To better understand our model, we can compute the expected number of fish 
#caught for different combinations of our predictors. In fact, since we are 
#working with essentially categorical predictors, we can compute the expected 
#values for all combinations using the expand.grid function to create all 
#combinations and then the predict function to do it. 

newdata1 <- expand.grid(24:31, c(1,3,4))
colnames(newdata1) <- c("week_eclosed", "zone")

newdata1 <- newdata1 %>%
  mutate(zone = as.factor(zone))

newdata1$phat <- predict(m1, newdata1)

predict(m1, newdata1, interval="confidence")

ggplot(newdata1, aes(x = week_eclosed, y = phat, colour = factor(zone))) +
  geom_point() +
  geom_line() +
  labs(x = "Week_Eclosed", y = "Mean Number of Eclosed Adults", color = "Zone")

write.csv(newdata1, "Bill/output/predictions_adults.csv")

