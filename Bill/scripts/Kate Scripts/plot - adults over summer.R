# Load Libraries ----
library(tidyverse)
library(readxl)
library(patchwork)
library(janitor)

#import adult emergence dataset and treatment assignments per ovicup

adult_emergence <- read_csv("data/adult_emergence_weekly.csv")

#pull the treatment assignments from the output directory
#this treatment assignments are according to Rachel Morreale
treatment_assignments <- read_csv("output/treatment_assignments_RM.csv")

#prediction lines from the zero-i-poisson. used in plot 2
adults_zip <- read_csv("output/predictions_adults.csv")


#combine treatment assignments to the other datasets

adults <- merge(x = adult_emergence, y = treatment_assignments, by = "ovicup", all = TRUE)

#write.csv(adults, "output/adults.csv")

# drop NA's for emergence and when pupae died
#also dropped id number 83 b/c didn't record the ovicup
adults <- adults %>%
  filter(pupa_died == "no") %>%
  filter(week_eclosed != "na") %>%
  filter(id != 83)


#I want to graph over time/date
#number of larvae hatched
#with containers color coded by treatment

#p1 <- ggplot()



#I want to graph over time/date
#number of adults emerged per container
#with containers color coded by treatment

p1.df <-adults %>% 
 group_by(ovicup,week_eclosed,zone) %>% 
  summarise(n = n()) %>%
  ungroup()


p1.df <- p1.df %>%
  mutate(zone = as.factor(zone))

### fill in the data frame ###
# zeroes for the weeks when no mosquitoes emerged
# complete  = finishes the matrix
# fill = zone values based on what's already recorded for ovicup, filled upwards

complete.df <- p1.df %>%
  complete(ovicup, week_eclosed = 24:31, fill = list(n = 0)) %>% 
  fill(zone, .direction = "up") %>%
  fill(zone, .direction = "down")


#write.csv(complete.df, "output/complete_adults.csv")


### plotting time! ###
p1 <- ggplot(complete.df, aes(x = week_eclosed, y = n, color = zone)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3,
               position = position_dodge(width = 0.3)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               linewidth = 0.2,
               position = position_dodge(width = 0.3)) +
  theme_classic() +
  labs(x = "Week", y = "Mean Eclosed Adults", color = "Zone") +
  coord_cartesian(xlim = c(23, 32), ylim = c(0, 6))+
  scale_color_manual(values= c ("darkgoldenrod2", "cornflowerblue", "darkslateblue"),  
                     labels = c("SIT, Captiva", "Control, Captiva", "Control, Sanibel")) + #paired color palette
  scale_x_continuous(breaks = c(22, 24, 26, 28, 30, 32, 34)) #set axis ticks

p1


p2.df <- merge(x = complete.df, y = adults_zip, by = c("week_eclosed", "zone"), all = TRUE)

p2 <- ggplot(p2.df, aes(x = week_eclosed, y = n, color = zone)) +
  stat_summary(fun = mean, na.rm = TRUE,
               geom = "point",
               size = 3,
               position = position_dodge(width = 0.3)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               linewidth = 0.2,
               position = position_dodge(width = 0.3)) +
  geom_line(aes(x = week_eclosed, y = phat, color = zone))+
  theme_classic() +
  labs(x = "Week", y = "Mean Eclosed Adults", color = "Zone") +
  coord_cartesian(xlim = c(23, 32), ylim = c(0, 6))+
  scale_color_manual(values= c ("darkgoldenrod2", "cornflowerblue", "darkslateblue"),  
                     labels = c("SIT, Captiva", "Control, Captiva", "Control, Sanibel")) + #paired color palette
  scale_x_continuous(breaks = c(22, 24, 26, 28, 30, 32, 34)) #set axis ticks

p2


  