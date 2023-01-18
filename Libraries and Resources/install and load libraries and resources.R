# INSTALL PACKAGES ----
# TIDYVERSE
if(!require(tidyverse)){install.packages("tidyverse")}
# READING AND CLEANING DATA 
# makes working with dates easier
if(!require(lubridate)){install.packages("lubridate")}
# reads in excel files
if(!require(readxl)){install.packages("readxl")}
# useful for pulling summary data 
if(!require(skimr)){install.packages("skimr")}
# used for cleaning data frames
if(!require(janitor)){install.packages("janitor")}

# PLOTTING
# puts plots together
if(!require(patchwork)){install.packages("patchwork")}
# used for creating interactive graphics
if(!require(plotly)){install.packages("plotly")}
# different color schemes
if(!require(colorRamps)){install.packages("colorRamps")}
# interactive tool for quickly and easily changing plots
if(!require(ggThemeAssist)){install.packages("ggThemeAssist")}
# helps work with scale breaks
if(!require(scales)){install.packages("scales")}

# STATISTICS
# basic statistics package for running general linear models
if(!require(car)){install.packages("car")}
# used for determining estimated marginal means, or least square means from SAS
if(!require(emmeans)){install.packages("emmeans")}
# used to help work with categorical variables
if(!require(forcats)){install.packages("forcats")}
# all of the below packages are used for mixed models
if(!require(nlme)){install.packages("nlme")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}


# LOAD LIBRARIES ----
library(tidyverse)
library(lubridate)
library(readxl)
library(skimr)
library(janitor)
library(patchwork)
library(plotly)
library(colorRamps)
library(ggThemeAssist)
library(scales)
library(car)
library(emmeans)
library(forcats)
library(nlme)
library(lme4)
library(lmerTest)

# RESOURCES TO HELP YOU LEARN AND RESOURCES I USED ON THE DAILY ---- 

# an excellent textbook to help learn how to manage data in tidyverse
# https://r4ds.had.co.nz/index.html

# if you are trying to answer a question in R, start here its like yahoo answers for data science
# https://stackoverflow.com

# an excellent resouce providing lots of guides for how to do statistics in R
# https://rcompanion.org/rcompanion/a_02.html 














