# LOAD LIBRARIES
library(tidyverse)
library(janitor)
library(scales)
library(plotly)
library(patchwork)

# READ IN THE DATA ----
# this is the way to read in
daily.df <- 
  read_csv("Resources DO NOT EDIT/prism weather plotting/prism data/daily_data.csv", 
           skip = 10) %>% 
  clean_names()



date = daily.df$date #dates at daily format, however you can use any temporal resolution
temp = daily.df$tmean_degrees_c # flow data
rainfall = daily.df$ppt_mm # rainfall data



#https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html

# Value used to transform the data
coeff <- 30

rain.plot <-  daily.df %>% 
  ggplot() +
  geom_bar(aes(x=date, ppt_mm), stat="identity", color="blue")+
  scale_y_reverse() 
rain.plot

# A few constants

rain.plot <- daily.df %>% 
  ggplot() +
  geom_line(aes(x = date, y=ppt_mm), linewidth = 0.5, color="blue") +
  scale_y_reverse()

temp.plot <- daily.df  %>% 
  ggplot() +
  geom_line(aes(x = date, y=tmean_degrees_c), linewidth=0.5, color="red")
  

 p1 <- daily.df %>% 
   ggplot() +
   geom_line(aes(x = date, y=ppt_mm), linewidth = 0.5, color="blue") +
   scale_y_reverse()+
   geom_line(aes(x = date, y=tmean_degrees_c), linewidth=0.5, color="red") + 
   scale_y_continuous(name = "Temperature (Celsius)",
                      sec.axis = sec_axis(~.*coeff, name = "Rainfall (mm)")) 
   
 p1 + theme_classic()
    
p2 <- temp.plot + rain.plot +
  scale_y_continuous(name = "Temperature (Celsius)",
                     sec.axis = sec_axis(~.*coeff, name = "Rainfall (mm)")) 

    
    