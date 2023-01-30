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


# this creates a summary file that summarizes data weekly
seven.df <- daily.df %>% 
  mutate(
    year = year(date),
    week = week(date)
  ) %>% 
  group_by(year, week) %>% 
  summarize(
    date = first(date),
    ppt_mm = sum(ppt_mm, na.rm=TRUE),
    tmean_degrees_c = mean(tmean_degrees_c, na.rm=TRUE)
  )



# this creates the rain plot ----
rain.plot <-  seven.df %>% # this creates a new plot using the df here
  ggplot() + # starts the plot command
  geom_bar(aes(x=date, ppt_mm), stat="identity", color="blue")+ # makes a bar plot
  scale_y_reverse(expand=c(0,0))  + # reverses the scale
  labs(y = "Rain (mm/week)")
rain.plot

temp.plot <-  seven.df %>% 
  ggplot() +
  geom_line(aes(x=date, tmean_degrees_c), color="red")+
  scale_y_continuous(expand=c(0,0)) +
  labs(y = "Temp (C weekly mean)")
temp.plot


# combine the plots
rain.plot + scale_x_date(position = 'top', expand = c(0,0)) +
  coord_cartesian(ylim=c(150,0))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line(linetype = "solid"),
        axis.ticks = element_line(linetype = "blank"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        panel.background = element_rect(fill = NA)) + 
  plot_spacer()  + 
  temp.plot +
  theme(
    axis.line = element_line(linetype = "solid"),
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    panel.background = element_rect(fill = NA))+
  plot_layout(ncol = 1, heights = c(5,-.9, 3))
