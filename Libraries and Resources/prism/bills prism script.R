library(tidyverse)
library(janitor)
library(scales)
library(plotly)
library(patchwork)

daily.df <- read_csv("Bill/data/prism_data/daily_data.csv", 
                     skip = 10) %>% 
  clean_names()

# wow This is one way to do it and the second in this list is also really hard...
# https://stackoverflow.com/questions/42057832/how-to-draw-rainfall-runoff-graph-in-r-using-ggplot


rainAx = list(
  overlaying = "y",
  side = "right",
  title = "Rainfall (mm)",
  #autorange="reversed",
  range = c(150,0),
  showgrid=FALSE
)

date = daily.df$date #dates at daily format, however you can use any temporal resolution
temp = daily.df$tmean_degrees_c # flow data
rainfall = daily.df$ppt_mm # rainfall data

plot_ly() %>%
  add_trace(
    x=~date, y=~temp,
    type="scatter", mode="lines", line = list
    (color = 'black', width = 1, 
      dash = 'solid'),name ='Temp C') %>%
  add_trace(
    x=~date, y=~rainfall,
    type="bar", yaxis="y2", marker = list
    (color ="blue",width = 1),name = 'rainfall') %>%
  layout(title = "Rainfall-Streamflow",xaxis =list
         (title = "time (daily)"), yaxis=list
         (title="Temp C",range=c(-25,50)),yaxis2=rainAx)


# 3 this might be the better way to do it and used some code that we need to use
# for Lizas data


rain.plot <-  daily.df %>% 
  ggplot() +
  geom_bar(aes(x=date, ppt_mm), stat="identity", color="blue")+
  scale_y_reverse() 
rain.plot

temp.plot <-  daily.df %>% 
  ggplot() +
  geom_line(aes(x=date, tmean_degrees_c), color="red")+
  scale_y_continuous()
temp.plot


rain.plot + scale_x_date(position = 'top') +
  coord_cartesian(ylim=c(55,0))+
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
  plot_layout(ncol = 1, heights = c(1,-.4, 5))