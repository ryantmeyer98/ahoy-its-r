# LOAD LIBRARIES
library(tidyverse)
library(janitor)
library(scales)
library(plotly)
library(patchwork)
library(lubridate)



# READ IN THE DATA 
daily.df <- read_csv("Bill/data/prism_data/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_provisional_4km_20190101_20221231_40.6765_-88.7846.csv", 
                    skip = 10) %>% 
  clean_names()


# wow This is one way to do it and the second in this list is also really hard...
# https://stackoverflow.com/questions/42057832/how-to-draw-rainfall-runoff-graph-in-r-using-ggplot


rainAx = list(
  overlaying = "y",
  side = "right",=
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


# how to get 30 year average data into the graph
avg30.df <- read_csv("Bill/data/prism_data/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_800m_monthly_normals_40.6756_-88.7825.csv",
                     skip=10) %>% clean_names() %>% 
  filter(date != "Annual")  %>% 
  rename(month = date)

# need to create a dataframe for the full set of dates to add points
avg30_full.df <-  data.frame(date = ymd(seq(as.Date("2019-01-15"), 
                                              as.Date("2022-12-15"), 
                                              by = "month"))) %>% 
  mutate(month = month(date)) %>% 
  mutate(month = as.character(lubridate::month(month, label = TRUE, abbr = FALSE)))

# now can merge values to the full
avg30_full.df <- left_join(avg30_full.df, avg30.df, by="month")

daily.df <- full_join(daily.df, avg30_full.df, by = "date",
                     suffix= c(x="", y="_avg_30yr"))


temp.plot <-  daily.df %>% 
  ggplot() +
  geom_line(aes(x=date, tmean_degrees_c), color="red")+
  geom_line(data = daily.df[!is.na(daily.df$tmean_degrees_c_avg_30yr),],
             aes(x=date, y= tmean_degrees_c_avg_30yr), 
            color="black", linewidth = 1.5, alpha=0.5)+
  scale_y_continuous()
temp.plot


