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
rain.plot <-  daily.df %>% # this creates a new plot using the df here
  ggplot() + # starts the plot command
  geom_bar(aes(x=date, ppt_mm), stat="identity", color="blue", size = .1)+ # makes a bar plot
  scale_y_reverse(expand=c(0,0)) + # reverses the scale
  labs(y = "Rain (mm/day)")
rain.plot



# create temp plot
temp.plot <-  daily.df %>% 
  ggplot() +
  geom_line(aes(x=date, tmean_degrees_c), color="red")+
  scale_y_continuous(expand=c(0,0)) +
  labs(y = "Temperature (C)")
temp.plot


# combine plots

rain.plot + scale_x_date(position = 'top', expand = c(0,0)) +
  coord_cartesian(ylim=c(65,0))+
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


# now for the 30 year data
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


rain_30yr.plot <-   # this creates a new plot using the df here
  ggplot() + # starts the plot command
   geom_bar(data=daily.df, aes(x=date, ppt_mm), stat="identity", color="blue", size = .1) + # makes a bar plot
  geom_line(data = daily.df[!is.na(daily.df$ppt_mm_avg_30yr),],
            aes(x=date, y= ppt_mm_avg_30yr), 
            color="black", linewidth = 1.5, alpha=0.5)+
  scale_y_reverse(expand=c(0,0)) + # reverses the scale
  labs(y = "Rain (mm/day)")
rain_30yr.plot

temp_30yr.plot <-  daily.df %>% 
  ggplot() +
  geom_line(aes(x=date, tmean_degrees_c), color="red")+
  geom_line(data = daily.df[!is.na(daily.df$tmean_degrees_c_avg_30yr),],
            aes(x=date, y= tmean_degrees_c_avg_30yr), 
            color="black", linewidth = 1.5, alpha=0.5)+
  scale_y_continuous()
temp_30yr.plot




# combine plots

rain_30yr.plot + scale_x_date(position = 'top', expand = c(0,0)) +
  coord_cartesian(ylim=c(65,0))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line = element_line(linetype = "solid"),
        axis.ticks = element_line(linetype = "blank"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        panel.background = element_rect(fill = NA)) + 
  plot_spacer()  + 
  temp_30yr.plot +
  theme(
    axis.line = element_line(linetype = "solid"),
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    panel.background = element_rect(fill = NA))+
  plot_layout(ncol = 1, heights = c(5,-.9, 3))
