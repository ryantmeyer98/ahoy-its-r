library(tidyverse)
library(janitor)
<<<<<<< HEAD
library(scales)
library(plotly)
library(patchwork)
library(lubridate)
=======
>>>>>>> parent of 889939f... new data?

daily.df <- read_csv("Bill/data/prism_data/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_provisional_4km_20190101_20221231_40.6765_-88.7846.csv", 
                        skip = 10) %>% 
  clean_names()


daily.df %>% 
  ggplot() +
<<<<<<< HEAD
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
avg30.df <- read_csv("Bill/data/prism_data/monthly_data.csv",
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
            color="black", size = 1.5, alpha=0.5)+
  scale_y_continuous()
temp.plot
=======
  geom_bar(aes(x=date, y=ppt_mm), stat="identity", width = 4) +
  scale_y_reverse(expand = c(0,0))
>>>>>>> parent of 889939f... new data?
