library(tidyverse)
library(janitor)

daily.df <- read_csv("Bill/data/prism_data/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_provisional_4km_20190101_20221231_40.6765_-88.7846.csv", 
                        skip = 10) %>% 
  clean_names()


daily.df %>% 
  ggplot() +
  geom_bar(aes(x=date, y=ppt_mm), stat="identity", width = 4) +
  scale_y_reverse(expand = c(0,0))
