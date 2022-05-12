library(tidyverse)

data_path <- "~/Dropbox/McGill/teaching/2021-2022/2022_summer/ORGB690/data/"
setwd(data_path)

examiner_art_units <- read_csv("examiner_aus.csv")

new_table <- examiner_art_units %>% 
  mutate( tc = floor(examiner_art_unit/100)*100 )

temp <- new_table %>% group_by(tc) %>% 
  count(year) %>% 
  filter(tc %in% c(2100,2400))

examiner_art_units %>% count(year)


hist(examiner_art_units$year)
