library(dplyr)

mpg <- ggplot2::mpg

mpg %>% 
  distinct(manufacturer)

honda <- 
  mpg %>% 
  dplyr::filter(manufacturer == "honda") %>% 
  select(-manufacturer)

honda
rm(honda)

mpg %>% 
  group_by(manufacturer) %>% 
  group_walk(~ assign(x = .y$manufacturer, value = .x, envir = .GlobalEnv))
