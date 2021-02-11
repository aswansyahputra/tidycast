library(jabr)
library(tidyverse)

jabar_data <- jabr_list_dataset(update = TRUE)

bencana_raw <- 
  jabar_data %>% 
  dplyr::filter(str_detect(title, "Jenis Bencana")) %>% 
  jabr_fetch() %>% 
  unnest(dataset)

glimpse(bencana_raw)

bencana <- 
  bencana_raw %>% 
  select(-provinsi, -satuan) %>% 
  extract(title, "tahun", regex = "(\\d{4})", convert = TRUE) %>% 
  pivot_wider(names_from = "jenis_bencana", values_from = "jumlah") %>% 
  janitor::clean_names()
