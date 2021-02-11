system("instagram-scraper gnfi -m 20 --media-types none --comments")

fs::dir_tree()

system("mkdir data-raw; mv gnfi/gnfi.json data-raw/gnfi.json")

fs::dir_tree()

system("cat data-raw/gnfi.json")

library(jsonlite)
library(tidyverse)
library(listviewer)

gnfi_raw <- fromJSON("data-raw/gnfi.json")
str(gnfi_raw, max.level = 3)
jsonedit(gnfi_raw)

gnfi_raw %>% 
  pluck(1) %>% 
  jsonlite::flatten() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  rowid_to_column("post_id") %>% 
  select(post_id, 
         tags, 
         time = taken_at_timestamp, 
         comments= comments_data, 
         n_like = edge_media_preview_like_count) %>% 
  mutate(
    tags = map_chr(tags, toString),
    time = as.POSIXct(time, origin = "1970-01-01")
  ) %>% 
  unnest_wider(comments) %>% 
  unnest() %>% 
  glimpse()
