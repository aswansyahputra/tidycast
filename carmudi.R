# Author: Muhammad Aswan Syahputra
# License: GPL-3.0

library(rvest)
library(tidyverse)
library(furrr)

plan(multiprocess)

get_listing <- function(x) {
  
  read_html(x) %>% {
    tibble(
      item = html_nodes(., ".title-blue") %>%
        html_text(),
      gearshift = html_nodes(., ".icon-gearshift+ span") %>%
        html_text(),
      fuel = html_nodes(., ".icon-fuel+ span") %>%
        html_text(),
      distance = html_nodes(., ".attribute-distance span") %>%
        html_text(),
      city = html_nodes(., ".catalog-listing-item-location span") %>%
        html_text(),
      price = html_nodes(., ".price a") %>%
        html_text()
    )
  }
}

usedcars_raw <- 
  tibble(
    url = paste0("https://www.carmudi.co.id/used/?page=", seq_len(946)),
    details = future_map(url, possibly(get_listing, otherwise = NA), .progress = TRUE)
  ) %>% 
  select(-url)

usedcars <- 
  usedcars_raw %>% 
  dplyr::filter(!is.na(details)) %>% 
  unnest(details) %>% 
  mutate(
    item = str_remove_all(item, "\\n") %>% 
      str_squish() %>% 
      str_replace_all("(.*)\\s(\\d{4})", "\\2 \\1"),
    city = str_remove_all(city, "\\n") %>% 
      str_squish()
  ) %>% 
  extract(item, into = c("year", "manufacturer", "type"), regex = "(\\d{4})\\s([a-zA-Z0-9\\-]*)\\s(.*)") %>% 
  mutate(
    year = parse_number(year),
    price = parse_number(price),
    distance = parse_number(distance),
  )

library(tidytext)
usedcars %>% 
  select(type) %>% 
  unnest_tokens("zzz", type) %>% 
  count(zzz, sort = TRUE) %>% 
  filter(nchar(zzz) >= 3) %>% 
  view()


pin(usedcars, description = "The dataset about used cars in Indonesia listed at carmudi.co.id", board = "github")

usedmotorcycles_raw <- 
  tibble(
    url = paste0("https://www.carmudi.co.id/motorcycles/used/?page=", seq_len(366)),
    details = future_map(url, possibly(get_listing, otherwise = NA), .progress = TRUE)
  ) %>% 
  select(-url)

usedmotorcycles <- 
  usedmotorcycles_raw %>% 
  dplyr::filter(!is.na(details)) %>% 
  unnest(details) %>% 
  mutate(
    item = str_remove_all(item, "\\n") %>% 
      str_squish() %>% 
      str_replace_all("(.*)\\s(\\d{4})", "\\2 \\1"),
    city = str_remove_all(city, "\\n") %>% 
      str_squish()
  ) %>% 
  extract(item, into = c("year", "manufacturer", "type"), regex = "(\\d{4})\\s([a-zA-Z0-9\\-]*)\\s(.*)") %>% 
  mutate(
    year = parse_number(year),
    price = parse_number(price),
    distance = parse_number(distance),
  )

pin(usedmotorcycles, description = "The dataset about used motorcycles in Indonesia listed at carmudi.co.id", board = "github")
