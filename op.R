library(rvest)
library(tidyverse)
library(furrr)

plan(multiprocess)

read_html("https://onepiece.fandom.com/wiki/Chapter_1") %>% {
  tibble(
    key =  html_nodes(., ".pi-data-label") %>% 
      html_text(),
    value = html_nodes(., ".pi-item-spacing .pi-font") %>% 
      html_text()
  )
} %>% 
  mutate(
    key = janitor::make_clean_names(key)
  ) %>% 
  pivot_wider(names_from = "key", values_from = "value")
  
 
scrape_op <- function(x) {
  read_html(x) %>% {
    tibble(
      key =  html_nodes(., ".pi-data-label") %>% 
        html_text(),
      value = html_nodes(., ".pi-item-spacing .pi-font") %>% 
        html_text()
    )
  } %>% 
    mutate(
      key = janitor::make_clean_names(key)
    ) %>% 
    pivot_wider(names_from = "key", values_from = "value")
}


op_urls <- paste0("https://onepiece.fandom.com/wiki/Chapter_", seq_len(959))


op_raw <- future_map_dfr(op_urls, 
                         possibly(scrape_op, 
                                  otherwise = NULL), 
                         .progress = TRUE)

op <- 
  op_raw %>% 
  mutate_if(is.character, str_squish) %>% 
  mutate_all(parse_guess) %>% 
  mutate(
    release_date = str_remove_all(release_date, "\\[ref\\]") %>% 
      lubridate::mdy()
  )

op

readr::write_csv(op, "data-raw/op_manga.csv")
