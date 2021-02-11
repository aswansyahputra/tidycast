library(tidyverse)

mokas_raw <- pins::pin_get("usedcars", board = "github")

mokas <- 
  mokas_raw %>% 
  mutate_if(is.character, str_to_upper) %>% 
  filter(!str_detect(type, "MOTORCYCLES?")) %>%
  filter(year > 1990) %>% 
  filter(distance > 200) %>% 
  filter(price > 30) %>% 
  filter(fuel %in% c("BENSIN", "SOLAR")) %>% 
  filter(!city %in% c("INDONESIA", "ID")) %>% 
  filter(!str_detect(manufacturer, "\\d{4}")) %>% 
  filter(!str_detect(manufacturer, "^MINI|^LAND")) %>% 
  mutate(city = str_replace(city, "JKT", "JAKARTA")) %>% 
  mutate(city = str_replace(city, "BKS", "BEKASI")) %>% 
  mutate(city = str_replace(city, "SBY", "SURABAYA")) %>% 
  mutate(city = str_replace(city, "TGR\\. SEL\\.", "TANGERANG SELATAN")) %>% 
  mutate(city = str_remove(city, "KOTA|KABUPATEN|REGENCY|CITY")) %>% 
  mutate(city = str_squish(city))
  
type_filter <- 
  mokas %>% 
  semi_join(
    mokas %>% 
      count(city, sort = TRUE) %>% 
      top_n(20, wt = n),
    by = "city"
  ) %>% 
  separate_rows(type) %>% 
  count(type, sort = TRUE) %>% 
  filter(nchar(type) > 3) %>% 
  filter(!str_detect(type, "\\d+")) %>% 
  filter(!str_detect(type, "GRAND?|CITY|TIPE")) %>%
  top_n(20, wt = n) %>% 
  pull(type) %>% 
  paste0(collapse = "|")

selected_type <-   
  mokas %>% 
  semi_join(
    mokas %>% 
      count(city, sort = TRUE) %>% 
      top_n(20, wt = n),
    by = "city"
  ) %>% 
  filter(str_detect(type, type_filter)) %>% 
  filter(nchar(type) > 3) %>% 
  mutate(
    temp1 = str_extract(type, "([A-Z])+") %>% 
      na_if("KIJANG"),
    temp2 = str_extract(type, "^[A-Z]+\\s([A-Z])+"),
    type = coalesce(temp1, temp2)
  ) %>% 
  select(-temp1, -temp2) %>% 
  count(type, sort = TRUE) %>% 
  filter(type >= 200)

mobil_bekas <-   
  mokas %>% 
  semi_join(
    mokas %>% 
      count(city, sort = TRUE) %>% 
      top_n(20, wt = n),
    by = "city"
  ) %>% 
  filter(str_detect(type, type_filter)) %>% 
  mutate(
    temp1 = str_extract(type, "([A-Z])+") %>% 
      na_if("KIJANG"),
    temp2 = str_extract(type, "^[A-Z]+\\s([A-Z])+"),
    type = coalesce(temp1, temp2),
    type = case_when(
      type == "KIJANG INNVA" ~ "KIJANG INNOVA",
      type == "KIJANG PIK" ~ "KIJANG PICKUP",
      TRUE ~ type
    )
  ) %>% 
  select(-temp1, -temp2) %>% 
  semi_join(
    selected_type,
    by = "type"
  ) %>% 
  filter(nchar(type) > 2) %>% 
  mutate(
    price = price * 1e9
  )

mobil_bekas %>% 
  count(nchar(type))
mobil_bekas
  distinct(manufacturer, type) %>% 
  view()

pins::pin(mobil_bekas, name = "mobil_bekas", description = "Data mobil bekas dari Carmudi.co.id yang telah dilakukan pemrosesan", board = "github")
view()
  filter(nchar(type) > 3) %>% 
  transmute(
    lala = str_extract(mokas)
  )
  view()

select(type)
  transmute(
    type,
    name = str_extract(type, "([:alnum:]+)\\s([:alnum:]+)")
  )

  view(zzz)
  