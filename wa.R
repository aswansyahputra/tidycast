library(tidyverse)

wa_raw <- 
  read_lines("WhatsApp Chat with AIDI - Indonesia (Anonymized).txt")

wa <-
  wa_raw %>% 
  enframe(name = NULL, value = "content") %>% 
  extract(
    content,
    into = c("datetime", "content"),
    regex = "(\\d{2}/\\d{2}/\\d{4}, \\d{2}:\\d{2}) - (.*)"
  ) %>% 
  filter(str_detect(content, ":")) %>% 
  extract(
    content,
    into = c("user", "message"),
    regex = "([\\w\\(\\)\\s+-]+): (.*)"
  ) %>% 
  mutate(
    datetime = lubridate::dmy_hm(datetime),
    in_contact = str_detect(user, "[:alpha:]"),
    any_media = str_detect(message, "<Media omitted>"),
    nchar = case_when(
      message == "<Media omitted>" ~ NA_integer_,
      TRUE ~ nchar(message)
    ),
    any_emoji = emo::ji_detect(message),
    n_emoji = emo::ji_count(message)
  )

wa_emoji <- 
  wa %>% 
  filter(any_emoji == TRUE) %>% 
  transmute(
    datetime,
    user,
    emoji = emo::ji_extract_all(message)
  ) %>% 
  unnest_longer(emoji) %>% 
  count(datetime, user, emoji, name = "n_emoji") %>% 
  left_join(
    emo::jis %>% 
      select(emoji,
             emoji_group = group, 
             emoji_subgroup = subgroup)
  )
  
wa_emoji %>% 
  count(user, emoji, wt = n_emoji, sort = TRUE)

wa_emoji %>% 
  count(emoji, wt = n_emoji, sort = TRUE) %>% 
  mutate(n =  log10(n)) %>%
  # top_n(30, wt = n) %>% 
  wordcloud2::wordcloud2()
  