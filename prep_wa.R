library(tidyverse)
library(lubridate)

wa_raw <- 
  read_lines("~/Downloads/Telegram Desktop/WhatsApp Chat with AIDI - Indonesia.txt")

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
    regex = "([\\w\\s+-]+): (.*)"
  )
  # mutate(
  #   datetime = dmy_hm(datetime),
  #   nchar = nchar(message),
  #   any_emoji = emo::ji_detect(message)
  # )
  
wa_anon <- 
  wa %>% 
  mutate(
    user_rand = stringi::stri_rand_shuffle(user),
    user_rand = str_squish(user_rand),
    user_rand = case_when(
      str_detect(user, "[:alpha:]") ~ user,
      TRUE ~ user_rand
    ),
    message_rand = stringi::stri_rand_shuffle(message),
    message_rand = case_when(
      message == "<Media omitted>" ~ message,
      TRUE ~ message_rand
    ),
  )

wa_anon_raw <- 
  wa_anon %>% 
  select(datetime, user_rand, message_rand) %>% 
  transmute(
    content = glue::glue("{datetime} - {user_rand}: {message_rand}")
  ) %>% 
  deframe()

wa_anon_raw %>% 
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
    regex = "([\\w\\s+-]+): (.*)"
  ) %>% 
  mutate(
    datetime = dmy_hm(datetime),
    nchar = nchar(message),
    any_emoji = emo::ji_detect(message)
  )

write_lines(wa_anon_raw, "WhatsApp Chat with AIDI - Indonesia (Anonymized).txt")

wa_raw

read_lines("WhatsApp Chat with AIDI - Indonesia (Anonymized).txt") %>% 
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
    regex = "([\\w\\s+-]+): (.*)"
  ) %>% 
  mutate(
    datetime = dmy_hm(datetime),
    nchar = nchar(message),
    any_emoji = emo::ji_detect(message)
  )

wa %>% 
  distinct(user)

users <- c("+62 123 123 123",
           "+90 (555) 154 233",
           "+62 888-123-123",
           "Teman Saya",
           "Unknown")

tibble(
  user = sample(users, size = 1e3, replace = TRUE),
  message = stringi::stri_rand_lipsum(nparagraphs = 1e3)
) %>% 
  mutate(
    message = case_when(
      row_number() %in% sample(seq_len(1e3), size = 0.2*1e3) ~ "<Media omitted>",
      row_number() %in% sample(seq_len(1e3), size = 0.05*1e3) ~ sample(emo::jis$emoji, size = 1),  
      row_number() %in% sample(seq_len(1e3), size = 0.05*1e3) ~ sample(emo::jis$emoji, size = 1),
      row_number() %in% sample(seq_len(1e3), size = 0.05*1e3) ~ sample(emo::jis$emoji, size = 1),
      TRUE ~ message
    )
  ) %>% 
  view()
  filter(message == "<Media omitted>")
sample(emo::jis)
