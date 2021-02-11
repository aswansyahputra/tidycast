# Load packages -----------------------------------------------------------
library(geniusr)
library(spotifyr)
library(tidyverse)
library(furrr)

plan(multiprocess)

# Get lyrics from songs ---------------------------------------------------
so7_songs <- get_artist_songs("343924")

so7_lyrics_raw <- 
  so7_songs %>% 
  pull(song_id) %>% 
  future_map_dfr(possibly(scrape_lyrics_id, otherwise = NULL), .progress = TRUE)

so7_lyrics <- 
  so7_lyrics_raw %>% 
  group_by(track_name = song_name) %>% 
  summarise(
    lyric = paste0(line, collapse = "\n")
  )


# Get audio features ------------------------------------------------------

so7_audio_raw <- 
  get_artist_audio_features("Sheila On 7", include_groups = c("single", "album"))


# Combine dataset ---------------------------------------------------------
so7_musics <- 
  so7_audio_raw %>% 
  inner_join(so7_lyrics) %>% 
  as_tibble() %>% 
  select(
    track_name,
    album_name,
    album_release_date,
    danceability:tempo,
    lyric
  )
