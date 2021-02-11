library(pins)
board_register_github(repo = "aswansyahputra/board")

op_manga <- readr::read_csv("data-raw/op_manga.csv")
pin(op_manga, description = "The dataset about chapter releases of One Piece", board = "github")
pin(mtcars, description = "The dataset about cars' profile", board = "github")

pin_find(board = "github")

op <- pin_get("op-manga", board = "github")

pin_remove("mtcars", board = "github")
pin(filmindonesia, description = "The dataset about Indonesian movies obtained from filmindonesia.or.id", board = "github")

pin(bukuindonesia, description = "The dataset about everlasting Indonesian books listed at Goodreads.com", board = "github")

pin(usedcar, description = "The dataset about used car in Indonesia listed at carmudi.co.id", board = "github")



pin(iwanfals, description = "The dataset about Iwan Fals' songs features, descriptions, and lyrics", board = "github")

iwanfals <- 
  iwanfals %>% 
  mutate(
    lyric = map_chr(lyric, paste, collapse = "|"),
    lyric = na_if(lyric, "NA")
  ) %>% 
  dplyr::filter(!is.na(lyric))
