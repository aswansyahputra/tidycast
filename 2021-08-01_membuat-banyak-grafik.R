# Load packages -----------------------------------------------------------

library(tidyverse)
library(datasauRus)
library(scales)
library(hrbrthemes)
library(janitor)

# Data inspection ---------------------------------------------------------

datasaurus_dozen

datasaurus_dozen %>%
  distinct(dataset) %>%
  pull()

# Data visualisation ------------------------------------------------------


dino_stats <-
  datasaurus_dozen %>%
  filter(dataset == "dino") %>%
  summarise(
    across(c(x, y), list(avg = mean, std = sd)),
    r = cor(x, y)
  ) %>%
  mutate(
    across(everything(), ~ number(.x, accuracy = 0.01)),
    label = str_glue(
      "
      Mean X: {x_avg}
      Mean Y: {y_avg}
      Std. X: {x_std}
      Std. Y: {y_std}
      Corr. : {r}
      "
    )
  ) %>%
  pull(label)

dino_plot <-
  datasaurus_dozen %>%
  filter(dataset == "dino") %>%
  ggplot(aes(x, y)) +
  geom_point(colour = "greenyellow") +
  annotate(
    geom = "label",
    x = -Inf,
    y = -Inf,
    label = dino_stats,
    fill = "ivory",
    alpha = 0.8,
    colour = "gray20",
    family = "Fira Code",
    size = 3.5,
    hjust = "left",
    vjust = "bottom",
    lineheight = 0.85
  ) +
  scale_x_continuous(
    name = NULL,
    limits = c(0, 100),
    breaks = seq.int(0, 100, by = 25),
    labels = seq.int(0, 100, by = 25),
    sec.axis = dup_axis()
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0, 100),
    breaks = seq.int(0, 100, by = 25),
    labels = seq.int(0, 100, by = 25),
    sec.axis = dup_axis()
  ) +
  labs(
    title = "DATASAURUS DOZEN",
    subtitle = "DATASET: DINO"
  ) +
  theme_ft_rc(
    base_family = "Fira Code",
    grid = "XY",
    ticks = TRUE
  )

ggsave(
  filename = "outfile/dino.png",
  plot = dino_plot,
  width = 8,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

# Data visualisation production -------------------------------------------

datasaurus_dozen %>%
  split(.$dataset) %>%
  walk2(
    .x = .,
    .y = names(.),
    ~ {

      message("Generating plot for: ", .y)

      subplot_stats <-
        .x %>%
        summarise(
          across(c(x, y), list(avg = mean, std = sd)),
          r = cor(x, y)
        ) %>%
        mutate(
          across(everything(), ~ number(.x, accuracy = 0.01)),
          label = str_glue(
            "
            Mean X: {x_avg}
            Mean Y: {y_avg}
            Std. X: {x_std}
            Std. Y: {y_std}
            Corr. : {r}
           "
          )
        ) %>%
        pull(label)

      subplot <-
        .x %>%
        ggplot(aes(x, y)) +
        geom_point(colour = "greenyellow") +
        annotate(
          geom = "label",
          x = -Inf,
          y = -Inf,
          label = subplot_stats,
          fill = "ivory",
          alpha = 0.8,
          colour = "gray20",
          family = "Fira Code",
          size = 3.5,
          hjust = "left",
          vjust = "bottom",
          lineheight = 0.85
        ) +
        scale_x_continuous(
          name = NULL,
          limits = c(0, 100),
          breaks = seq.int(0, 100, by = 25),
          labels = seq.int(0, 100, by = 25),
          sec.axis = dup_axis()
        ) +
        scale_y_continuous(
          name = NULL,
          limits = c(0, 100),
          breaks = seq.int(0, 100, by = 25),
          labels = seq.int(0, 100, by = 25),
          sec.axis = dup_axis()
        ) +
        labs(
          title = "DATASAURUS DOZEN",
          subtitle = str_glue("DATASET: {toupper(str_replace(.y, '_', '-'))}")
        ) +
        theme_ft_rc(
          base_family = "Fira Code",
          grid = "XY",
          ticks = TRUE
        )

      ggsave(
        filename = str_glue("outfile/{make_clean_names(.y)}.png"),
        plot = subplot,
        width = 8,
        height = 5,
        dpi = 300,
        type = "cairo-png"
      )
    }
  )
