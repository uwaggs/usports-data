library(rvest)
library(tidyverse)
library(lubridate)

fball_schedule <-
  read_csv(
    "~/R for Data Science/usports-data/schedules/fball_schedules.csv",
    col_names = TRUE
  )

fball_schedule <- fball_schedule |>
  filter(exhibition != 1) |>
  filter(is.na(box_scores)) |>
  mutate(
    month = ymd(month),
    month = format(month, "%B %Y")
  )

fh_schedule <-
  read_csv(
    "~/R for Data Science/usports-data/schedules/fh_schedules.csv",
    col_names = TRUE
  )

fh_schedule <- fh_schedule |>
  filter(exhibition != 1) |>
  filter(is.na(box_scores))
