library(usportsscaper)
library(tidyverse)
library(piggyback)
source("R/utils.R")

# get schedule

leagues <- c("mvball", "wvball")

for(league in leagues) {

  # get schedule

  schedule <- pb_download(
    file = paste0(league, "_schedules.csv"),
    repo = "uwaggs/usports-data",
    tag = "schedules",
    pb_dir = "data/schedules"
  )

  # get game links
  links <- schedule %>%
    pull(link) %>%
    unique()

  # scrape game data

  all_team_box <- df()
  all_player_box <- df()
  all_pbp <- df()

  for(link in links) {

    webpage <- tryCatch({
      rvest::read_html(link)
    }, error = function(e) {
      return(NULL)
    })

    if(is.null(webpage)) next

    team_box <- scrape_vb_team_box_score(html = webpage) %>% add_info(link)
    player_box <- scrape_vb_player_box_score(html = webpage) %>% add_info(link)
    pbp <- scrape_vb_play_by_play(html = webpage)  %>% add_info(link)

    all_team_box <- bind_rows(all_team_box, team_box)
    all_player_box <- bind_rows(all_player_box, player_box)
    all_pbp <- bind_rows(all_pbp, pbp)
  }

  # save data

  write_csv(all_team_box, paste0("data/", league, "/",league, "_team_box.csv"))
  write_csv(all_player_box, paste0("data/", league, "/",league, "_player_box.csv"))

  # split up pbp by season and write to file using purrr::walk2

  all_pbp %>%
    group_by(season) %>%
    group_split() %>%
    purrr::walk2(
      .x = paste0("data/", league, "/", league, "_pbp_"),
      .y = seq_along(.),
      ~ write_csv(.x, paste0(.y, .x$season[1], ".csv"))
    )

  write_csv(all_pbp, paste0("data/", league, "/", league, "_pbp.csv"))

  # upload to releases
  pb_upload(
    file = paste0("data/", league, "/", league, "_team_box.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_team_box"),
    overwrite = T
  )

  pb_upload(
    file = paste0("data/", league, "/", league, "_player_box.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_player_box"),
    overwrite = T
  )

  # pb_upload(
  #   file = paste0("data/", league, "/", league, "_pbp.csv"),
  #   repo = "uwaggs/usports-data",
  #   tag = paste0(league, "_pbp"),
  #   overwrite = T
  # )

  sapply(
  unique(all_pbp$season), \(x)
  pb_upload(
    file = paste0("data/", league, "/", league, "_pbp_", x, ".csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_pbp"),
    overwrite = T
  )
  )

}




