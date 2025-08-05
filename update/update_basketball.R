source("R/utils.R")
pacman::p_load(tidyverse, piggyback, rvest)

current_year <- as.integer(format(Sys.Date(), "%Y"))
current_month <- lubridate::month(Sys.Date())
current_season <- dplyr::if_else(
  current_month <= 8,
  paste0(current_year - 1, "-", substr(current_year, 3, 4)),
  paste0(current_year, "-", substr(current_year + 1, 3, 4))
)

leagues <- c("mbkb", "wbkb")

for(league in leagues) {
  schedule <- usportsscraper::scrape_schedule(
    league,
    season = current_season
  )

  if (!dir.exists("data/{league}/team_box")) {
    dir.create("data/{league}/team_box")
  }

  if (!dir.exists("data/{league}_player_box")) {
    dir.create("data/{league}_player_box")
  }

  if (!dir.exists("data/{league}_pbp")) {
    dir.create("data/{league}_pbp")
  }

  existing_team_box <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_team_box/", league, "_team_box_", current_season,".csv"))
  existing_player_box <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_player_box/", league, "_player_box_", current_season,".csv"))
  existing_pbp <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_pbp/", league, "_pbp_", current_season,".csv"))

  games_to_scrape <-
    schedule |>
    dplyr::filter(
      date <= Sys.Date(),
      !is.na(box_scores),
      box_scores != "",
      !(box_scores %in% unique(existing_team_box$link))
    ) |>
    dplyr::pull(box_scores)

  all_team_box <- data.frame()
  all_player_box <- data.frame()
  all_pbp <- data.frame()

  for(link in games_to_scrape) {
    Sys.sleep(5)

    content = httr::GET(link, httr::user_agent("httr"))
    if (content$status_code != 200) {
      cat("Failed to retrieve:", link, "\nstatus code:", content$status_code)
      next
    }

  webpage <- rvest::read_html(content)

  team_box <- usportsscraper::scrape_bkb_team_box_score_safe(html = webpage) |>  add_info(link)
  player_box <- usportsscraper::scrape_bkb_player_box_score_safe(html = webpage) |> add_info(link)
  pbp <- usportsscraper::scrape_bkb_play_by_play_safe(html = webpage) |> add_info(link)

  all_team_box <- dplyr::bind_rows(all_team_box, team_box)
  all_player_box <- dplyr::bind_rows(all_player_box, player_box)
  all_pbp <- dplyr::bind_rows(all_pbp, pbp)
}

all_team_box <- dplyr::bind_rows(all_team_box, existing_team_box) |> dplyr::distinct()
all_player_box <- dplyr::bind_rows(all_player_box, existing_player_box) |> dplyr::distinct()
all_pbp <- dplyr::bind_rows(all_pbp, existing_pbp) |> dplyr::distinct()

readr::write_csv(all_team_box, paste0("data/", league, "_team_box/", league, "_team_box_", current_season, ".csv"))
readr::write_csv(all_player_box, paste0("data/", league, "_player_box/", league, "_player_box_", current_season, ".csv"))
readr::write_csv(all_pbp, paste0("data/", league, "_pbp/", league, "_pbp_", current_season, ".csv"))

piggyback::pb_upload(
  file = paste0("data/", league, "_team_box/", league, "_team_box_", current_season, ".csv"),
  repo = "uwaggs/usports-data",
  tag = paste0(league, "_team_box"),
  overwrite = TRUE
)

piggyback::pb_upload(
  file = paste0("data/", league, "_player_box/", league, "_player_box_", current_season, ".csv"),
  repo = "uwaggs/usports-data",
  tag = paste0(league, "_player_box"),
  overwrite = TRUE
)

piggyback::pb_upload(
  file = paste0("data/", league, "_pbp/", league, "_pbp_", current_season, ".csv"),
  repo = "uwaggs/usports-data",
  tag = paste0(league, "_pbp"),
  overwrite = TRUE
)
}

