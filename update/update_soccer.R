source("R/utils.R")
normalize_player_box <- function(df) {

  double_cols <- c(
    "shots",
    "shots_on_goal",
    "goals",
    "assists",
    "shots_on_goal_against",
    "goals_against",
    "saves",
    "minutes_played"
  )

  time_cols <- c(
    "card_time_received",
    "card_two_time_received",
    "card_three_time_received"
  )

  df |>
    dplyr::mutate(

      # --- Convert numeric columns ---
      dplyr::across(
        dplyr::any_of(double_cols),
        as.double
      ),

      # --- Clean time columns and keep as character ---
      dplyr::across(
        dplyr::any_of(time_cols),
        ~ {
          x <- as.character(.)
          x <- stringr::str_trim(x)
          x[x == ""] <- NA_character_

          # Keep only first two time segments (MM:SS)
          x <- sub("^([0-9]+:[0-9]+):.*$", "\\1", x)

          x
        }
      )
    )
}
normalize_pbp <- function(df) {

  double_cols <- c(
    "halves",
    "score_away",
    "score_home"
  )

  time_cols <- c("time")

  df |>
    dplyr::mutate(

      # --- Convert numeric columns ---
      dplyr::across(
        dplyr::any_of(double_cols),
        as.double
      ),

      # --- Clean time column and keep as character ---
      dplyr::across(
        dplyr::any_of(time_cols),
        ~ {
          x <- as.character(.)
          x <- stringr::str_trim(x)
          x[x == ""] <- NA_character_

          # Convert MM:SS:00 -> MM:SS
          x <- sub("^([0-9]+:[0-9]+):.*$", "\\1", x)

          x
        }
      )
    )
}
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- lubridate::month(Sys.Date())
  current_season <- dplyr::if_else(
    current_month <= 6,
    paste0(current_year - 1, "-", substr(current_year, 3, 4)),
    paste0(current_year, "-", substr(current_year + 1, 3, 4))
  )

  leagues <- c("msoc", "wsoc")

  for(league in leagues) {
    schedule <- usportsscraper::scrape_schedule_gamelog(
    league,
    season = current_season
  )

    existing_team_box <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_team_box/", league, "_team_box_", current_season, ".csv"))
    existing_player_box <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_player_box/", league, "_player_box_", current_season, ".csv"))
    existing_pbp <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_pbp/", league, "_pbp_", current_season, ".csv"))

    games_to_scrape <-
      schedule |>
      dplyr::filter(
        date <= Sys.Date(),
        !is.na(game_link),
        game_link != "",
        !(game_link %in% unique(existing_team_box$link))
      ) |>
      pull(game_link)

    all_team_box <- data.frame()
    all_player_box <- data.frame()
    all_pbp <- data.frame()

    for(link in games_to_scrape) {
      Sys.sleep(11)

      content = httr::GET(link, httr::user_agent("httr"))
      if (content$status_code != 200) {
        cat("Failed to retrieve:", link, "\nstatus code:", content$status_code)
        next
      }

      webpage <- rvest::read_html(content, encoding = "ISO-8859-1")

      team_box <- usportsscraper::scrape_soc_team_box_score_safe(html = webpage) |>  add_info(link)
      player_box <- usportsscraper::scrape_soc_player_box_score_safe(html = webpage) |> add_info(link)
      pbp <- usportsscraper::scrape_soc_play_by_play_safe(html = webpage) |> add_info(link)

      all_team_box <- dplyr::bind_rows(all_team_box, team_box)
      all_player_box <- dplyr::bind_rows(all_player_box, player_box)
      all_pbp <- dplyr::bind_rows(all_pbp, pbp)
    }
    all_team_box<-normalize_team_box(all_team_box)
    all_player_box<-normalize_player_box(all_player_box)
    all_pbp<-normalize_pbp(all_pbp)
    all_team_box <- dplyr::bind_rows(all_team_box, existing_team_box) |> distinct()
    all_player_box <- dplyr::bind_rows(all_player_box, existing_player_box) |> distinct()
    all_pbp <- dplyr::bind_rows(all_pbp, existing_pbp) |> distinct()

    readr::write_csv(all_team_box, paste0("data/", league, "_team_box/", league, "_team_box.csv"))
    readr::write_csv(all_player_box, paste0("data/", league, "_player_box/", league, "_player_box.csv"))
    readr::write_csv(all_pbp, paste0("data/", league, "_pbp/", league, "_pbp.csv"))

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

