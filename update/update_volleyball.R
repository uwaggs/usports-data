source("R/utils.R")
update_volleyball <- function(league = "usports") {

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- lubridate::month(Sys.Date())
  current_season <- dplyr::if_else(
    current_month <= 6,
    paste0(current_year - 1, "-", substr(current_year, 3, 4)),
    paste0(current_year, "-", substr(current_year + 1, 3, 4))
  )

  leagues <- c("mvball", "wvball")

  for(league in leagues) {
    schedule <- usportsscraper::scrape_schedule(
      sport = league,
      season = paste0(current_year = 1, "-", substr(current_year, 3, 4))
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
      webpage <- tryCatch({
        rvest::read_html(link)
      }, error = function(e) {
        return(NULL)
      })

      if(is.null(webpage)) next

      team_box <- usportsscraper::scrape_vb_team_box_score(html = webpage) |>  add_info(link)
      player_box <- usportsscraper::scrape_vb_player_box_score(html = webpage) |> add_info(link)
      pbp <- usportsscraper::scrape_soc_vb_by_play(html = webpage) |> add_info(link)

      all_team_box <- dplyr::bind_rows(all_team_box, team_box)
      all_player_box <- dplyr::bind_rows(all_player_box, player_box)
      all_pbp <- dplyr::bind_rows(all_pbp, pbp)
    }

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
}
