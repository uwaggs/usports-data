update_fieldhockey <- function(league = "usports") {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- lubridate::month(Sys.Date())
  current_season <- dplyr::if_else(
    current_month <= 6,
    paste0(current_year - 1, "-", substr(current_year, 3, 4)),
    paste0(current_year, "-", substr(current_year + 1, 3, 4))
  )

  league <- "fh"

  schedule <- usportsscraper::scrape_schedule(
    sport = league,
    season = paste0(current_year = 1, "-", substr(current_year, 3, 4))
  )
  existing_team_box <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_team_box_", current_season, ".csv"))
  existing_player_box <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_player_box_", current_season, ".csv"))
  existing_pbp <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_pbp_", current_season, ".csv"))

  games_to_scrape <-
    schedule |>
    dplyr::filter(
      date = Sys.Date(),
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

    team_box <- usportsscraper::scrape_fh_team_box_score(html = webpage) |> add_info(link)
    player_box <- usportsscraper::scrape_fh_player_box_score(html = webpage) |> add_info(link)
    pbp <- usportsscraper::scrape_fh_play_by_play(html = webpage) |> add_info(link)

    all_team_box <- dplyr::bind_rows(all_team_box, team_box)
    all_player_box <- dplyr::bind_rows(all_player_box, player_box)
    all_pbp <- dplyr::bind_rows(all_pbp, pbp)
  }

  all_team_box <- dplyr::bind_rows(all_team_box, existing_team_box) |> distinct()
  all_player_box <- dplyr::bind_rows(all_player_box, existing_player_box) |> distinct()
  all_pbp <- dplyr::bind_rows(all_pbp, existing_pbp) |> distinct()

  readr::write_csv(all_team_box, paste0("data/", league, "/", league, "_team_box.csv"))
  readr::write_csv(all_player_box, paste0("data/", league, "/", league, "_player_box.csv"))
  readr::write_csv(all_pbp, paste0("data/", league, "/", league, "_pbp.csv"))

  piggyback::pb_upload(
    file = paste0("data/", league, "/", league, "team_box.csv"),
    repo = "uwaggs/usports-data",
    tag = "volleyball",
    overwrite = TRUE
  )

  piggyback::pb_upload(
    file = paste0("data/", league, "/", league, "_player_box.csv"),
    repo = "uwaggs/usports-data",
    tag = "volleyball",
    overwrite = TRUE
  )

  piggyback::pb_upload(
    file = paste0("data/", league, "/", league, "_pbp.csv"),
    repo = "uwaggs/usports-data",
    tag = "volleyball",
    overwrite = TRUE
  )
}
