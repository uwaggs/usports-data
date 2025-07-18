library(usportsscraper)

updae_football <- function(league = "usports") {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- lubridate::month(Sys.Date())
  current_season <- dplyr::if_else(
    current_month <= 6,
    paste0(current_year - 1, "-", substr(current_year, 3, 4)),
    paste0(current_year, "-", substr(current_year + 1, 3, 4))
  )

  leagues <- c("mbkb", "wbkb")

  for(league in leagues) {
    schedule <- usportsscraper::scrape_bkb_schedule(
      sport = league,
      season = paste0(current_year = 1, "-", substr(current_year, 3, 4))
    )
    existing_returns <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_returns_", current_season, ".csv"))
    existing_kicking <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_kicking_", current_season, ".csv"))
    existing_offence <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_offence_", current_season, ".csv"))
    existing_defence <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_defence_", current_season, ".csv"))
    existing_drive_summaries <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_drive_summaries_", current_season, ".csv"))
    existing_scoring_summaries <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_scoring_summaries_", current_season, ".csv"))
    existing_pbp <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_pbp_", current_season, ".csv"))
    existing_team <- readr::read_csv(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "/", league, "_team_", current_season, ".csv"))


    games_to_scrape <-
      schedule |>
      dplyr::filter(
        date <= Sys.Date(),
        !is.na(game_link),
        game_link != "",
        !(game_link %in% unique(existing_team_box$link))
      ) |>
      pull(game_link)

    all_returns <- data.frame()
    all_kicking <- data.frame()
    all_offence <- data.frame()
    all_defence <- data.frame()
    all_drive_summaries <- data.frame()
    all_scoring_summaries <- data.frame()
    all_pbp <- data.frame()
    all_team <- data.frame()

    for(link in games_to_scrape) {
      webpage <- tryCatch({
        rvest::read_html(link)
      }, error = function(e) {
        return(NULL)
      })

      if(is.null(webpage)) next

      returns <- usportsscraper::scrape_fb_returns(html = webpage) |> add_info(link)
      kicking <- usportsscraper::scrape_fb_kicking(html = webpage) |> add_info(link)
      offence <- usportsscraper::scrape_fb_offence(html = webpage) |> add_info(link)
      defence <- usportsscraper::scrape_fb_defence(html = webpage) |> add_info(link)
      drive_summaries <- usportsscraper::scrape_fb_drive_summary(html = webpage) |> add_info(link)
      scoring_summaries <- usportsscraper::scrape_fb_scoring_summary(html = webpage) |> add_info(link)
      pbp <- usportsscraper::scrape_fb_play_by_play(html = webpage) |> add_info(link)
      team <- usportsscraper::scrape_fb_team(html = webpage) |> add_info(link)

      all_returns <- dplyr::bind_rows(all_returns, returns)
      all_kicking <- dplyr::bind_rows(all_kicking, kicking)
      all_offence <- dplyr::bind_rows(all_offence, offence)
      all_defence <- dplyr::bind_rows(all_defence, defence)
      all_drive_summaries <- dplyr::bind_rows(all_drive_summaries, drive_summaries)
      all_scoring_summaries <- dplyr::bind_rows(all_scoring_summaries, scoring_summaries)
      all_pbp <- dplyr::bind_rows(all_pbp, pbp)
      all_team <- dplyr::bind_rows(all_team, team)
    }

    all_returns <- dplyr::bind_rows(all_returns, existing_returns) |> distinct()
    all_kicking <- dplyr::bind_rows(all_kicking, existing_kicking) |> distinct()
    all_offence <- dplyr::bind_rows(all_offence, existing_offence) |> distinct()
    all_defence <- dplyr::bind_rows(all_defence, existing_defence) |> distinct()
    all_drive_summaries <- dplyr::bind_rows(all_drive_summaries, existing_drive_summaries) |> distinct()
    all_scoring_summaries <- dplyr::bind_rows(all_scoring_summaries, existing_scoring_summaries) |> distinct()
    all_pbp <- dplyr::bind_rows(all_pbp, existing_pbp) |> distinct()
    all_team <- dplyr::bind_rows(all_team, existing_team) |> distinct()

    readr::write_csv(all_returns, paste0("data/", league, "/", league, "_returns.csv"))
    readr::write_csv(all_kicking, paste0("data/", league, "/", league, "_kicking.csv"))
    readr::write_csv(all_offence, paste0("data/", league, "/", league, "_offence.csv"))
    readr::write_csv(all_defence, paste0("data/", league, "/", league, "_defence.csv"))
    readr::write_csv(all_drive_summaries, paste0("data/", league, "/", league, "_drive_summaries.csv"))
    readr::write_csv(all_scoring_summaries, paste0("data/", league, "/", league, "_scoring_summaries.csv"))
    readr::write_csv(all_pbp, paste0("data/", league, "/", league, "_pbp.csv"))
    readr::write_csv(all_team, paste0("data/", league, "/", league, "_team.csv"))

    piggyback::pb_upload(
      file = paste0("data/", league, "/", league, "_returns.csv"),
      repo = "uwaggs/usports-data",
      tag = "football",
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "/", league, "_kicking.csv"),
      repo = "uwaggs/usports-data",
      tag = "football",
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "/", league, "_offence.csv"),
      repo = "uwaggs/usports-data",
      tag = "football",
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "/", league, "_defence.csv"),
      repo = "uwaggs/usports-data",
      tag = "football",
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "/", league, "_drive_summaries.csv"),
      repo = "uwaggs/usports-data",
      tag = "football",
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "/", league, "_scoring_summaries.csv"),
      repo = "uwaggs/usports-data",
      tag = "football",
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "/", league, "_pbp.csv"),
      repo = "uwaggs/usports-data",
      tag = "football",
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "/", league, "_team.csv"),
      repo = "uwaggs/usports-data",
      tag = "football",
      overwrite = TRUE
    )
  }
}
