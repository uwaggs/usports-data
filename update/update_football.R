update_football <- function(league = "usports") {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- lubridate::month(Sys.Date())
  current_season <- dplyr::if_else(
    current_month <= 6,
    paste0(current_year - 1, "-", substr(current_year, 3, 4)),
    paste0(current_year, "-", substr(current_year + 1, 3, 4))
  )

  leagues <- c("fball")

  for(league in leagues) {
    schedule <- usportsscraper::scrape_schedule(
      sport = league,
      season = paste0(current_year = 1, "-", substr(current_year, 3, 4))
    )

    if (!dir.exists("data/{league}_all_returns")) {
      dir.create("data/{league}_team_box")
    }

    if (!dir.exists("data/{league}_all_kicking")) {
      dir.create("data/{league}_player_box")
    }

    if (!dir.exists("data/{league}_all_offence")) {
      dir.create("data/{league}_pbp")
    }

    if (!dir.exists("data/{league}_all_defence")) {
      dir.create("data/{league}_team_box")
    }

    if (!dir.exists("data/{league}_all_drive_summaries")) {
      dir.create("data/{league}_player_box")
    }

    if (!dir.exists("data/{league}_all_scoring_summaries")) {
      dir.create("data/{league}_pbp")
    }

    if (!dir.exists("data/{league}_all_pbp")) {
      dir.create("data/{league}_team_box")
    }

    if (!dir.exists("data/{league}_all_team")) {
      dir.create("data/{league}_player_box")
    }

    existing_returns <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_returns/", league, "_returns_", current_season, ".csv"))
    existing_kicking <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "_kicking/", league, "_kicking_", current_season, ".csv"))
    existing_offence <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "_offence/", league, "_offence_", current_season, ".csv"))
    existing_defence <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "_defence/", league, "_defence_", current_season, ".csv"))
    existing_drive_summaries <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "_drive_summaries/", league, "_drive_summaries_", current_season, ".csv"))
    existing_scoring_summaries <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "_scoring_summaries/", league, "_scoring_summaries_", current_season, ".csv"))
    existing_pbp <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "_pbp/", league, "_pbp_", current_season, ".csv"))
    existing_team <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/data/", league, "_team/", league, "_team_", current_season, ".csv"))


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
      Sys.sleep(11)

      content = httr::GET(link, httr::user_agent("httr"))
      if (content$status_code != 200) {
        cat("Failed to retrieve:", link, "\nstatus code:", content$status_code)
        next
      }

      webpage <- rvest::read_html(content, encoding = "ISO-8859-1")

      returns <- usportsscraper::scrape_fb_returns_safe(html = webpage) |> add_info(link)
      kicking <- usportsscraper::scrape_fb_kicking_safe(html = webpage) |> add_info(link)
      offence <- usportsscraper::scrape_fb_offence_safe(html = webpage) |> add_info(link)
      defence <- usportsscraper::scrape_fb_defence_safe(html = webpage) |> add_info(link)
      drive_summaries <- usportsscraper::scrape_fb_drive_summary_safe(html = webpage) |> add_info(link)
      scoring_summaries <- usportsscraper::scrape_fb_scoring_summary_safe(html = webpage) |> add_info(link)
      pbp <- usportsscraper::scrape_fb_play_by_play_safe(html = webpage) |> add_info(link)
      team <- usportsscraper::scrape_fb_team_safe(html = webpage) |> add_info(link)

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

    readr::write_csv(all_returns, paste0("data/", league, "_returns/", league, "_returns_", current_season, ".csv"))
    readr::write_csv(all_kicking, paste0("data/", league, "_kicking/", league, "_kicking_", current_season, ".csv"))
    readr::write_csv(all_offence, paste0("data/", league, "_offence/", league, "_offence_", current_season, ".csv"))
    readr::write_csv(all_defence, paste0("data/", league, "_defence/", league, "_defence_", current_season, ".csv"))
    readr::write_csv(all_drive_summaries, paste0("data/", league, "_drive_summmaries/", league, "_drive_summaries_", current_season, ".csv"))
    readr::write_csv(all_scoring_summaries, paste0("data/", league, "_scoring_summaries/", league, "_scoring_summaries_", current_season, ".csv"))
    readr::write_csv(all_pbp, paste0("data/", league, "_pbp/", league, "_pbp_", current_season, ".csv"))
    readr::write_csv(all_team, paste0("data/", league, "_team/", league, "_team_", current_season, ".csv"))

    piggyback::pb_upload(
      file = paste0("data/", league, "_returns/", league, "_returns_", current_season, ".csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_returns"),
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "_kicking/", league, "_kicking_", current_season, ".csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_kicking"),
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "_offence/", league, "_offence_", current_season, ".csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_offence"),
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "_defence/", league, "_defence_", current_season, ".csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_defence"),
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "_drive_summaries/", league, "_drive_summaries_", current_season, ".csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_drive_summaries"),
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "_scoring_summaries/", league, "_scoring_summaries_", current_season, ".csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_scoring_summaries"),
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "_pbp/", league, "_pbp_", current_season, ".csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_pbp"),
      overwrite = TRUE
    )

    piggyback::pb_upload(
      file = paste0("data/", league, "_team/", league, "_team_", current_season, ".csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_team"),
      overwrite = TRUE
    )
  }
}
