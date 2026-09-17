source("R/utils.R")
normalize_returns <- function(df) {
  cols_to_double <- c("number", "yards", "average", "longest", "touchdowns")

  df %>%
    mutate(across(any_of(cols_to_double), as.double))
}

normalize_kicking <- function(df) {
  df |>
    mutate(
      field_goals_made=as.double(field_goals_made),
      field_goals_attempted=as.double(field_goals_attempted),
      longest=as.double(longest),
      extra_points_made=as.double(extra_points_made),
      extra_points_attempted=as.double(extra_points_attempted),
      return_yards=as.double(return_yards),
      points=as.double(points),
      number=as.double(number),
      yards=as.double(yards),
      average=as.double(average),
      touchbacks=as.double(touchbacks),
      inside_20=as.double(inside_20),
      out_of_bounds=as.double(out_of_bounds)
    )
}
normalize_offence<-function(df){
  df |>
    mutate(
      passing_completions=as.double(passing_completions),
      passing_attempts=as.double(passing_attempts),
      passing_yards=as.double(passing_yards),
      passing_longest=as.double(passing_longest),
      passing_touchdowns=as.double(passing_touchdowns),
      passing_interceptions=as.double(passing_interceptions),
      passing_rtg=as.double(passing_rtg),
      rushing_attempts=as.double(rushing_attempts),
      rushing_yards=as.double(rushing_yards),
      rushing_longest=as.double(rushing_longest),
      rushing_touchdowns=as.double(rushing_touchdowns),
      rushing_average=as.double(rushing_average),
      receiving_receptions=as.double(receiving_receptions),
      receiving_yards=as.double(receiving_yards),
      receiving_longest=as.double(receiving_longest),
      receiving_touchdowns=as.double(receiving_touchdowns),
      receiving_average=as.double(receiving_average),
      fumble_number=as.double(fumble_number),
      fumbles_lost=as.double(fumbles_lost)
    )
}
normalize_defence<-function(df){
  cols_to_convert<-c("solo","sack_assists","total","sacks","sack_yards","tackles_for_loss","tackles_for_loss_yards","forced_fumbles","fumble_recovery","fumble_recovery_yards_gained","interceptions","interception_yards_gained","passes_broken_up","blocked_kicks","quarterback_hurries")
  df <- df %>%
    mutate(across(any_of(cols_to_convert), as.double))

}
normalize_drive_summaries <- function(df) {

  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  numeric_cols <- c("quarter", "plays", "yards", "drive_id")

  time_cols <- c("start", "possessions")

  df <- df |>
    mutate(
      # Force all time-like columns to character
      across(any_of(time_cols), ~ as.character(.x))
    ) |>
    mutate(
      across(any_of(numeric_cols), as.double)
    ) |>
    mutate(
      drive_start = as.numeric(as.difftime(start, format = "%M:%S"))
    )

  return(df)
}

normalize_scoring_summaries<-function(df){
  cols_to_convert<-c("prd","away_score","home_score")
  df <- df %>%
    mutate(across(any_of(cols_to_convert), as.double))
  df<-df|>
    mutate(time=as.numeric(as.difftime(time, format = "%M:%S")))
}
normalize_pbp<-function(df){
  cols_to_convert<-c("down", "quarter", "drive_id", "kick", "kickoff", "punt", "pass_complete", "pass_incomplete", "rush","sack","fumble","fumble_forced","touchdown","timeout","penalty","intercepted","fumble_recovered","rouge_point")
  df <- df %>%
    mutate(across(any_of(cols_to_convert), as.double))
  df<-df|>
    mutate(drive_start=as.numeric(as.difftime(drive_start, format = "%M:%S")))
}
normalize_team<-function(df){
  cols_to_convert<-c("first_downs", "passing", "rushing", "penalty", "total_offense", "total_offensive_plays","average_gain_per_play","net_yards_passing","completions","attempts","net_yards_per_pass_play","number_of_sacked","yards_lost_from_sacked","had_intercepted","net_yards_rushing","rushing_attempts","average_gain_per_rush","number_of_punts","yards_punted","average","total_return_yards","number_of_punt_returns","yards_from_punt_returns","number_of_kickoff_returns","yards_from_kickoff_returns","number_of_interception_returns","yards_from_interception_returns","number_of_penalties","yards_from_penalties","fumbles_number","fumbles_lost","number_of_sacks","yards_lost_from_sacks","number_of_interceptions","yards_from_interceptions")
  df <- df %>%
    mutate(across(any_of(cols_to_convert), as.double))
  df<-df|>
    mutate(time_of_possession=as.numeric(as.difftime(time_of_possession, format = "%M:%S")))
}
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- lubridate::month(Sys.Date())
  current_season <- dplyr::if_else(
    current_month <= 6,
    paste0(current_year - 1, "-", substr(current_year, 3, 4)),
    paste0(current_year, "-", substr(current_year + 1, 3, 4))
  )

  leagues <- c("fball")

  for(league in leagues) {
    schedule <- usportsscraper::scrape_schedule_gamelog(
    league,
    season = current_season
  )

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
    all_kicking<-normalize_kicking(all_kicking)

    all_offence<-normalize_offence(all_offence)

    all_defence<-normalize_defence(all_defence)

    all_drive_summaries<-normalize_drive_summaries(all_drive_summaries)

    all_scoring_summaries<-normalize_scoring_summaries(all_scoring_summaries)

    all_pbp<-normalize_pbp(all_pbp)

    all_team<-normalize_team(all_team)
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

