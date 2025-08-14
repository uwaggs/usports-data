initialize_schedule <- function() {
  years <- 2009:(lubridate::year(Sys.Date()) - 1)
  seasons <- paste0(years, "-", substr(years + 1, 3, 4))

  leagues <- c("mbkb", "wbkb", "fh", "mice", "wice", "msoc", "wsoc", "mvball", "wvball", "fball")

  for(league in leagues) {
    all_schedule <- data.frame()
    print(league)
    for(season in seasons) {
      teams <- usportsscraper::team_names_safe(league, season)
      variants <- usportsscraper::full_season(season)
      print(season)
      for(team in teams) {
        Sys.sleep(6)
        for(variant in variants) {
          schedule <- usportsscraper::scrape_game_log_safe(league, variant, team)
          all_schedule <- dplyr::bind_rows(all_schedule, schedule)
        }
      }
      closeAllConnections()
      print(nrow(all_schedule))
    }
    all_schedule <- dplyr::distinct(all_schedule, home_team, away_team, date, .keep_all = TRUE)

    readr::write_csv(all_schedule, glue::glue("data/schedules/{league}_schedule.csv"))

    piggyback::pb_upload(
      file = glue::glue("data/schedules/{league}_schedule.csv"),
      repo = "uwaggs/usports-data",
      tag = paste0("schedules"),
      overwrite = TRUE
    )
  }
}
