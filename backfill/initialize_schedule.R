initialize_schedule <- function() {
  years <- 2009:(lubridate::year(Sys.Date()) - 1)
  seasons <- paste0(years, "-", substr(years + 1, 3, 4))

  leagues <- c("mbkb", "wbkb", "fh", "fball", "msoc", "wsoc", "mice", "wice", "mvball", "wvball")

  all_schedule <- data.frame()

  for(league in leagues) {
    for(season in seasons) {
      teams <- usportsscraper::team_names(league, season)
      variants <- usportsscraper::full_season(season)
      print(season)
      for(team in teams) {
        Sys.sleep(6)
        for(variant in variants) {
          schedule <- usportsscraper::scrape_game_log_safe(league, season, team)
          all_schedule <- dplyr::bind_rows(all_schedule, schedule)
        }
        all_schedule |>
          dplyr::mutate(path = stringr::str_glue("data/{league}_schedule/{league}_schedule_{season}.csv")) |>
          dplyr::group_by(season) |>
          dplyr::group_split() |>
          purrr::walk( ~ {
            fs::dir_create(dirname(.x$path[1]))
            readr::write_csv(.x, .x$path[1])
          })

        sapply(
          unique(all_schedule$season), \(x)
          piggyback::pb_upload(
            file = glue::glue("data/{league}_schedule/{league}_schedule_{x}.csv"),
            repo = "uwaggs/usports-data",
            tag = paste0(league, "_schedule"),
            overwrite = TRUE
          )
        )
      }
    }
  }
}
