source("R/utils.R")
leagues <- c("msoc", "wsoc")

for(league in leagues) {
  piggyback::pb_download(
    file = paste0(league, "_schedules.csv"),
    repo = "uwaggs/usports-data",
    tag = "schedules",
    dest = "data/schedules"
  )

  schedule <- read_file(paste0("data/schedules/", league, "_schedules.csv"))

  links <- schedule |>
    dplyr::filter(box_scores != "", !is.na(box_scores)) |>
    dplyr::pull(box_scores) |>
    unique()

  all_team_box <- data.frame()
  all_player_box <- data.frame()
  all_pbp <- data.frame()

  for(link in links) {
    Sys.sleep(11)

    content = httr::GET(link, httr::user_agent("httr"))
    if (content$status_code != 200) {
      cat("Failed to retrieve:", link, "\nstatus code:", content$status_code)
      next
    }

    webpage <- rvest::read_html(content, encoding = "ISO-8859-1")

    team_box <- usportsscraper::scrape_soc_team_box_score_safe(html = webpage) |> add_info(link)
    player_box <- usportsscraper::scrape_soc_player_box_score_safe(html = webpage) |> add_info(link)
    pbp <- usportsscraper::scrape_soc_play_by_play_safe(html = webpage) |> add_info(link)

    all_team_box <- dplyr::bind_rows(all_team_box, team_box)
    all_player_box <- dplyr::bind_rows(all_player_box, player_box)
    all_pbp <- dplyr::bind_rows(all_pbp, pbp)
  }

  all_team_box |>
    dplyr::mutate(path = stringr::str_glue("data/{league}_team_box/{league}_team_box_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_player_box |>
    dplyr::mutate(path = stringr::str_glue("data/{league}_player_box/{league}_player_box_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_pbp |>
    dplyr::mutate(path = stringr::str_glue("data/{league}_pbp/{league}_pbp_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  sapply(
    unique(all_team_box$season), \(x)
    piggyback::pb_upload(
      file = glue::glue("data/{league}_pbp/{league}_pbp_{x}.csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_team_box"),
      overwrite = TRUE
    )
  )

  sapply(
    unique(all_player_box$season), \(x)
    piggyback::pb_upload(
      file = glue::glue("data/{league}_player_box/{league}_player_box_{x}.csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_player_box"),
      overwrite = TRUE
    )
  )

  sapply(
    unique(all_pbp$season), \(x)
    piggyback::pb_upload(
      file = glue::glue("data/{league}_team_box/{league}_team_box_{x}.csv"),
      repo = "uwaggs/usports-data",
      tag = paste0(league, "_pbp"),
      overwrite = TRUE
    )
  )
}
