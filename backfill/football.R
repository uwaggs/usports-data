library(usportsscraper)


# get game id's
leagues <- "fball"

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

  all_returns <- data.frame()
  all_kicking <- data.frame()
  all_offence <- data.frame()
  all_defence <- data.frame()
  all_drive_summaries <- data.frame()
  all_scoring_summaries <- data.frame()
  all_pbp <- data.frame()
  all_team <- data.frame()


  for(link in head(links)) {
    webpage <- tryCatch({
      rvest::read_html(link)
    }, error = function(e) {
      return(NULL)
    })

    if(is.null(link)) next

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

  all_returns |>
    dplyr::mutate(path = stringr::str_glue("data/{league}/returns/{league}_returns_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_kicking |>
    dplyr::mutate(path = stringr::str_glue("data/{league}/kicking/{league}_kicking_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_offence |>
    dplyr::mutate(path = stringr::str_glue("data/{league}/offence/{league}_offence_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_defence |>
    dplyr::mutate(path = stringr::str_glue("data/{league}/defence/{league}_defence_{season}.csv")) |>
    dplyr::group_by(season) |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_drive_summaries |>
    dplyr::mutate(path = stringr::str_glue("data/{league}/drive_summaries/{league}_drive_summaries_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_scoring_summaries |>
    dplyr::mutate(path = stringr::str_glue("data/{league}/scoring_summaries/{league}_scoring_summaries_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_pbp |>
    dplyr::mutate(path = stringr::str_glue("data/{league}/pbp/{league}_pbp_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })

  all_team |>
    dplyr::mutate(path = stringr::str_glue("data/{league}/team/{league}_team_{season}.csv")) |>
    dplyr::group_by(season) |>
    dplyr::group_split() |>
    purrr::walk(~ {
      fs::dir_create(dirname(.x$path[1]))
      readr::write_csv(.x, .x$path[1])
    })
}

sapply(
  unique(all_returns$season), \(x)
  piggyback::pb_upload(
    file = glue::glue("data/{league}/returns/{league}_returns_{x}.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_team_box"),
    overwrite = TRUE
  )
)

sapply(
  unique(all_kicking$season), \(x)
  piggyback::pb_upload(
    file = glue::glue("data/{league}/kicking/{league}_kicking_{x}.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_kicking"),
    overwrite = TRUE
  )
)

sapply(
  unique(all_offence$season), \(x)
  piggyback::pb_upload(
    file = glue::glue("data/{league}/offence/{league}_offence_{x}.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_offence"),
    overwrite = TRUE
  )
)

sapply(
  unique(all_defence$season), \(x)
  piggyback::pb_upload(
    file = glue::glue("data/{league}/defence/{league}_defence_{x}.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_defence"),
    overwrite = TRUE
  )
)

sapply(
  unique(all_drive_summaries$season), \(x)
  piggyback::pb_upload(
    file = glue::glue("data/{league}/drive_summaries/{league}_drive_summaries_{x}.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_drive_summaries"),
    overwrite = TRUE
  )
)

sapply(
  unique(all_scoring_summaries$season), \(x)
  piggyback::pb_upload(
    file = glue::glue("data/{league}/scoring_summaries/{league}_scoring_summaries_{x}.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_scoring_summaries"),
    overwrite = TRUE
  )
)

sapply(
  unique(all_pbp$season), \(x)
  piggyback::pb_upload(
    file = glue::glue("data/{league}/pbp/{league}_pbp_{x}.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_pbp"),
    overwrite = TRUE
  )
)

sapply(
  unique(all_team$season), \(x)
  piggyback::pb_upload(
    file = glue::glue("data/{league}/team/{league}_team_{x}.csv"),
    repo = "uwaggs/usports-data",
    tag = paste0(league, "_team"),
    overwrite = TRUE
  )
)
