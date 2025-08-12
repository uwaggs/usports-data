# library(usportsscraper)
# library(rvest)
# library(tidyverse)
# library(piggyback)
#
# # Function to initialize the schedule
#
# initialize_schedule <- function() {
#
#   # Get year
#   current_year <- as.integer(format(Sys.Date(), "%Y"))
#   current_year_abbreviated <- substr(current_year, 3, 4)
#   sports <- c("mvball", "wvball", "mice", "mbkb", "wbkb", "msoc", "wsoc", "wice", "fh", "fball")
#
#   for(sport in sports) {
#     print(sport)
#
#     # find available seasons
#     seasons <- paste0("https://en.usports.ca/sports/",sport,"/",
#                     current_year-1,"-",
#                     current_year_abbreviated,
#                     "/schedule") %>%
#       read_html() %>%
#       html_elements(".form-control.season-filter.form-select") %>%
#       html_children() %>%
#       html_text2()
#
#     # scrape all schedules
#     schedule <- lapply(seasons, function(season) usportsscraper::scrape_schedule(sport = sport, season = season)) %>%
#       bind_cols()
#
#     write_csv(schedule, paste(sport, "schedules.csv", sep = "_"))
#
#     # upload to releases
#     piggyback::pb_upload(
#       file = paste(sport, "schedules.csv", sep = "_"),
#       repo = "uwaggs/usports-data",
#       tag = "schedules",
#       overwrite = TRUE
#     )
#
#     sys.sleep(10) # Sleep to avoid hitting the server too hard
#   }
# }

initialize_schedule <- function() {
  years <- 2009:(lubridate::year(Sys.Date()) - 1)
  seasons <- paste0(years, "-", substr(years + 1, 3, 4))

  leagues <- c("mbkb", "wbkb", "fh", "fball", "msoc", "wsoc", "mice", "wice", "mvball", "wvball")

  all_schedule <- data.frame()

  for (league in leagues) {
    for(season in seasons) {
      schedule <- usportsscraper::scrape_schedule(league, season)
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
