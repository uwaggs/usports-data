library(usportsscraper)
library(rvest)
library(tidyverse)
library(piggyback)

# Function to initialize the schedule

initialize_schedule <- function() {

  # Get year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_year_abbreviated <- substr(current_year, 3, 4)
  sports <- c("mvball", "wvball", "mice", "mbkb", "wbkb", "msoc", "wsoc", "wice", "fh", "fball")

  for(sport in sports) {
    print(sport)

    # find available seasons
    seasons <- paste0("https://en.usports.ca/sports/",sport,"/",
                    current_year-1,"-",
                    current_year_abbreviated,
                    "/schedule") %>%
      read_html() %>%
      html_elements(".form-control.season-filter.form-select") %>%
      html_children() %>%
      html_text2()

    # scrape all schedules
    schedule <- lapply(seasons, function(season) usportsscraper::scrape_schedule(sport = sport, season = season)) %>%
      bind_cols()

    write_csv(schedule, paste(sport, "schedules.csv", sep = "_"))

    # upload to releases
    piggyback::pb_upload(
      file = paste(sport, "schedules.csv", sep = "_"),
      repo = "uwaggs/usports-data",
      tag = "schedules",
      overwrite = TRUE
    )

    sys.sleep(10) # Sleep to avoid hitting the server too hard
  }
}
