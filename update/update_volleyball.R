# Load required libraries
library(usportsscraper)
library(rvest)
library(tidyverse)
library(piggyback)


update_volleyball <- function(league = "usports") {

  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_season <- "..." # figure this out

  leagues <- c("mvball", "wvball")

  for(league in leagues) {

  # Scrape the volleyball schedule for the current season
  schedule <- usportsscraper::scrape_schedule(sport = league, season = paste0(current_year - 1, "-", substr(current_year, 3, 4)))
  existing_team_box <- read_csv(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "/", league, "_team_box.csv"))
  existing_player_box <- read_csv(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "/", league, "_player_box.csv"))
  existing_pbp <- read_csv(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "/", league, "_pbp_", current_season, ".csv"))

  ## check for existing games
  games_to_scrape = schedule %>% filter(date <= Sys.Date(),
                                        !is.na(game_link),
                                        game_link != "",
                                        !(game_link %in% unique(existing_team_box$link))
                                        ) %>%
    pull(game_link)

  # Initialize empty data frames for team box scores, player box scores, and play-by-play data
  all_team_box <- tibble()
  all_player_box <- tibble()
  all_pbp <- tibble()

  # Loop through each game in the schedule
  for (game in games_to_scrape) {

    webpage <- tryCatch({
      rvest::read_html(link)
    }, error = function(e) {
      return(NULL)
    })

    if(is.null(webpage)) next

    team_box <- scrape_vb_team_box_score(html = webpage) %>% add_info(link)
    player_box <- scrape_vb_player_box_score(html = webpage) %>% add_info(link)
    pbp <- scrape_vb_play_by_play(html = webpage)  %>% add_info(link)

    # Combine the data into the main data frames
    all_team_box <- bind_rows(all_team_box, team_box)
    all_player_box <- bind_rows(all_player_box, player_box)
    all_pbp <- bind_rows(all_pbp, pbp)

  }

  all_team_box <- bind_rows(all_team_box, existing_team_box) %>% distinct()
  all_player_box <- bind_rows(all_player_box, existing_player_box) %>% distinct()
  all_pbp <- bind_rows(all_pbp, existing_pbp) %>% distinct()

  # Save the data to CSV files
  write_csv(all_team_box, paste0("data/", league, "/", league, "_team_box.csv"))
  write_csv(all_player_box, paste0("data/", league, "/", league, "_player_box.csv"))
  write_csv(all_pbp, paste0("data/", league, "/", league, "_pbp_", current_season, ".csv"))

  # Upload the data to GitHub releases
  piggyback::pb_upload(
    file = paste0("data/", league, "/", league, "_team_box.csv"),
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
    file = paste0("data/", league, "/", league, "_pbp_",current_season,".csv"),
    repo = "uwaggs/usports-data",
    tag = "volleyball",
    overwrite = TRUE
  )

  }
}






