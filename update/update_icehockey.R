source("R/utils.R")
library(hms)
normalize_player_box <- function(df) {
  df |>
    mutate(
      across(c(goals, assists, plus_minus, shots,
               faceoffs_won, faceoffs_total,
               penalty_minutes, goals_allowed,
               saves, shots_against), as.numeric),
      
      minutes_played = if (inherits(df$minutes_played, "hms") || inherits(df$minutes_played, "difftime")) {
        minutes_played
      } else {
        hms::parse_hms(
          ifelse(
            is.na(minutes_played) | minutes_played == "",
            NA,
            paste0(as.character(minutes_played), ":00")
          )
        )
      }
    )
}
normalize_pbp <- function(df) {

  
  time_cols <- c("time")
  
  df |>
    dplyr::mutate(

      
      # --- Clean time column and keep as character ---
      dplyr::across(
        dplyr::any_of(time_cols),
        ~ {
          x <- as.character(.)
          x <- stringr::str_trim(x)
          x[x == ""] <- NA_character_
          
          # Convert MM:SS:00 -> MM:SS
          x <- sub("^([0-9]+:[0-9]+):.*$", "\\1", x)
          
          x
        }
      )
    )
}
current_year <- as.integer(format(Sys.Date(), "%Y"))
current_month <- lubridate::month(Sys.Date())
current_season <- dplyr::if_else(
  current_month <= 6,
  paste0(current_year - 1, "-", substr(current_year, 3, 4)),
  paste0(current_year, "-", substr(current_year + 1, 3, 4))
)

leagues <- c("mice", "wice")

for(league in leagues) {
  schedule <- scrape_schedule_gamelog(
    league,
    season = current_season
  )
  #
  if(league=="mice"){
   schedule_two=hockey_schedule("m")
  }
  if(league=="wice"){
   schedule_two=hockey_schedule("w")
  }
  
  join_schedule(schedule,schedule_two,league)
  
  existing_player_box <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_player_box/", league, "_player_box_", current_season, ".csv"))
  existing_pbp <- read_file(paste0("https://github.com/uwaggs/usports-data/releases/download/", league, "_pbp/", league, "_pbp_", current_season, ".csv"))
  
  games_to_scrape <-
    schedule |>
    dplyr::filter(
      date <= Sys.Date(),
      !is.na(game_link),
      game_link != "",
      !(game_link %in% unique(existing_player_box$link))
    ) |>
    pull(game_link)
  
 
  all_player_box <- data.frame()
  all_pbp <- data.frame()
  
  for(link in games_to_scrape) {
    Sys.sleep(11)
    
    content = httr::GET(link, httr::user_agent("httr"))
    if (content$status_code != 200) {
      cat("Failed to retrieve:", link, "\nstatus code:", content$status_code)
      next
    }
    
    webpage <- rvest::read_html(content, encoding = "ISO-8859-1")
    
   
    player_box <- usportsscraper::scrape_ice_player_box_score_safe(html = webpage) |> add_info(link)
    pbp <-  usportsscraper::scrape_ice_play_by_play_safe(html = webpage) |> add_info(link)
    
   
    all_player_box <- dplyr::bind_rows(all_player_box, player_box)
    all_pbp <- dplyr::bind_rows(all_pbp, pbp)
  }
  
  all_player_box<-normalize_player_box(all_player_box)
  all_pbp<- normalize_pbp(all_pbp)
 
  all_player_box <- dplyr::bind_rows(all_player_box, normalize_player_box(existing_player_box)) |> distinct()
  all_pbp <- dplyr::bind_rows(all_pbp, normalize_pbp(existing_pbp)) |> distinct()
  
  

readr::write_csv(all_player_box, paste0("data/", league, "_player_box/", league, "_player_box_", current_season, ".csv"))
readr::write_csv(all_pbp, paste0("data/", league, "_pbp/", league, "_pbp_", current_season, ".csv"))

  piggyback::pb_upload(
  file = paste0("data/",league,"_scehdule/",league, "_schedule",".csv"),
  repo = "uwaggs/usports-data",
  tag = "new_schedules,
  overwrite = TRUE
)


piggyback::pb_upload(
  file = paste0("data/", league, "_player_box/", league, "_player_box_", current_season, ".csv"),
  repo = "uwaggs/usports-data",
  tag = paste0(league, "_player_box"),
  overwrite = TRUE
)

piggyback::pb_upload(
  file = paste0("data/", league, "_pbp/", league, "_pbp_", current_season, ".csv"),
  repo = "uwaggs/usports-data",
  tag = paste0(league, "_pbp"),
  overwrite = TRUE
)
}

