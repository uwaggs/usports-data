library(usportsscraper)


# get game id's

links <- readr::read_csv("mbb_schedule.csv")

for(link in links) {
  html <- tryCatch({
    rvest::read_html(link)
  }, error = function(e) {
    return(NULL)
  })

  start_string <- "boxscores/"
  end_string <- ".xml"

  pattern <- paste0("(?<=", start_string, ").*?(?=", end_string, ")")
  game_id <- stringr::str_extract(link, pattern)

  season <-  stringr::str_extract(link, "\\d{4}-\\d{2}")

  team_box <- scrape_bkb_team_box_score(html)

  player_box <- scrape_bkb_player_box_score(html)

  pbp <- scrape_bkb_play_by_play(html)

  # add identifier columns
  team_box$game_id <- game_id
  team_box$season <- season

  player_box$game_id <- game_id
  player_box$season <- season

  pbp$game_id <- game_id
  pbp$season <- season

  # save the data to releases
  readr::write_csv(team_box, paste0("data/team_box_scores/", season, "/", game_id, "_team_box_score.csv"))
  readr::write_csv(player_box, paste0("data/player_box_scores/", season, "/", game_id, "_player_box_score.csv"))
  readr::write_csv(pbp, paste0("data/play_by_play/", season, "/", game_id, "_play_by_play.csv"))
}



